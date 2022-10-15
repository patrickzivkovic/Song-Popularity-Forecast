pacman::p_load(dplyr, data.table, forcats, ggplot2, qdap, tm, udpipe, hunspell, tidytext, parallel, tictoc, tidyr)
rm(list = ls())

data = fread("data/perfectly_matched_complete_dataset.csv")
genres = fread("data/artists_with_overgenre.csv")
genres_top1 = fread("data/artists_with_only_top1_overgenre.csv")

text = data$text
data = data %>% select(artist, songname)

clean_not_sentences_to_sentences = function(x){
  x = tm::removePunctuation(x) #gets rid of punctuation
  x = x[!is.na(qdap::word_count(x))] #gets rid of rows that do not contain words
  for(i in 2:length(x)) { 
    tryCatch({ 
    if(qdap::word_count(x[i])<2 & !is.na(x[i-1])) { 
      x[i-1] = udpipe::txt_collapse(c(x[i-1],x[i]), collapse = " ") 
      x[i] = NA 
    } else if(qdap::word_count(x[i])<2 & is.na(x[i-1])){ 
      x[i-2] = udpipe::txt_collapse(c(x[i-2],x[i]), collapse = " ")
      x[i] = NA 
    } else {next} 
    }, error = function(e) { print(e) }) 
  }
  x = x[!is.na(word_count(x))] 
  x = udpipe::txt_collapse(x, collapse = ". ") 
  return(x)
}

my_punctuation_fun = function(df, text.var) {
  cores = parallel::detectCores() 
  cl = parallel::makePSOCKcluster(names = cores-1) 
  clusterEvalQ(cl = cl, {c(library(qdap), library(tm), library(udpipe))}) 
  text.var = replace_contraction(qprep(text.var, num.paste = T, rm.dash = T)) 
  text.var = str_replace_all(text.var, pattern = "I", replacement = "i") 
  temp1 = str_split(string = text.var, pattern = "\\s(?=[:upper:])|[\\?\\!\\.]") 
  temp1 = parSapply(cl = cl, temp1, clean_not_sentences_to_sentences) 
  df = bind_cols(df, text_clean = temp1) 
  parallel::stopCluster(cl) 
  return(df) 
}

tic()
data = my_punctuation_fun(data, text) 
toc()

data$text_clean = add_incomplete(data$text_clean, silent = T)
data$text_clean = gsub("\\|", ".", data$text_clean)

# fwrite(data, "data/1_data_text_cleaned.csv") #gecleante Daten speichern
# data = fread("data/1_data_text_cleaned.csv")
# data$songname = rvest::repair_encoding(data$songname)


# my_sentSplit = function(x) {
#   y = sentSplit(get("test"), "text_clean") %>% select(songname, text_clean)
#   return(y)
# }

duplicates = data[duplicated(data %>% select("artist", "songname"))] %>% select(artist, songname)
data = data %>% distinct_at(c("artist", "songname"), .keep_all = T) 

#--------------------------------- Coleman Liau ---------------------------------------#

  data$songname = gsub("\\.", "|", data$songname)
  data$artist = gsub("\\.", "|", data$artist)
  
  temp_count = ceiling(dim(data)[1]/1000)
  bottom = 1
  top = 1000
  
  for(i in seq(1,temp_count,1)) { 
    assign(paste0("temp",i), data %>% select(artist, songname, text_clean) %>% slice(bottom:top, .preserve = T))
    bottom = top+1
    top = top + 1000
  }  

get_coleman_liau = function(temp_count) { 
  df_fin = tibble()
  cores = parallel::detectCores() 
  cl = parallel::makePSOCKcluster(names = cores-1) 
  clusterEvalQ(cl = cl, {c(library(qdap), library(dplyr))})
  for(j in paste0("temp", seq(1, temp_count, 1))){
      assign(j, sentSplit(get(j), "text_clean") %>% select(artist, songname, text_clean))
      assign(j, scores(with(get(j), coleman_liau(text_clean, list(artist, songname)))))
      
      df_fin = bind_rows(df_fin, 
                         colsplit2df(get(j), splitcols = "artist&songname", keep.orig = F) 
                         %>% select(artist, songname, Coleman_Liau)) 
      print(paste0("Finished processing", j))                     
      rm(list=eval(j))
      gc()
  }
  parallel::stopCluster(cl)
  return(df_fin)
}

df_new = get_coleman_liau(temp_count) 

toc()


df_new$songname = gsub("\\|", ".", df_new$songname)  
df_new$artist = gsub("\\|", ".", df_new$artist)
#fwrite(df_new, "data/songs_with_colemanlieau.csv")
# df_new = fread("data/songs_with_colemanlieau.csv")
  
#------------------------------------ SMOG Index -----------------------------------------#

data$songname = gsub("\\.", "|", data$songname)
data$artist = gsub("\\.", "|", data$artist)
temp_count = ceiling(dim(data)[1]/1000)
bottom = 1
top = 1000

for(i in seq(1,temp_count,1)) { 
  assign(paste0("temp",i), data %>% select(artist, songname, text_clean) %>% slice(bottom:top, .preserve = T))
  bottom = top+1
  top = top + 1000
}

get_SMOG = function(temp_count) {
  df_fin = tibble()
  cores = parallel::detectCores() 
  cl = parallel::makePSOCKcluster(names = cores-1) 
  clusterEvalQ(cl = cl, {c(library(qdap), library(dplyr))})
  for(j in paste0("temp", seq(1, temp_count, 1))){
    assign(j, sentSplit(get(j), "text_clean") %>% select(artist, songname, text_clean))
    assign(j, scores(with(get(j), SMOG(text_clean, list(artist, songname), output = "all"))))
    
    df_fin = bind_rows(df_fin, 
                       colsplit2df(get(j), splitcols = "artist&songname", keep.orig = F) 
                       %>% select(artist, songname, SMOG)) 
    print(paste0("Finished processing ", j))                     
    rm(list=eval(j))
    gc()
  }
  parallel::stopCluster(cl)
  return(df_fin)
}
tic()
df_SMOG = get_SMOG(temp_count) 
toc()

df_SMOG$songname = gsub("\\|", ".", df_SMOG$songname)  
df_SMOG$artist = gsub("\\|", ".", df_SMOG$artist)
fwrite(df_SMOG, "data/songs_with_SMOG.csv")
#df_new = fread("data/songs_with_SMOG.csv")

data$songname = gsub("\\|", ".", data$songname)  
data$artist = gsub("\\|", ".", data$artist)

#------------------------------ get sentiment --------------------------------------------#

get_sentiment_fun = function(df) {
  
  cores = parallel::detectCores()
  cl = parallel::makePSOCKcluster(names = cores-1) 
  clusterEvalQ(cl = cl, {c(library(tidyr), library(tidytext), library(dplyr))})
  
  nrc = tidytext::get_sentiments(lexicon = "nrc")
  temp_df = tibble(artist = "0", songname = "0", trust = 0, fear = 0, negative = 0, sadness = 0, anger = 0,
                    surprise = 0, positive = 0, disgust = 0, joy = 0, anticipation = 0)
  for(i in 1:dim(df)[1]) {
    temp_as = df[i] %>% select(artist, songname)
    temp_text = df[i] %>% select(text_clean)
    emotion_word = tibble(word = bag_o_words(temp_text)) %>% inner_join(nrc, by = "word")
    emotion_word_count = emotion_word %>% count()
    if(sjmisc::is_empty(emotion_word)){
      done = temp_as
    } else {
      done = bind_cols(temp_as,
                       pivot_wider(emotion_word %>% 
                    group_by(sentiment) %>% 
                    count() %>% 
                    mutate(n = n/unlist(emotion_word_count)),
                  names_from = sentiment, values_from = n))
    }

    temp_df = bind_rows(temp_df, done)
  }
  temp_df = temp_df %>% slice(-1)
  parallel::stopCluster(cl)
  return(temp_df)
}


sentiment_df = get_sentiment_fun(df = data) 

# fwrite(sentiment_df, "data/songs_with_sentiment_percent.csv")

#------------------------------ get sentiment summed --------------------------------------------#

get_sentiment_sum = function(df) {
  
  cores = parallel::detectCores() 
  cl = parallel::makePSOCKcluster(names = cores-1) 
  clusterEvalQ(cl = cl, {c(library(tidyr), library(tidytext), library(dplyr))})
  
  afinn = tidytext::get_sentiments(lexicon = "afinn")
  sentiments = distinct(get_sentiments(lexicon = "loughran") %>% bind_rows(get_sentiments(lexicon = "bing")), word, .keep_all = T)
  temp_df = tibble(artist = "0", songname = "0", negative = 0, neutral = 0, positive = 0)
  for(i in 1:dim(df)[1]) {
    temp_as = df[i] %>% select(artist, songname)
    temp_text = df[i] %>% select(text_clean)
    emotion_word = tibble(word = bag_o_words(temp_text)) %>% inner_join(afinn, by = "word") %>% left_join(sentiments, by = "word")
    emotion_word[is.na(emotion_word$sentiment), 3] <- "neutral"
    emotion_word[!(emotion_word$sentiment == "positive" | emotion_word$sentiment == "negative"), 3] <- "neutral"
    emotion_word_count = emotion_word %>% group_by(sentiment) %>% summarise(n = sum(abs(value)))
    if(sjmisc::is_empty(emotion_word)){
      done = temp_as
    } else {
      done = bind_cols(temp_as,
                       pivot_wider(emotion_word_count,
                                   names_from = sentiment, values_from = n))
    }
    
    temp_df = bind_rows(temp_df, done)
  }
  temp_df = temp_df %>% slice(-1)
  parallel::stopCluster(cl)
  return(temp_df)
}

tic()
sentiment_sum_df = get_sentiment_sum(data) 
toc()
# fwrite(sentiment_sum_df, "data/songs_with_sentiment_summed.csv")

