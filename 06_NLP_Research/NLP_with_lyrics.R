pacman::p_load(dplyr, data.table, forcats, ggplot2, qdap, tm, udpipe, hunspell, tidytext, parallel, tictoc, tidyr)
rm(list = ls())

data = fread("Data-Challenge-Songlyrics/perfectly_matched_complete_dataset.csv")
genres = fread("Data-Challenge-Songlyrics/03_Genre Issue/artists_with_overgenre.csv")
genres_top1 = fread("Data-Challenge-Songlyrics/03_Genre Issue/artists_with_only_top1_overgenre.csv")

# aufteilen in Text und Data mit nur Artist und Song_name (andere brauchen wir in der Zwischenzeit nicht)
text = data$text
data = data %>% select(artist, songname)

# Funktion innerhalb erklärt
clean_not_sentences_to_sentences = function(x){
  #wird später auf durch my_punctuation_fun vorbereitete Spalten angewandt
  x = tm::removePunctuation(x) #entfernt Punktuation
  x = x[!is.na(qdap::word_count(x))] #entfernt Reihen, die keine Wörter enthalten (leer -> NA -> entfernt)
  for(i in 2:length(x)) { #ab Index 2 bis zum Ende
    tryCatch({ #tryCatch nur damit sich die Funktion bei Fehlern nicht komplett aufhängt
    if(qdap::word_count(x[i])<2 & !is.na(x[i-1])) { #Wenn die Reihe nur ein Wort enthält und der Index davpr nicht NA ist
      x[i-1] = udpipe::txt_collapse(c(x[i-1],x[i]), collapse = " ") #vereine dieses eine Wort mit der Reihe davor
      x[i] = NA #setze die aktuelle Spalte auf NA
    } else if(qdap::word_count(x[i])<2 & is.na(x[i-1])){ #wurde if-Kondition bereits angewandt und der nächste Vektor hat wieder nur ein Wort
      x[i-2] = udpipe::txt_collapse(c(x[i-2],x[i]), collapse = " ") #vereine mit dem Index zwei davor
      x[i] = NA #setze aktuelles wieder auf NA
    } else {next} #enthält es mehr als ein Wort, überspringe die Reihe
    }, error = function(e) { print(e) }) #zu tryCatch -> Fehler = nicht abbrechen, sondern print und weiter gehts
  }
  x = x[!is.na(word_count(x))] #Liste wieder um NAs cleanen
  x = udpipe::txt_collapse(x, collapse = ". ") #den Songtext wieder zu einem Charaktervektor mit eingefügter Punktuation vereinen
  return(x)
}

#cleaned den Text parallelisiert, damit es schneller geht 
my_punctuation_fun = function(df, text.var) {
  cores = parallel::detectCores() #schaut, wie viel Cores dein PC hat
  cl = parallel::makePSOCKcluster(names = cores-1) #verwendet alle, bis auf einen Core und macht ein Cluster
  clusterEvalQ(cl = cl, {c(library(qdap), library(tm), library(udpipe))}) #damit die Cluster die Funktionen finden
  text.var = replace_contraction(qprep(text.var, num.paste = T, rm.dash = T)) #entfernt \n, ersetzt Nummern durch ausgeschriebene Nummern ersetzt z.B. i'll durch i will
  text.var = str_replace_all(text.var, pattern = "I", replacement = "i") #ersetzt alle großen I's durch kleine, da Uppercase Indikator für Satzbeginn
  temp1 = str_split(string = text.var, pattern = "\\s(?=[:upper:])|[\\?\\!\\.]") #Trennt Sätze am Leerzeichen vor Uppercase-Wörtern
  temp1 = parSapply(cl = cl, temp1, clean_not_sentences_to_sentences) #Cleaned Eigennamen so gut wie möglich (siehe Beschreibung oben)
  df = bind_cols(df, text_clean = temp1) #fügt die gecleante Textspalte zum DF hinzu
  parallel::stopCluster(cl) #beendet das gemachte Cluster für die Parallele Verarbeitung
  return(df) #gibt fertigen DF zurück
}

tic()
data = my_punctuation_fun(data, text) #braucht auf gesamten Datenset bei mir 648 Sekunden (716 Sekunden mit größerem Set) -> fast 11 Minuten
toc()

data$text_clean = add_incomplete(data$text_clean, silent = T)
data$text_clean = gsub("\\|", ".", data$text_clean)

# fwrite(data, "Data-Challenge-Songlyrics/06_NLP_Research/1_data_text_cleaned.csv") #gecleante Daten speichern
# data = fread("Data-Challenge-Songlyrics/06_NLP_Research/1_data_text_cleaned.csv")
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
  cores = parallel::detectCores() #schaut, wie viel Cores dein PC hat
  cl = parallel::makePSOCKcluster(names = cores-1) #verwendet alle, bis auf einen Core und macht ein Cluster
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

df_new = get_coleman_liau(temp_count) #hat 530 Sekunden gebraucht (~9 Minuten)

toc()


df_new$songname = gsub("\\|", ".", df_new$songname)  
df_new$artist = gsub("\\|", ".", df_new$artist)
#fwrite(df_new, "Data-Challenge-Songlyrics/06_NLP_Research/songs_with_colemanlieau.csv")
# df_new = fread("Data-Challenge-Songlyrics/06_NLP_Research/songs_with_colemanlieau.csv")
  
#------------------------------------ SMOG Index -----------------------------------------#

#vorbereiten der Daten
data$songname = gsub("\\.", "|", data$songname)
data$artist = gsub("\\.", "|", data$artist)
#initialisieren der Datenerstellung
temp_count = ceiling(dim(data)[1]/1000)
bottom = 1
top = 1000

#erstellen der kleineren Files
for(i in seq(1,temp_count,1)) { 
  assign(paste0("temp",i), data %>% select(artist, songname, text_clean) %>% slice(bottom:top, .preserve = T))
  bottom = top+1
  top = top + 1000
}

get_SMOG = function(temp_count) {
  df_fin = tibble()
  cores = parallel::detectCores() #schaut, wie viel Cores dein PC hat
  cl = parallel::makePSOCKcluster(names = cores-1) #verwendet alle, bis auf einen Core und macht ein Cluster
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
df_SMOG = get_SMOG(temp_count) #hat 4626 Sekunden gebraucht (~1h 17 Minuten)
toc()

df_SMOG$songname = gsub("\\|", ".", df_SMOG$songname)  
df_SMOG$artist = gsub("\\|", ".", df_SMOG$artist)
fwrite(df_SMOG, "Data-Challenge-Songlyrics/06_NLP_Research/songs_with_SMOG.csv")
#df_new = fread("Data-Challenge-Songlyrics/06_NLP_Research/songs_with_SMOG.csv")

data$songname = gsub("\\|", ".", data$songname)  
data$artist = gsub("\\|", ".", data$artist)

#------------------------------ get sentiment --------------------------------------------#

get_sentiment_fun = function(df) {
  
  cores = parallel::detectCores() #schaut, wie viel Cores dein PC hat
  cl = parallel::makePSOCKcluster(names = cores-1) #verwendet alle, bis auf einen Core und macht ein Cluster
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


sentiment_df = get_sentiment_fun(df = data) #unparallelisiert: 862.34 (~14 Minuten) Sekunden, parallelisiert ziemlich gleich - ist aber auch nicht optimiert

# fwrite(sentiment_df, "Data-Challenge-Songlyrics/06_NLP_Research/songs_with_sentiment_percent.csv")

#------------------------------ get sentiment summed --------------------------------------------#

get_sentiment_sum = function(df) {
  
  cores = parallel::detectCores() #schaut, wie viel Cores dein PC hat
  cl = parallel::makePSOCKcluster(names = cores-1) #verwendet alle, bis auf einen Core und macht ein Cluster
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
sentiment_sum_df = get_sentiment_sum(data) # hat 790 Sekunden gedauert (~13 Minuten)
toc()
# fwrite(sentiment_sum_df, "Data-Challenge-Songlyrics/06_NLP_Research/songs_with_sentiment_summed.csv")

