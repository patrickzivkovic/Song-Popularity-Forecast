pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, 
               cld3, quanteda, tm, rlang, stringdist, rvest, purrr, tidyverse,
               janitor, qdap, wordcloud, gridExtra, hunspell)
rm(list = ls())

# Prepare to merge

data = fread("data/perfectly_matched_complete_dataset.csv") 
genre_table = fread("data/genres_with_subgenres.csv")
genre_wiki = fread("data/overgenres_from_wikipedia.csv")

genre_table$genre = repair_encoding(genre_table$genre, from = "UTF-8")

artist_genre = data %>% distinct(artist, .keep_all = T) %>% select(artist, artist_genre) #get the genres from the artists from the final dataset
artist_genre$artist_genre = strsplit(artist_genre$artist_genre, split = ",") #split the genres for further analysis

df = NULL #initialize data
for(i in 1:dim(artist_genre)[1]) { #for all artists
  temp = data.frame(artist = rep(artist_genre[[1]][i], length(unlist(artist_genre[[2]][i]))),
                    genres = rep(0, length(unlist(artist_genre[[2]][i]))))
  genres = unlist(artist_genre[[2]][i])
  for(j in 1:length(genres)) {
    temp[j,2] = genres[j] #put genres in individual columns
  }
  df = bind_rows(df, temp) #merge the columns
}

df$genres = str_trim(df$genres) #cuts white-spaces
genre_table$subgenre = tolower(genre_table$subgenre) #lower-case
genre_table$genre = tolower(genre_table$genre) #lower-case
genre_wiki$subgenre = tolower(genre_wiki$subgenre)
genre_wiki$overgenre = tolower(genre_wiki$overgenre)

getgenre = function(x) {
  overgenres = ifelse(length(genre_table[genre_table$subgenre %in% x,]$genre)>1, 
                      list(genre_table[genre_table$subgenre %in% x,]$genre),
                      genre_table[genre_table$subgenre %in% x,]$genre) 
  temp = ifelse(x %in% genre_table$genre, x, 
                ifelse(is_empty(overgenres), NA, overgenres)) 
  return(temp)
}

getgenre_wiki = function(x) {
  overgenres = ifelse(length(genre_wiki[genre_wiki$subgenre %in% x,]$overgenre)>1, 
                      list(genre_wiki[genre_wiki$subgenre %in% x,]$overgenre), 
                      genre_wiki[genre_wiki$subgenre %in% x,]$overgenre) 
  temp = ifelse(x %in% genre_wiki$overgenre, x, 
                ifelse(is_empty(overgenres), NA, overgenres)) 
  return(temp)
}

df$overgenre_wiki = lapply(df$genres, getgenre_wiki) #apply the function

df$overgenre = lapply(df$genres, getgenre)
not_matched_sub = df %>% filter(is.na(overgenre_wiki)&is.na(overgenre)) %>% distinct(genres) %>% arrange(genres)

parsed = tibble(words = unlist(hunspell_parse(unlist(not_matched_sub)))) %>% group_by(words) %>% summarise(n = n()) %>% arrange(desc(n))



convert_unknown_genre = function(x) {
  temp = tibble(subgenres = x[[1]], overgenre = rep(list(NULL), length(x[[1]])))
  for(i in 1:dim(x)[1]) { 
    words = unlist(hunspell_parse(x[i,1]))
    if("pop" %in% words){
      temp[i,2] = "pop"
    } else if("mellow" %in% words) {
      temp[i,2] = "rock"
    } else if("rock" %in% words) {
      temp[i,2] = "rock"
    } else if("metal" %in% words){
      temp[i,2] = "heavy metal"
    } else if("indie" %in% words){
      temp[i,2] = "indie"
    } else if("rap" %in% words){
      temp[i,2] = "hip hop"
    } else if("hip" %in% words){
      temp[i,2] = "hip hop"
    } else if("punk" %in% words) {
      temp[i,2] = "punk rock"
    } else if("country" %in% words){
      temp[i,2] = "country"
    } else if("reggae" %in% words) {
      temp[i,2] = "caribbean and caribbean-influenced"
    } else if("comedy" %in% words) {
        temp[i,2] = "comedy"
    } else if("dance" %in% words) {
      temp[i,2] = "electronic"
    } else if("folk" %in% words) {
        temp[i,2] = "folk"
    } else if("christian" %in% words) {
      temp[i,2] = "christian"
    } else if("soul" %in% words) {
      temp[i,2] = "r&b and soul"
    } else if("r" %in% words & "b" %in% words) {
      temp[i,2] = "r&b and soul"
    } else if("comic" %in% words) {
      temp[i,2] = "comedy"
    } else if("trap" %in% words) {
      temp[i,2] = "hip hop"
    } else if("hardcore" %in% words) {
        temp[i,2] = "punk rock"
    } else {
      temp[i,2] = NA
    }
    }
  return(temp)
}


genre_merge_list = convert_unknown_genre(df %>% filter(is.na(overgenre_wiki)) %>% distinct(genres))

df_to_merge = df %>% filter(is.na(overgenre_wiki)) %>%
  left_join(genre_merge_list %>% rename(og = overgenre), by = c("genres" = "subgenres")) %>% 
  select(-overgenre_wiki) %>% rename(overgenre_wiki = og)

df = df %>% filter(!is.na(overgenre_wiki)) 
df = bind_rows(df, df_to_merge)

df_to_merge2 = df %>% filter(is.na(overgenre_wiki) & !is.na(overgenre)) %>% select(-overgenre_wiki) %>% rename(overgenre_wiki = overgenre)
df1 = df %>% filter(!(is.na(overgenre_wiki) & !is.na(overgenre))) %>% select(-overgenre) %>% bind_rows(df_to_merge2)

df2 = NULL
for(i in 1:dim(df1)[1]) {
  temp = data.frame(artist = rep(df1[[1]][i], length(unlist(df1[[3]][i]))), 
                    overgenres = rep(0, length(unlist(df1[[3]][i]))))
  subgenre = unlist(df1[[3]][i])
  for(j in 1:length(subgenre)) {
    temp[j,2] = subgenre[j]
  }
  df2 = bind_rows(df2, temp) 
}

df2$overgenres = forcats::fct_collapse(df2$overgenres,
                                                    "christian" = c("christian", "inspirational – christian & gospel"),
                                                    "hip-hop" = c("hip-hop/rap", "hip hop"),
                                                    "r&b and soul" = c("r&b/soul", "r&b and soul"),
                                                    "rock" = c("rock", "rockabilly"),
                                                    "world" = c("southeast asian", "world", "east asian", "latin", "flamenco"),
                                                    "electronic" = c("electronic", "house", "new age"),
                                                    "pop" = c("pop", "vocal"))
df2$overgenres = fct_explicit_na(df2$overgenres, na_level = "Various")
genre_reduced = df2 %>% group_by(artist, overgenres) %>% summarise(n = n()) %>% top_n(1,n) %>% select(-n)
various_artists = unname(unlist(genre_reduced %>% group_by(artist) %>% summarise(n = n()) %>% filter(n > 1) %>% select(artist)))
genre_reduced[genre_reduced$artist %in% various_artists,]$overgenres = "Various" 
genre_reduced = droplevels(genre_reduced %>% distinct_at(c("artist", "overgenres")))
table(genre_reduced$overgenres)

fwrite(genre_reduced, "data/artists_with_only_top1_overgenre.csv")
#Neuer DF mit übergenres
overgenres_table = df2 %>% filter(!is.na(overgenres)) %>% distinct_at(c("artist", "overgenres")) %>% arrange(artist)

overgenres_table$overgenres = forcats::fct_collapse(overgenres_table$overgenres,
                      "christian" = c("christian", "inspirational – christian & gospel"),
                      "hip-hop" = c("hip-hop/rap", "hip hop"),
                      "r&b and soul" = c("r&b/soul", "r&b and soul"),
                      "rock" = c("rock", "rockabilly"),
                      "world" = c("southeast asian", "world", "east asian", "latin", "flamenco"),
                      "electronic" = c("electronic", "house", "new age"),
                      "pop" = c("pop", "vocal"))

fwrite(overgenres_table, "Data-Challenge-Songlyrics/03_Genre Issue/artists_with_overgenre.csv")
# fwrite(df %>% select(1,2), "data/subgenre_in _set_list.csv")

# #Count how often subgenres appear
# subgenre_table = df %>% group_by(genres) %>% summarise(n = n()) %>% arrange(desc(n))
# #Wordcloud 
# wordcloud::wordcloud(words = subgenre_table$genres, freq = subgenre_table$n, colors = c("black", "green", "blue", "red"))
# 
# # Wordcloud and table for all genres extracted
# genre_table = overgenres_table %>% group_by(overgenres) %>% summarise(n = n()) %>% arrange(n)
# wordcloud::wordcloud(words = genre_table$overgenres, freq = genre_table$n, colors = c("black", "green", "blue", "red"))
# 
# 
# #Wordcloud of number of titles from artists
# artist_table = data %>% group_by(artist) %>% summarise(n = n()) %>% arrange(desc(n))
# wordcloud::wordcloud(words = artist_table$artist, freq = artist_table$n, scale = c(.1,2), max.words = 100, colors = c("purple", "red", "blue"))
# 
# #------------------------------- Check which subgenres were not found ---------------------------#
# 
# 
# artists_with_overgenre = unlist(unname(overgenres_table %>% distinct(artist)))
# all_artists = unlist(unname(df %>% distinct(artist)))
# artists_without_overgenre = all_artists[!(all_artists %in% artists_with_overgenre)]
# not_hit_genres = df %>% filter(artist %in% artists_without_overgenre) %>% distinct_at(c("genres"))
# not_hit_genres = tibble(subgenre = not_hit_genres[!is.na(not_hit_genres),])
# 
# 
# # not_hit_genres$parsed = unname(sapply(not_hit_genres$subgenre, hunspell_parse))
# # not_hit_genres$stemmed = sapply(not_hit_genres$parsed, function(x){unlist(hunspell_stem(unlist(x)))})
# # 
# # parsed = hunspell_parse(unlist(not_hit_genres))
# # stemmed = hunspell_stem(unlist(parsed))
# # 
# # not_hit_stems = NULL
# # for(i in 1:dim(not_hit_genres)[1]) {
# #   temp = data.frame(subgenre = rep(not_hit_genres[[1]][i], length(unlist(not_hit_genres[[2]][i]))), 
# #                     subgenre_stem = rep(0, length(unlist(not_hit_genres[[2]][i]))))
# #   subgenre = unlist(not_hit_genres[[2]][i])
# #   for(j in 1:length(subgenre)) {
# #     temp[j,2] = subgenre[j]
# #   }
# #   not_hit_stems = bind_rows(not_hit_stems, temp) 
# # }
# # 
# # not_hit_stems$overgenre = sapply(not_hit_stems$subgenre_stem, getgenre)
# 
# #-------------------------------------------------------------------------------------------------------#
# 
# #Visualizations
# 
# artist_pop = data %>% select(artist, artist_popularity) 
# 
# #artists with their popularity
# artist_pop_melt = melt(artist_pop) %>% distinct_at(c("artist", "value"), .keep_all = F) %>% rename(popularity = value)
# 
# 
# ggplot(artist_pop_melt, aes(x = popularity)) + geom_histogram(bins = 20, col = "blue", fill = "red") + ggtitle("Histogramm der Popularität der einzelnen Künstler")
# 
# #split years in decades
# labels = paste0(seq(1941, 2011, 10), "-", seq(1950, 2020, 10))
# data$decades = cut(data$year, breaks = seq(1940,2020, by = 10), labels = labels, include.lowest = F, right = T)
# 
# #check how the number of songs develops across years
# ggplot(data, aes(x = decades, fill = decades)) + 
#   geom_bar(stat = "count") +
#   ggtitle("Number of songs in the dataset per year") +
#   theme_bw() +
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 
#   
# #check how the popularity develops across years
# ggplot(data, aes(x = year, y = popularity, color = decades)) + geom_point() +
#   ggtitle("Popularity of songs by years/decades") + theme_classic()
# 
# 
# genre_popularity = left_join(overgenres_table, data %>% select(artist, artist_popularity) %>% distinct(artist, .keep_all = T), by = "artist")
# genre_pop_mean = genre_popularity %>% group_by(overgenre) %>% summarise(artists_in_genre = n(), mean_genre_pop = mean(artist_popularity, na.rm = T)) %>% arrange(desc(mean_genre_pop))
# 
# q = ggplot(genre_pop_mean, aes(x = reorder(overgenre, -artists_in_genre), y = artists_in_genre, fill = overgenre)) + geom_bar(stat = "identity") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("Anzahl an Künstlern") + xlab("Genre") +
#   ggtitle("Number of artists per genre")
# 
# w = ggplot(genre_pop_mean, aes(x = reorder(overgenre, -mean_genre_pop), y = mean_genre_pop, fill = overgenre)) + geom_bar(stat = "identity")+ 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("Mittlere Genre-Popularität") + xlab("Genre") +
#   ggtitle("Average genre popularity")
# 
# grid.arrange(q,w)
