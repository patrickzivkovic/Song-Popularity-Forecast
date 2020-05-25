pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, 
               cld3, quanteda, tm, rlang, stringdist, rvest, purrr, tidyverse,
               janitor, qdap, wordcloud, gridExtra, hunspell)
rm(list = ls())

# Aufbereiten zum Mergen

data = fread("Data-Challenge-Songlyrics/perfectly_matched_complete_dataset.csv") #Lade dein Datenset
genre_table = fread("Data-Challenge-Songlyrics/03_Genre Issue/genres_with_subgenres.csv") #Lade das Datenset mit den Subgenres
genre_wiki = fread("Data-Challenge-Songlyrics/03_Genre Issue/overgenres_from_wikipedia.csv")

genre_table$genre = repair_encoding(genre_table$genre, from = "UTF-8")

artist_genre = data %>% distinct(artist, .keep_all = T) %>% select(artist, artist_genre) #Vom finalen Datenset die Genres der Künstler holen
artist_genre$artist_genre = strsplit(artist_genre$artist_genre, split = ",") #Spliten der Genres, um sie weiterverarbeiten zu können

df = NULL #Df initialisieren
for(i in 1:dim(artist_genre)[1]) { #für alle Künstler
  temp = data.frame(artist = rep(artist_genre[[1]][i], length(unlist(artist_genre[[2]][i]))), #Data frame initialisieren, je nach Anzahl der Genres
                    genres = rep(0, length(unlist(artist_genre[[2]][i]))))
  genres = unlist(artist_genre[[2]][i]) #genre aufspalten
  for(j in 1:length(genres)) {
    temp[j,2] = genres[j] #die Genres in einzelne Spalten geben
  }
  df = bind_rows(df, temp) #die Spalten dazumergen
}

df$genres = str_trim(df$genres) #trimmt Leerzeichen
genre_table$subgenre = tolower(genre_table$subgenre) #alles in Kleinbuchstaben
genre_table$genre = tolower(genre_table$genre) #alles in Kleinbuchstaben
genre_wiki$subgenre = tolower(genre_wiki$subgenre)
genre_wiki$overgenre = tolower(genre_wiki$overgenre)

getgenre = function(x) {
  overgenres = ifelse(length(genre_table[genre_table$subgenre %in% x,]$genre)>1, #wenn mehr als 1 subgenre (Hard Rock z.B. in mehereren)
                      list(genre_table[genre_table$subgenre %in% x,]$genre), #gib es als Liste aus
                      genre_table[genre_table$subgenre %in% x,]$genre) #sonst nur den einfachen Wert
  temp = ifelse(x %in% genre_table$genre, x, #ist das genre bereits ein Übergenre nimm das
                ifelse(is_empty(overgenres), NA, overgenres)) #gibt es kein Übergenre schreib NA, sonst nimm das, was wir erhalten haben
  return(temp)
}

getgenre_wiki = function(x) {
  overgenres = ifelse(length(genre_wiki[genre_wiki$subgenre %in% x,]$overgenre)>1, #wenn mehr als 1 subgenre (Hard Rock z.B. in mehereren)
                      list(genre_wiki[genre_wiki$subgenre %in% x,]$overgenre), #gib es als Liste aus
                      genre_wiki[genre_wiki$subgenre %in% x,]$overgenre) #sonst nur den einfachen Wert
  temp = ifelse(x %in% genre_wiki$overgenre, x, #ist das genre bereits ein Übergenre nimm das
                ifelse(is_empty(overgenres), NA, overgenres)) #gibt es kein Übergenre schreib NA, sonst nimm das, was wir erhalten haben
  return(temp)
}

df$overgenre_wiki = lapply(df$genres, getgenre_wiki) #anwenden  der Funktion

df$overgenre = lapply(df$genres, getgenre) #anwenden  der Funktion
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
#für die Genres, die in Listen ausgegeben werden, wieder in einzelne Spalten
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

fwrite(genre_reduced, "Data-Challenge-Songlyrics/03_Genre Issue/artists_with_only_top1_overgenre.csv")
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
# fwrite(df %>% select(1,2), "subgenre_in _set_list.csv")

# #Count, wie oft wirkliche Subgenres vorkommen
# subgenre_table = df %>% group_by(genres) %>% summarise(n = n()) %>% arrange(desc(n))
# #Wordcloud davon
# wordcloud::wordcloud(words = subgenre_table$genres, freq = subgenre_table$n, colors = c("black", "green", "blue", "red"))
# 
# # Wordcloud (und table) über Anzahl der Übergenres die wir extrahiert haben
# genre_table = overgenres_table %>% group_by(overgenres) %>% summarise(n = n()) %>% arrange(n)
# wordcloud::wordcloud(words = genre_table$overgenres, freq = genre_table$n, colors = c("black", "green", "blue", "red"))
# 
# 
# #Wordcloud über Titelanzahl, die wir von den Artists haben
# artist_table = data %>% group_by(artist) %>% summarise(n = n()) %>% arrange(desc(n))
# wordcloud::wordcloud(words = artist_table$artist, freq = artist_table$n, scale = c(.1,2), max.words = 100, colors = c("purple", "red", "blue"))
# 
# #------------------------------- Check, welche Subgenres nicht gefunden wurden ---------------------------#
# 
# #Als Check, falls du dir was antun willst: Artisten, wo ich kein Übergenre gefunden habe
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
# 
# 
# artist_pop = data %>% select(artist, artist_popularity) 
# 
# #Künstler mit ihrer Popularität
# artist_pop_melt = melt(artist_pop) %>% distinct_at(c("artist", "value"), .keep_all = F) %>% rename(popularity = value)
# 
# #siehe ggtitle
# ggplot(artist_pop_melt, aes(x = popularity)) + geom_histogram(bins = 20, col = "blue", fill = "red") + ggtitle("Histogramm der Popularität der einzelnen Künstler")
# 
# #Aufteilen der Jahre Dekaden
# labels = paste0(seq(1941, 2011, 10), "-", seq(1950, 2020, 10))
# data$decades = cut(data$year, breaks = seq(1940,2020, by = 10), labels = labels, include.lowest = F, right = T)
# 
# #Check, wie sich die Songanzahl über die Jahre/Dekaden verteilt
# ggplot(data, aes(x = decades, fill = decades)) + 
#   geom_bar(stat = "count") +
#   ggtitle("Anzahl der Songs im Datenset aufgeteilt nach Dekaden") +
#   theme_bw() +
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 
#   
# #Check, wie sich die Popularität über die Jahre verteilt
# ggplot(data, aes(x = year, y = popularity, color = decades)) + geom_point() +
#   ggtitle("Popularität der Songs aufgeteilt nach Jahre/Dekaden") + theme_classic()
# 
# #Vorbereiten für untere Plots - wie verteilen sich die Artists und die Popularität auf die einzelnen Genres - gibt es Ausreißer?
# genre_popularity = left_join(overgenres_table, data %>% select(artist, artist_popularity) %>% distinct(artist, .keep_all = T), by = "artist")
# genre_pop_mean = genre_popularity %>% group_by(overgenre) %>% summarise(artists_in_genre = n(), mean_genre_pop = mean(artist_popularity, na.rm = T)) %>% arrange(desc(mean_genre_pop))
# 
# q = ggplot(genre_pop_mean, aes(x = reorder(overgenre, -artists_in_genre), y = artists_in_genre, fill = overgenre)) + geom_bar(stat = "identity") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("Anzahl an Künstlern") + xlab("Genre") +
#   ggtitle("Anzahl an Künstlern im Genre (absteigend nach Anzahl)")
# 
# w = ggplot(genre_pop_mean, aes(x = reorder(overgenre, -mean_genre_pop), y = mean_genre_pop, fill = overgenre)) + geom_bar(stat = "identity")+ 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("Mittlere Genre-Popularität") + xlab("Genre") +
#   ggtitle("Mittlere Genrepopularität (absteigend nach Popularität)")
# 
# grid.arrange(q,w)
