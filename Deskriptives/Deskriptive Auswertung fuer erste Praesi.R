pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, 
               cld3, quanteda, tm, rlang, stringdist, rvest, purrr, tidyverse,
               janitor, qdap, wordcloud, gridExtra)
rm(list = ls())

# Aufbereiten zum Mergen

data = fread("lyrics_final_dataset.csv") #Lade dein Datenset
dim(data)
genre_table = fread("Deskriptives/genres_with_subgenres.csv") #Lade das Datenset mit den Subgenres

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
View(df) #Wie man sieht, jedes Genre in einer einzelnen Reihe
df$genres = str_trim(df$genres) #trimmt Leerzeichen
genre_table$subgenre = tolower(genre_table$subgenre) #alles in Kleinbuchstaben
genre_table$genre = tolower(genre_table$genre) #alles in Kleinbuchstaben


getgenre = function(x) {
  overgenres = ifelse(length(genre_table[genre_table$subgenre %in% x,]$genre)>1, #wenn mehr als 1 subgenre (Hard Rock z.B. in mehereren)
                      list(genre_table[genre_table$subgenre %in% x,]$genre), #gib es als Liste aus
                      genre_table[genre_table$subgenre %in% x,]$genre) #sonst nur den einfachen Wert
  temp = ifelse(x %in% genre_table$genre, x, #ist das genre bereits ein Übergenre nimm das
                ifelse(is_empty(overgenres), NA, overgenres)) #gibt es kein Übergenre schreib NA, sonst nimm das, was wir erhalten haben
  return(temp)
}
df$overgenre = lapply(df$genres, getgenre) #anwenden  der Funktion
df1 = df %>% select(1,3) 

#für die Genres, die in Listen ausgegeben werden, wieder in einzelne Spalten
df2 = NULL
for(i in 1:dim(df1)[1]) {
  temp = data.frame(artist = rep(df1[[1]][i], length(unlist(df1[[2]][i]))), 
                    subgenres = rep(0, length(unlist(df1[[2]][i]))))
  subgenre = unlist(df1[[2]][i])
  for(j in 1:length(subgenre)) {
    temp[j,2] = subgenre[j]
  }
  df2 = bind_rows(df2, temp) 
}

#Neuer DF mit übergenres
overgenres_table = df2 %>% filter(!is.na(subgenres)) %>% distinct_at(c("artist", "subgenres")) %>% rename(overgenre = subgenres)

#Count, wie oft wirkliche Subgenres vorkommen
subgenre_table = df %>% group_by(genres) %>% summarise(n = n()) %>% arrange(desc(n))
#Wordcloud davon
wordcloud::wordcloud(words = subgenre_table$genres, freq = subgenre_table$n, colors = c("black", "green", "blue", "red"))

# Wordcloud (und table) über Anzahl der Übergenres die wir extrahiert haben
genre_table = overgenres_table %>% group_by(overgenre) %>% summarise(n = n()) %>% arrange(n)
wordcloud::wordcloud(words = genre_table$overgenre, freq = genre_table$n, colors = c("black", "green", "blue", "red"))


#Wordcloud über Titelanzahl, die wir von den Artists haben
artist_table = data %>% group_by(artist) %>% summarise(n = n()) %>% arrange(desc(n))
wordcloud::wordcloud(words = artist_table$artist, freq = artist_table$n, scale = c(.1,2), max.words = 100, colors = c("purple", "red", "blue"))

#Als Check, falls du dir was antun willst: Artisten, wo ich kein Übergenre gefunden habe
artists_with_overgenre = unlist(unname(overgenres_table %>% distinct(artist)))
all_artists = unlist(unname(df %>% distinct(artist)))
artists_without_overgenre = all_artists[!(all_artists %in% artists_with_overgenre)]

artist_pop = data %>% select(artist, artist_popularity) 

#Künstler mit ihrer Popularität
artist_pop_melt = melt(artist_pop) %>% distinct_at(c("artist", "value"), .keep_all = F) %>% rename(popularity = value)

#siehe ggtitle
ggplot(artist_pop_melt, aes(x = popularity)) + geom_histogram(bins = 20, col = "blue", fill = "red") + ggtitle("Histogramm der Popularität der einzelnen Künstler")

#Aufteilen der Jahre Dekaden
labels = paste0(seq(1941, 2011, 10), "-", seq(1950, 2020, 10))
data$decades = cut(data$year, breaks = seq(1940,2020, by = 10), labels = labels, include.lowest = F, right = T)

#Check, wie sich die Songanzahl über die Jahre/Dekaden verteilt
ggplot(data, aes(x = decades, fill = decades)) + 
  geom_bar(stat = "count") +
  ggtitle("Anzahl der Songs im Datenset aufgeteilt nach Dekaden") +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 
  
#Check, wie sich die Popularität über die Jahre verteilt
ggplot(data, aes(x = year, y = popularity, color = decades)) + geom_point() +
  ggtitle("Popularität der Songs aufgeteilt nach Jahre/Dekaden") + theme_classic()

#Vorbereiten für untere Plots - wie verteilen sich die Artists und die Popularität auf die einzelnen Genres - gibt es Ausreißer?
genre_popularity = left_join(overgenres_table, data %>% select(artist, artist_popularity) %>% distinct(artist, .keep_all = T), by = "artist")
genre_pop_mean = genre_popularity %>% group_by(overgenre) %>% summarise(artists_in_genre = n(), mean_genre_pop = mean(artist_popularity, na.rm = T)) %>% arrange(desc(mean_genre_pop))

q = ggplot(genre_pop_mean, aes(x = reorder(overgenre, -artists_in_genre), y = artists_in_genre, fill = overgenre)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("Anzahl an Künstlern") + xlab("Genre") +
  ggtitle("Anzahl an Künstlern im Genre (absteigend nach Anzahl)")

w = ggplot(genre_pop_mean, aes(x = reorder(overgenre, -mean_genre_pop), y = mean_genre_pop, fill = overgenre)) + geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("Mittlere Genre-Popularität") + xlab("Genre") +
  ggtitle("Mittlere Genrepopularität (absteigend nach Popularität)")

grid.arrange(q,w)
