pacman::p_load(data.table, ggplot2, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist)

rm(list = ls())

lyrics <- fread("data/perfectly_matched_complete_dataset.csv")

summary(as.numeric(lyrics$year)) #mean = 1994

unique_artists <- length(unique(lyrics$artist))
dim(lyrics)[1]/unique_artists #mean number of songs per artist

summary(lyrics$popularity) #mean = 22.86 - apparently a lot of songs are pretty unpopular 


#10 most popular songs - Selena Gomez most popular 
lyrics %>%
  select(artist,songname,year,popularity)%>%
  top_n(n=10,wt=popularity) %>%
  arrange(desc(popularity))

#Quick check how stronlgy artist_popularity and song_popularity are connected to each other
model <- lm(lyrics$artist_popularity~lyrics$popularity)
summary(model)
ggplot(lyrics,aes(artist_popularity,popularity))+
  geom_point(cex=0.1)
cor(lyrics$artist_popularity,lyrics$popularity)
