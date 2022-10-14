pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist)

rm(list = ls())

#insert key here
access_token <- get_spotify_access_token() #kreeirt den Zugangstoken für Spotifyr

#--------------------------------------- Vorbereiten der Daten --------------------------------------#

lyrics <- fread("perfectly_matched_dataset.csv")

colnames(lyrics)[5] <- "artist_id"


#--------------------------------------- Anreichern der Daten --------------------------------------#


#Introducing three new variables 
lyrics$popularity <- rep(0,dim(lyrics)[1]) #value between 0 and 100
lyrics$duration <- rep(0,dim(lyrics)[1]) #duration of the song in milliseconds
lyrics$year <- rep(0,dim(lyrics)[1]) #release date given by various precision (day,month,year)

start_time <- Sys.time()
for (i in 1:dim(lyrics)[1]) {
    track_info <- get_track(lyrics$song_id[i])
    lyrics$popularity[i] <- track_info$popularity
    lyrics$duration[i] <- track_info$duration_ms
    lyrics$year[i] <- track_info$album$release_date
    print(i)
}
end_time <- Sys.time()
end_time - start_time

lyrics$year <- as.numeric(str_sub(lyrics$year,1,4)) #just taking the information about the year
#fwrite(lyrics, "lyrics.csv")

########################## Adding Genre and Popularity of the Artist #####################################

#lyrics <- fread("Enriching_Dataset/lyrics_with_artist_id.csv")
lyrics$artist_genre <- rep("",dim(lyrics)[1])
lyrics$artist_popularity <- rep(0,dim(lyrics)[1])

#Doing the first entry seperately so that the upcoming loop works (indices-wise)
list <- get_artist(lyrics$artist_id[1])
lyrics$artist_genre[1] <- paste(list$genres,collapse = ", ")
lyrics$artist_popularity[1] <- list$popularity

#Looping over all the songs but only updating the list if there is an artist change
#Runs much much faster
for (i in 2:dim(lyrics)[1]){
    if (lyrics$artist_id[i] != lyrics$artist_id[i-1]) {
        list <- get_artist(lyrics$artist_id[i])
    }
    lyrics$artist_genre[i] <- paste(list$genres,collapse = ", ")
    lyrics$artist_popularity[i] <- list$popularity
    print(i)
}

#To check functionality
lyrics %>%
  select(artist,artist_id,artist_popularity) -> artists

################# Changing unit of duration of a song from millisecond to second #############

names(lyrics)[names(lyrics)=="duration"] <- "duration_ms"

lyrics$duration_s <- round(lyrics$duration_ms/1000,0)

lyrics <- lyrics %>% select(artist, songname, year, popularity, duration_s, artist_popularity,
                            artist_genre, original_songname, song_id, artist_id, duration_ms)

fwrite(lyrics, "lyrics_final_corrected.csv")

################################################################################################
# Further Preprocessing

old <- fread("original_dataset.csv")
colnames(old)
colnames(lyrics)
colnames(old) <- c("artist","original_songname", "link", "text")
lyrics_done <- unique(left_join(lyrics, old, by = c("artist","original_songname")))
lyrics_done <- lyrics_done %>% select(-link)

fwrite(lyrics_done, "perfectly_matched_complete_dataset.csv")
