pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist)

#Reading the original dataset
original_df <- fread("data/original_dataset.csv")
original_df <- original_df %>% select(-link)

#Reading all the tracks that Spotify could extract from all the artist in the original_df
complete_spotify <- fread("data/all_tracks_from_spotify.csv")

############### Matching over the complete Dataset ##########################

#Extracting all unique artists from the original dataset
unique_artists <- original_df %>% distinct(artist)

#Creating a shell for the final dataframe
full_df <- NULL

for (i in 1:nrow(unique_artists)){
    
    #Extracting all the songs of the current artist from the original and the complete dataset
    artist_in_original <- original_df %>% filter(artist == pull(unique_artists[i])) %>% select(artist, song)
    artist_in_complete <- complete_spotify %>% filter(artist == pull(unique_artists[i])) %>% select(id, artist, name)
    
    #Trying to match each song from the current artist from the orginal to the complete dataset
    #Taking maxDist = stringlength of songname * 0.15 - 15% of the length of the song
    index <- rep(0,nrow(artist_in_original))
    for (j in 1:nrow(artist_in_original)){
      custom_max_dist <- floor(str_count(artist_in_original[j,]$song)*0.15)
      if (custom_max_dist == 0){custom_max_dist = 1}
      index[j] <- amatch(artist_in_original[j,]$song, artist_in_complete$name, maxDist = custom_max_dist)
    }
    
    #Putting everything together
    matched_songs <- artist_in_original[!is.na(index),]
    matched_songs_plus_info <- bind_cols(matched_songs, artist_in_complete[index[!is.na(index)],-2])
    
    #Binding the rows after one artist is done
    full_df <- bind_rows(full_df, matched_songs_plus_info)
}

#A little embellishing
final_matched_songs <- full_df %>% select(artist,Original_Songname=song,Songname = name,Song_ID = id)

#No evident problems with small or long songnames
potential_problems_small <- final_matched_songs[which(str_count(final_matched_songs$Original_Songname)<4),]
potential_problems_big <- final_matched_songs[which(str_count(final_matched_songs$Original_Songname)>40),]

#All changes that were made during the matching
changes <- final_matched_songs[which(final_matched_songs$Original_Songname != final_matched_songs$Matched_Songname),]

#Match the artist IDs in order to add variables to the dataset
artist_ids <- fread("data/artist_ids.csv")
final <- left_join(final_matched_songs,artist_ids, by= "artist")
colnames(final) <- c("artist", "original_songname", "songname", "song_id", "artist_id")

#Write the final dataset 
fwrite(final, "data/perfectly_matched_dataset.csv")
