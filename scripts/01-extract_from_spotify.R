pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist)

rm(list = ls())

#insert keys here
access_token <- get_spotify_access_token() #creates the authorization token

#--------------------------------------- Preparing the data --------------------------------------#

lyrics <- fread("data/original_dataset.csv") #reads the file from Kaggle
lyrics$language = detect_language(lyrics$text) #Learns about the languages of the lyrics and saves it
table(lyrics$language)
lyrics %>% filter(language == "de") %>% select(artist, song, language) #German songs
lyrics = lyrics %>% filter(language == "en") %>% select(-language,-link) #Choose all songs with English lyrics

#------------------------------------------- Procedure -----------------------------------------#

#Data in Spotify is given as follows: Having an artist it is possible to query -> ArtistID -> 
#with the ArtistID we can get the AlbumIDs of the artist -> with the AlbumIDs we can access the individual SongIDs
#This is done in the next couple of steps

#--------------------------------------- Get the ArtistID using the artist name  --------------------------------------#

#Place all artists in a vector to extract the ArtistID
unique_artist = pull(lyrics %>% distinct(artist),artist) 
#Create data set with column 1: name of the artist, 
#column 2: the corresponding ArtistID, 
#column 3: done successfully?
# -> for the following R-loop:

artist_ids = data.frame(artist = unique_artist, 
                    id = rep(0, length(unique_artist)), 
                    success = rep(FALSE, length(unique_artist))) 

max_tries = 3 #3 tries to query the inforamation from Spotify

for(try in 1:max_tries) {
  for (i in 1:length(unique_artist)) { #Repeat the loop for each artist
    if(artist_ids[i,2] != 0) {next} #If the ArtistID for the current artist was already found in a previous run, skip!
    
    tryCatch({ 
      #finds the corresponding ArtistID
      artist_ids[i,2] <- pull(search_spotify(unique_artist[i], type = "artist", limit = 1) %>% select(id),id) #saves the ID in the dataframe
      artist_ids[i,3] = TRUE #mark try to get the ID as successful
    }, error = function(e) { print(e) }) #Print the error if something went wrong for this particular artist
    print(i)
  }
}

#search_spotify("Led Zeppelin","artist")
#search_spotify("Stairway to Heaven","track")
get_artist_albums(search_spotify("Twenty One Pilots","artist")[1,]$id, limit = 50)

filter(artist_ids, success == FALSE) #Filter for artist for which the ID extraction was not sucessful
artist_id_success = artist_ids %>% filter(success == T) %>% select(-success) #save all successful ones to a new dataset
fwrite(artist_id_success, "data/artist_ids.csv")

#--------------------------------------- Get the AlbumIDs via the ArtistIDs  --------------------------------------#

#Create empty list
album_list = list(NULL)
#A dataframe to check if query was successful
suc_table = data.frame(artist = artist_id_success[,1], success = rep(FALSE,dim(artist_id_success)[1]))
album_number <- rep(0,dim(artist_id_success)[1])
ids = NULL
ids1 = NULL
ids2 = NULL


for(j in 1:dim(artist_id_success)[1]) { #Loop that gets all AlbumIDs associated with an ArtistID
  tryCatch({ 
  ids1 = unname(unlist(get_artist_albums(artist_id_success[j,2], limit = 50) %>% #gets all albums from a particular artist (limit = 50)
                        filter(album_type == "album" | album_type == "single") %>% select(id))) #only retains unique albums (the oldest one)
  if (length(ids1) == 50) { #if the artist has more than 50 albums we query 50 more
    ids2 = unname(unlist(get_artist_albums(artist_id_success[j,2], limit = 50,offset = 50) %>%
                           filter(album_type == "album" | album_type == "single") %>% #filters for albums that are only either albums or singles
                           arrange(release_date) %>% distinct(name, .keep_all = T) %>% select(id))) #only retains unique albums (the oldest one)
  }
  ids = c(ids1,ids2)
  album_number[j] <- length(ids) #keep track of the number of albums
  suc_table[j,2] = TRUE #write success if successful
  }, error = function(e) { print(e) }) 
  album_list[[j]] = rep(0, length(ids)) #initialize a list for one artist
  for(i in 1:length(ids)) {
    album_list[[j]][i] = ids[i] #write all the AlbumIDs
  }
  print(j)
  ids2 = NULL
}

#album_number[which(album_number >= 50)]
#unique_artist[which(album_number >= 50)]
#which(unique_artist == "Elvis Presley")

#get_artist_albums(artist_id_success[1,2])

names(album_list) = pull(artist_id_success,artist) 

#--------------------------------------- Get all the SongIDs from the AlbumIDs  --------------------------------------#

#initialize empty vector
ids_tracks = NULL
all_tracks_spotify = NULL
lyrics_with_ids = data.frame()

for(i in 511:length(album_list)) { #Loop over all the albums
  interpret = names(album_list)[i]  
  in_lyrics = pull(lyrics %>% filter(artist == interpret),song) 
  
  for(j in album_list[[i]]) { #get all the SongIDs for all the artist's albums
    tryCatch({ids_tracks = bind_rows(ids_tracks, get_album_tracks(j) %>% select(id, name))
              } 
             , error = function(e) { print(e) })
  }
  ids_tracks$artist <- interpret
  ids_tracks = unique(ids_tracks)
  all_tracks_spotify <- bind_rows(all_tracks_spotify,ids_tracks) #Save all songs from Spotify in extra dataframe
  
  #Compare song names from the original dataset with those from the Spotify dataset
  #Match them if the distance between those strings falls under a certain value
  index = amatch(in_lyrics,unname(unlist(ids_tracks$name)), maxDist = 2) 
  
  song = in_lyrics[!is.na(index)] #Remove all songs that did not have a match
  song_id = ids_tracks[index[!is.na(index)],]$id 
  song_name = ids_tracks[index[!is.na(index)],]$name 
  artist = rep(interpret, length(index[!is.na(index)])) 
  
  #Combine the whole dataset
  lyrics_with_ids = bind_rows(lyrics_with_ids, data.frame(artist, song, song_name, song_id)) 
  ids_tracks = NULL 
  print(i)
}


#Note: The dataset contained 57044 songs, now it only contains 34016 - however these are clean and successfully matched!
lyrics_final = lyrics_with_ids %>% left_join(lyrics, by = c("artist","song"))
fwrite(lyrics_final, "data/lyrics_more_albums.csv")
str(lyrics_final)
#34116/57044 #percentage of succefully joined songs from the Spotify API
#mit Stringdist = 3 erhöhen wir die Zahl von 34116 auf 35383

#Adding artist_id to dataframe
artist_id_success[2,1]
lyrics_final$artist_id <- ""
for (i in 1:dim(artist_id_success)[1]){
  artist <- artist_id_success[i,1]
  artist_id <- artist_id_success[i,2]
  lyrics_final$artist_id[lyrics_final$artist == artist] <- artist_id
}
fwrite(lyrics_final,"data/lyrics_more_albums_with_artist_id.csv")

#Check which songs had to be corrected using the matching method
check <- lyrics_final[lyrics_final$song != lyrics_final$song_name,] 
stringdist_correction <- check
fwrite(stringdist_correction, "data/stringdist_correction.csv")

#Takes the songs out, that were not matched
`%out%` <- function(a,b) ! a %in% b
lyrics_final %>%
  select(artist,song) -> lyrics_final_compare
lyrics %>%
  select(artist,song) -> lyrics_original_compare
compare <- anti_join(lyrics_original_compare,lyrics_final_compare)

fwrite(compare,"data/Unmatched_Songs.csv")

fwrite(all_tracks_spotify,"data/all_tracks_from_spotify.csv")
