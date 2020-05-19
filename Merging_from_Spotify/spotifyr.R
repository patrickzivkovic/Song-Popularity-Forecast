pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist)

rm(list = ls())

Sys.setenv(SPOTIFY_CLIENT_ID = '70318fd33b4b4a2c82f52223f00c8a9e') #Setzt die Spotify_Client_ID
Sys.setenv(SPOTIFY_CLIENT_SECRET = '93f79360b69348dbbc9adb64c10bd6be') #Setzt das Spotify_client_secret
access_token <- get_spotify_access_token() #kreeirt den Zugangstoken für Spotifyr

#--------------------------------------- Vorbereiten der Daten --------------------------------------#

lyrics <- fread("original_dataset.csv") #liest das File von Kaggle ein
lyrics$language = detect_language(lyrics$text) #Entdeckt die Sprache von den lyrics und wird gespeichert in language
table(lyrics$language)
lyrics %>% filter(language == "de") %>% select(artist, song, language) #deutsche Lieder
lyrics = lyrics %>% filter(language == "en") %>% select(-language,-link) #Wähle alle Lieder mit Englischem Text aus

#------------------------------------------- Vorgehensweise -----------------------------------------#

#Daten in Spotify sind folgendermaßen aufgebaut: Hat man einen Künstler -> KünstlerID -> 
#über KünstlerID AlbenID der Künstler abfragen -> über AlbenID SongID abfragen
#Das wird in den folgenden Schritten gemacht

#--------------------------------------- Umwandeln der Artists als Namen in Artist-IDs von Spotify  --------------------------------------#

#Alle Artists, die übrig geblieben sind in einen Vektor geben, damit die IDs dieser geholt werden können
unique_artist = pull(lyrics %>% distinct(artist),artist) 
#Ein Datenset vorbereiten mit Spalte 1: Namen der Artisten, 
#Spalte 2 die zugewiesene ID, 
#Spalte 3  erfolgreich ausgeführt?
# -> für die folgende R-Schleife

artist_ids = data.frame(artist = unique_artist, 
                    id = rep(0, length(unique_artist)), 
                    success = rep(FALSE, length(unique_artist))) 

max_tries = 3 #Anzahl der Versuche, wie oft bei Spotify angefragt wird (die äußere Schleifenwiederholung)

for(try in 1:max_tries) { #Versuche
  for (i in 1:length(unique_artist)) { #Wiederhole die Schleife für jeden Artist
    if(artist_ids[i,2] != 0) {next} #Wurde in einem vorherigen Schleifendurchlauf einem Artist schon eine ID zugeordnet, überspringe ihn
    #Normalerweilse: Fehler in Schleife -> Schleife bricht ab. Mit tryCatch wird die Schleife trotzdem weiter ausgeführt (in Kombination mit error = ... unterhaleb)
    tryCatch({ 
      #durchsucht Spotify nach dem Künstlernamen und nimmt die ID des ersten Ergebnisses und fügt sie in das Datenset neben den Artisten als Namen
      artist_ids[i,2] <- pull(search_spotify(unique_artist[i], type = "artist", limit = 1) %>% select(id),id) #speichert die ID im DF ab
      artist_ids[i,3] = TRUE #Falls in tryCatch ein Fehler auftritt wird der Rest der Schleife geskippt -> somit bleibt False im DF, ansonsten wird hier True eingetragen
    }, error = function(e) { print(e) }) #Wird ein error gecatcht wird er hier einfach nur geprinted, aber die Schleife nicht abgebrochen
    print(i)
  }
}

#search_spotify("Led Zeppelin","artist")
#search_spotify("Stairway to Heaven","track")
get_artist_albums(search_spotify("Twenty One Pilots","artist")[1,]$id, limit = 50)

filter(artist_ids, success == FALSE) #Filter nach Personen, wo es nicht geklappt hat (einen - den gibts nicht auf Spotify)
artist_id_success = artist_ids %>% filter(success == T) %>% select(-success) #Alle wo es funktioniert hat in neues Datenset für Weiterverarbeitung weiterverarbeiten
fwrite(artist_id_success, "artist_ids.csv")

#--------------------------------------- Holen der Alben-IDs von Artist-IDs von Spotify  --------------------------------------#
#Ich will alle AlbenIDs eines Künstlers in eine Liste schmeißen, damit ich im nächsten Schritt die Songs mit IDs daraus ziehen kann

#Eine leere Liste initialisieren
album_list = list(NULL)
#Ein DF der mir sagt, ob es funktioniert hat oder nicht
suc_table = data.frame(artist = artist_id_success[,1], success = rep(FALSE,dim(artist_id_success)[1]))
album_number <- rep(0,dim(artist_id_success)[1])
ids = NULL
ids1 = NULL
ids2 = NULL


for(j in 1:dim(artist_id_success)[1]) { #Schleife, die für alle ArtistIDs die AlbenID rauszieht
  tryCatch({ 
  ids1 = unname(unlist(get_artist_albums(artist_id_success[j,2], limit = 50) %>% #holt alle Alben von Künstlern (maximales Albenlimit = 50)
                        filter(album_type == "album" | album_type == "single") %>% select(id))) #behält nur einzigartige Alben - davon das Älteste
  if (length(ids1) == 50) { #wenn mehr als 50 Alben, dann ziehen wir noch weitere 50 raus
    ids2 = unname(unlist(get_artist_albums(artist_id_success[j,2], limit = 50,offset = 50) %>% #holt alle Alben von Künstlern (maximales Albenlimit = 50)
                           filter(album_type == "album" | album_type == "single") %>% #filtert nur Alben, die entweder Alben oder Singles sind
                           arrange(release_date) %>% distinct(name, .keep_all = T) %>% select(id))) #behält nur einzigartige Alben - davon das Älteste
  }
  ids = c(ids1,ids2)
  album_number[j] <- length(ids) #die Albenanzahl mitschreiben
  suc_table[j,2] = TRUE #wenn es funktioniert hat -> Success einfügen
  }, error = function(e) { print(e) }) #gehört zu tryCatch
  album_list[[j]] = rep(0, length(ids)) #für den einen Künstler eine Liste initialisieren mit der entsprechenden Länge der IDs
  for(i in 1:length(ids)) {
    album_list[[j]][i] = ids[i] #die Indexe der Liste befüllen mit der richtigen ID
  }
  print(j)
  ids2 = NULL #reset ids2, weil das nicht automatisch passiert
}

#album_number[which(album_number >= 50)]
#unique_artist[which(album_number >= 50)]
#which(unique_artist == "Elvis Presley")

#get_artist_albums(artist_id_success[1,2]) #Beispiel: Alben von Abba, hier lässt sich dann das Erscheinungsjahr des Songs herausfiltern

names(album_list) = pull(artist_id_success,artist) #benennen der Liste mit Artisten, die dazugehören

#---------------------------------------Alben-IDs in SongIDs umwandeln (und noch etwas mehr)  --------------------------------------#

#leeren Vektor initialisieren, der dann befüllt wird
ids_tracks = NULL
all_tracks_spotify = NULL
#leeren Data Frame initialisieren - wird in der Schleife erklärt, was wir damit machen
lyrics_with_ids = data.frame()

for(i in 511:length(album_list)) { #Schleife über jede Liste der Albumliste
  interpret = names(album_list)[i] #Da die Albumliste Namen hat, sind die Artisten leicht zuzuordnen über den Namensindex 
  in_lyrics = pull(lyrics %>% filter(artist == interpret),song) #Welche Songs sind im Ursprungsdatenset? - hier in "in_lyrics" gespeichert
  
  for(j in album_list[[i]]) { #für jedes Album in der Albenliste für einen äußeren Schleifendurchlauf ...
    # ... hol dir ID und Name der Songs von jedem Album und füge sie in eine kurzfristigen Vektor
    tryCatch({ids_tracks = bind_rows(ids_tracks, get_album_tracks(j) %>% select(id, name))
              } 
             , error = function(e) { print(e) })
  }
  ids_tracks$artist <- interpret
  ids_tracks = unique(ids_tracks)
  all_tracks_spotify <- bind_rows(all_tracks_spotify,ids_tracks) #Speichere alle Lieder von Spotify in extra Dataframe
  #Vergleiche Songname vom Ursprungsdatenset und von Spotify. Ähneln sich welche fast aufs Haar, sag mir welche das sind über einen Match-Index
  #Anmerkung (Der Match-index bezieht sich auf das Spotify-Set)
  index = amatch(in_lyrics,unname(unlist(ids_tracks$name)), maxDist = 2) 
  song = in_lyrics[!is.na(index)] #Entferne alle Songs, wo es kein Match in den Spotify-Daten gab und speichere es in "song"
  song_id = ids_tracks[index[!is.na(index)],]$id #Wähle nur die SongID im Spotify-Set aus, die ein Songname im Ursprungsset gematched hat
  song_name = ids_tracks[index[!is.na(index)],]$name #Wähle nur den Song-Namen aus, die ein Songname im Ursprungsset gematched hat
  artist = rep(interpret, length(index[!is.na(index)])) #Füge den Artist hinzu, damit das Set mit dem Ursprungsset wieder vereint werden kann
  #Füge in den vorher initialisierten Data Frame alle extrahierten Daten ein und füge die neuen Interpreten immer wieder als Reihen hinzu
  lyrics_with_ids = bind_rows(lyrics_with_ids, data.frame(artist, song, song_name, song_id)) 
  ids_tracks = NULL #setze die Spotify-Lieder wieder auf Null, damit alte Künstler nicht doppelt überspielt werden
  print(i)
}

str(lyrics) #weiß nicht, ob wir den Link dann noch wollen - ich lass ihn mal drinnen
str(lyrics_with_ids) #von hier wollen wir dann "song_name" - also die sauberen Songnamen und die song_id fürs anreichern 
#Anmerkung: Vorher hatte das Datenset 57044 Songs, jetzt nur mehr 34116 Songs - die dafür aber Clean und richtig zuordenbar!
lyrics_final = lyrics_with_ids %>% left_join(lyrics, by = c("artist","song"))
fwrite(lyrics_final, "lyrics_more_albums.csv")
str(lyrics_final)
#34116/57044 #percentage of succefully joined songs from the Spotify API
#mit Stringdist = 3 erhöhen wir die Zahl von 34116 auf 35383

#adding artist_id to dataframe
artist_id_success[2,1]
lyrics_final$artist_id <- ""
for (i in 1:dim(artist_id_success)[1]){
  artist <- artist_id_success[i,1]
  artist_id <- artist_id_success[i,2]
  lyrics_final$artist_id[lyrics_final$artist == artist] <- artist_id
}
fwrite(lyrics_final,"lyrics_more_albums_with_artist_id.csv")

check <- lyrics_final[lyrics_final$song != lyrics_final$song_name,] #check welche lieder durch stringdist korrigiert werden mussten
stringdist_correction <- check
fwrite(stringdist_correction, "stringdist_correction.csv")

#Zieht die Lieder heraus, die bis zum Ende nicht gematcht wurden
`%out%` <- function(a,b) ! a %in% b
lyrics_final %>%
  select(artist,song) -> lyrics_final_compare
lyrics %>%
  select(artist,song) -> lyrics_original_compare
compare <- anti_join(lyrics_original_compare,lyrics_final_compare)

fwrite(compare,"Unmatched_Songs.csv")

fwrite(all_tracks_spotify,"all_tracks_from_spotify.csv")

#Beispiele herauspicken, bei welchen Songs es nicht geklappt hat und kurz erklären wieso
#anhand von 3-4 Beispielen - man muss nicht weiter analysieren und versuchen die Zahl zu erhöhen
