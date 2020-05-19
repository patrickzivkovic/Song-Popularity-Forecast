pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist)

lyrics <- fread("perfectly_matched_complete_dataset.csv")
old_lyrics <- fread("original_dataset.csv")

#Filter which songnames were not exactly the same in both datasets
corrected <- lyrics[which(songname != original_songname),] %>% select(artist, original_songname, songname)
fwrite(corrected, "Datenverlust/stringdist_correction.csv")

#Filter which songs could not be matched
colnames(old_lyrics) <- c("artist","original_songname", "link", "text")
unmatched <- anti_join(old_lyrics, lyrics, by = c("artist", "original_songname"))
fwrite(unmatched, "Datenverlust/Unmatched_Songs.csv")