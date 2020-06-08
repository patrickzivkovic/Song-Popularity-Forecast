pacman::p_load(dplyr, data.table, tidyverse, magrittr)
rm(list = ls())

orig = fread("Data-Challenge-Songlyrics/perfectly_matched_complete_dataset.csv") %>% select(-text, -artist_genre, -song_id, -artist_id, -original_songname)
orig = orig %>% distinct_at(c("artist", "songname"), .keep_all = T) 
genres_top1 = fread("Data-Challenge-Songlyrics/03_Genre Issue/artists_with_only_top1_overgenre.csv") %>% rename(genre = overgenres)
df1 = fread("Data-Challenge-Songlyrics/06_NLP_Research/songs_with_colemanlieau.csv")
df2 = fread("Data-Challenge-Songlyrics/06_NLP_Research/songs_with_SMOG.csv")
df3 = fread("Data-Challenge-Songlyrics/06_NLP_Research/songs_with_sentiment_percent.csv")
df4 = fread("Data-Challenge-Songlyrics/06_NLP_Research/songs_with_sentiment_summed.csv") %>% rename(neg_sent = negative, pos_sent = positive)

df1$songname = gsub("\\|", ".", df1$songname)  
df2$songname = gsub("\\|", ".", df2$songname)  
df3$songname = gsub("\\|", ".", df3$songname)  
df4$songname = gsub("\\|", ".", df4$songname)  
df1$artist = gsub("\\|", ".", df1$artist)
df2$artist = gsub("\\|", ".", df2$artist)
df3$artist = gsub("\\|", ".", df3$artist)
df4$artist = gsub("\\|", ".", df4$artist)


merged = orig %>% left_join(genres_top1, by = "artist") %>% 
  full_join(df1, by = c("artist", "songname")) %>%
  full_join(df2, by = c("artist", "songname")) %>%
  full_join(df3, by = c("artist", "songname")) %>%
  full_join(df4, by = c("artist", "songname"))

#Replace all NAs in the sentiment columns with zeroes
merged <- fread("merged_dataset.csv")
sum(is.na(merged))
merged %<>% mutate_at(c(11:23), ~replace(., is.na(.), 0))
sum(is.na(merged))
#There are still some rows left with NAs - we remove these rows to prevent calculation difficulties
merged %<>% drop_na()
sum(is.na(merged))

fwrite(merged, "merged_dataset.csv")
