pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist, rvest, purrr, tidyverse, janitor, qdap)
rm(list = ls())

url <- "https://www.musicgenreslist.com/" 

webpage <- read_html(url)



genres_with_subgenres = html_nodes(webpage, "div>ul>li") %>% map_df(~
                                                                  data_frame(
                                                                    overgenre = html_nodes(.x, "a") %>% html_text(trim = T), 
                                                                    subgenre = list(html_nodes(.x, "ul>li") %>% html_text(trim = T))
                                                                  ))

html_nodes(webpage, "div>ul>li")

genres_with_subgenre = genres_with_subgenres[lapply(genres_with_subgenres$subgenre, length)>0,] %>% filter()


df = NULL
for(i in 1:dim(genres_with_subgenre)[1]) {
  temp = data.frame(genre = rep(genres_with_subgenre[[1]][i], length(unlist(genres_with_subgenre[[2]][i]))), 
                    subgenre = rep(0, length(unlist(genres_with_subgenre[[2]][i]))))
  subgenres = unlist(genres_with_subgenre[[2]][i])
  for(j in 1:length(subgenres)) {
    temp[j,2] = gsub("\\(.*?\\)", "",subgenres[j])
  }
  df = bind_rows(df, temp) 
}

df2 = df %>% filter(nchar(subgenre)>20)

df2$subgenre = strsplit(df2$subgenre, "\\n")

df3 = NULL
for(i in 1:dim(df2)[1]) {
  temp = data.frame(genre = rep(df2[[1]][i], length(unlist(df2[[2]][i]))), 
                    subgenre = rep(0, length(unlist(df2[[2]][i]))))
  subgenres = unlist(df2[[2]][i])
  for(j in 1:length(subgenres)) {
    temp[j,2] = subgenres[j]
  }
  df3 = bind_rows(df3, temp) 
}



df = df %>% filter(!nchar(subgenre)>20)
df = bind_rows(df,df3) %>% arrange(genre)
df[df$subgenre == "",]$subgenre = NA
df[grepl(pattern = ".push", x = df$subgenre),]$subgenre = NA
df = remove_missing(df, vars = "subgenre")

fwrite(df, "genres_with_subgenres.csv")