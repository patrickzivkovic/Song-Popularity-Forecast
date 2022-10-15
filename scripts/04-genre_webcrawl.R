pacman::p_load(data.table, dplyr, stringi, stringr, spotifyr, lubridate, knitr, cld3, quanteda, tm, rlang, stringdist, rvest, purrr, tidyverse, janitor, qdap)
rm(list = ls())

#--------------------------------------- from Wikipedia ----------------------------------------------------------------#

url <- "https://en.wikipedia.org/wiki/List_of_popular_music_genres" 

webpage <- read_html(url, encoding = "UTF-8")
html_nodes(webpage, "[class='mw-headline']")[2:28] %>% html_text()
html_nodes(webpage, "h3")
html_nodes(webpage, "[class='div-col columns column-width']>ul") %>% html_attr("title")
html_nodes(webpage, "[class='toctext']") %>% html_text()
html_nodes(webpage, "div>ul>li>a") %>% html_attr("title")
html_nodes(webpage, "h4>span") %>% html_attr("id")
html_nodes(webpage, "h2>span") %>% html_attr("id")

overgenres = (html_nodes(webpage, "div") %>% html_nodes("[class='toctext']") %>% html_text())[3:25]
subgenres = (html_nodes(webpage, "div") %>% html_nodes("ul>li>a") %>% html_text())[112:986]

boundaries = c("Zouglou", "Sawt", "Trot", "Sufi rock", "V-pop", "Electroacoustic", "West Coast blues", "Zouk", "Parody", "Zydeco",
               "New-age music", "Nightcore", "Witch house", "Cantes de ida y vuelta", "Western music", "Horrorcore", "West Coast jazz",
               "Vallenato", "Worldbeat", "Southern soul", "Wrock", "Groove metal", "Street punk")

sapply(subgenres, grep)
bound_in_df = NULL
compare = 0
for(i in 1:length(boundaries)) {
  object = grep(boundaries[i], subgenres)
  decision = object[compare < object][1]
  compare = object
  bound_in_df[i] = decision
}

bound_in_df2 = c(0, bound_in_df)
to_rep = NULL
for(i in 1:length(bound_in_df2)-1) {
  to_rep[i] = bound_in_df2[i+1]-bound_in_df2[i]
}

overgenre_table = tibble(subgenre = subgenres, overgenre = rep(overgenres, to_rep))

fwrite(overgenre_table, "overgenres_from_wikipedia.csv")


#----------------------------------------- from musicgenrelist -----------------------------------------------------#


url <- "https://www.musicgenreslist.com/" 

webpage <- read_html(url, encoding = "UTF-8")



genres_with_subgenres = html_nodes(webpage, "div>ul>li") %>% map_df(~
                                                                      data_frame(
                                                                        overgenre = html_nodes(.x, "a")  %>% html_text(trim = T), 
                                                                        subgenre = list(html_nodes(.x, "ul>li")  %>% html_text(trim = T))
                                                                      ))


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

fwrite(df, "data/genres_with_subgenres.csv")
