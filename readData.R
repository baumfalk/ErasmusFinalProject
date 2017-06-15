rm(list=ls())
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

#setwd("~/Programming/ErasmusFinalProject")
# movielens dataset

# load the datasets
movielensFilmDataRaw <-  read.csv2("data/movielens/movies.csv")
movielensRatingDataRaw <- read.csv2("data/movielens/ratings.csv")
movielensUserDataRaw <- read.csv2("data/movielens/users.csv")
imdbDataRaw <- read.csv("data/imdb/imdbtop5000.csv")

springfieldDataRaw <- read.csv2("data/springfield/urls_titles_springfield.txt")
SpringfieldMatchedScripts <- readRDS("data/springfield/springfieldmatchedscripts.rds")
SpringfieldMatchedScripts <- SpringfieldMatchedScripts %>%
  mutate(titlesSmall=tolower(SpringfieldMatchedScripts$title))

SpringfieldMatchedIMDBScripts <- readRDS("data/springfield/IMDBMatchedScripts.rds")

SpringfieldMatchedIMDBScripts <- SpringfieldMatchedIMDBScripts %>%
  mutate(titlesSmall=tolower(SpringfieldMatchedIMDBScripts$title))

springfieldDataRaw <- springfieldDataRaw %>%
  mutate(titles=iconv(titles,"WINDOWS-1252","UTF-8"))%>%
  mutate(titlesSmall=tolower(stringr::str_trim(titles)))
#SpringfieldMatchedScriptsTidy <- SpringfieldMatchedScripts %>%
  #unnest_tokens(word, content)

#remove trailing spaces in imdb titles
imdbData <- imdbDataRaw %>%
  mutate(movie_title_char = as.character(movie_title) %>% str_trim) %>%
  mutate(title_year=as.integer(title_year))

#intersection of imdb-data with springfield data
filmDataImdbSpringfield <- imdbData %>%
  separate(genres, into = paste("Genre", 1:8,sep=""), sep = "\\Q|\\E",fill="right") %>%
  gather(key = GenreNum, value=Genre,Genre1:Genre8) %>%
  filter(!is.na(Genre)) %>%
  select(-GenreNum) %>%
  unique() %>%
  mutate(titlesSmall=tolower(substr(movie_title,1,length(movie_title)-2))) %>%
  mutate(TitleAndYear = paste(titlesSmall,paste("(",title_year,")",sep=""))) 
  
  

  

fixYear <- function(data) {
  data %>%
    separate(Title, into = c("Title","YearRaw"), sep = " (?=[^ ]+$)") %>%
    mutate(YearRaw=sub(pattern = "\\Q(\\E",replacement="", x=YearRaw)) %>%
    mutate(YearRaw=sub(pattern = "\\Q)\\E",replacement="", x=YearRaw)) %>%
    mutate(Year=as.integer(YearRaw)) %>%
    select(-YearRaw)
}

fixGenres <- function(data) {
  data %>% mutate(allgenres=paste(Genres,genres,sep="|")) %>%
    separate(allgenres, into = paste("Genre", 1:12,sep=""), sep = "\\Q|\\E") %>%
    gather(key = GenreNum, value=Genre,Genre1:Genre12) %>%
    filter(!is.na(Genre)) %>%
    select(-Genres, -genres,-GenreNum)
}

# filmData <- movielensFilmDataRaw %>%
#   mutate(test=1)

filmData <- movielensFilmDataRaw %>%
  fixYear() %>%
  inner_join(imdbData,by=c("Title"="movie_title_char", "Year"="title_year")) %>%
  fixGenres() %>%
  unique() %>%
  mutate(titlesSmall=tolower(iconv(Title,"WINDOWS-1252","UTF-8"))) %>%
  mutate(TitleAndYear = paste(titlesSmall,paste("(",Year,")",sep="")))


filmDataPivot <- filmData %>%
  mutate(exists=1) %>%
  spread(key=Genre,value=exists,fill=0)

movielensRatingDataRaw %>%
  group_by(MovieID)%>%
  summarize(count=n()) %>%
  arrange(desc(count))

movielensRatingData = movielensRatingDataRaw[movielensRatingDataRaw$MovieID %in% filmDataPivot$MovieID,]
movielensUserData = movielensUserDataRaw[movielensUserDataRaw$UserID %in% movielensRatingData$UserID,]



springfieldData <- springfieldDataRaw[springfieldDataRaw$titlesSmall %in% filmDataPivot$TitleAndYear,]
filmDataNotSpringfield <- filmDataPivot[!(filmDataPivot$TitleAndYear %in% springfieldDataRaw$titlesSmall),]

filmDataInSpringfield <- filmData[(filmData$TitleAndYear %in% springfieldDataRaw$titlesSmall),]
##IMDB met sprinfield
imdbData <- imdbData %>%
mutate(titlesSmall=tolower(iconv(movie_title_char,"WINDOWS-1252","UTF-8"))) %>%
mutate(TitleAndYear = paste(titlesSmall,paste("(",title_year,")",sep="")))

springfieldDataIMDBTitles <- springfieldDataRaw[springfieldDataRaw$titlesSmall %in% imdbData$TitleAndYear,]
filmDataNotSpringfieldIMDBTitles <- imdbData[!(imdbData$TitleAndYear %in% springfieldDataRaw$titlesSmall),]




genres <- unique((filmDataInSpringfield %>%
  arrange(Genre))$Genre)



