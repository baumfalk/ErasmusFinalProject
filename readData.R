rm(list=ls())
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

##################################################

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

################################################

#setwd("~/Programming/ErasmusFinalProject")
print("readData")

# load the datasets
print("  loading movielensdata")
movielensFilmDataRaw <-  read.csv2("data/movielens/movies.csv")
movielensRatingDataRaw <- read.csv2("data/movielens/ratings.csv")
movielensUserDataRaw <- read.csv2("data/movielens/users.csv")
movieDistances  <- readRDS("data/movielens/movieDistances.rds")

movielens_age <- read.csv2("data/movielens/movielens_age.csv", sep=";")
names(movielens_age) <- c("age","text")
movielens_occupation <- read.csv2("data/movielens/movielens_occupation.csv", sep=";",stringsAsFactors = F)
names(movielens_occupation) <- c("occupation","text")

print("  done loading movielensdata")


print("  loading imdb data")
imdbDataRaw <- read.csv("data/imdb/imdbtop5000.csv")
IMDBMatchedScripts <- readRDS("data/imdb/IMDBMatchedScripts.rds")
movie_sentiment_IMDB <- readRDS("data/imdb/movie_script_sentiment.RDS")
print("  done loading imdb data")

print("  loading springfield data")
springfieldDataRaw <- read.csv2("data/springfield/urls_titles_springfield.txt")
SpringfieldMatchedScripts <- readRDS("data/springfield/springfieldmatchedscripts.rds")
movie_sentiment_springfield <- readRDS("data/springfield/movie_script_sentiment.RDS")

print("  done loading springfield data")


print(" preparing data")
SpringfieldMatchedScripts <- SpringfieldMatchedScripts %>%
  mutate(titlesSmall=tolower(SpringfieldMatchedScripts$title))

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
filmDataPivotInSpringfield <- filmDataPivot[(filmDataPivot$TitleAndYear %in% springfieldDataRaw$titlesSmall),]


#summarize ratings per category

movielensRatingDataSummarizedGender <- movielensRatingData %>%
  inner_join(movielensUserData) %>%
  group_by(Gender,MovieID) %>%
  summarize(GenderRating = mean(Rating)) %>%
  spread(Gender,GenderRating)

movielensRatingDataSummarizedGender[is.na(movielensRatingDataSummarizedGender)] <- 1

movielensRatingDataSummarizedAge <- movielensRatingData %>%
  inner_join(movielensUserData) %>%
  group_by(Age,MovieID) %>%
  summarize(AgeRating = mean(Rating)) %>%
  spread(Age,AgeRating)

names(movielensRatingDataSummarizedAge)[2:ncol(movielensRatingDataSummarizedAge)] <- paste(names(movielensRatingDataSummarizedAge)[2:ncol(movielensRatingDataSummarizedAge)],"age", sep = "_")

movielensRatingDataSummarizedAge[is.na(movielensRatingDataSummarizedAge)] <- 1

movielensRatingDataSummarizedOccupation <- movielensRatingData %>%
  inner_join(movielensUserData) %>%
  group_by(Occupation,MovieID) %>%
  summarize(OccupationRating = mean(Rating)) %>%
  spread(Occupation,OccupationRating) %>%
  replace_na(list())

names(movielensRatingDataSummarizedOccupation)[2:ncol(movielensRatingDataSummarizedOccupation)] <- paste(names(movielensRatingDataSummarizedOccupation)[2:ncol(movielensRatingDataSummarizedOccupation)],"occ", sep = "_")

movielensRatingDataSummarizedOccupation[is.na(movielensRatingDataSummarizedOccupation)] <- 1

movielensRatingDataSummarized <- movielensRatingDataSummarizedGender %>%
  inner_join(movielensRatingDataSummarizedAge, by=c("MovieID"="MovieID")) %>%
  inner_join(movielensRatingDataSummarizedOccupation, by=c("MovieID"="MovieID"))



filmDataRated <- filmDataPivotInSpringfield %>%
  inner_join(movielensRatingDataSummarized) 




##IMDB met sprinfield
imdbData <- imdbData %>%
  mutate(titlesSmall=tolower(iconv(movie_title_char,"WINDOWS-1252","UTF-8"))) %>%
  mutate(TitleAndYear = paste(titlesSmall,paste("(",title_year,")",sep="")))

springfieldDataIMDBTitles <- springfieldDataRaw[springfieldDataRaw$titlesSmall %in% imdbData$TitleAndYear,]
filmDataNotSpringfieldIMDBTitles <- imdbData[!(imdbData$TitleAndYear %in% springfieldDataRaw$titlesSmall),]


genres <- unique((filmDataInSpringfield %>%
  arrange(Genre))$Genre)


age_list <- with(movielens_age, split(age, text))
occupation_list <- with(movielens_occupation, split(occupation, text))

IMDBMatchedScripts <- IMDBMatchedScripts %>%
  mutate(titlesSmall=tolower(IMDBMatchedScripts$title))


print(" done preparing data")