rm(list=ls())
library(dplyr)
library(tidyr)
library(stringr)
library(omdbapi)
#setwd("~/Programming/ErasmusFinalProject")
# movielens dataset

# load the datasets
movielensFilmDataRaw <-  read.csv2("data/movielens/movies.csv")
movielensRatingDataRaw <- read.csv2("data/movielens/ratings.csv")
movielensUserDataRaw <- read.csv2("data/movielens/users.csv")

# split movie genres
movielensFilmData <- movielensFilmDataRaw %>%
  separate(Genres, into = paste("Genre", 1:6,sep=""), sep = "\\Q|\\E") %>%
  separate(Title, into = c("Title","Year"), sep = " (?=[^ ]+$)")

movielensFilmDataSplitted <- movielensFilmData %>%
  gather(key = GenreNum, value=Genre,Genre1:Genre6) %>%
  select(-GenreNum) %>%
  filter(!is.na(Genre))


# load imdb dataset
imdbDataRaw <- read.csv("data/imdb/imdbtop5000.csv")
imdbData <- imdbDataRaw %>%
  inner_join(movielensFilmData,by=c("movie_title"="Title"))
