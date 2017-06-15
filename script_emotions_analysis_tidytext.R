
# Introduction ------------------------------------------------------------

rm(list = ls())

#Set working directory

setwd("C:/Users/Arthur/Dropbox/Studie/2016-17 EUR D&B Analytics/Blok 5/Eindopdracht")

library(tidytext)

#library(plyr)
library(ggplot2)
#library(wordcloud)
#library(RColorBrewer)
#library(stringr)
#library(tidytext)
library(tidyr)
library(dplyr)

dat <- readRDS("IMDBMatchedScripts_backup.rds")

get_sentiments(c("nrc"))

#library(janeaustenr)
library(dplyr)
library(stringr)
movie_script <- dat[501,c("content")]

movie_sentences <- data.frame(text = movie_script ) %>% 
  unnest_tokens(sentence, text, token = "sentences")
movie_sentences$SentenceID <- seq(1:nrow(movie_sentences))
movie_sentences <- movie_sentences  %>% ungroup() %>%
  unnest_tokens(word, sentence)

moviesentiment2 <- movie_sentences %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(index = SentenceID %/% 40, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(som = joy + anger + anticipation + disgust + fear + sadness + surprise + trust) %>%
  gather(emotion,value, -index,-som) %>%
  filter(#value != 0, 
         emotion != "sentiment",
         emotion != "positive",
         emotion != "negative") %>%
  mutate(perc = value/som)


moviesentiment1 <- movie_sentences %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(index = SentenceID %/% 40, sentiment) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



g <- moviesentiment2 %>% ggplot () %>% +geom_area(aes(x=index, y=perc,fill=emotion))

g2 <- ggplot(moviesentiment1, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) + ggtitle(paste("Sentiment during the movie"))
  
nrcjoy <- get_sentiments("nrc") %>% 
  mutate(sentiment = as.factor(sentiment))
summary(nrcjoy$sentiment)





#tidy_books <- austen_books() %>%
#  group_by(book) %>%
#  mutate(linenumber = row_number(),
#         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
#                                                 ignore_case = TRUE))))

# tidy_books %>%
#   filter(book == "Emma") %>%
#   inner_join(nrcjoy) %>%
#   count(word, sort = TRUE)
