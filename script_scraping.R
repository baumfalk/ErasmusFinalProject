#Set working directory
setwd("C:/Users/Arthur/Dropbox/Studie/2016-17 EUR D&B Analytics/Blok 5/Eindopdracht")

rm(list = ls())

#Set libraries

library(rjson)
library(rtimes)
library(stringr)
library(RCurl)
library(XML)
library(rvest)
library(tm)
library(tm.plugin.webmining)
require(reshape2)
library(plyr)
library(dplyr)
library(magrittr)
library(coreNLP)
library(rvest)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(tidyr)
library(sentimentr)

#Code om tijd goed te zetten
Sys.setlocale("LC_TIME","C")

#check for NULL
f_isna <- function(a){
  ifelse(is.na(a), "0",a)
}

#Test URL content
url <- "http://www.springfieldspringfield.co.uk/movie_script.php?movie=dracula-1958"
a <- SpringfieldScraper(url)
#Test URL links


SOURCE <-  getURL(url,encoding="UTF-8")
PARSED <- htmlParse(SOURCE)
content <-  f_isna(paste(unlist(xpathSApply(
  PARSED, "//div[@class='scrolling-script-container']", 
  xmlValue)),collapse = ""))


#Gather urls
urls <- vector()

order <- c(0,paste(letters[1:26]))
for(j in 1:1){
url <- paste("http://www.springfieldspringfield.co.uk/movie_scripts.php?order=",order[j],"&page=1", sep="")

SOURCE <-  getURL(url,encoding="UTF-8")
PARSED <- htmlParse(SOURCE)

max <-  as.integer(tail(f_isna(xpathSApply(PARSED, "//a",xmlValue)),1))
for(i in 1:max)
{
  raw_html <- read_html(paste("http://www.springfieldspringfield.co.uk/movie_scripts.php?order=",
                              order[j],"&page=",
                              i, sep=""))
  temp_url <- ( raw_html %>%
                  html_nodes("[class!=search-and-alpha]") %>% html_nodes("a") %>%
                  html_attr("href"))
 temp_url <- temp_url[grepl("?movie=",temp_url)]
     
  temp_url <- unique(temp_url)
  urls <- append(urls, temp_url)
  remove(temp_url)
}

}
urls <- unique(urls)
urls <- paste("http://www.springfieldspringfield.co.uk",urls,sep="")


#Define scraper function

SpringfieldScraper <- function(url){
  SOURCE <- getURL(url,encoding="UTF-8")
  PARSED <- htmlParse(SOURCE)
  title= f_isna(xpathSApply(PARSED, "//title",xmlValue))
  if(length(title)==0){title <- "No title"}
  content <-  f_isna(paste(unlist(xpathSApply(
    PARSED, "//div[@class='scrolling-script-container']", 
    xmlValue)),collapse = ""))
  if(content==""){content <- "No content"}
  extract_date <- Sys.Date()
  website <- "Springfield"
  print(paste(url))
  output <- data.frame(website, url, title,
                       extract_date, content,
                       stringsAsFactors=FALSE)
  return(output)
  Sys.sleep(1)
}

i<- 0
#Test functie
temp <- SpringfieldScraper(url)
#temp kan geprint worden, dan is te zien dat het werkt
remove(temp)
urls

dat <- ldply(urls[1:54],SpringfieldScraper)
write.csv2(dat, file=paste("data_",gsub(":","",Sys.time()),".csv"))


#library(xlsx)
#write.xlsx(dat,paste("data_",gsub(":","",Sys.time()),".xlsx"))

######
######
#####

initCoreNLP(type="english", 
            parameterFile="C:/Users/Arthur/Dropbox/Studie/2016-17 EUR D&B Analytics/Blok 3/Semantic web and Text mining/Text mining/basic.properties",
            mem="4g")


tidyContent <- data.frame()

for (i in 1:length(df$title)){
  # Dit verwerkt de huidige speech
  annContent <- annotateString(df$content[i])
  #Van het annotatie object zijn we voor nu alleen geinteresseerd in de woorden.
  #Omdat we basic.properties gebruiken, halen we de laatste twee (lege) kolommen weg
  #Ook voegen we de urls, titels, datums en trend toe
  tokens <- annContent$token %>% 
    select(-c(NER,Speaker)) %>%
    mutate(url = df$url[i]) %>%
    mutate(title = df$title[i]) %>%
    mutate(trend = df$trend[i]) %>%
    mutate(date = df$date[i])
  if(dim(tidyContent)==0){
    tidyContent = data.frame(tokens)
  }else{
    tidyContent %<>% union(tokens)
  }
}



tidyContent$url <- as.factor(tidyContent$url)
tidyContent$ID <- as.factor(paste(tidyContent$date,tidyContent$title))

df$url <- as.factor(df$url)



test<- tidyContent %>% 
  count(url) %>%
  left_join(df)

tidyContent %<>% filter(abs(CharacterOffsetBegin - CharacterOffsetEnd) > 1)
tidyContent %<>% filter(nchar(POS) > 1)
tidyContent %<>% filter(token !="'s")
tidyContent %<>% filter(token !="me")#81343


DTM <- cast_dtm(tidyContent %>%
                  filter(!lemma %in% stop_words$word) %>%
                  group_by(ID) %>% count(lemma), 
                ID, lemma, n)
