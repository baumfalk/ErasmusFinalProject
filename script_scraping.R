
# Introduction ------------------------------------------------------------


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


# Specify function NULL --------------------------------------------------------

#check for NULL
f_isna <- function(a){
  ifelse(is.na(a), "0",a)
}


# Gather URLs and titles --------------------------------------------------

urls <- vector()
titles <- vector()

order <- c(0,paste(letters[1:26]))

for(j in 1:length(order))
{
  url <- paste("http://www.springfieldspringfield.co.uk/movie_scripts.php?order=",order[j],"&page=1", sep="")
  
  SOURCE <-  getURL(url,encoding="UTF-8")
  PARSED <- htmlParse(SOURCE)
  
  max <-  as.integer(tail(f_isna(xpathSApply(PARSED, "//a",xmlValue)),1))
  for(i in 1:max)
  {
    url <- paste("http://www.springfieldspringfield.co.uk/movie_scripts.php?order=",
                 order[j],"&page=",
                 i, sep="")
    SOURCE <-  getURL(url,encoding="UTF-8")
    PARSED <- htmlParse(SOURCE)
    raw_html <- read_html(url)
    temp_url <- ( raw_html %>%
                    html_nodes("[class!=search-and-alpha]") %>% html_nodes("a") %>%
                    html_attr("href"))
    temp_url <- temp_url[grepl("?movie=",temp_url)]
    
    temp_title <- xpathSApply(PARSED, "//a[contains(@href, '?movie=')]",xmlValue)
    
    urls <- append(urls, temp_url)
    titles <- append(titles, temp_title)
    remove(temp_url)
    remove(temp_title)
    print(tail(titles))
  }
}

#make urls working with prefix
urls <- paste("http://www.springfieldspringfield.co.uk",urls,sep="")

#Store titles and url's in dataframe
data <- data.frame(urls=urls,titles=titles,stringsAsFactors=FALSE)

#Save found urls
write.table(data,"urls_titles.txt",row.names = F,sep=";")

#unique(urls)
#unique(titles)


# Define scraper function -------------------------------------------------

# Define error content
url <- "http://www.springfieldspringfield.co.uk/movie_script.php?movie=horror"
SOURCE <- getURL(url,encoding="UTF-8")
PARSED <- htmlParse(SOURCE)

ERROR.CONTENT <- paste(unlist(xpathSApply(
  PARSED, "//div[@class='scrolling-script-container']", 
  xmlValue)),collapse = "")


SpringfieldScraper <- function(url)
{
  SOURCE <- getURL(url,encoding="UTF-8")
  PARSED <- try_default(htmlParse(SOURCE),htmlParse(getURL("http://www.springfieldspringfield.co.uk/movie_script.php?movie=horror",encoding="UTF-8")))
  #title= f_isna(xpathSApply(PARSED, "//title",xmlValue))
  #if(length(title)==0){title <- "No title"}
  #title = title_input
  content <-  f_isna(paste(unlist(xpathSApply(
    PARSED, "//div[@class='scrolling-script-container']", 
    xmlValue)),collapse = ""))
  if(content==""){content <- "No content"}
  if(content==ERROR.CONTENT){content <- "ERROR"}
  extract_date <- Sys.Date()
  website <- "Springfield"
  print(paste(url))
  output <- data.frame(website, url,# title,
                       extract_date, content,
                       stringsAsFactors=FALSE)
  return(output)
}

# output.default <- data.frame("Springfield", "Mislukt",# title,
#                      Sys.Date() , "no_content",
#                      stringsAsFactors=FALSE)

# Test function -----------------------------------------------------------

#Test functie
#dat <- SpringfieldScraper(url=data$urls[1:2], title_input=data$titles[1:2])
#temp kan geprint worden, dan is te zien dat het werkt
#remove(temp)
#?ldply



dat <- ldply(springfieldMatched$urls[263:264],SpringfieldScraper)
dat$title <- data$titles[1:4]
write.table(dat, file=paste("data_",gsub(":","",Sys.time()),".txt"),row.names = F,sep=";")


# Apply function to matched movies ----------------------------------------

library(readr)
springfieldMatched <- read_csv("springfieldMatched.csv")

m <- ceiling(nrow(springfieldMatched)/100)
a <- seq(from=1, to=nrow(springfieldMatched), by=100)
b <- append(seq(from=100, to=nrow(springfieldMatched),by=100),nrow(springfieldMatched))

for(k in 1:m)
{
  chunk <- paste0("dat_",k)
  assign(chunk, ldply(springfieldMatched$urls[a[k]:b[k]],SpringfieldScraper))
  print(paste("This was batch",k))
  Sys.sleep(1)
}

dat <- rbind(dat_1,dat_2,dat_3,dat_4,dat_5,dat_6,dat_7)
dat$title <- springfieldMatched$titles


saveRDS(dat, "SpringfieldMatchedScripts.rds")
write.table(dat, file=paste("data_",gsub(":","",Sys.time()),".txt"),row.names = F,sep=";")


# Word cloud --------------------------------------------------------------

library(tidytext)
library(wordcloud)
library(RColorBrewer)

words <- dat[,] %>% unnest_tokens(word, content)

saveRDS(words, "words_SpringfieldMatchedScripts.rds")
write.table(words,"words_SpringfieldMatchedScripts.txt", sep = ";")

palette <- brewer.pal(9,"BuGn")[-(1:4)]
set.seed(40)
words %>% anti_join(stop_words) %>% count(word) %>% with(wordcloud(word,n,max.word=20,color = palette))



# WIP ---------------------------------------------------------------------



palette <- brewer.pal(9,"BuGn")[-(1:4)]
set.seed(42)
wordcloud(words, min.freq = 55, color = palette)

initCoreNLP(type="english", 
            parameterFile="C:/Users/Arthur/Dropbox/Studie/2016-17 EUR D&B Analytics/Blok 3/Semantic web and Text mining/Text mining/basic.properties",
            mem="4g")


tidyContent <- data.frame()

for (i in 1:1){#nrow(dat)){
  annContent <- annotateString(dat$content[i])
  tokens <- annContent$token %>% 
    select(-c(NER,Speaker)) %>%
    mutate(url = dat$url[i]) %>%
    mutate(title = dat$title[i])
  if(dim(tidyContent)==0){
    tidyContent = data.frame(tokens)
  }else{
    tidyContent %<>% union(tokens)
  }
}



tidyContent$url <- as.factor(tidyContent$url)
tidyContent$ID <- as.factor(paste(tidyContent$title))

dat$url <- as.factor(df$url)



#test<- tidyContent %>% 
#  count(url) %>%
#  left_join(dat)

tidyContent %<>% filter(abs(CharacterOffsetBegin - CharacterOffsetEnd) > 1)
tidyContent %<>% filter(nchar(POS) > 1)
tidyContent %<>% filter(token !="'s")
tidyContent %<>% filter(token !="me")#81343


DTM <- cast_dtm(tidyContent %>%
                  filter(!lemma %in% stop_words$word) %>%
                  group_by(ID) %>% count(lemma), 
                ID, lemma, n)
