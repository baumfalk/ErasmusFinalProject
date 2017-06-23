library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(xml2)
library(rvest)
require(selectr)
library(jpeg)

#library(recommenderlab)
source("readData.R")
source("uiApp.R")
source("serverApp.R")


# Run the application 
shinyApp(ui = ui, server = server)