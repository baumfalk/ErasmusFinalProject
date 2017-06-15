library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(recommenderlab)
source("readData.R")
source("uiApp.R")
source("serverApp.R")


# Run the application 
shinyApp(ui = ui, server = server)

