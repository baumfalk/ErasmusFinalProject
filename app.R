library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
source("readData.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Movies That Matter"),
  
  # Sidebar with a slider input for number of bins 
  
  fluidRow(
    column(3, selectInput("selectedGenre", 
                          label = h3("Select genre"),
                          choices = genres
                          ) 
           ),
    column(5,  
           plotOutput("budgetPerYearPlot", click="handleGenreClick")
                          ) 
    ), 
    column(4,
           verbatimTextOutput("info"),
           plotOutput("moviePlot")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$budgetPerYearPlot <- renderPlot({
            filmData <- filmDataInSpringfield %>%
              filter(Genre == input$selectedGenre)
            ggplot(data=filmData, aes(x=Year,y=budget)) + geom_point()
          })
  output$info <- renderPrint({
    filmData <- filmDataInSpringfield %>%
      filter(Genre == input$selectedGenre)
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleGenreClick, xvar = "Year", yvar = "budget")[1,]
    c(movie$Title,movie$Year)
    # nearPoints() also works with hover and dblclick events
  })
  
  output$moviePlot <- renderPlot({
    filmData <- filmDataInSpringfield %>%
      filter(Genre == input$selectedGenre)
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleGenreClick, xvar = "Year", yvar = "budget")[1,]
    print(str(movie))
    print(movie$TitleAndYear)
    movieText <- SpringfieldMatchedScripts %>% filter(titlesSmall==movie$TitleAndYear)
    movieTextTidy <- movieText %>% unnest_tokens(word, content)
    movieTextTidy %>% 
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word,n,max.word=20))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

