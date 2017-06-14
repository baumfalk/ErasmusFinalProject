library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(recommenderlab)
source("readData.R")


# Define UI for application that draws a histogram
ui <- 
  navbarPage("Movies that Matter",
             tabPanel(
               "Descriptive",
               fluidPage(
                 
                 # Application title
                 #titlePanel("Movies That Matter"),
                 
                 # Sidebar with a slider input for number of bins
                 
                 fluidRow(
                   column(3, selectInput("selectedGenre",
                                         label = h3("Select genre"),
                                         choices = genres)),
                   
                   column(4, offset = 1, selectInput("selectedGenre2",
                                                     label = h3("Select genre"),
                                                     choices = genres)),
                   
                   column(4, selectInput("selectedGenre3",
                                         label = h3("Select genre"),
                                         choices = genres))
                 ),
                 hr(),
                 
                 plotOutput("budgetPerYearPlot", click="handleGenreClick"),
                 
                 br(),
                 
                 verbatimTextOutput("info"),
                 plotOutput("moviePlot")
               )
             ),
             tabPanel("Recommender",
                      fluidPage(
                        
                        # Application title
                        #titlePanel("Movies That Matter"),
                        
                        # Sidebar with a slider input for number of bins
                        
                        fluidRow(
                          column(2, selectInput("selectedMinimalScore",
                                                label = h3("Minimal score"),
                                                choices = 1:5,
                                                selected = 3)),
                          
                          column(3, offset = 1, selectInput("selectedGender",
                                                            label = h3("Select gender"),
                                                            choices = sort(unique(movielensUserData$Gender)),
                                                            selected = sort(unique(movielensUserData$Gender))[1])),
                          
                          column(3, selectInput("selectedAge",
                                                label = h3("Select age"),
                                                choices = sort(unique(movielensUserData$Age)),
                                                selected = sort(unique(movielensUserData$Age)[1]))),
                          column(3, selectInput("selectedOccupation",
                                                label = h3("Select occupation"),
                                                choices = sort(unique(movielensUserData$Occupation)),
                                                selected = sort(unique(movielensUserData$Occupation)[1])))
                        ),
                        hr(),
                        uiOutput("film1"),
                        uiOutput("film2"),
                        uiOutput("film3")
                      )
                      ),
             tabPanel("Component 3")
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
  
  ##############
  
  # input$selectedMinimalScore
  # input$selectedGender
  # input$selectedAge
  # input$selectedOccupation
  
  
  # filter de lijst met films aan de hand van gemiddelde score voor users die voldoen aan
  # de drie gevraagde criteria.

  filteredMovies <- filmDataPivot
  filteredMoviesFunc <- reactive({
      filteredRaters <- movielensUserData %>%
      filter(Gender == input$selectedGender, Age == input$selectedAge, Occupation == input$selectedOccupation)
    
    filteredRatings <- movielensRatingData %>%
      filter(UserID %in% filteredRaters$UserID) %>%
      group_by(MovieID) %>%
      summarize(meanRating=mean(Rating), count=n()) %>%
      arrange(meanRating)
    
    filteredMovies <- filmDataPivot %>%
      filter(MovieID %in% filteredRatings$MovieID) %>%
      arrange(TitleAndYear)
    filteredMovies
  })
  
  
  # (gebruiker kiest uit deze lijst 1 tot 3 films)
  output$film1 <- renderUI({
      filteredMovies <- filteredMoviesFunc()
      selectInput("selMovie1",
              label = h3("Select first movie"),
              choices = filteredMovies$TitleAndYear
              )
  })
  
  output$film2 <- renderUI({
    filteredMovies <- filteredMoviesFunc()
    selectInput("selMovie2",
                label = h3("Select second movie"),
                choices = filteredMovies$TitleAndYear
    )
  })
  
  output$film3 <- renderUI({
    filteredMovies <- filteredMoviesFunc()
    selectInput("selMovie3",
                label = h3("Select third movie"),
                choices = filteredMovies$TitleAndYear
    )
  })
  
  # voor elke geselecteerde films:
  #   verkrijg uit cluster de top 20 dichtsbijzijnde films (uit IMDB+script films)
  
  selectedFilm1 <- filteredMovies$TitleAndYear[1]
  selectedFilm2 <- filteredMovies$TitleAndYear[2]
  selectedFilm3 <- filteredMovies$TitleAndYear[3]
  
  top20_1 <- filteredMovies$TitleAndYear[20:30]
  top20_2 <- filteredMovies$TitleAndYear[25:35]
  top20_3 <- filteredMovies$TitleAndYear[30:40]
  
  # sorteer de dichtsbijzijnde films op aantal voorkomens in deze 3 top-20s, daarna op IMDB-cijfer
  
  top20 <- data.frame(titlesSmall = c(top20_1,top20_2, top20_3))
  
  top20 <- top20 %>% group_by(titlesSmall) %>% summarise(count = n())
  
  subset <- springfieldDataIMDB %>% 
    inner_join(top20,by=c("titlesSmall"="titlesSmall")) %>% 
    inner_join(imdbData,by=c("titlesSmall"="TitleAndYear")) %>%
    arrange(desc(count),desc(imdb_score)) %>%
    head(20)
  summary(subset)
  
  # display de top 20 dichtsbijzijnde films
  
  
  
  # (gebruiker klikt op een film)
  # geef details over geselecteerde film
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

