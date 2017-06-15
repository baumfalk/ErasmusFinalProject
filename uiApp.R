# Define UI for application that draws a histogram

library(shiny)

firstPanel <- tabPanel(
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
)

secondPanel <- tabPanel("Recommender",
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
                          uiOutput("film3"),
                          verbatimTextOutput("filmInfo"),
                          verbatimTextOutput("filmInfo2"),
                          hr(),
                          
                          plotOutput("recommendedMoviesPlot", click="handleRecMovieClick"),
                          
                          br(),
                          hr(),
                          verbatimTextOutput("clickedMovieText"),
                          plotOutput("clickedMoviePlot")
                        )
)


ui <- 
  navbarPage("Movies that Matter",
             firstPanel,
             secondPanel,
             tabPanel("Component 3")
  )