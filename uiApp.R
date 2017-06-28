# Define UI for application that draws a histogram

library(shiny)

firstPanel <- tabPanel("Recommender",
                        fluidPage(
                          
                          # Application title
                          #titlePanel("Movies That Matter"),
                          
                          # Sidebar with a slider input for number of bins
                          
                          fluidRow(
                            column(2, selectInput("selectedMinimalScore",
                                                  label = h3("Minimal score on movielens (best=5)"),
                                                  choices = 1:5,
                                                  selected = 3)),
                            
                            column(3, offset = 1, selectInput("selectedGender",
                                                              label = h3("Select gender"),
                                                              choices = c("Female"= "F","Male"="M"),#sort(unique(movielensUserData$Gender)),
                                                              selected = sort(unique(movielensUserData$Gender))[1])),
                            
                            column(3, selectInput("selectedAge",
                                                  label = h3("Select age"),
                                                  choices = age_list,
                                                  selected = sort(unique(movielensUserData$Age)[1]))),
                            column(3, selectInput("selectedOccupation",
                                                  label = h3("Select occupation"),
                                                  choices = occupation_list,
                                                  selected = sort(unique(movielensUserData$Occupation)[1])))
                          ),
                          hr(),
                          fluidRow(
                            column(4,uiOutput("film1")),
                            column(4,uiOutput("film2")),
                            column(4,uiOutput("film3"))
                          ),
                          hr(),
                          
                          #
                          uiOutput("images"),
                          
                          br(),
                          hr(),
                          uiOutput("trailer"),
                          
                          plotOutput("clickedMovieWordcloud"),
                          plotOutput("clickedMoviePlot"),
                          plotOutput("clickedMoviePlotSentiment"),
                          plotOutput("clickedMovieRadarPlot"),
                          verbatimTextOutput("clickedMovieText")
                          
                        )
)

secondPanel <- tabPanel(
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

source("thirdpanel.R")
source("fourthPanel.R")


ui <- 
  navbarPage("Movies that Matter",
             firstPanel,
             secondPanel,
             thirdPanel,
             fourthPanel
  )