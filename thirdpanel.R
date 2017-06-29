library(ggvis)

actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

thirdPanel <- tabPanel(
  "Explore genres",
  fluidPage(
    titlePanel("Movie explorer"),
    fluidRow(
      column(3,
             wellPanel(
               h4("Filter Genre"),
               sliderInput("year_explore", "Year released", 
                           min(imdbData$title_year,na.rm=T), max(imdbData$title_year,na.rm=T), 
                           value = c(1970, 2001)),
               selectInput("genre_explore", "Genre (a movie can have multiple genres)",
                           c("All",genres), multiple = F
               ),
               textInput("title_explore", "Title of the movie contains (e.g. pulp fiction)")
             ),
             wellPanel(
               h4("Select axis variables"),
               selectInput("xvar", "X-axis variable", axis_vars, selected = "Year"),
               selectInput("yvar", "Y-axis variable", axis_vars, selected = "budget"),
               tags$small(paste0(""))
             )
      ),
      column(9,
             ggvisOutput("dynamic_plot"),
             wellPanel(
               span("Number of movies selected:",
                    textOutput("n_movies"),
                    "ID selected movie",
                    textOutput("selected_MovieID")
               )
             )
      )
    )
  )
)