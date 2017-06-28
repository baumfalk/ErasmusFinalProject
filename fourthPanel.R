#fourth panel

fourthPanel <- tabPanel(
  "Explore preferences",
  fluidPage(
    titlePanel("Preference explorer"),
    fluidRow(
      column(2, wellPanel(
                  h4("Filter reviewers"),
                  selectInput("selectedGender_pref",
                                        label = "Select gender",
                                        choices = c("Female"= "F","Male"="M"),#sort(unique(movielensUserData$Gender)),
                                        selected = sort(unique(movielensUserData$Gender))[1]),
                  selectInput("selectedAge_pref",
                            label = "Select age",
                            choices = age_list,
                            selected = sort(unique(movielensUserData$Age)[1])),
                  selectInput("selectedOccupation_pref",
                            label = "Select occupation",
                            choices = occupation_list,
                            selected = sort(unique(movielensUserData$Occupation)[1])),
                  span("Total number of reviewers selected:",
             textOutput("n_reviewers_pref")))),
      column(2, wellPanel(sliderInput("num_reviews",label=h4("Select number of reviews"),
                            0, 150, value=c(10,30))),
               wellPanel(
                            h4("Select axis variables"),
                            selectInput("xvar_2", "X-axis variable", axis_vars, selected = "Year"),
                            selectInput("yvar_2", "Y-axis variable", axis_vars, selected = "imdb_score"),
                            tags$small(paste0(""))
                          )
                
            ),
      column(8,ggvisOutput("dynamic_plot_2"),
      wellPanel(span("Total number of movies displayed :",textOutput("n_movies_pref"))))
  
    )
  )
)
