#fourth panel

fourthPanel <- tabPanel(
  "Explore preferences",
  fluidPage(
    titlePanel("Preference explorer"),
    fluidRow(
      column(3, wellPanel(
                  selectInput("selectedGender_pref",
                                        label = h3("Select gender"),
                                        choices = c("Female"= "F","Male"="M"),#sort(unique(movielensUserData$Gender)),
                                        selected = sort(unique(movielensUserData$Gender))[1]),
                  selectInput("selectedAge_pref",
                            label = h3("Select age"),
                            choices = age_list,
                            selected = sort(unique(movielensUserData$Age)[1])),
                  selectInput("selectedOccupation_pref",
                            label = h3("Select occupation"),
                            choices = occupation_list,
                            selected = sort(unique(movielensUserData$Occupation)[1])),
                  span("Total number of reviewers selected:",
             textOutput("n_reviewers_pref"))),
             sliderInput("num_reviews",label=h3("Select minimum number of reviews"),
                            0, 150, value=c(10,30))),
      column(9,ggvisOutput("dynamic_plot_2"),
      wellPanel(span("Total umber of different movies reviewd by reviewers :",textOutput("n_movies_pref"))))
  
    )
  )
)
