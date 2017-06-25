#fourth panel

fourthPanel <- tabPanel(
  "Explore preferences",
  fluidPage(
    titlePanel("Preference explorer"),
    fluidRow(
      column(3, offset = 1, selectInput("selectedGender_pref",
                                        label = h3("Select gender"),
                                        choices = c("Female"= "F","Male"="M"),#sort(unique(movielensUserData$Gender)),
                                        selected = sort(unique(movielensUserData$Gender))[1])),
      
      column(3, selectInput("selectedAge_pref",
                            label = h3("Select age"),
                            choices = age_list,
                            selected = sort(unique(movielensUserData$Age)[1]))),
      column(3, selectInput("selectedOccupation_pref",
                            label = h3("Select occupation"),
                            choices = occupation_list,
                            selected = sort(unique(movielensUserData$Occupation)[1]))),
      column(3, sliderInput("num_reviews",label=h3("Select minimum number of reviews"),
                            0,
                            3428,
                            value=c(10,30))),
      
      hr(),

      ggvisOutput("dynamic_plot_2")

    )
  )
)
