# Define server logic required to draw a graphs etc.

library(ggvis)
library(shiny)
library(fmsb)


server <- function(input, output, session) {
  
  clicked_value <- reactiveValues() #MovieID = as.integer(1)
  clicked_value$MovieID <- as.integer(0)
  movie_click<-function(data, ...){
    clicked_value$MovieID <- as.integer(data$MovieID)
    str(clicked_value$MovieID)
    clicked_value$MovieID
  }
  
  vals <- reactiveValues()
  n <- 10
  nImagesPerRow <- 5
  
  output$budgetPerYearPlot <- renderPlot({
    filmData <- filmDataInSpringfield %>%
      filter(Genre == input$selectedGenre)
    ggplot(data=filmData, aes(x=Year,y=budget)) + geom_point()
  })
  
  movies <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    minyear <- input$year_explore[1]
    maxyear <- input$year_explore[2]
    
    # Apply filters
    m <- filmData %>%
      filter(
        Year >= minyear,
        Year <= maxyear
        
      ) %>%
      distinct(MovieID,.keep_all=T) %>%
      arrange(imdb_score)
    
    # Optional: filter by genre
    if (input$genre_explore != "All") {
      genre <- paste0(input$genre_explore)
      m <- m %>% filter(grepl(genre,Genre))
    }
    # Optional: filter by title
    if (!is.null(input$title_explore) && input$title_explore != "") {
      m_title <- paste0(input$title_explore)
      m <- m %>% filter(grepl(m_title,titlesSmall))
    }
    m <- as.data.frame(m)
    
  })
  
  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$MovieID)) return(NULL)

    # Pick out the movie with this ID
    all_movies <- isolate(movies())
    movie <- filmData[filmData$MovieID == x$MovieID, ] %>%
      distinct(MovieID,.keep_all=T)

    paste0("<b>", movie$TitleAndYear, "</b><br>",
           movie$Year, "<br>",
           "$", format(movie$budget, big.mark = ",", scientific = FALSE)
    )
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    movies %>%
    ggvis(x = xvar, y = yvar) %>%
    layer_points(size := 50, size.hover := 200,
                 fillOpacity := 0.2, fillOpacity.hover := 0.5,
                 key := ~MovieID) %>%
    add_tooltip(movie_tooltip, "hover") %>%
    handle_click(movie_click) %>%
    add_axis("x", title = xvar_name) %>%
    add_axis("y", title = yvar_name) %>%
    set_options(width = 500, height = 500)
  })

  #selectedMovieID <- reactive(input$movieSelect)
  Selected_MovieID <- reactive({ 
    selection <- clicked_value$MovieID
    selection })
    
  output$selected_MovieID <- renderText({
      selectedMovieID <- Selected_MovieID()
      
      selection <- filmDataPivot %>%
      filter(MovieID == selectedMovieID) %>%
      select(MovieID) %>% head(1) 
      selection$MovieID
    })
  output$n_movies <- renderText({ nrow(movies())})
  
  
  vis %>% bind_shiny("dynamic_plot")
  
  
  ##############
  
  filteredMovies_pref <- filmDataPivot
  filteredRaters_pref <- movielensUserData
  
  filteredRatersFunc_pref <- reactive({ 
    filteredRaters_pref <- movielensUserData %>%
    filter(Gender == input$selectedGender_pref, 
           Age == input$selectedAge_pref, 
           Occupation == input$selectedOccupation_pref)
    filteredRaters_pref
  })
  
  filteredMoviesFunc_pref <- reactive({
    filteredRaters_pref <- filteredRatersFunc_pref()
    
    filteredRatings_pref <- movielensRatingData %>%
      filter(UserID %in% filteredRaters_pref$UserID) %>%
      group_by(MovieID) %>%
      summarize(meanRating=mean(Rating), count=n())# %>%
      #filter(meanRating >= input$selectedMinimalScore) %>%
      #arrange(meanRating)
    
    filteredMovies_pref <- filmDataPivot %>%
      filter(MovieID %in% filteredRatings_pref$MovieID)%>%
      left_join(filteredRatings_pref,by='MovieID')%>%
      arrange(TitleAndYear) %>%
      distinct(MovieID,.keep_all=T) %>%
      filter(count >= input$num_reviews[1],
             count <= input$num_reviews[2])
    filteredMovies_pref
  })
  
  movie_tooltip_2 <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$MovieID)) return(NULL)
    
    # Pick out the movie with this ID
    movie <- filteredMoviesFunc_pref() %>%
      filter(MovieID==x$MovieID) %>%
      distinct(MovieID,.keep_all=T)
      
    
    paste0("<b>", movie$TitleAndYear, "</b><br>",
           "number of reviews ", movie$count,"<br>",
           "mean rating ", movie$meanRating,"<br>",
           "budget $", format(movie$budget, big.mark = ",", scientific = FALSE)
    )
  }
  
  vis2 <- reactive({
    xvar_name <- names(axis_vars)[axis_vars == input$xvar_2]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar_2]
    
    xvar <- prop("x", as.symbol(input$xvar_2))
    yvar <- prop("y", as.symbol(input$yvar_2))
    
    filteredMovies_pref <- filteredMoviesFunc_pref()
    
    filteredMovies_pref  %>%
      ggvis(x = xvar, y = yvar, fill= ~count, size= ~meanRating) %>%
            layer_points(size.hover := 300,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   key := ~MovieID) %>%
      add_tooltip(movie_tooltip_2, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend(scales = "size", title="Mean movielens rating \n of selected users", properties = legend_props(legend = list(y = 0)))%>%
      add_legend(scales = "fill",title="Number of reviews", properties = legend_props(legend = list(y = 150)))%>%
      set_options(duration = 0)
      })
  
  vis2 %>% bind_shiny("dynamic_plot_2")
  
  output$n_movies_pref <- renderText({ nrow(filteredMoviesFunc_pref())})
  output$n_reviewers_pref <- renderText({ nrow(filteredRatersFunc_pref())})  
  ##############
  
 
  # filter de lijst met films aan de hand van gemiddelde score voor users die voldoen aan
  # de drie gevraagde criteria.
  filteredMoviesFunc <- reactive({
    # filteredRaters <- movielensUserData %>%
    #   filter(Gender == input$selectedGender, Age == input$selectedAge, Occupation == input$selectedOccupation)
    # 
    # filteredRatings <- movielensRatingData %>%
    #   filter(UserID %in% filteredRaters$UserID) %>%
    #   group_by(MovieID) %>%
    #   summarize(meanRating=mean(Rating), count=n()) %>%
    #   filter(meanRating >= input$selectedMinimalScore) %>%
    #   arrange(meanRating)
    # 
    # filteredMovies <- filmDataPivot %>%
    #   filter(MovieID %in% filteredRatings$MovieID) %>%
    #   arrange(TitleAndYear)
    filmDataPivotInSpringfield %>%
      arrange(TitleAndYear)
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
  
  getFirstLinkFromHTML <- function(raw_html){
    tmp <- raw_html %>% html_nodes("a") %>% html_attr("href")
    tmp_df <- data.frame(url=tmp,stringsAsFactors = F) %>%
      filter(startsWith(url,"/watch?v=")) %>%
      mutate(url=gsub("\\Qwatch?v=\\E","embed/",url))
    return(paste0("//www.youtube.com",tmp_df$url[1]))
  }
  
  getFirstUrlFromSearchQuery <- function(searchurl){
    #HTML van searchlink opslaan
    HTMLRAW <- read_html(searchurl)
    #Eerste link weergeven
    url <- getFirstLinkFromHTML(HTMLRAW)
    #Teruggeven eerste link
    return(url)
  }
  
  distance <- function(f1, f2) {
    sqrt(sum((f1-f2)^2))
  }
  
  
  normalize <- function(val,min,max) {
    (val - min)/(max-min)
  }
  normalizedMovies <- reactive({

    first_index <- which(names(filmDataPivot)=="Action")
    final_index <- which(names(filmDataPivot)=="Western")
    # normalize and filter
    movies <- filmDataPivotInSpringfield %>%
      select(MovieID, Title, Year, TitleAndYear, duration, imdb_score, first_index:final_index) %>%
      mutate(norm_dur = normalize(duration,min(duration),max(duration)),
             norm_score = normalize(imdb_score,min(imdb_score),max(imdb_score))) %>%
      select(-duration,-imdb_score)
    
  })
  
  top20 <- function(movies,selectedFilm) {
    distances <- movieDistances[as.character(selectedFilm$MovieID[1]),]
    top20_distances_indexes <- (order(distances))[2:51]
    movieIDs <- as.numeric(names(movieDistances[as.character(selectedFilm$MovieID[1]),top20_distances_indexes]))
    movieIDsDF <- data.frame(num=1:50, MovieID=movieIDs, distances=distances[top20_distances_indexes])
    
    top20_movies <- movies %>%
      inner_join(movieIDsDF) %>% 
      arrange(num) 
    top20_movies$index <- 1:nrow(top20_movies)
    top20_movies
  }
  
  
  sharedtop20 <- reactive({
    movies <- na.omit(normalizedMovies())
    
    selectedFilm1 <- movies %>% filter(TitleAndYear == input$selMovie1)
    selectedFilm2 <- movies %>% filter(TitleAndYear == input$selMovie2)
    selectedFilm3 <- movies %>% filter(TitleAndYear == input$selMovie3)
    
    
    top20_1 <- top20(movies,selectedFilm1)
    top20_2 <- top20(movies,selectedFilm2)
    top20_3 <- top20(movies,selectedFilm3)
    
    # With base graphics, need to tell it what the x and y variables are.
    
    top20 <- rbind(top20_1,top20_2,top20_3)
    top20 <- top20 %>%
      filter(!(TitleAndYear %in% c(selectedFilm1$TitleAndYear,
                                 selectedFilm2$TitleAndYear,
                                 selectedFilm3$TitleAndYear))) %>%
      group_by(TitleAndYear) %>%
      summarize(count=n(),
                meanDistCountAdjusted = mean(distances)/count,
                MovieID = mean(MovieID)) %>%
      arrange(meanDistCountAdjusted)
    top20 %>% head(n)
   })
  
  movieSentences <- reactive({
    subset <- sharedtop20()
    get_sentiments(c("nrc"))
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleRecMovieClick, xvar = "Year", yvar = "budget")[1,]
    
    movie_script <- (SpringfieldMatchedScripts %>%
                       filter(titlesSmall==movie$TitleAndYear))$content
    
    
    movie_sentences <- data.frame(text = movie_script ) %>% 
      unnest_tokens(sentence, text, token = "sentences")
    movie_sentences$SentenceID <- seq(1:nrow(movie_sentences))
    movie_sentences <- movie_sentences  %>% ungroup() %>%
      unnest_tokens(word, sentence)
    movie_sentences
  })
  


  #filmnames <- as.character(sample(filmData$TitleAndYear,n)) %>% iconv("latin1", "ASCII", sub="")
  movieImages <- reactive({
    films <- sharedtop20()
  
    vals[["filmIDs"]] <- films$MovieID
    binary_images <- list()
    for(i in 1:n) {
      
      img_file_url <- paste0("data/images/",films$MovieID[i],".jpg")
      binary_image <- list(list(src = img_file_url,
                                contentType = "image/jpg",
                                title = films$TitleAndYear[i],
                                height="250px"))
      print(paste0(films$TitleAndYear[i],binary_image))
      binary_images <- append(binary_images,binary_image)
    }
    binary_images
  })
  
  lapply(1:10,function(i) {
    output[[paste("img",i,sep="")]] <- renderImage({
      movieImages()[[i]]
    },deleteFile=F) 
  })
 
  output[["images"]] <- renderUI({
    
    numRows <- ceiling(n/nImagesPerRow)
    
    # voor elke rij
    rows <- list()
    count<-1
    for(i in 1:numRows) {
      # voeg de nImagesPerRow toe
      images <- list()
      for(j in 1:nImagesPerRow) {
        img_name <- paste("img",count,sep="")
        #img_click <- paste("img_","_click",sep="")
        img_click <- paste0("img_click","_",(i-1)*nImagesPerRow+j)
        images <- append(images,list(column(2,imageOutput(img_name, height="500px",
                                                          click = img_click))))

        count <- count + 1
      }
      rows <- append(rows, list(fluidRow(images)))
    }
    fluidPage(rows)
    
  })
  
  lapply(
    X = 1:10,
    FUN = function(i){
      print("ok...")
      observeEvent(input[[paste0("img_click_", i)]], {
        vals[[paste0("clicked")]] <- i
        print(paste0("clicked on ",i))
      })
    }
  )
  
  output$clickedMovieText <- renderPrint({
    #c(input[["img_click_1"]])
    list <- reactiveValuesToList(vals)
    id <- vals[["clicked"]]
    list[["filmIDs"]][id]
  })
  
  
  output$clickedMoviePlot <- renderPlot({
    
    list <- reactiveValuesToList(vals)
    id <- vals[["clicked"]]
    selectedMovieID <- list[["filmIDs"]][id]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
      filter(MovieID == selectedMovieID))[1,]
   
    get_sentiments(c("nrc"))
    
    #movie_script <- dat[501,c("content")]
    movie_script <- (SpringfieldMatchedScripts %>%
                       filter(titlesSmall==movie$TitleAndYear))$content
    
    
    movie_sentences <- data.frame(text = movie_script ) %>% 
      unnest_tokens(sentence, text, token = "sentences")
    movie_sentences$SentenceID <- seq(1:nrow(movie_sentences))
    movie_sentences <- movie_sentences  %>% ungroup() %>%
      unnest_tokens(word, sentence)
    
    moviesentiment2 <- movie_sentences %>%
      inner_join(get_sentiments("nrc"), by="word") %>%
      count(index = SentenceID %/% 40, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(som = joy + anger + anticipation + disgust + fear + sadness + surprise + trust) %>%
      gather(emotion,value, -index,-som) %>%
      filter(#value != 0, 
        emotion != "sentiment",
        emotion != "positive",
        emotion != "negative") %>%
      mutate(perc = value/som)
   
    g <- moviesentiment2 %>% ggplot () %>% +geom_area(aes(x=index, y=perc,fill=emotion)) +
      ggtitle(paste("Emotional distribution during the movie",movie$TitleAndYear))
    g
  })
  
  
  output$clickedMoviePlotSentiment <- renderPlot({
    list <- reactiveValuesToList(vals)
    id <- vals[["clicked"]]
    selectedMovieID <- list[["filmIDs"]][id]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
   
    get_sentiments(c("nrc"))
    
    #movie_script <- dat[501,c("content")]
    movie_script <- (SpringfieldMatchedScripts %>%
                       filter(titlesSmall==movie$TitleAndYear))$content
    
    
    movie_sentences <- data.frame(text = movie_script ) %>% 
      unnest_tokens(sentence, text, token = "sentences")
    movie_sentences$SentenceID <- seq(1:nrow(movie_sentences))
    movie_sentences <- movie_sentences  %>% ungroup() %>%
      unnest_tokens(word, sentence)
    
    moviesentiment1 <- movie_sentences %>%
      inner_join(get_sentiments("nrc"), by="word") %>%
      count(index = SentenceID %/% 40, sentiment) %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
    g2 <- ggplot(moviesentiment1, aes(index, sentiment)) +
      geom_col(show.legend = FALSE) + ggtitle(paste("Sentiment during the movie",movie$TitleAndYear))
    
    g2
  })
  
  output$trailer <- renderUI({
    #krijg geselecteerde film
    list <- reactiveValuesToList(vals)
    id <- vals[["clicked"]]
    selectedMovieID <- list[["filmIDs"]][id]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
    
    
    #krijg trailer
    urlEncodedTitle <- URLencode(if_else(is.na(movie$TitleAndYear),
                                         "dQw4w9WgXcQ",movie$TitleAndYear),
                                 reserved = TRUE)
    
    searchQueryURL<- paste0("https://www.youtube.com/results?search_query=",urlEncodedTitle,"+official+trailer")
    
    trailerURL <- getFirstUrlFromSearchQuery(searchQueryURL)
    #geef trailer weer
    print(trailerURL)
    embedURL <- paste0('<iframe width="600" height="300" src="', trailerURL,'" frameborder="0" allowfullscreen></iframe>')
    HTML(embedURL)
  })
  
  output$clickedMovieRadarPlot <- renderPlot({
    list <- reactiveValuesToList(vals)
    id <- vals[["clicked"]]
    selectedMovieID <- list[["filmIDs"]][id]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
  
    sentiments <- movie_sentiment_IMDB[movie_sentiment_IMDB$index==movie$TitleAndYear,]
    anger <- sentiments[[12]]
    anticipation <- sentiments[[13]]
    disgust <- sentiments[[14]]
    fear <- sentiments[[15]]
    joy <- sentiments[[16]]
    sadness <- sentiments[[19]]
    surprise <- sentiments[[20]]
    trust <- sentiments[[21]]
    
    whose <- c(movie$TitleAndYear)
    
    df2 <- data.frame(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)
    df2 <- normalize(df2,min(df2),max(df2))
    rownames(df2) <- whose
    df2 <- rbind(rep(1,8),rep(0,8),df2)
    
    #==================
    # Plot 2: Same plot with custom features
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart( df2  , axistype=1 , 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=(0:10), cglwd=0.8,
                #custom labels
                vlcex=1.7,
                seg=10
    )
    #legend(x=0.7, y=1, legend = rownames(df2[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
    
  })
  
  output$clickedMovieWordcloud <- renderPlot({
    list <- reactiveValuesToList(vals)
    id <- vals[["clicked"]]
    selectedMovieID <- list[["filmIDs"]][id]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
    movieText <- SpringfieldMatchedScripts %>% filter(titlesSmall==movie$TitleAndYear)
    movieTextTidy <- movieText %>% unnest_tokens(word, content)
    movieTextTidy %>% 
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word,n,max.word=20))
  })
  
  #####
  
  output$clickedMovieWordcloud_2 <- renderPlot({
    list <- reactiveValuesToList(clicked_value)
    selectedMovieID <- list[["MovieID"]]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
    
    movieText <- SpringfieldMatchedScripts %>% filter(titlesSmall==movie$TitleAndYear)
    movieTextTidy <- movieText %>% unnest_tokens(word, content)
    movieTextTidy %>% 
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word,n,max.word=20))
  })
  
  output$trailer_2 <- renderUI({
    #krijg geselecteerde film
    list <- reactiveValuesToList(clicked_value)
    selectedMovieID <- list[["MovieID"]]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
    
    
    #krijg trailer
    urlEncodedTitle <- URLencode(if_else(is.na(movie$TitleAndYear),
                                         "dQw4w9WgXcQ",movie$TitleAndYear),
                                 reserved = TRUE)
    
    searchQueryURL<- paste0("https://www.youtube.com/results?search_query=",urlEncodedTitle,"+official+trailer")
    
    trailerURL <- getFirstUrlFromSearchQuery(searchQueryURL)
    #geef trailer weer
    print(trailerURL)
    embedURL <- paste0('<iframe width="600" height="300" src="', trailerURL,'" frameborder="0" allowfullscreen></iframe>')
    HTML(embedURL)
  })
  
  
  output$clickedMovieRadarPlot_2 <- renderPlot({
    list <- reactiveValuesToList(clicked_value)
    selectedMovieID <- list[["MovieID"]]
    if(is.null(selectedMovieID) || length(selectedMovieID) == 0) {
      return()
    }
    # With base graphics, need to tell it what the x and y variables are.
    movie <- (filmDataPivot %>%
                filter(MovieID == selectedMovieID))[1,]
    
    sentiments <- movie_sentiment_IMDB[movie_sentiment_IMDB$index==movie$TitleAndYear,]
    anger <- sentiments[[12]]
    anticipation <- sentiments[[13]]
    disgust <- sentiments[[14]]
    fear <- sentiments[[15]]
    joy <- sentiments[[16]]
    sadness <- sentiments[[19]]
    surprise <- sentiments[[20]]
    trust <- sentiments[[21]]
    
    whose <- c(movie$TitleAndYear)
    
    df2 <- data.frame(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)
    df2 <- normalize(df2,min(df2),max(df2))
    rownames(df2) <- whose
    df2 <- rbind(rep(1,8),rep(0,8),df2)
    
    #==================
    # Plot 2: Same plot with custom features
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart( df2  , axistype=1 , 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=(0:10), cglwd=0.8,
                #custom labels
                vlcex=1.7,
                seg=10
    )
    #legend(x=0.7, y=1, legend = rownames(df2[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
    
  })
  
}