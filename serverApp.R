# Define server logic required to draw a histogram


library(shiny)

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
      filter(meanRating >= input$selectedMinimalScore) %>%
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
  
  
  output$trailer <- renderUI({
    
    print("TEST")
    #krijg geselecteerde film
    subset <- sharedtop20()
    
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleRecMovieClick, xvar = "Year", yvar = "budget")[1,]
    print(movie)
    
    #krijg trailer
    urlEncodedTitle <- URLencode(movie$TitleAndYear,reserved = TRUE)
    
    searchQueryURL<- paste0("https://www.youtube.com/results?search_query=",urlEncodedTitle,"+official+trailer")
    
    trailerURL <- getFirstUrlFromSearchQuery(searchQueryURL)
    #geef trailer weer
    print(trailerURL)
    embedURL <- paste0('<iframe width="600" height="300" src="', trailerURL,'" frameborder="0" allowfullscreen></iframe>')
    HTML(embedURL)
  })
  
  distance <- function(f1, f2) {
    sqrt(sum((f1-f2)^2))
  }
  
  normalizedMovies <- reactive({
    normalize <- function(val,min,max) {
      val <- (val - min)/(max-min)
    }
    
    first_index <- which(names(filmDataPivot)=="Action")
    final_index <- which(names(filmDataPivot)=="Western")
    # normalize and filter
    movies <- filmDataPivot %>%
      select(MovieID, Title, Year, TitleAndYear, duration, imdb_score, first_index:final_index) %>%
      mutate(norm_dur = normalize(duration,min(duration),max(duration)),
             norm_score = normalize(imdb_score,min(imdb_score),max(imdb_score))) %>%
      select(-duration,-imdb_score)
    
  })
  
  output$filmInfo2 <- renderPrint({
    "test"
  })
  
  top20 <- function(movies,selectedFilm) {
    distances <- apply(movies[,c(-1,-2,-3,-4)],1,distance,f2=selectedFilm[-c(1,2,3,4)])
    top20_distances_indexes <- (order(distances))[2:23]
    
    top20_movies <- movies[top20_distances_indexes,]$TitleAndYear
  }
  
  
  sharedtop20 <- reactive({
    movies <- normalizedMovies()
    
    selectedFilm1 <- movies %>% filter(TitleAndYear == input$selMovie1)
    selectedFilm2 <- movies %>% filter(TitleAndYear == input$selMovie2)
    selectedFilm3 <- movies %>% filter(TitleAndYear == input$selMovie3)
    
    
    top20_1 <- top20(movies,selectedFilm1)
    top20_2 <- top20(movies,selectedFilm2)
    top20_3 <- top20(movies,selectedFilm3)
    
    # With base graphics, need to tell it what the x and y variables are.
    
    top20 <- data.frame(titlesSmall = c(top20_1,top20_2, top20_3))
    
    top20 <- top20 %>% 
      group_by(titlesSmall) %>%
      summarise(count = n()) %>%
      filter(!(titlesSmall %in% c(input$selMovie1, input$selMovie2, input$selMovie3 )))
    
    
    subset <- springfieldDataIMDBTitles %>% 
      inner_join(top20,by=c("titlesSmall"="titlesSmall")) %>% 
      inner_join(imdbData,by=c("titlesSmall"="TitleAndYear")) %>%
      arrange(desc(count),desc(imdb_score)) %>%
      head(20)
    subset
  })
  
  output$filmInfo <- renderPrint({
    
    # voor elke geselecteerde films:
    #   verkrijg uit cluster de top 20 dichtsbijzijnde films (uit IMDB+script films)
    
    
    
    subset <- sharedtop20()
    subset
    # nearPoints() also works with hover and dblclick events
  })
  
  output$recommendedMoviesPlot <- renderPlot({
    subset <- sharedtop20()
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    ggplot(data=filmData, aes(x=Year,y=budget)) + geom_point()
    
  })
  

  output$clickedMovieText <- renderPrint({
    subset <- sharedtop20()
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleRecMovieClick, xvar = "Year", yvar = "budget")[1,]
    print(str(movie))
    print(movie$TitleAndYear)
    c(movie$Title,movie$Year)
    # nearPoints() also works with hover and dblclick events
  })
  
  
  movieSentences <- reactive({
    subset <- sharedtop20()
    get_sentiments(c("nrc"))
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleRecMovieClick, xvar = "Year", yvar = "budget")[1,]
    
    #movie_script <- dat[501,c("content")]
    movie_script <- (SpringfieldMatchedScripts %>%
                       filter(titlesSmall==movie$TitleAndYear))$content
    
    
    movie_sentences <- data.frame(text = movie_script ) %>% 
      unnest_tokens(sentence, text, token = "sentences")
    movie_sentences$SentenceID <- seq(1:nrow(movie_sentences))
    movie_sentences <- movie_sentences  %>% ungroup() %>%
      unnest_tokens(word, sentence)
    movie_sentences
  })
  
  output$clickedMoviePlot <- renderPlot({
    subset <- sharedtop20()
    get_sentiments(c("nrc"))
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleRecMovieClick, xvar = "Year", yvar = "budget")[1,]
    
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
    subset <- sharedtop20()
    get_sentiments(c("nrc"))
    filmData <- filmDataInSpringfield %>%
      filter(TitleAndYear %in% subset$titlesSmall)
    
    # With base graphics, need to tell it what the x and y variables are.
    movie <- nearPoints(filmData, input$handleRecMovieClick, xvar = "Year", yvar = "budget")[1,]
    
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
  
  output$image1 = renderImage({
    url="https://images-na.ssl-images-amazon.com/images/M/MV5BMDU2ZWJlMjktMTRhMy00ZTA5LWEzNDgtYmNmZTEwZTViZWJkXkEyXkFqcGdeQXVyNDQ2OTk4MzI@._V1_UX182_CR0,0,182,268_AL_.jpg"
    
    z <- tempfile()
    download.file(url,z,mode="wb")
    pic <- readJPEG(z)
    
    list(
      src = z,
      contentType = "image/jpg",
      alt = "This is alternate text"
    )
  })
  
  # sorteer de dichtsbijzijnde films op aantal voorkomens in deze 3 top-20s, daarna op IMDB-cijfer
  
  
  
  
  # display de top 20 dichtsbijzijnde films
  
  
  
  # (gebruiker klikt op een film)
  # geef details over geselecteerde film
 
  
  n <- 10
  nImagesPerRow <- 5

  #filmnames <- as.character(sample(filmData$TitleAndYear,n)) %>% iconv("latin1", "ASCII", sub="")
  movieImages <- reactive({
    subset <- sharedtop20()
    
    filmnames <- subset$titlesSmall
    makeNiceImageURL <- function(filmname) {
      url <- paste("https://www.google.nl/search?q=",URLencode(paste(filmname,"movie poster"),reserved=TRUE),"&tbm=isch",sep="")
    }
    urls <- sapply(filmnames,makeNiceImageURL)
    binary_images <- list()
    print(getwd())
    for(i in 1:n) {
      print(paste("URL",i,":",urls[i]))
      html <- read_html(urls[i])
      img_url <- (html %>% html_nodes("img") %>% html_attr("src"))[1]
      
      img_file_url <- tempfile(fileext = ".jpg")
      download.file(img_url,img_file_url,mode="wb",cacheOK=FALSE)
      
      binary_image <- list(list(src = img_file_url,
                                contentType = "image/jpg",
                                title = filmnames[i],
                                height="250px")
      )
      binary_images <- append(binary_images,binary_image)
      print(paste(i,filmnames[i]))
    }
    binary_images
  })
  
  
  
  # #retrieve img url
  # for(x in 1:n) {
  # 
  #   #output[[paste("img",i,sep="")]] <- renderPlot({
  #    # plot(c(1,2,3))
  #   #})
  #   img <-
  #   print(x)
  #   output[[paste("img",1,sep="")]] <- img
  # }
  output[[paste("img",1,sep="")]] <- renderImage({
    movieImages()[[1]]
  },deleteFile=F) 
  output[[paste("img",2,sep="")]] <- renderImage({
    movieImages()[[2]]
  },deleteFile=F) 
  output[[paste("img",3,sep="")]] <- renderImage({
    movieImages()[[3]]
  },deleteFile=F) 
  output[[paste("img",4,sep="")]] <- renderImage({
    movieImages()[[4]]
  },deleteFile=F) 
  output[[paste("img",5,sep="")]] <- renderImage({
    movieImages()[[5]]
  },deleteFile=F) 
  output[[paste("img",6,sep="")]] <- renderImage({
    movieImages()[[6]]
  },deleteFile=F) 
  output[[paste("img",7,sep="")]] <- renderImage({
    movieImages()[[7]]
  },deleteFile=F) 
  output[[paste("img",8,sep="")]] <- renderImage({
    movieImages()[[8]]
  },deleteFile=F) 
  output[[paste("img",9,sep="")]] <- renderImage({
    movieImages()[[9]]
  },deleteFile=F) 
  output[[paste("img",10,sep="")]] <- renderImage({
    movieImages()[[10]]
  },deleteFile=F) 
  
    #binary_images[[1]]
  # })
  # output[[paste("img",2,sep="")]] <-  renderImage({
  #   binary_images[[2]]
  # })
  # output[[paste("img",3,sep="")]] <-  renderImage({
  #   binary_images[[3]]
  # })
  # output[[paste("img",4,sep="")]] <-  renderImage({
  #   binary_images[[4]]
  # })
  # output[[paste("img",5,sep="")]] <-  renderImage({
  #   binary_images[[5]]
  # })
  # output[[paste("img",6,sep="")]] <-  renderImage({
  #   binary_images[[6]]
  # })
  # output[[paste("img",7,sep="")]] <-  renderImage({
  #   binary_images[[7]]
  # })
  # output[[paste("img",8,sep="")]] <-  renderImage({
  #   binary_images[[8]]
  # })
  # output[[paste("img",9,sep="")]] <-  renderImage({
  #   binary_images[[9]]
  # })
  # output[[paste("img",10,sep="")]] <-  renderImage({
  #   binary_images[[10]]
  # })
  # 
  
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
        images <- append(images,list(column(2,imageOutput(img_name,height="500px"))))
        
        count <- count + 1
      }
      rows <- append(rows, list(fluidRow(images)))
    }
    fluidPage(rows)
    #})
  })
  
}