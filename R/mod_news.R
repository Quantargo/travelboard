# Module UI

#' @title   mod_news_ui and mod_news_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_news
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @import ggplot2
#' @import wordcloud2
#' @import rtweet


mod_news_ui <- function(id, dest){
  ns <- NS(id)
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    tags$h1(paste(dest, "Twitter News", sep = "-")),
    fluidRow(
      #  box( title='Source',selectInput(inputId='source', label = 'Select the tweeet source' ,
      #             choices=c('All')
      #              )
      # ),
      box(title="Sentiment of the tweets from this destination" ,plotOutput(ns("plot1"))),
      box(title="Hot or Not - destination trending based on #tweets" ,plotOutput(ns("plot2")))
      
    ),
   
    fluidRow( box(title="The most recent Twitter picture from this destination" ,imageOutput(ns("image"))) ,
              
              box(title="Cloud with most important words", plotOutput(ns("wordcloud")))
              ),
    fluidRow(h1(paste('Most Recent News from',dest))),
  fluidRow(
    column(  width = 12,
      title = paste('Most Recent News from',dest),DT::dataTableOutput(ns("newstable")
  )
  )
  )
  )
}

# Module Server

#' @rdname mod_news
#' @export
#' @keywords internal

mod_news_server <- function(input, output, session, dest){
  ns <- session$ns
  
  # This is just an example Server to be modified
  # Please change for your purpose
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data, main = dest())
  })
  
  
  
  # Select the right NY Times file based on the destination selected from the user----------
  dest_nytimes <- reactive({
    readRDS( paste0('~/workshop/data/twitter/',tolower(dest()), '_nytimes.rds')   )
    
  })
  
  # Select the right data file based on the destination selected from the user----------
  dest_data <- reactive({
    readRDS( paste0('~/workshop/data/twitter/',tolower(dest()), '.rds')   )
    
  })
  
  
 # observe({
 #   d <- dest_data()
  #  updateSelectInput(session,"source",choices=dplyr::distinct(d[, c("source")],source))
    
 # })
  
  ## Plot for the most recent News from NYT
  
  #  
  output$newstable <- DT::renderDataTable({
    nytimes = dest_nytimes()$data[, c("headline.main",'pub_date','abstract','web_url'), drop = FALSE]
    arranged_data <-  dplyr::arrange(nytimes,desc(pub_date))
    DT::datatable(data = arranged_data,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  
  output$image <- renderImage({
  
    recent<-
      dest_data()  %>% 
      as.data.frame %>% 
      dplyr::filter(!is.na(dest_data()$media_url)) %>%
      arrange(desc(created_at)) %>%
      slice(1)
    pic_url<- as.character(recent$media_url)
    
    fname <- tempfile(fileext = '.jpg')
    download.file(pic_url, fname, mode = 'wb')
    
    # Return a list containing the filename
    list(src = fname,
         contentType = 'image/jpg',
         width = '100%',
         height = '100%'
    )
  })
  
  output$plot1 <- renderPlot({
    
    bing_lex <- get_sentiments("bing")
    sentiment_words <- dest_data() %>% select(status_id, screen_name, created_at, text) %>% 
      unnest_tokens(word, text) %>% 
      inner_join(bing_lex, by = "word") 
    
    sentiment_words %>% 
      mutate(created_at_day = lubridate::as_date(lubridate::round_date(created_at, "day")),
             sentiment_num = ifelse(sentiment == "positive", 1, -1), 
             count = n()) %>%
      ggplot() + 
      geom_bar(aes(created_at_day, fill = sentiment), stat = "count") + 
      facet_wrap(~sentiment, ncol = 1)+
      labs(x='Date',y='# of tweets')
    
    
    
  })
  
  files <- list.files(path = "~/workshop/data/twitter/", pattern = "\\.rds$", full.names = TRUE)
  files <- files[regexpr("_",files)<0]
  
  coln <- gsub(".rds","",files) 
  coln <- gsub(".*/","",coln)                 
  #r <- lapply(files, read_rds)
  r <- lapply(files, readRDS)
  
  formattwr <- function(x,i){
    rt <- x[[i]]$created_at
    res <- as.data.frame(table(as.Date(rt)))
    res[,1] <- as.Date(res[,1])
    res
  }
  
  
  k <- reactive({
    location <- tolower(dest())
    locations <- c("crete","lisbon","mallorca","rome","vienna")
    which(locations == location) 
  })
  #k <- 3
  
  output$plot2 <- renderPlot({
    plot(formattwr(r,1), type="n", ylim = c(0,2000),xlab="",ylab="# tweets")
    
    colvec <- paste("grey",10*1:length(r), sep="")
    colvec[k()] <- "red"
    
    for (i in 1:length(r)) {
      lines(formattwr(r,i), pch = 18, lty= 3, col = colvec[i], cex = 0.8)
    }
    
    # Add a line
    lines(formattwr(r,k()), pch=18, col="red", type="b", lty=2, lwd = 3)
    # Add a legend
    legend(formattwr(r,1)[1,1], 2000, legend=coln,
           col=colvec, lty=3, cex=0.8)  
    
  })
  output$wordcloud <- renderPlot({
    
    
    ## Most important words
    
    tweet_words <- dest_data() %>% select(screen_name, created_at, text) %>% 
      unnest_tokens(word, text) %>%
      count(word, sort=T)
    my_stop_words <- tidytext::stop_words %>% select(-lexicon) %>% 
      bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))
    tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words, by = "word")
    
    top_50 <- tweet_words_interesting %>% 
      group_by(word) %>% 
      tally(sort=TRUE) %>% 
      slice(1:50)
    
    top_50$n<-floor((top_50$n)^(1/3))
    
    top_50 %>% mutate(word = reorder(word, n, function(n) -n)) %>%
      ggplot() + geom_bar(aes(word, n), stat = "identity") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
      xlab("")
    
    
    ## Create word cloud
    wordcloud::wordcloud(top_50[, 1, drop=TRUE], top_50[, 2, drop=TRUE])#,color="random-light", shape="cardioide", size = .4, shuffle=T, rotateRatio = sample(c(1:100) / 100))
    
  })
  
}

## To be copied in the UI
# mod_news_ui("news_ui_1")

## To be copied in the server
# callModule(mod_news_server, "news_ui_1")

