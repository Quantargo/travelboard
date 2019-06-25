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

library(dplyr)
mod_news_ui <- function(id, dest){
  ns <- NS(id)

  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    tags$h1(paste(dest, "Twitter News", sep = "-")),
    fluidRow(
      box( title='Source',
           selectInput(inputId='source', label = 'Select the tweeet source' ,
                       choices=c('All')
                       )
           ),
      box(title = dest, plotOutput(ns("plot1"), height = 250)),
      
      box(
        title = "Controls",
        sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
      ),
      box( title = paste('Most Recent News from',dest),
           
             DT::dataTableOutput(ns("newstable"))
           
           
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
   
  
  observe({
    d <- dest_data()
    updateSelectInput(session,"source",choices=dplyr::distinct(d[, c("source")],source))
    
  })
  
  ## Plot for the most recent News from NYT
  
 #  
 output$newstable <- DT::renderDataTable({
    nytimes = dest_nytimes()$data[, c("headline.main",'pub_date'), drop = FALSE]
    arranged_data <-  dplyr::arrange(nytimes,desc(pub_date))
    DT::datatable(data = arranged_data,
                  options = list(pageLength = 10),
                  rownames = FALSE)

 })
  
 
 
  
}
    
## To be copied in the UI
# mod_news_ui("news_ui_1")
    
## To be copied in the server
# callModule(mod_news_server, "news_ui_1")
 
