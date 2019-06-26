# Module UI
  
#' @title   mod_airbnb_ui and mod_airbnb_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_airbnb
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
#' @import tidyverse
#' @import leaflet
#' @import leaflet.extras
#' @import googleway
#' @import rgdal
#' @import xml2
#' @import stringr
#' @importFrom rvest html_text
mod_airbnb_ui <- function(id, dest){
  ns <- NS(id)

  tagList(
    
    #tags$h1(paste(dest, "Airbnb - Filter", sep = "-")),
    tags$head(tags$style(HTML(".box {margin: 5px;}"))),
    
    fluidRow(
      box(title = paste(dest, " Airbnb - Filter", sep = " -"), width = '100%', height = "140px", 
          div(style="display: inline-block;vertical-align:top; width: 200px;",sliderInput(ns("input_price"), "Price", 0, 500, value = c(0,500), dragRange = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 120px;",sliderInput(ns("input_beds"), "No. of beds", 0, 50, value = c(0,50), dragRange = TRUE)),
          #div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          #div(style="display: inline-block;vertical-align:top; width: 120px;",numericInput(ns("input_rating"), "Min. rating", value = 0, min = 0, max = 100)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 400px;",selectInput(ns("input_city"), "City", choices = c(), multiple = TRUE))
          )
    ),
    fluidRow(
      box(leafletOutput(outputId = ns("map1"), width="100%", height = "250px"), width = '100%')
    ),
    
    fluidRow(
      verbatimTextOutput(ns("reviewout"))
    ),
    
    fluidRow(
      column(6, div(DT::dataTableOutput(ns("airbnb_table")), style = "font-size: 80%")),
      column(6, plotOutput(ns("price_distr"), height = "350px")) 
    )
    
  )

}
    
# Module Server
    
#' @rdname mod_airbnb
#' @export
#' @keywords internal
#' @importFrom rlang .data
mod_airbnb_server <- function(input, output, session, dest){
  ns <- session$ns
  
  # Loads the selected data
  dt_country <- reactive({
    dt_imported <- readRDS(paste0("~/workshop/data/airbnb/", tolower(dest()), ".rds"))
    dt_imported <- dt_imported %>% mutate(price = as.numeric(sub("$", "", .data$price, fixed = TRUE)))
  })
  
  # Country level price data 
  total_price  <- reactive({
    data.frame(sample = "Total", price = as.matrix(dt_country()$price))
  })
  
  dt_filtered <- reactive({
    
    dt_filtered <- filter(dt_country(), 
                          .data$price >= input$input_price[1] & .data$price <= input$input_price[2],
                          .data$beds >= input$input_beds[1] & .data$beds <= input$input_beds[2])  %>% 
                            filter(eval(parse(text = ifelse(is.null(input$input_city),TRUE, 
                                              paste0("city %in% c(",'"',paste0(input$input_city,collapse='","'), '")')))))
  })
  
  observe({
    updateSliderInput(session, "input_price", value = c(0,max(dt_country()$price, na.rm = TRUE)), 
                      max = max(dt_country()$price, na.rm = TRUE))
    updateSliderInput(session, "input_beds", value = c(0,max(dt_country()$beds, na.rm = TRUE)), 
                      max = max(dt_country()$beds, na.rm = TRUE))
    updateSelectInput(session, "input_city", choices = sort(unique(dt_country()$city)))
  })
  


  output$airbnb_table <- DT::renderDataTable(
      DT::datatable(data = dt_filtered()[, c("name","city","price", "beds", "review_scores_rating", "room_type")],
                    selection = "single",
                    options = list(pageLength = 5), 
                    rownames = FALSE)
  )
  

  observeEvent(input$airbnb_table_rows_selected, {
    rownum <- req(input$airbnb_table_rows_selected)
    selectedRow <- dt_filtered()[rownum,]
    dialog <- modalDialog(title = selectedRow[, "name"],
                          selectedRow[, "description"], br(), br()
                          , HTML(paste0('<a href="', selectedRow[, "listing_url"], '">Site on AirB&B</a>')), br(), br() 
                          , HTML(paste0("<img width='100%' src='",dt_filtered()[rownum,"picture_url"], "'>"))
                         , easyClose = TRUE)
    showModal(dialog)
    # Clear selection
    DT::dataTableProxy("airbnb_table") %>% DT::selectRows(NULL)
  })
  
  #leeaflet graphics:

  house = makeIcon("home.png",25,25)
  
  output$map1 <- renderLeaflet({  
    leaflet() %>% 
      addProviderTiles(leaflet::providers$Hydda.Full) %>% 
      addMarkers(lng=dt_filtered()$longitude, 
                 lat=dt_filtered()$latitude, popup=dt_filtered()$name, 
                 clusterOptions = markerClusterOptions(), 
                 icon = house)
  })
  
  #Webscraping the latest review:
  observe({

    click<- input$map1_marker_click
    if(is.null(click))
      return()
  
    lat <- click$lat
    lng <- click$lng
    url2 <- dt_filtered() %>% 
      filter(.data$latitude == lat, .data$longitude == lng) %>% 
      select(.data$listing_url) %>%
      paste()
    

    if(identical(url2, "character(0)")){
        output$reviewout<-renderText({
          ""
        })
        return()
    }
    
    webpage <- read_html(url2)
    tmp <- html_text(webpage)
    aa <- str_locate(string = tmp,pattern = "comments")
    bb <- str_locate(string = tmp,pattern = "created_at")
    reviewtext <- str_sub(tmp, aa[1]+11, bb[1]-35)
    output$reviewout<-renderText({
      reviewtext
    })
    
  })

  
  output$price_distr <- renderPlot({
    
    dt_price <- rbind(total_price(), data.frame(sample = "Selected", price = dt_filtered()$price))
    means    <- dt_price %>% group_by(sample) %>% summarize(mean_price = mean(.data$price, na.rm = TRUE))
    
    ggplot(dt_price, aes(x = .data$price, group = sample, color = sample, fill = sample)) + 
      geom_density(alpha = 0.4) + 
      labs(title = "Price distribution",
           x     = "Price / Night", 
           y     = "Relative frequency") +
      geom_vline(data = means, 
                 aes(xintercept = means$mean_price, 
                     color      = sample),
                 linetype   = "dashed")
    
    
  })
  

}

 
