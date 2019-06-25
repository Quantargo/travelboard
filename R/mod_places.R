# Module UI

#' @title   mod_places_ui and mod_places_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_places
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import dplyr leaflet leaflet.extras readr 

mod_places_ui <- function(id, dest){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      checkboxGroupInput(inputId = ns("selected_type"),
                         label = "Select Places",
                         choices = c("Restaurants", "Bars", "Cinemas","Disco","Museums"),
                         selected = "Restaurants")
    ),
    fluidRow(
      fluidRow(column(width = 8,
                      leafletOutput(outputId = ns("map"))
      ), 
      column(width = 4,
             sliderInput(inputId = ns("pricelevel"),
                         label = "Price Level",
                         min = 0, max = 5,
                         value = c(1,3),
                         step = 1),
             sliderInput(inputId = ns("rating"),
                         label = "Rating",
                         min = 1, max = 5,
                         value = c(4,5),
                         step = 0.1),
             sliderInput(inputId = ns("numberofratings"),
                         label = "Number of Ratings",
                         min = 1, max = 10000,
                         value = c(1,5000)),
             uiOutput(outputId = "slideroutput"))),
      fluidRow(
        br(),
        valueBoxOutput(outputId = ns("averagerating"),width = 2),
        valueBoxOutput(outputId = ns("numberofobjects"),width = 2),
        valueBoxOutput(outputId = ns("averageprice"),width = 2),
        valueBoxOutput(outputId = ns("numberofreviews"),width = 2)
      )
    )
  )
}

# Module Server

#' @rdname mod_places
#' @export
#' @keywords internal

mod_places_server <- function(input, output, session, dest){
  
  ns <- session$ns
  key <- "AIzaSyBCGvNSks4_NvBcAwdRLw9hXM0J0RkQhQg"
  # 
  data <- reactive({
    read_rds(paste0("~/workshop/data/google_places/",tolower(dest()),".rds"))[["Restaurants"]]
  })
  
  data <- reactive({
    req(input$selected_type)
    get_places_data(dest,input$selected_type[1],
                    c("price_level","rating","user_ratings_total"),
                    c(input$pricelevel[1],input$rating[1],input$numberofratings[1]),
                    c(input$pricelevel[2],input$rating[2],ifelse(input$numberofratings[2]<10000,input$numberofratings[2],Inf) ) )
  })
  
  output$map <- renderLeaflet({
    leaflet(data()) %>%
      addTiles() %>% 
      addMarkers(lng=data()$lng, 
                 lat=data()$lat, popup=data()$name, 
                 clusterOptions = markerClusterOptions())
  })
  output$averagerating <- renderValueBox({
    valueBox(
      round(mean(data()$rating),1), "Average Rating", icon = icon("smile"),
      color = "yellow"
    )
  })
  
  output$numberofobjects <- renderValueBox({
    valueBox(
      nrow(data()), "Number of places", icon = icon("map-marked"),
      color = "purple"
    )
  })
  
  output$averageprice <- renderValueBox({
    valueBox(
      round(mean(data()$price_level[data()$price_level != 0],na.rm = TRUE),1), "Average price level", 
      icon = icon("euro-sign"),
      color = "blue"
    )
  })
  
  output$numberofreviews <- renderValueBox({
    valueBox(
      round(mean(data()$user_ratings_total,na.rm = TRUE),0), "Average number of reviews", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "fuchsia"
    )
  })
  
  
  
}