

# Module UI

#' @title   mod_weather_ui and mod_weather_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_weather
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import dplyr 
#' @import owmr 
#' @import leaflet
#' @import ggplot2
mod_weather_ui <- function(id, dest){
  ns <- NS(id)
  
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    #  plotOutput(ns("plot2")),
    tags$h1(paste(dest, "Weather", sep = "-")),
    tabsetPanel(
      id = 'Weather',
      tabPanel('Map',
               fluidPage(
                 title = dest, leafletOutput(ns('map') , width = "100%", height = 600))
               ),
      tabPanel('Details', 
               
               fluidRow( id = 'Historical',
                      #   column(4, selectInput("visual", "Choose visual", choices = c("Map", "Plot historical info"), selected = "Map")),
                         column(4,dateInput(ns("datefrom"), label = "Date from", value = as.Date("2000-01-01"))),
                         column(4,dateInput(ns("dateto"), label = "Date to", value = as.Date("2000-01-01")))
               ),
               fluidRow(

                 #box(title = dest, plotOutput(ns("plot1"), height = 250)),
                 box(title = dest, plotOutput(ns("plot2"), width = "100%")),
                 uiOutput(ns("infobox")),
                 uiOutput(ns("infobox2")),
                 uiOutput(ns("infobox3")),
                 uiOutput(ns("infobox4"))

             #    box(
             #      title = "Controls",
             #      sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
             #    )
               ))#)
  ))
}

# Module Server

#' @rdname mod_weather
#' @export
#' @keywords internal

mod_weather_server <- function(input, output, session, dest){
  ns <- session$ns
  
  # This is just an example Server to be modified
  # Please change for your purpose
  
 # histdata <- rnorm(500)
#  output$plot1 <- renderPlot({
 #   data <- histdata[seq_len(input$slider)]
 #   hist(data, main = dest())
 # })
  
  long <- reactive(
    if( dest() =="Crete"){
      24.80927
    }
    else if(dest() =="Vienna"){
      16.37382
    }
    else if(dest() =="Rome"){
      12.49637
    }
    else if(dest() =="Mallorca"){
      3.017571
    }
    else {
      -9.139337
    })
  
  lat <-  reactive(
    if( dest() =="Crete"){
      35.24012
    }
    else if(dest() =="Vienna"){
      48.20817
    }
    else if(dest() =="Rome"){
      41.90278
    }
    else if(dest() =="Mallorca"){
      39.69526
    }
    else {
      38.72225
    })
  #temp <- c('temp:'+find_city$list$maintemp_max(dest()), 
  
#  icon <- reactive(
 #   addProviderTiles(providers$OpenWeatherMap.Temperature, options = providerTileOptions(apiKey=apikey))
 # )
 
      
  # google_geocode(address = "Vienna, Austria") -> (16.37382, 48.20817)
  # google_geocode(address = "Crete, Greece") -> (24.80927, 35.24012)
  # google_geocode(address = "Rome, Italy") -> (12.49637, 41.90278)
  # google_geocode(address = "Mallorca, Spain") -> (3.017571, 39.69526)
  # google_geocode(address = "Lisbon, Portugal") -> (-9.139337, 38.72225)
  apikey <- "d7eae13fe954ea0e04b0c40a172c4a10"
  owmr_settings(apikey)
  
 cities <- reactive(c('Vienna', 'Creete', 'Palma de Mallorka', 'Lisbon', 'Rome'))
  
  output$map <- renderLeaflet({leaflet() %>% 
                                addTiles() %>% 

                                add_owm_tiles(layer_name = owmr::owm_layers$Temperature_new, data = cities()) %>% 
                                add_weather(cities, long(), lat()) %>% 
                                addProviderTiles(leaflet::providers$OpenWeatherMap.Clouds, options = providerTileOptions(apiKey=apikey)) %>% 
                                addMarkers(long(), lat(), data = find_city(dest()))%>%
                                setView(long(), lat(), zoom = 9)
   
    })

  
  output$plot2 <- renderPlot({
    fname <- sprintf("~/workshop/data/weather/%s.rds", tolower(dest()))
    dat <- readRDS(fname)
    dat %>% 
      mutate(date = as.Date(date)) %>% 
      filter(date >= input$datefrom) %>%
      filter(date <= input$dateto) %>%
      ggplot() + 
      geom_line(aes(date, value, color = datatype, group = datatype)) +  
      facet_wrap(~datatype, scales = "free_y", ncol = 1)
  })
  
  output$infobox <- renderUI({
    currtemp <- readRDS("~/workshop/data/weather/currtemp.rds")
    infoBox(title="Current Temp", value=currtemp[[dest()]]$temp, color = "green" ,icon = icon("info-circle"))
  })
  
  output$infobox2 <- renderUI({
    currtemp <- readRDS("~/workshop/data/weather/currtemp.rds")
    infoBox(title="Min Temp", value=currtemp[[dest()]]$temp_min , color = "light-blue" ,icon = icon("minus"))
  })
  
  output$infobox3 <- renderUI({
    currtemp <- readRDS("~/workshop/data/weather/currtemp.rds")
    infoBox(title="Max Temp", value=currtemp[[dest()]]$temp_max, color = "yellow" ,icon = icon("sun"))
  })
  
  output$infobox4 <- renderUI({
    currtemp <- readRDS("~/workshop/data/weather/currtemp.rds")
    infoBox(title="Weather today ", value=currtemp[[dest()]]$weather_description,color = "aqua" ,icon = icon("users"))
  })
}

## To be copied in the UI
# mod_weather_ui("weather_ui_1")

## To be copied in the server
# callModule(mod_weather_server, "weather_ui_1")