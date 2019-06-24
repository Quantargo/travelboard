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
mod_places_ui <- function(id, dest){
  ns <- NS(id)
  # data1 <- reactive({
  #   read_rds(paste0("~/workshop/data/google_places/",tolower(dest),".rds"))[["Restaurants"]]
  # })
  # 
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tabBox(width = 12,
         tabPanel("Restaurants",
                  fluidRow(column(width = 8,
                                  google_mapOutput(outputId = ns("map"))), 
                           column(width = 4,
                                  sliderInput(inputId = ns("pricelevel"),
                                              label = "Price Level",
                                              #min = min(data1()$price_level,na.rm = TRUE), max = max(data1()$price_level,na.rm = TRUE),
                                              min = 1, max = 3,
                                              value = c(1,3),
                                              step = 1),
                                  sliderInput(inputId = ns("rating"),
                                              label = "Rating",
                                              #min = min(data1()$rating,na.rm = TRUE), max = max(data1()$rating,na.rm = TRUE),
                                              min = 1, max = 5,
                                              value = c(4,5),
                                              step = 0.1),
                                  sliderInput(inputId = ns("numberofratings"),
                                              label = "Number of Ratings",
                                              #min = min(data1()$user_ratings_total,na.rm = TRUE), max = max(data1()$user_ratings_total,na.rm = TRUE),
                                              min = 1, max = 5000,
                                              value = c(1,5000)))),
                  fluidRow(
                    valueBox(10 * 2, "New Orders", icon = icon("credit-card"),width = 4)#,
                    #valueBoxOutput(outputId = ns("ratingbox"))
                  )
         ),
         tabPanel("Bars",
                  fluidRow(column(width = 6,"plot"), column(width = 6,"slideinputs"))
         ),
         tabPanel("Museums",
                  fluidRow(column(width = 6,"plot"), column(width = 6,"slideinputs"))
         ),
         tabPanel("Cinemas",
                  fluidRow(column(width = 6,"plot"), column(width = 6,"slideinputs"))
         ),
         tabPanel("Discos",
                  fluidRow(column(width = 6,"plot"), column(width = 6,"slideinputs"))
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
  # data <- reactive({
  #   read_rds(paste0("~/workshop/data/google_places/",tolower(dest()),".rds"))[["Restaurants"]]
  # })
  
  data <- reactive({
    get_places_data(dest,"Restaurants",
                    c("price_level","rating","user_ratings_total"),
                    c(input$pricelevel[1],input$rating[1],input$numberofratings[1]),c(input$pricelevel[2],input$rating[2],input$numberofratings[2]) )
  })
  
  output$map <- renderGoogle_map(google_map(data = data(), location = NULL, key = key) %>%
                                   add_markers(lat = "lat", lon = "lng", info_window = "info") %>%
                                   add_heatmap(lat = "lat", lon = "lng", weight = "user_ratings_total", option_radius = 0.15, legend = TRUE))
  
  # output$ratingbox <- renderValueBox({
  #   valueBox(value = "80",subtitle = "Average Rating",width = 4)
  # })
  
  
}