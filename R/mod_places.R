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
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tabBox(width = 12,
    tabPanel("Restaurants",
             fluidRow(column(width = 8,
                             #DT::dataTableOutput(ns("table"))),
                             google_mapOutput(outputId = ns("map"))), 
                      column(width = 4,
                             sliderInput(inputId = "pricelevel",
                                         label = "Price Level",
                                         min = 1, max = 3,value = c(1,3)),
                             sliderInput(inputId = "rating",
                                         label = "Rating",
                                         min = 4.3, max = 5.0,value = c(4,5),
                                         step = 0.1),
                             sliderInput(inputId = "numberofratings",
                                         label = "Number of Ratings",
                                         min = 1, max = 3,value = c(1,3))))
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
  # tagList(
  #   tags$h1(paste(dest, "Places", sep = "-")),
  #   fluidRow(
  #     box(title = dest, plotOutput(ns("plot1"), height = 250)),
  #     
  #     box(
  #       title = "Controls",
  #       sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
  #     )
  #   )
  # )
}
    
# Module Server
    
#' @rdname mod_places
#' @export
#' @keywords internal
    
mod_places_server <- function(input, output, session, dest){
  ns <- session$ns
  
  key <- "AIzaSyBCGvNSks4_NvBcAwdRLw9hXM0J0RkQhQg"
  
  data <- reactive({
    read_rds(paste0("~/workshop/data/google_places/",tolower(dest()),".rds"))[["Restaurants"]]
  })
  
  #data <- read_rds(paste0("~/workshop/data/google_places/",tolower("vienna"),".rds"))[["Restaurants"]]
 
  
  # output$table <- DT::renderDataTable({
  #   data = DT::datatable(data())
  # })

  output$map <- renderGoogle_map(google_map(data = data(), key = key) %>%
                                   add_markers(lat = "lat", lon = "lng", info_window = "info") %>%
                                   add_heatmap(lat = "lat", lon = "lng", weight = "user_ratings_total", option_radius = 0.15, legend = TRUE))

}
    
## To be copied in the UI
# mod_places_ui("places_ui_1")
    
## To be copied in the server
# callModule(mod_places_server, "places_ui_1")
 
