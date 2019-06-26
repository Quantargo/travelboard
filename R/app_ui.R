#' @import shiny
#' @import shinydashboard
app_ui <- function(destinations = c("Crete", "Lisbon", "Mallorca", "Rome", "Vienna")) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    dashboardPage(
      dashboardHeader(title = "Travel Board"),
      dashboardSidebar(
        sidebarMenu(id = "menu",
                    selectInput("dest", "Destination", destinations),
                    menuItem("Airbnb", tabName = "airbnb", icon = icon("home"), selected = TRUE),
                    menuItem("Places", tabName = "places", icon = icon("map-marker-alt")),
                    menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
                    menuItem("Weather", tabName = "weather", icon = icon("sun")),
                    menuItem("News", tabName = "news", icon = icon("newspaper"))
        )
      ),
      dashboardBody(
        # Boxes need to be put in a row (or column)
        uiOutput("ui")
      ), skin='red'
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'travelboard')
  )
 
  tags$head(
    golem::js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
