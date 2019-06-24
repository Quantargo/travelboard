# Load data ---------------------------------------------------------------
bli <- readRDS(file = "~/workshop/data/oecd/bli.rds")
pppgdp <- readRDS(file = "~/workshop/data/oecd/pppgdp.rds")
eo <- readRDS(file = "~/workshop/data/oecd/eo.rds")

# Module UI
  
#' @title   mod_statistics_ui and mod_statistics_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_statistics
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_statistics_ui <- function(id, dest){
  ns <- NS(id)
  
  selectInput("countries",
              label = "Choose your countries",
              multiple = TRUE,
              choices = NULL)
  radioButtons("gender", "Choose sex", c("all", "female", "male"), selected = "all")
  
  plotOutput("plot")
  
  selectInput("indicator",
              label = "Choose your indicator",
              multiple = TRUE,
              choices = NULL)
  
  
}
    
# Module Server
    
#' @rdname mod_statistics
#' @export
#' @keywords internal
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns
  
  all_countries <- sort(unique(bli$LOCATION))
  #updateSelectInput(session, inputId = "countries", choices = all_countries, selected = dest)

    
}
    
## To be copied in the UI
# mod_statistics_ui("statistics_ui_1")
    
## To be copied in the server
# callModule(mod_statistics_server, "statistics_ui_1")
 
