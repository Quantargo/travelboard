# Load data ---------------------------------------------------------------
bli <- readRDS(file = "~/workshop/data/oecd/bli.rds")
p#ppgdp <- readRDS(file = "~/workshop/data/oecd/pppgdp.rds")
#eo <- readRDS(file = "~/workshop/data/oecd/eo.rds")

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
mod_statistics_ui <- function(id, dest) {
    
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(width = 12, title = "Choose indicator and coungtries to compare",
        column(width = 6, selectInput(ns("countries"), label = "Countries to compare", multiple = TRUE, choices = sort(unique(bli$LOCATION)), selected = c("AUT", "GRC", "ITA", "ESP", "PRT"))),
        column(width = 6, selectInput(ns("indicator"), label = "Choose indicator", choices = sort(unique(bli$INDICATOR))))
      )
    ),
    fluidRow(
      box(width = 12, title = "Indicator decription", 
        uiOutput(ns("ind_descr"))
      )
    ),
   fluidRow(
     box(width = 12, 
         plotOutput(ns("plot"))
     )
    )
  )
  
}