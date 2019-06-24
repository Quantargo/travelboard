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
mod_statistics_ui <- function(id, dest) {
  ns <- NS(id)
  fluidRow(
    column(width = 6,
           box(width = 12, 
               selectInput(
                 "countries",
                 label = "Countries to compare",
                 multiple = TRUE,
                 choices = sort(unique(bli$LOCATION))
               ),
               selectInput(
                 "indicator",
                 label = "BLI indicator",
                 choices = sort(unique(bli$INDICATOR))
               )
           )
    ),
    column(width = 6,
           box(width = 12,
               radioButtons(
                 "gender",
                 "Choose sex",
                 c(
                   All = "all",
                   Female = "female",
                   Male = "male"
                 ),
                 selected = "all"
               )
           ))
  )
  
  
  
}