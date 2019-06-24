# Module Server
    
#' @rdname mod_statistics
#' @export
#' @keywords internal
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns

  updateSelectInput(session, inputId = "countries", choices = all_countries, selected = dest)
}
    
## To be copied in the UI
# mod_statistics_ui("statistics_ui_1")
    
## To be copied in the server
# callModule(mod_statistics_server, "statistics_ui_1")
 
