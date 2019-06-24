# Module Server
    
#' @rdname mod_statistics
#' @export
#' @keywords internal
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns
  
  output$plot <- renderPlot({

    data <- bli %>% 
      filter(LOCATION %in% input$countries) %>% 
      filter(INEQUALITY == input$gender) %>% 
      filter(INDICATOR == input$indicator)
      
      ggplot(data = data, aes(x = LOCATION, y = obsValue)) + geom_point()
  })

}
    
## To be copied in the UI
# mod_statistics_ui("statistics_ui_1")
    
## To be copied in the server
# callModule(mod_statistics_server, "statistics_ui_1")
 
