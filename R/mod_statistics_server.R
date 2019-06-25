# Module Server
    
#' @rdname mod_statistics
#' @export
#' @keywords internal
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns
  
  output$plot <- renderPlot({

    browser
    
    bli %>%
      filter(LOCATION %in% input$countries) %>%
#      filter(INEQUALITY == input$gender) %>%
      filter(INDICATOR == input$indicator) %>%
      ggplot(aes(x = LOCATION, y = obsValue, fill = INEQUALITY)) +
      geom_point(alpha = 0.4) +
      labs(title = input$indicator, 
           subtitle = paste(input$countries, collapse = ", "),
           caption = bli_indicator[bli_indicator$Indicator == input$indicator, 2]) +
      theme_minimal()
    

  })
  

    
 
}
    
## To be copied in the UI
# mod_statistics_ui("statistics_ui_1")
    
## To be copied in the server
# callModule(mod_statistics_server, "statistics_ui_1")
 
