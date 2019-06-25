# Module Server
    
#' @rdname mod_statistics
#' @export
#' @keywords internal
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns
  
  output$ind_descr <- renderUI({
    em(bli_indicator[bli_indicator$Indicator == input$indicator, 2])
  })
  
  output$plot <- renderPlot({
    
    plot_data <- bli %>%
      filter(LOCATION %in% input$countries) %>%
      filter(INEQUALITY %in% c("TOT")) %>%
      filter(INDICATOR == input$indicator)
    
    cmap <- c(Crete = "GRC", Lisbon = "PRT", Mallorca = "ESP", Vienna = "AUT", Rome = "ITA")
    
    highlight_data <- plot_data %>% filter(LOCATION == cmap[dest()])
    
    
    plot_data %>% 
      ggplot(aes(x = fct_reorder(LOCATION, obsValue), y = obsValue, label = obsValue)) +
      geom_point(size = 6, color = "steelblue") +
      geom_point(data = highlight_data, size = 6, color = "red") +
      geom_segment(aes(y = 0, yend = obsValue, xend = fct_reorder(LOCATION, obsValue)), color = "steelblue", linetype= 2) +
      geom_segment(data = highlight_data, aes(y = 0, yend = obsValue, xend = fct_reorder(LOCATION, obsValue)), color = "red", linetype= 2) +
      geom_text(color="white", size=2) +
      labs(title = "Comparison of countries", subtitle = str_wrap(paste(input$countries, collapse = ", "), width = 80)) +      
      theme_minimal() +
      coord_flip() +
      theme(legend.position = "bottom", 
            panel.grid = element_blank(),
            axis.title = element_blank())

  })
  

    
 
}
    
## To be copied in the UI
# mod_statistics_ui("statistics_ui_1")
    
## To be copied in the server
# callModule(mod_statistics_server, "statistics_ui_1")
 
