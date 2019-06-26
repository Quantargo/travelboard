# Load data ---------------------------------------------------------------

bli <- readRDS(file = "~/workshop/data/oecd/bli.rds")
#ppgdp <- readRDS(file = "~/workshop/data/oecd/pppgdp.rds")
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
      box(width = 12, title = "Choose indicator and countries to compare",
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
    ), uiOutput(ns("wikiLink"))
  )
  
}


# Module Server

#' @rdname mod_statistics
#' @export
#' @keywords internal
#' @importFrom forcats fct_reorder
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns
  
  output$wikiLink <- renderUI({
    div(a(href = paste0("https://en.wikipedia.org/wiki/", dest()), "find out more on wikipedia"), align = "right")
  })
  
  output$ind_descr <- renderUI({
    em(bli[bli$INDICATOR == input$indicator, 2])
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
            axis.title = element_blank(),
            axis.text.x = element_blank())
    
  })
}

## To be copied in the UI
# mod_statistics_ui("statistics_ui_1")

## To be copied in the server
# callModule(mod_statistics_server, "statistics_ui_1")

