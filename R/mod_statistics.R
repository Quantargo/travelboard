# Load data ---------------------------------------------------------------
#bli <- readRDS(file = "~/workshop/data/oecd/bli.rds")
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
mod_statistics_ui <- function(id, dest, 
                              locations = c('AUS','AUT','BEL','BRA','CAN','CHE','CHL','COL','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR','LTU','LUX','LVA','MEX','NLD','NOR','NZL','OECD','POL','PRT','RUS','SVK','SVN','SWE','TUR','USA','ZAF'),
                              indicators = c('CG_SENG','CG_VOTO','EQ_AIRP','EQ_WATER','ES_EDUA','ES_EDUEX','ES_STCS','HO_BASE','HO_HISH','HO_NUMR','HS_LEB','HS_SFRH','IW_HADI','IW_HNFW','JE_EMPL','JE_LMIS','JE_LTUR','JE_PEARN','PS_FSAFEN','PS_REPH','SC_SNTWS','SW_LIFS','WL_EWLH','WL_TNOW')) {
    
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(width = 12, title = "Choose indicator and countries to compare",
        column(width = 6, selectInput(ns("countries"), label = "Countries to compare", multiple = TRUE, choices = locations, selected = c("AUT", "GRC", "ITA", "ESP", "PRT"))),
        column(width = 6, selectInput(ns("indicator"), label = "Choose indicator", choices = indicators))
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
#' @importFrom rlang .data
mod_statistics_server <- function(input, output, session, dest){
  ns <- session$ns
  bli <- readRDS(file = file.path(get_prefix(), "data/oecd/bli.rds"))
  
  
  
  output$wikiLink <- renderUI({
    div(a(href = paste0("https://en.wikipedia.org/wiki/", dest()), "find out more on wikipedia"), align = "right")
  })
  
  output$ind_descr <- renderUI({
    em(bli[bli$INDICATOR == input$indicator, 2])
  })
  
  output$plot <- renderPlot({
    
    plot_data <- bli %>%
      filter(.data$LOCATION %in% input$countries) %>%
      filter(.data$INEQUALITY %in% c("TOT")) %>%
      filter(.data$INDICATOR == input$indicator)
    
    cmap <- c(Crete = "GRC", Lisbon = "PRT", Mallorca = "ESP", Vienna = "AUT", Rome = "ITA")
    
    highlight_data <- plot_data %>% filter(.data$LOCATION == cmap[dest()])
    
    plot_data %>% 
      ggplot(aes(x = fct_reorder(.data$LOCATION, .data$obsValue), y = .data$obsValue, label = .data$obsValue)) +
      geom_point(size = 6, color = "steelblue") +
      geom_point(data = highlight_data, size = 6, color = "red") +
      geom_segment(aes(y = 0, yend = .data$obsValue, xend = fct_reorder(.data$LOCATION, .data$obsValue)), color = "steelblue", linetype= 2) +
      geom_segment(data = highlight_data, aes(y = 0, yend = .data$obsValue, xend = fct_reorder(.data$LOCATION, .data$obsValue)), color = "red", linetype= 2) +
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

