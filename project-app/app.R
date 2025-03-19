# loading packages ----
library(tidyverse)
library(ggthemes)
library(bslib)
library(sf)
library(shiny)

# loading data ----
non_iso <- read_sf(here::here("data/non-ISO/non_ISO_final_v2.shp"))
load(here::here("data/intq.rda"))

# defining functions ----
sum_var <- function(data, viz_type) {
  if (viz_type == "Number of projects") {
    data %>%
      group_by(region) %>%
      summarize(sum = n(), .groups = "drop")
  } 
  else if (viz_type == "Total capacity") {
    data %>%
      group_by(region) %>%
      summarize(sum = sum(mw_total, na.rm = TRUE), .groups = "drop")
  } 
}

shinyApp(
     
ui <-  page_sidebar(
  
  sidebar = sidebar(
    
    sliderInput(
      "time",
      label = "Adjust the slider to set time bounds:",
      min = as.Date("01/01/1996","%m/%d/%Y"),
      max = as.Date("01/01/2024","%m/%d/%Y"),
      value = c(as.Date("1996/01/01"), as.Date("2024/01/01")),
      timeFormat="%Y-%m-%d"
    ),
    
    radioButtons(
      "viz_type",
      label = "Display number of projects or total capacity?",
      c("Number of projects" = "Number of projects",
        "Total capacity" = "Total capacity")
    )
    
  ),
  
  navset_card_underline(
    title = "Visualizations",
    
    nav_panel(
      
      "Tab 1",
      
      navset_card_underline(
        
        nav_panel(
          "requests_map", 
          "title1",
          plotOutput(outputId = "requests_map")
          ),
        
        nav_panel(
          "requests_bar", 
          "title2",
          plotOutput(outputId = "requests_bar")
          ),
        
        id = "subtab_1"
      )
    ),
    
    nav_panel(
      "Tab 2",
      navset_card_underline(
        nav_panel("Function 1", "Here's the content for Tab 2, Function 1, with no space between tab levels"),
        nav_panel("Function 2", "Here's the content for Tab 2, Function 2, with no space between tab levels"),
        id = "subtab_2"
      )
    ),
    
    nav_panel("Tab 3", "Here's some orphaned content without sub-tabs"),
    id = "parent_tabs"
  )
),
  
server <-  function(input, output) {
    
    output$tab_controls <- renderUI({
      choices = if (input$parent_tabs == "Tab 1") {
        c("choices", "for", "tab 1")
      } else if (input$parent_tabs == "Tab 2") {
        c("tab 2", "settings")
      }
      
      if (length(choices)) {
        radioButtons(
          "tab_controls",
          "Controls",
          choices = choices
        )
      }
      
    })
    
    output$subtab_controls <- renderUI({
      if (input$parent_tabs == "Tab 2" & input$subtab_2 == "Function 1") {
        radioButtons(
          "subtab_controls",
          "Additional controls for Tab 2, Function 1",
          choices = letters[1:5]
        )
      } else if (input$parent_tabs == "Tab 2" & input$subtab_2 == "Function 2") {
        selectInput(
          "subtab_controls",
          "Different input for Tab 2, Function 2",
          choices = letters[6:10]
        )
      }
      
    })
    
    # reactive expression for requests_map and requests_bar
    sumvar_Input <- reactive({
      intq %>% 
        filter(q_date >= input$time[1] & q_date <= input$time[2]) %>% 
        filter(!is.na(region)) %>% 
        sum_var(input$viz_type)
    })
    
    # suffix for requests_map
    suffix <- reactive({
      if_else(
        input$viz_type == "Number of projects",
        " projects",
        " MW"
      )
    })
    
    # requests_map
    output$requests_map <- renderPlot({
      
      non_iso %>% 
        ggplot(aes(fill = RTO_ISO)) +
        geom_sf() +
        theme_void() +
        scale_fill_viridis_d() +
        theme(
          legend.position = "none"
        ) +
        annotate(
          "text",
          x = -13898124,
          y = 4091053,
          label = paste(
            "CAISO\n", 
            subset(sumvar_Input(), region == "CAISO", select = sum),
            suffix(),
            sep = "")
        )
    })
    
    # requests_bar
    output$requests_bar <- renderPlot({
      
      # DATES
      
      sumvar_Input() %>%
        ggplot(aes(x = region, y = sum, fill = region)) +
        geom_col() +
        theme_minimal() +
        labs(
          title = "Sum of mw_total by Region",
          x = "Region",
          y = "Total MW"
        ) +
        scale_fill_viridis_d() +
        theme(
          legend.position = "none"
        )
      
    })
    
    
  }
)


shinyApp(ui = ui, server = server)

