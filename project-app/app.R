# loading packages ----
library(tidyverse)
library(ggthemes)
library(bslib)
library(sf)
library(shiny)

# loading data ----
non_iso <- read_sf(here::here("data/non-ISO/non_ISO_final.shp"))
intq <- load(here::here("data/intq.rda"))

shinyApp(
  
ui <-  page_sidebar(
  
  sidebar = sidebar(
    
    sliderInput(
      "time",
      label = "Years",
      min = 1995,
      max = 2023,
      value = c(1995, 2023),
      sep = ""
    ),
    
    radioButtons(
      "viz_type",
      label = "Project count or capacity?",
      c("Number of projects" = 1,
        "Total capacity" = 2)
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
          plotOutput(outputID = "requests_bar")
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
  
server <-  function(input, output, session) {
    
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
    
    output$requests_map <- renderPlot({
      
      non_iso %>% 
        ggplot() +
        geom_sf() +
        theme_void()
    })
    
    output$requests_bar <- renderPlot({
      
      intq %>% 
        ggplot(aes(x = ))
      
    })
    
    
  }
)


shinyApp(ui = ui, server = server)

