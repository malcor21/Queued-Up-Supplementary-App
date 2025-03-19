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

upper_bound <- function(data, time) {
  data %>%
    filter(
      (!is.na(wd_date) & wd_date <= time[2]) |
        (!is.na(on_date) & on_date <= time[2]) |
        (is.na(wd_date) & is.na(on_date))
    )
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
      title = "\"Queued Up\" Supplementary App",
      
      nav_panel(
        
        "Interconnection Queue by Region",
        
        navset_card_underline(
          
          nav_panel(
            "Mainland US Map", 
            plotOutput(outputId = "requests_map")
          ),
          
          nav_panel(
            "Bar Chart", 
            plotOutput(outputId = "requests_bar")
          ),
          
          id = "subtab_1"
        )
      ),
      
      nav_panel(
        
        "Tab 2",
        
        plotOutput(outputId = "queue_time")
        
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
        filter(q_date >= input$time[1]) %>% 
        upper_bound(input$time) %>% 
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
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -12298124,
          y = 4981053,
          label = paste(
            "West (non-ISO)\n", 
            subset(sumvar_Input(), region == "West (non-ISO)", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -11698124,
          y = 3099053,
          label = paste(
            "ERCOT\n", 
            subset(sumvar_Input(), region == "ERCOT", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -10890124,
          y = 4651053,
          label = paste(
            "SPP\n", 
            subset(sumvar_Input(), region == "SPP", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -9350124,
          y = 6100000,
          label = paste(
            "MISO\n", 
            subset(sumvar_Input(), region == "MISO", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -9400124,
          y = 3920000,
          label = paste(
            "Southeast (non-ISO)\n", 
            subset(sumvar_Input(), region == "Southeast (non-ISO)", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -8050000,
          y = 4671053,
          label = paste(
            "PJM\n", 
            subset(sumvar_Input(), region == "PJM", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -8780124,
          y = 5550000,
          label = paste(
            "NYISO\n", 
            subset(sumvar_Input(), region == "NYISO", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        annotate(
          "text",
          x = -7600000,
          y = 5305053,
          label = paste(
            "ISO-NE\n", 
            subset(sumvar_Input(), region == "ISO-NE", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        )
    })
    
    # reactive labels for requests_bar
    requests_bar_title <- reactive({
      case_when(
        input$viz_type == "Number of projects" ~ "Projects in queue by region",
        input$viz_type == "Total capacity" ~ "Total queued capacity by region"
      )
    })
    
    requests_bar_ylab <- reactive({
      case_when(
        input$viz_type == "Number of projects" ~ "Number of projects",
        input$viz_type == "Total capacity" ~ "Total queued capacity (MW)"
      )
    })
    
    # requests_bar
    output$requests_bar <- renderPlot({
      
      sumvar_Input() %>%
        ggplot(aes(x = region, y = sum, fill = region)) +
        geom_col() +
        theme_minimal() +
        labs(
          title = requests_bar_title(),
          x = "Region",
          y = requests_bar_ylab()
        ) +
        scale_fill_viridis_d() +
        theme(
          legend.position = "none"
        ) +
        scale_y_continuous(
          labels = scales::comma,
          expand= c(0, 0)
        ) +
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          ),
          axis.title = element_text(
            size = 14
          ),
          axis.text.x = element_text(
            size = 13
          ),
          axis.text.y = element_text(
            size = 11
          )
        )
      
    })
    
    # queue time 
    output$queue_time <- renderPlot({
      
      ## PLACEHOLDER
      sumvar_Input() %>%
        ggplot(aes(x = region, y = sum, fill = region)) +
        geom_col() +
        theme_minimal() +
        labs(
          title = requests_bar_title(),
          x = "Region",
          y = requests_bar_ylab()
        ) +
        scale_fill_viridis_d() +
        theme(
          legend.position = "none"
        ) +
        scale_y_continuous(
          labels = scales::comma,
          expand= c(0, 0)
        ) +
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          ),
          axis.title = element_text(
            size = 14
          ),
          axis.text.x = element_text(
            size = 13
          ),
          axis.text.y = element_text(
            size = 11
          )
        )
      
    })
    
    
  }
)


shinyApp(ui = ui, server = server)

