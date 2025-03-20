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

date_difference <- function(data, time) {
  data %>% 
    filter(q_status != "active") %>% 
    filter(
    (!is.na(wd_date) & wd_date <= as.Date(
      paste("1", "1", time[2], sep = "/"), 
      "%m/%d/%Y")
    )|
      (!is.na(on_date) & on_date <= as.Date(
        paste("1", "1", time[2], sep = "/"), 
        "%m/%d/%Y")
      )
  ) %>% 
    mutate(
      complete_date = pmax(wd_date, on_date, na.rm = TRUE)
    ) %>% 
    mutate(
      date_diff = complete_date - q_date
    ) %>%  
    filter(
      date_diff >= 0
    ) %>% 
    mutate(
      complete_year = factor(as.character(format(complete_date, format = "%Y")))
    ) %>%
    summarize(
      mean = as.numeric(mean(date_diff, na.rm = TRUE)),
      median = as.numeric(median(date_diff, na.rm = TRUE)),
      .by = c(region, complete_year)
    ) %>% 
  filter(as.numeric(as.character(complete_year)) >= as.numeric(time[1]))
}

upper_bound_date <- function(data, time) {
  data %>%
    filter(
      (!is.na(wd_date) & wd_date <= time[2]) |
        (!is.na(on_date) & on_date <= time[2]) |
        (is.na(wd_date) & is.na(on_date))
    )
}

approval_rate <- function(data, time) {
  data %>% 
  filter(q_status != "active") %>% 
    filter(
      (!is.na(wd_date) & wd_date <= as.Date(
        paste("1", "1", time[2], sep = "/"), 
        "%m/%d/%Y")
      )|
        (!is.na(on_date) & on_date <= as.Date(
          paste("1", "1", time[2], sep = "/"), 
          "%m/%d/%Y")
        )
    ) %>%  
  mutate(
    complete_date = pmax(wd_date, on_date, na.rm = TRUE)
  ) %>% 
  mutate(
    complete_year = factor(as.character(format(complete_date, format = "%Y")))
  ) %>% 
  group_by(region, complete_year) %>% 
  summarize(
    operational = sum(q_status == "operational", na.rm = TRUE),
    withdrawn = sum(q_status == "withdrawn", na.rm = TRUE),
    suspended = sum(q_status == "suspended", na.rm = TRUE),
    total = operational + withdrawn + suspended,
    rate = ifelse(total > 0, operational / total, NA)
  ) %>% 
    filter(as.numeric(as.character(complete_year)) >= as.numeric(time[1]))
}

approval_rate <- function(data, time) {
  data %>% 
  filter(q_status != "active") %>% 
    filter(
      (!is.na(wd_date) & wd_date <= as.Date(
        paste("1", "1", time[2], sep = "/"), 
        "%m/%d/%Y")
      )|
        (!is.na(on_date) & on_date <= as.Date(
          paste("1", "1", time[2], sep = "/"), 
          "%m/%d/%Y")
        )
    ) %>%  
  mutate(
    complete_date = pmax(wd_date, on_date, na.rm = TRUE)
  ) %>% 
  mutate(
    complete_year = factor(as.character(format(complete_date, format = "%Y")))
  ) %>% 
  group_by(region, complete_year) %>% 
  summarize(
    operational = sum(q_status == "operational", na.rm = TRUE),
    withdrawn = sum(q_status == "withdrawn", na.rm = TRUE),
    suspended = sum(q_status == "suspended", na.rm = TRUE),
    total = operational + withdrawn + suspended,
    rate = ifelse(total > 0, operational / total, NA)
  ) %>% 
    filter(as.numeric(as.character(complete_year)) >= as.numeric(time[1])) %>% 
    ungroup() %>% 
    mutate(id = factor("avg"))
}

approval_rate_avg <- function(data, time) {
  data %>% 
    filter(q_status != "active") %>% 
    filter(
      (!is.na(wd_date) & wd_date <= as.Date(
        paste("1", "1", time[2], sep = "/"), 
        "%m/%d/%Y")
      )|
        (!is.na(on_date) & on_date <= as.Date(
          paste("1", "1", time[2], sep = "/"), 
          "%m/%d/%Y")
        )
    ) %>%  
    mutate(
      complete_date = pmax(wd_date, on_date, na.rm = TRUE)
    ) %>% 
    mutate(
      complete_year = factor(as.character(format(complete_date, format = "%Y")))
    ) %>% 
    group_by(complete_year) %>% 
    summarize(
      operational = sum(q_status == "operational", na.rm = TRUE),
      withdrawn = sum(q_status == "withdrawn", na.rm = TRUE),
      suspended = sum(q_status == "suspended", na.rm = TRUE),
      total = operational + withdrawn + suspended,
      rate = ifelse(total > 0, operational / total, NA)
    ) %>% 
    filter(as.numeric(as.character(complete_year)) >= as.numeric(time[1])) %>% 
    mutate(
      placeholder = "Average approval rate"
    )
}


shinyApp(
  
  ui <-  page_sidebar(
    
    sidebar = sidebar(
      "This ShinyApp was developed as a interactive visualization supplement
      to the \"Queued Up 2024\" report produced by the Lawrence Berkeley National
      Laboratory's Energy Markets & Policy department. See the final project report
      located in this project's GitHub repository for more details."
    ),
    
    navset_card_underline(
      title = "\"Queued Up\" Supplementary App",
      
      nav_panel(
        
        "Interconnection Queue by Region",
        
        card(
          
          flowLayout(
            
            br(),
            
            sliderInput(
              "time",
              label = "Adjust the slider to set time bounds:",
              min = as.Date("01/01/1996","%m/%d/%Y"),
              max = as.Date("01/01/2024","%m/%d/%Y"),
              value = c(as.Date("1996/01/01"), as.Date("2024/01/01")),
              timeFormat="%Y-%m-%d"
            ),
            
            br(),
            
            radioButtons(
              "viz_type",
              label = "Display number of projects or total capacity?",
              c("Number of projects" = "Number of projects",
                "Total capacity" = "Total capacity")
            )
          )
        ),
        
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
        
        "Queue Duration by Region",
        
        card(
          
          flowLayout(
            
            br(),
            
            sliderInput(
              "time_duration",
              label = "Adjust the slider to set time bounds:",
              min = 1999,
              max = 2023,
              value = c(1999, 2023),
              sep = ""
            ),
            
            br(),
            
            radioButtons(
              "stat_duration",
              label = "Mean or median queue time?",
              choices = c(
                "Mean" = "Mean",
                "Median" = "Median"
              )
            )
          )
        ),
        
        plotOutput(outputId = "queue_time")
        
      ),
      
      nav_panel(
        
        "Approval Rate by Region", 
                
        card(
          
          flowLayout(
            
            br(),
            
            sliderInput(
              "time_rate",
              label = "Adjust the slider to set time bounds:",
              min = 1999,
              max = 2023,
              value = c(1999, 2023),
              sep = ""
            ),
            
            br(),
            
            checkboxInput(
              "mean_check",
              "Display the yearly average approval rate?",
              value = FALSE
            )
            
          )
        ),
        
        plotOutput(outputId = "approval_rate")
                
                ),
      
      id = "parent_tabs"
    )
  ),
  
  server <-  function(input, output) {
    
    # reactive expression for requests_map and requests_bar
    sumvar_Input <- reactive({
      intq %>% 
        filter(q_date >= input$time[1]) %>% 
        upper_bound_date(input$time) %>% 
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
          x = -7950000,
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
          x = -8840124,
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
          x = -7520000,
          y = 5305053,
          label = paste(
            "ISO-NE\n", 
            subset(sumvar_Input(), region == "ISO-NE", select = sum),
            suffix(),
            sep = ""
          ),
          size = 4
        ) +
        labs(
          title = "Active Projects in the Interconnection Queue by US Grid Region"
        ) +
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          )
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
    
    # queue time var
    time_var <- reactive({
      if_else(
        input$stat_duration == "Mean",
        "mean",
        "median"
      )
    })
    
    # queue time 
    output$queue_time <- renderPlot({
      
      intq %>% 
        date_difference(input$time_duration) %>% 
        ggplot(aes(x = complete_year, y = .data[[time_var()]], color = region)) +
        geom_point(size = 4) +
        geom_line(aes(group = region)) +
        theme_minimal() +
        labs(
          x = "Queue exit year",
          color = "Region",
          y = paste(input$stat_duration, "days in queue"),
          title = paste(input$stat_duration, "time spent in the interconnection queue by completed projects")
        ) +
        scale_color_viridis_d() +
        scale_y_continuous(
          labels = scales::comma,
          expand= c(0, 0),
          limits = c(0, 4500)
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
    
    # approval rate chart
    output$approval_rate <- renderPlot({
      
      intq %>% 
        approval_rate(input$time_rate) %>% 
        ggplot() +
        geom_point(
          aes(x = complete_year, y = rate, color = region),
          size = 4, alpha = 0.6
          ) +
        geom_line(aes(x = complete_year, y = rate, group = region, color = region)) +
        theme_minimal() +
        labs(
          x = "Queue exit year",
          color = "Region",
          y = "Approval rate",
          title = "Project approval rate by queue exit year"
        ) +
        scale_color_viridis_d() +
        scale_y_continuous(
          limits = c(0, 1)
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
        ) +
        {if(input$mean_check)geom_point(
          data = (approval_rate_avg(intq, input$time_rate)),
          aes(x = complete_year, y = rate),
          size = 5,
          color = "black")}  +
        {if(input$mean_check)geom_line(
          data = (approval_rate_avg(intq, input$time_rate)),
          aes(x = complete_year, y = rate, group = placeholder),
          linewidth = 1.5,
          color = "black"
        )}
    })
    
    
  }
)


shinyApp(ui = ui, server = server)

