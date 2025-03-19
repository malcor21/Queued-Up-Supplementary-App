library(tidyverse)
library(sf)
load(here::here('data/intq.rda'))
non_iso <- read_sf(here::here("data/non-ISO/non_ISO_final_v2.shp"))

# requests_map
st_bbox(non_iso) # retrieve x and y limits

non_iso %>% 
  ggplot(aes(fill = RTO_ISO)) +
  geom_sf() +
  # theme_void() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none"
  ) +
  annotate(
    "text",
    x = -13898124,
    y = 4091053,
    label = paste("CAISO", sep = "\n")
  )
  


# extract sum where region = CAISO


county_data <- read_sf("data/county_data.shp") %>% 
  mutate(state = str_to_title(state))

ui <- page_sidebar(
  
  title = "County Demographic Map by State",
  
  sidebar = sidebar(
    
    position = "left",
    
    p("Create demographic maps with information from the 2010 US Census."),
    
    selectInput(
      "state",
      label = strong("Choose a state to display"),
      choices = unique(county_data$state),
      selected = "Illinois"
    ),
    
    selectInput(
      "var",
      label = strong("Choose a variable to display"),
      choices = list(
        "Percent White",
        "Percent Black",
        "Percent Hispanic",
        "Percent Asian"
      ),
      selected = "Percent White"
    ),
    
  ),
  
  plotOutput(outputId = "stateMap")
  
)

server <- function(input, output) {
  
  output$stateMap <- renderPlot({
    
    new_data <- county_data %>% 
      filter(
        state == input$state
      )
    
    race <- 
      case_when(
        input$var == "Percent White" ~ new_data$white,
        input$var == "Percent Black" ~ new_data$black,
        input$var == "Percent Hispanic" ~ new_data$hispanic,
        input$var == "Percent Asian" ~ new_data$asian
      )
    
    color <- input$var %>% 
      switch(
        "Percent White" = "darkgreen",
        "Percent Black" = "black",
        "Percent Hispanic" = "darkorange",
        "Percent Asian" = "darkviolet"
      )
    
    new_data %>% 
      ggplot(aes(fill = race)) +
      geom_sf() +
      scale_fill_gradient2(
        low = "white",
        high = color,
        limits = c(0, 100)
      ) +
      labs(
        fill = paste("%", input$var),
        title = input$state
      ) +
      theme_void() +
      theme(
        plot.title = element_text(
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
}

shinyApp(ui = ui, server = server)

library(tidyverse)
intq <- read_csv(here::here("data/queues.csv"))
view(intq)
unique(intq$region)

library(sf)
non_iso <- read_sf(here::here("data/non-ISO/non_ISO_final.shp"))
non_iso %>% 
  ggplot(aes(fill = RTO_ISO)) +
  geom_sf() +
  theme(
    legend.position = "none"
  )


