library(tidyverse)
library(sf)
load(here::here('data/intq.rda'))
non_iso <- read_sf(here::here("data/non-ISO/non_ISO_final_v2.shp"))

# requests_map
st_bbox(non_iso) # retrieve x and y limits

unique(intq$region)

intq %>% view()

unique(intq$q_status)

# refiltering for tab 2
intq %>% 
  filter(q_status != "active") %>% 
  filter(
    (!is.na(wd_date) & wd_date <= as.Date(
      paste("1", "1", "2024", sep = "/"), 
      "%m/%d/%Y")
    )|
      (!is.na(on_date) & on_date <= as.Date(
        paste("1", "1", "2024", sep = "/"), 
        "%m/%d/%Y")
      )
  ) %>% 
  mutate(
    complete_date = pmax(wd_date, on_date, na.rm = TRUE)
  ) %>% 
  mutate(
    complete_year = factor(as.character(format(complete_date, format = "%Y")))
  ) %>% 
  mutate(
    date_diff = complete_date - q_date
  ) %>% 
  filter(
    date_diff >= 0
  ) %>% 
  summarize(
    mean = mean(date_diff, na.rm = TRUE),
    median = median(date_diff, na.rm = TRUE),
    .by = c(region, complete_year)
  ) %>% 
  filter(as.numeric(as.character(complete_year)) >= 2013) %>% 
  view()

# filtering tab 3
intq %>% 
  filter(q_status != "active") %>% 
  filter(
    (!is.na(wd_date) & wd_date <= as.Date(
      paste("1", "1", "2024", sep = "/"), 
      "%m/%d/%Y")
    )|
      (!is.na(on_date) & on_date <= as.Date(
        paste("1", "1", "2024", sep = "/"), 
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
  )

#approval rate avg
  intq %>% 
    filter(q_status != "active") %>% 
    filter(
      (!is.na(wd_date) & wd_date <= as.Date(
        paste("1", "1", "2024", sep = "/"), 
        "%m/%d/%Y")
      )|
        (!is.na(on_date) & on_date <= as.Date(
          paste("1", "1", "2024", sep = "/"), 
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
    filter(as.numeric(as.character(complete_year)) >= 2012) %>% 
    mutate(
      placeholder = "avg"
    ) %>% 
    ggplot(aes(x = complete_year, y = rate)) + 
    geom_point() +
    geom_step(aes(group = placeholder))

  
