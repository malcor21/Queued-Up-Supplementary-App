library(tidyverse)
library(sf)
load(here::here('data/intq.rda'))
non_iso <- read_sf(here::here("data/non-ISO/non_ISO_final_v2.shp"))

# requests_map
st_bbox(non_iso) # retrieve x and y limits

unique(intq$region)

intq %>% view()

unique(intq$q_status)

# filtering for tabs 2 and 3
intq %>% 
  filter(q_status == "operational") %>% 
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
    current_date = as.Date("01/01/2024", "%m/%d/%Y"),
    ) %>% 
  mutate(
    date_diff = pmin(complete_date, current_date) - q_date
  ) %>% 
  filter(
    date_diff >= 0
  ) %>% 
  summarize(
    mean = mean(date_diff, na.rm = TRUE),
    median = median(date_diff, na.rm = TRUE),
    .by = c(region, q_year)
  ) %>% 
  view()
