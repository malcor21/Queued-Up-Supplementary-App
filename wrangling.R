# Data wrangling script for Stat 302 final

# loading packages ----
library(tidyverse)

# loading original dataset
intq <- read_csv(here::here("data/queues_import/queues.csv"))

# wrangling intq ----
intq <- intq %>% 
  mutate(
    across(c(mw1, mw2, mw3), ~ if_else(is.na(.), 0, .))
  ) %>% 
  mutate(
    mw_total = mw1 + mw2 + mw3
  ) %>% 
  mutate(
    region = factor(region)
  )

# fixing a problem q_date column
intq[intq$q_date == "12/17/14" & !is.na(intq$q_date), "q_date"] <- "12/17/2014"

# reassigning q_date as date
intq <- intq %>% 
  mutate(
    q_date = as.Date(q_date, "%m/%d/%Y"),
    wd_date = as.Date(wd_date, "%m/%d/%Y"),
    on_date = as.Date(on_date, "%m/%d/%Y")
  )


# fixing MISO dates - assume 1/1, use given year
MISO_fix <- function(data){
  data %>%
    mutate(q_date = if_else(
      region == "MISO",
      as.Date(paste(1, 1, q_year, sep = "/"), "%m/%d/%Y"),
      q_date
    ))
}

intq <- intq %>% 
  MISO_fix()

intq %>% view()

# saving wrangling intq
save(intq, file = here::here("data/intq.rda"))
