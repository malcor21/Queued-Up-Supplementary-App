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

# fixing a problem date column
intq[intq$q_date == "12/17/14" & !is.na(intq$q_date), "q_date"] <- "12/17/2014"

# reassigning q_date as date
intq <- intq %>% 
  mutate(
    q_date = as.Date(q_date, "%m/%d/%Y")
  )

# saving wrangling intq
save(intq, file = here::here("data/intq.rda"))
