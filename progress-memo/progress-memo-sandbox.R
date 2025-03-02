# This script includes EDA tasks for the progress memo assignment

# loading packages
library(tidyverse)
library(here)

# loading data
qdup <- read_csv(here("data/queues.csv"))

# Basic data description ----

# skimr
skimr::skim_without_charts(qdup)

view(qdup)

# examining county_2
unique(qdup$county_2)

qdup %>% 
  filter(
    county_2 == "pershing"
  )
# can probably exclude