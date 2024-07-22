#### This script will be used to produce annual production maps for all years in the Yukon and Kuskokwim
#### as well as by quartile within each year. This process will be repeated for a sensitivity threshold .7-.9

library(here)
library(tidyverse)

source(here('Code/Helper Functions/Assignment and Map Functions.R'))

Prod<- Basin_prov_assign("Yukon", 2015, .7)

tidy_prod <- Prod %>%
  gather(key = "Quartile", value = "Production", Q1:Q4) %>%
  select(-Total)  # Remove the 'Total' column if not needed

# Add a year collumn associated with the year of the data
tidy_prod$Year <- 2015
tidy_prod$Watershed <- "Yukon"


