library(here)
library(tidyverse)
library(dbplyr)
library(RSQLite)

# Load the helper functions
source(here('Code/Helper Functions/Assignment and Map Functions.R'))

# Connect to SQL database 
SMH2_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/SHM2.db")

# Define the specific years and sensitivity thresholds
years <- c(2015, 2016, 2017, 2018, 2019, 2021)
sensitivity <- c(.7, .75, .8, .85, .9, .95)
watershed <- "Yukon"

### For Kuskokwim
years <- c(2017, 2018, 2019, 2020, 2021)
sensitivity <- c(.7, .75, .8, .85, .9, .95)
watershed <- "Kuskokwim"

# Loop through each year and each sensitivity threshold
for (year in years) {
  for (sens in sensitivity) {
    Prod <- Basin_prov_assign(watershed, year, sens)
    
    tidy_prod <- Prod %>%
      gather(key = "Quartile", value = "Production", Q1:Q4) %>%
      select(-Total)  # Remove the 'Total' column if not needed
    
    # Add columns for year, watershed, and sensitivity
    tidy_prod$Year <- year
    tidy_prod$Watershed <- watershed
    tidy_prod$Sensitivity <- sens
    
    # Append the data to the SQL database
    dbWriteTable(SMH2_db, "production_matrices", tidy_prod, append = TRUE, row.names = FALSE)
  }
}

# Disconnect from the database
DBI::dbDisconnect(SMH2_db)
