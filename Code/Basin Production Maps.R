# Load required libraries
library(tidyverse)
library(here)
library(RSQLite)
library(sf)

# Source in functions 
source(here("Code/Helper Functions/Assignment and Map Functions.R"))

# Connect to SQL database
SMH2_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

# Define the parameters
years <- c(2015, 2016, 2017, 2018, 2019, 2021)
sensitivities <- c(.7, .75, .8, .85, .9, .95)
quartiles <- c("Q1", "Q2", "Q3", "Q4", "Total")
watershed <- "Yukon"

# Define the parameters (KUSKOKWIM)
years <- c(2017, 2018, 2019, 2020, 2021)
sensitivities <- c(.7, .75, .8, .85, .9, .95)
quartiles <- c("Q1", "Q2", "Q3", "Q4", "Total")
watershed <- "Kuskokwim"

# Loop through years, sensitivities, and quartiles
for (year in years) {
  for (sensitivity in sensitivities) {
    for (Quartile in quartiles) {
      
      # Create the identifier
      identifier <- paste(watershed, year, sensitivity, Quartile, sep = "_")
      
      # Construct the query
      query <- paste(
        "SELECT * FROM production_matrices WHERE Year = ", year, 
        " AND Sensitivity = ", sensitivity, 
        " AND Watershed = '", watershed, 
        "' AND Quartile = '", Quartile, "'", sep = ""
      )
      
      # Fetch data from the database
      Prod <- dbGetQuery(SMH2_db, query)
      
      # Check if data is retrieved
      if (nrow(Prod) > 0) {
        # Scale Prod Production values to range from 0 to 1 
        Plotvar <- Prod$Production
        Plotvar <- (Plotvar - min(Plotvar)) / (max(Plotvar) - min(Plotvar))
        
        # Make the map
        Map_Base(watershed, Plotvar, identifier, sensitivity)
      } else {
        cat("No data found for", identifier, "\n")
      }
    }
  }
}

# Close the database connection
dbDisconnect(SMH2_db)
