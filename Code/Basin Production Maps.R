# Load required libraries
library(tidyverse)
library(here)
library(RSQLite)
library(sf)

# Source in functions 
source(here("Code/Helper Functions/Assignment and Map Functions.R"))

# Connect to SQL database
SMH2_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

# Define the parameters (Yukon)
years_yukon <- c(2015, 2016, 2017, 2018, 2019, 2021)
sensitivities <- c(.7, .75, .8, .85, .9, .95)
quartiles <- c("Q1", "Q2", "Q3", "Q4", "Total")
watershed_yukon <- "Yukon"

# Define the parameters (Kuskokwim)
years_kuskokwim <- c(2017, 2018, 2019, 2020, 2021)
watershed_kuskokwim <- "Kuskokwim"

# Initialize lists to store Total values for each year for both watersheds
total_production_yukon <- list()
total_production_kuskokwim <- list()

# Function to process each watershed
process_watershed <- function(years, watershed, total_production_list) {
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
          
          # Store the Total values for each year
          if (Quartile == "Total") {
            total_production_list[[paste(watershed, year, sensitivity, sep = "_")]] <<- Prod$Production
          }
        } else {
          cat("No data found for", identifier, "\n")
        }
      }
    }
  }
  return(total_production_list)
}

# Process both watersheds and store Total values in respective lists
total_production_yukon <- process_watershed(years_yukon, watershed_yukon, total_production_yukon)
total_production_kuskokwim <- process_watershed(years_kuskokwim, watershed_kuskokwim, total_production_kuskokwim)

# Convert the lists to dataframes
total_production_df_yukon <- bind_rows(
  lapply(names(total_production_yukon), function(id) {
    data.frame(
      Identifier = id,
      Production = total_production_yukon[[id]]
    )
  })
)

total_production_df_kuskokwim <- bind_rows(
  lapply(names(total_production_kuskokwim), function(id) {
    data.frame(
      Identifier = id,
      Production = total_production_kuskokwim[[id]]
    )
  })
)

# Calculate the average production for each watershed
average_production_yukon <- total_production_df_yukon %>%
  group_by(Identifier) %>%
  summarise(Average_Production = mean(Production, na.rm = TRUE))

average_production_kuskokwim <- total_production_df_kuskokwim %>%
  group_by(Identifier) %>%
  summarise(Average_Production = mean(Production, na.rm = TRUE))

# Print the average production for both watersheds
print("Average Production for Yukon:")
print(average_production_yukon)

print("Average Production for Kuskokwim:")
print(average_production_kuskokwim)

# Close the database connection
dbDisconnect(SMH2_db)
