# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)

# Connect to the database 
SMH2_db <- dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

# Define the parameters
years <- c(2015, 2016, 2017, 2018)
watershed <- "Yukon"

# Create an empty list to store the results
TotalProd <- list()

# Loop through each year and retrieve the data
for (year in years) {
  # Construct the query
  query <- paste(
    "SELECT Production FROM production_matrices WHERE Watershed = '", watershed, 
    "' AND Year = ", year, 
    " AND Quartile = 'Total'", sep = ""
  )
  
  # Fetch data from the database for the specified conditions
  data <- dbGetQuery(SMH2_db, query)
  
  # Store the result in the list, naming the column by the year
  TotalProd[[as.character(year)]] <- data$Production
}

# Convert the list into a data frame, filling shorter columns with NA
TotalProd_df <- bind_cols(TotalProd)

