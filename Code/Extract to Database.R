library(here)
library(tidyverse)
library(dbplyr)
library(RSQLite)

# Load the helper functions
source(here('Code/Helper Functions/Assignment and Map Functions.R'))

# Connect to SQL database 
SMH2_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

## Clear the table 
dbExecute(SMH2_db, "DELETE FROM production_matrices")

# Define the specific years and sensitivity thresholds
years <- c(2015, 2016, 2017, 2018, 2019, 2021)
sensitivity <- c(.7, .75, .8, .85, .9, .95)
watershed <- "Yukon"
shapefile <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")

### For Kuskokwim
#years <- c(2017, 2018, 2019, 2020, 2021)
#sensitivity <- c(.7, .75, .8, .85, .9, .95)
#watershed <- "Kuskokwim"

# Loop through each year and each sensitivity threshold
for (year in years) {
  for (sens in sensitivity) {
    Prod <- Basin_prov_assign(watershed, year, sens)
    
    Prod<- cbind(Prod, shapefile)
    
    tidy_prod <- Prod %>%
      gather(key = "Quartile", value = "Production", Q1:Total) %>%
      select(-geometry)  # Remove the 'Total' column if not needed
    
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

#################### ADD CPUE

library(DBI)
library(RSQLite)

# Connect to SQL database
SMH2_db <- dbConnect(SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/AYK_data.db")

# Create a table for CPUE data (if it doesn't exist)
dbExecute(SMH2_db, "CREATE TABLE IF NOT EXISTS CPUE (
          Date TEXT,
          dailyCatch INTEGER,
          dailyCPUE REAL,
          cumCPUE REAL,
          year INTEGER)")

# Empty the CPUE table before inserting new data
dbExecute(SMH2_db, "DELETE FROM CPUE")

# Define the directory containing the CPUE data files
cpue_dir <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/CPUE Data/Cleaned"

# Define the list of years you want to process
years <- c(2015, 2016, 2017, 2018, 2019, 2021) # Add all the years you need

# Loop through each year and insert data into the CPUE table
for (year in years) {
  # Construct the file path for the current year
  file <- file.path(cpue_dir, paste0("Yukon_CPUE_", year, ".csv"))
  
  # Check if the file exists
  if (file.exists(file)) {
    # Read the CSV file
    CPUE <- read.csv(file)
    
    # Check that the column names are correct
    if (all(c("Date", "dailyCatch", "dailyCPUE", "cumCPUE") %in% colnames(CPUE))) {
      # Add the year to the data
      CPUE$year <- year
      
      # Insert the data into the CPUE table
      dbWriteTable(SMH2_db, "CPUE", CPUE, append = TRUE, row.names = FALSE)
    } else {
      stop(paste("Column names do not match in file:", file))
    }
  } else {
    warning(paste("File does not exist:", file))
  }
}

# Disconnect from the dat
