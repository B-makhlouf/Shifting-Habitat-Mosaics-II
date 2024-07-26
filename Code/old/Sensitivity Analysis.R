### This script is to explore how changing the sensitivity value changes the final production maps 

# Load the necessary libraries
library(tidyverse)
library(here)
library(RSQLite)
library(sf)

# Source in functions 
source(here("Code/Helper Functions/Assignment and Map Functions.R"))

# Connect to SQL database
SMH2_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

# Create an empty dataframe with year, quartile, sensitivity value, watershed, and Stream length
StrLength_df<- data.frame(Year = integer(), Quartile = character(), Sensitivity = numeric(), Watershed = character(), Stream_Length = numeric())

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
        Scaled <- Prod$Production
        Scaled <- (Scaled - min(Scaled)) / (max(Scaled) - min(Scaled))
        
        # Create a boolian vector for being >.7
        likely <- Scaled > .7
        
        #Sum the stream length of the likely streams
        Stream_Length <- sum(Prod$Shp_Lng[likely])
        
        # Add the data to the dataframe
        StrLength_df <- rbind(StrLength_df, data.frame(Year = year, Quartile = Quartile, Sensitivity = sensitivity, Watershed = watershed, Stream_Length = Stream_Length))
      
      } else {
        cat("No data found for", identifier, "\n")
      }
    }
  }
}

# Close the database connection
dbDisconnect(SMH2_db)

## Create a plot showing StreamLength vs Sensitivity by year 
StrLength_df %>%
  ggplot(aes(x = Sensitivity, y = Stream_Length, color = Quartile)) +
  geom_line() +
  facet_wrap(~Year) +
  labs(title = "Stream Length vs Sensitivity by Year", x = "Sensitivity", y = "Stream Length") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Export as a pdf 
ggsave(here("Figures/Sensitivity_Analysis.pdf"), width = 10, height = 6, units = "in")


