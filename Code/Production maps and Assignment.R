#This script is where functions are iterated through to produce various maps and assignment values at the tributary scale 

library(tidyverse)
library(here)

# Read in the function
source("code/Functions/Yukon_map_function.R")

# List of reasonable sensitivity thresholds: .4-.9 
sensitivity_thresholds <- seq(.4, .9, .1)

# List of years with data
years <- c(2015, 2016, 2017)

# List of potential ways to divide the data into quartiles 
Quartiles <- levels(factor(c("Q1", "Q2", "Q3", "Q4")))

# Function to generate all combinations of years, thresholds, and quartiles
combinations <- expand.grid(year = years, threshold = sensitivity_thresholds, quartile = Quartiles)

# Loop through all combinations and produce a map for each
for (i in 1:nrow(combinations)) {
  Yukon_map(year = combinations$year[i], threshold = combinations$threshold[i], quartile = combinations$quartile[i])
}

# Do the same thing above but without cutting the data into quartiles
for (year in years) {
  for (threshold in sensitivity_thresholds) {
    # Run the function without quartiles
    Yukon_map(year = year, threshold = threshold)
  }
}

Yukon_map(2021, .7)

calculate_metrics <- function(threshold) {
  # Read in each production year for the given threshold
  prod_2015 <- read_csv(here(paste0("Data/Production/Yukon/2015_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2016 <- read_csv(here(paste0("Data/Production/Yukon/2016_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2017 <- read_csv(here(paste0("Data/Production/Yukon/2017_full_Yukon_", threshold, "_basin_norm.csv")))
  
  # Initialize an empty data frame
  prod_all <- data.frame(
    year_2015 = prod_2015$basin_assign_rescale,
    year_2016 = prod_2016$basin_assign_rescale,
    year_2017 = prod_2017$basin_assign_rescale
  )
  
  # Calculate the standard deviation, mean, CV for each row
  prod_all$sd <- apply(prod_all, 1, sd)
  prod_all$mean <- apply(prod_all, 1, mean)
  prod_all$cv <- prod_all$sd / prod_all$mean
  
  # Calculate the mean CV for each year without NAs
  prod_all$mean_cv <- apply(prod_all, 1, mean, na.rm = TRUE)
  
  # Calculate change from 2016 to 2017
  prod_all$change_2016_2017 <- prod_all$year_2017 - prod_all$year_2016
  
  # Calculate change from 2015 to 2016 
  prod_all$change_2015_2016 <- prod_all$year_2016 - prod_all$year_2015
  
  # Export as a .csv
  write_csv(prod_all, here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv")))
}









