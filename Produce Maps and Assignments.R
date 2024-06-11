
library(here)

source(here('Code/Assignment and Map Functions.R'))

# Define the specific years and sensitivity threshold
yukon_years <- c(2015, 2016, 2017, 2018)
kusko_years <- c(2017, 2018, 2020, 2021)
sensitivity_threshold <- 0.7


################################


# Loop through the specified years for Yukon assignments
for (year in yukon_years) {
  Basin_full_year <- Yukon_assign(year, sensitivity_threshold)
  
  # Write the output to a CSV file
  filename <- here(paste0("Outputs/Assignment Matrix/Yukon_", year, "_", sensitivity_threshold, "_basin_assignments.csv"))
  write.csv(Basin_full_year, filename, row.names = FALSE)
}


################################


# Loop through the specified years for Kusko assignments
for (year in kusko_years) {
  Basin_full_year <- Kusko_assign(year, sensitivity_threshold)
  
  # Write the output to a CSV file
  filename <- here(paste0("Outputs/Assignment Matrix/Kusko_", year, "_", sensitivity_threshold, "_basin_assignments.csv"))
  write.csv(Basin_full_year, filename, row.names = FALSE)
}



