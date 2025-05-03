# Integrated Watershed Mapping Script
# This script focuses on generating basin maps with DOY and CPUE quartile analysis

# Load necessary libraries
library(sf)
library(here)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(classInt)
library(tidyr)
library(viridis)
library(tidyverse)

#===============================================================================
# 1. CONFIGURATION
#===============================================================================

# Source all required function scripts
source_function_scripts <- function() {
  message("Loading mapping functions...")
  source(here("Code/Map Functions Full.R"))
  source(here("Code/Map_Utils.R"))
  source(here("Code/DOY_Quartile_Analysis.R"))
  source(here("Code/CPUE_Quartile_Analysis.R")) 
  message("Mapping functions loaded successfully.")
}

# Clean previous output files
clean_previous_outputs <- function() {
  message("Cleaning previous PDF outputs...")
  pdf_files <- list.files(here("Basin Maps"), 
                          pattern = "\\.pdf$", 
                          recursive = TRUE,
                          full.names = TRUE)
  if(length(pdf_files) > 0) {
    file.remove(pdf_files)
    message(paste(length(pdf_files), "PDF files removed."))
  } else {
    message("No PDF files found to remove.")
  }
}

# Update data from source repository
update_data <- function() {
  message("Updating datasets from repository...")
  source_dir <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE"
  dest_dir <- here("Data/Natal Origin Data for analysis")
  
  # Make sure destination directory exists
  if(!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("Created destination directory.")
  }
  
  # Get list of CSV files and copy them
  csv_files <- list.files(source_dir, pattern = "\\.csv$", full.names = TRUE)
  copy_result <- file.copy(from = csv_files, to = dest_dir, overwrite = TRUE)
  
  message(paste("Copied", sum(copy_result), "of", length(csv_files), "files."))
  
  # Return the destination directory for use in next steps
  return(dest_dir)
}

#===============================================================================
# 2. DATA PREPARATION
#===============================================================================

# Get available datasets
get_available_datasets <- function(data_dir) {
  message("Identifying available datasets...")
  
  # List all CSV files
  file_names <- list.files(data_dir, pattern = "\\.csv$", full.names = FALSE)
  
  # Extract unique year_watershed combinations
  dataset_names <- unique(sub("^([a-zA-Z0-9]+_[a-zA-Z0-9]+)_.*\\.csv$", "\\1", file_names))
  
  # Remove incomplete datasets
  dataset_names <- dataset_names[!dataset_names %in% c("2017_Yukon", "2018_Yukon", "2019_Yukon")]
  
  message(paste("Found", length(dataset_names), "valid datasets for analysis."))
  return(dataset_names)
}

#===============================================================================
# 3. MAPPING FUNCTIONS
#===============================================================================

# Process a single dataset
process_dataset <- function(dataset, run_quartiles = FALSE, quartile_types = c("DOY", "CPUE")) {
  # Extract year and watershed
  parts <- strsplit(dataset, "_")[[1]]
  year <- parts[1]
  watershed <- parts[2]
  
  message(paste("Processing dataset:", year, watershed))
  
  # Define global watershed variable needed by All_Map and quartile functions
  assign("watershed", watershed, envir = .GlobalEnv)
  
  # Set parameters based on watershed
  if (watershed == "Yukon") {
    sensitivity_threshold <- 0.7
    min_error <- 0.003
    min_stream_order <- 5
    
    # Run the mapping function for Yukon
    message(paste("Mapping", year, watershed, "with standard parameters"))
    All_Map(year, sensitivity_threshold, min_error, min_stream_order)
    
    # Run quartile mapping if requested
    if (run_quartiles) {
      # Run DOY quartile analysis if requested
      if ("DOY" %in% quartile_types) {
        message("  Processing DOY quartiles")
        DOY_Quartile_Analysis(
          year = year,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          HUC = 8
        )
      }
      
      # Run CPUE quartile analysis if requested
      if ("CPUE" %in% quartile_types) {
        message("  Processing CPUE quartiles")
        CPUE_Quartile_Analysis(
          year = year,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          HUC = 8
        )
      }
    }
    
  } else if (watershed == "Kusko") {
    sensitivity_threshold <- 0.7
    min_error <- 0.0006
    min_stream_order <- 3
    
    # Run the mapping function for Kuskokwim
    message(paste("Mapping", year, watershed, "with standard parameters"))
    All_Map(year, sensitivity_threshold, min_error, min_stream_order)
    
    # Run quartile mapping if requested
    if (run_quartiles) {
      # Run DOY quartile analysis if requested
      if ("DOY" %in% quartile_types) {
        message("  Processing DOY quartiles")
        DOY_Quartile_Analysis(
          year = year,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          HUC = 8
        )
      }
      
      # Run CPUE quartile analysis if requested
      if ("CPUE" %in% quartile_types) {
        message("  Processing CPUE quartiles")
        CPUE_Quartile_Analysis(
          year = year,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          HUC = 8
        )
      }
    }
  } else {
    stop(paste("Unknown watershed:", watershed))
  }
  
  # Clean up global variables to avoid conflicts in subsequent runs
  if(exists("assignment_matrix", envir = .GlobalEnv)) {
    rm("assignment_matrix", envir = .GlobalEnv)
  }
  if(exists("edges", envir = .GlobalEnv)) {
    rm("edges", envir = .GlobalEnv)
  }
  if(exists("basin", envir = .GlobalEnv)) {
    rm("basin", envir = .GlobalEnv)
  }
  if(exists("Huc", envir = .GlobalEnv)) {
    rm("Huc", envir = .GlobalEnv)
  }
  if(exists("identifier", envir = .GlobalEnv)) {
    rm("identifier", envir = .GlobalEnv)
  }
  if(exists("StreamOrderPrior", envir = .GlobalEnv)) {
    rm("StreamOrderPrior", envir = .GlobalEnv)
  }
  if(exists("pid_prior", envir = .GlobalEnv)) {
    rm("pid_prior", envir = .GlobalEnv)
  }
}

#===============================================================================
# 4. MAIN EXECUTION
#===============================================================================

run_analysis <- function(datasets = NULL, run_quartiles = FALSE, quartile_types = c("DOY", "CPUE")) {
  # Step 1: Load required functions
  source_function_scripts()
  
  # Step 2: Clean previous outputs
  clean_previous_outputs()
  
  # Step 3: Update datasets
  data_dir <- update_data()
  
  # Step 4: Get available datasets
  if(is.null(datasets)) {
    datasets <- get_available_datasets(data_dir)
  }

  # Step 5: Process each dataset
  for(dataset in datasets) {
    tryCatch({
      process_dataset(dataset, run_quartiles, quartile_types)
      message(paste("Successfully processed dataset:", dataset))
    }, error = function(e) {
      message(paste("Error processing dataset", dataset, ":", e$message))
    })
  }
  
  message("Map generation complete.")
}

#===============================================================================
# 5. UTILITY FUNCTIONS
#===============================================================================

# Function to process specific datasets
process_specific_datasets <- function(years, watersheds, run_quartiles = FALSE, 
                                      quartile_types = c("DOY", "CPUE")) {
  # Create all combinations of years and watersheds
  datasets <- c()
  for (year in years) {
    for (watershed in watersheds) {
      datasets <- c(datasets, paste(year, watershed, sep = "_"))
    }
  }
  
  message(paste("Processing specific datasets:", paste(datasets, collapse=", ")))
  run_analysis(datasets, run_quartiles, quartile_types)
}

#===============================================================================
# 6. EXECUTION
#===============================================================================

# Main execution - process all available datasets
main <- function(run_quartiles = FALSE, quartile_types = c("DOY", "CPUE")) {
  # Step 1: Load required functions
  source_function_scripts()
  
  # Step 2: Clean previous outputs
  clean_previous_outputs()
  
  # Step 3: Update datasets
  data_dir <- update_data()
  
  # Step 4: Get available datasets
  datasets <- get_available_datasets(data_dir)
  message(paste("Found", length(datasets), "datasets to process:", paste(datasets, collapse=", ")))
  
  # Step 5: Process each dataset
  for(dataset in datasets) {
    message(paste("Starting processing of dataset:", dataset))
    tryCatch({
      process_dataset(dataset, run_quartiles, quartile_types)
      message(paste("Successfully processed dataset:", dataset))
    }, error = function(e) {
      message(paste("ERROR processing dataset", dataset, ":", e$message))
    })
  }
  
  message("All map generation complete!")
}

# Uncomment this line to process ALL available datasets automatically
main(run_quartiles = TRUE, quartile_types = c("DOY"))


