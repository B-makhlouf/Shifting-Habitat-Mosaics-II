# main.R
# Main execution script for watershed mapping analysis

library(sf)
library(dplyr)
library(here)
library(ggplot2)

# Source all required function files
source(here("code/utils/spatial_utils.R"))
source(here("code/utils/visualization.R"))
source(here("code/assignment.R"))
source(here("code/mapping.R"))
source(here("code/doy_analysis.R"))
source(here("code/cpue_analysis.R"))

#' Clean previous output files
#'
#' @param base_dir Base directory to clean
#' @return Number of files removed
clean_previous_outputs <- function(base_dir = here("Basin Maps")) {
  message("Cleaning previous PDF outputs...")
  pdf_files <- list.files(base_dir, 
                          pattern = "\\.pdf$", 
                          recursive = TRUE,
                          full.names = TRUE)
  
  if(length(pdf_files) > 0) {
    file.remove(pdf_files)
    message(paste(length(pdf_files), "PDF files removed."))
  } else {
    message("No PDF files found to remove.")
  }
  
  return(length(pdf_files))
}

#' Update data from source repository
#'
#' @return Path to the destination directory
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
  
  return(dest_dir)
}

#' Get available datasets
#'
#' @param data_dir Directory containing the datasets
#' @return Character vector of dataset names
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

#' Process a single dataset
#'
#' @param dataset Dataset name in the format "year_watershed"
#' @param run_quartiles Whether to run quartile analyses
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
process_dataset <- function(dataset, run_quartiles = FALSE, quartile_types = c("DOY", "CPUE")) {
  # Extract year and watershed
  parts <- strsplit(dataset, "_")[[1]]
  year <- parts[1]
  watershed <- parts[2]
  
  message(paste("Processing dataset:", year, watershed))
  
  # Set parameters based on watershed
  if (watershed == "Yukon") {
    sensitivity_threshold <- 0.7
    min_error <- 0.003
    min_stream_order <- 5
  } else if (watershed == "Kusko") {
    sensitivity_threshold <- 0.7
    min_error <- 0.0006
    min_stream_order <- 3
  } else {
    stop(paste("Unknown watershed:", watershed))
  }
  
  # Run the standard basin mapping function
  message(paste("Mapping", year, watershed, "with standard parameters"))
  create_basin_maps(
    year = year,
    watershed = watershed,
    sensitivity_threshold = sensitivity_threshold,
    min_error = min_error,
    min_stream_order = min_stream_order,
    HUC = 8
  )
  
  # Run quartile mapping if requested
  if (run_quartiles) {
    # Run DOY quartile analysis if requested
    if ("DOY" %in% quartile_types) {
      message("  Processing DOY quartiles")
      DOY_Quartile_Analysis(
        year = year,
        watershed = watershed,
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
        watershed = watershed, 
        sensitivity_threshold = sensitivity_threshold,
        min_error = min_error,
        min_stream_order = min_stream_order,
        HUC = 8
      )
    }
  }
  
  # Clean up any variables that might cause conflicts in subsequent runs
  gc() # Force garbage collection
  
  return(invisible(NULL))
}

#' Process specific datasets by year and watershed
#'
#' @param years Vector of years to process
#' @param watersheds Vector of watersheds to process
#' @param run_quartiles Whether to run quartile analyses
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
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
  
  # Clean outputs
  clean_previous_outputs()
  
  # Update data
  update_data()
  
  # Process each dataset
  for (dataset in datasets) {
    tryCatch({
      process_dataset(dataset, run_quartiles, quartile_types)
      message(paste("Successfully processed dataset:", dataset))
    }, error = function(e) {
      message(paste("Error processing dataset", dataset, ":", e$message))
    })
  }
  
  message("Processing complete.")
  return(invisible(NULL))
}

#' Run full analysis on all available datasets
#'
#' @param run_quartiles Whether to run quartile analyses
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
run_all_analysis <- function(run_quartiles = FALSE, quartile_types = c("DOY", "CPUE")) {
  # Clean previous outputs
  clean_previous_outputs()
  
  # Update datasets
  data_dir <- update_data()
  
  # Get available datasets
  datasets <- get_available_datasets(data_dir)
  message(paste("Found", length(datasets), "datasets to process:", paste(datasets, collapse=", ")))
  
  # Process each dataset
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
  return(invisible(NULL))
}

# Example usage:
# Process all datasets with DOY quartile analysis
# run_all_analysis(run_quartiles = TRUE, quartile_types = c("DOY"))

# Process specific datasets with both quartile analyses
# process_specific_datasets(
#   years = c("2015", "2016"), 
#   watersheds = c("Yukon", "Kusko"),
#   run_quartiles = TRUE,
#   quartile_types = c("DOY", "CPUE")
# )