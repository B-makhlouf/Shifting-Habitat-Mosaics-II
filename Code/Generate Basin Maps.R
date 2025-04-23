# Integrated Watershed Mapping Script
# This script integrates standard basin mapping with optional quartile analysis

#===============================================================================
# HOW TO USE THIS SCRIPT
#===============================================================================
# To run all available datasets automatically:
# 1. Save this file to your project root directory
# 2. Make sure all required function scripts are in the proper locations
# 3. Uncomment the line that says: main()
# 4. Run the script
#
# The script will automatically:
# - Load all necessary functions
# - Clean up previous outputs
# - Update data from the repository
# - Process all available datasets
# - Generate a summary report
#
# To run with quartile analysis:
# - Set run_quartiles = TRUE in the main() function or process_specific_datasets()

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
  source(here("Code/Style_Map_Function.R"))
  source(here("Code/Map Functions QUARTILE.R"))
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
process_dataset <- function(dataset, run_quartiles = FALSE, quartile_types = c("DOY_Q", "CPUE_Q")) {
  # Extract year and watershed
  parts <- strsplit(dataset, "_")[[1]]
  year <- parts[1]
  watershed <- parts[2]
  
  message(paste("Processing dataset:", year, watershed))
  
  # Define global watershed variable needed by All_Map
  assign("watershed", watershed, envir = .GlobalEnv)
  
  # Set parameters based on watershed
  if (watershed == "Yukon") {
    sensitivity_threshold <- 0.0001
    min_error <- 0.003
    min_stream_order <- 3
    filter_conditions <- NULL
    
    # Run the mapping function for Yukon
    message(paste("Mapping", year, watershed, "with specific parameters"))
    map_result <- All_Map(year, sensitivity_threshold, min_error, min_stream_order)
    message(paste("Completed standard mapping for", year, watershed))
    
    # Run quartile mapping if requested
    if (run_quartiles) {
      message(paste("Running quartile analysis for", year, watershed))
      for (quartile_type in quartile_types) {
        message(paste("  Processing", quartile_type, "quartiles"))
        # Use the ALL_Map_func_Quartile from Map Functions QUARTILE.R instead
        quartile_result <- ALL_Map_func_Quartile(
          year = year,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          filter_by = quartile_type,
          HUC = 8
        )
        message(paste("  Completed", quartile_type, "quartile mapping"))
      }
    }
    
  } else if (watershed == "Kusko") {
    sensitivity_threshold <- 0.7
    min_error <- 0.0006
    min_stream_order <- 3
    filter_conditions <- NULL
    
    # Run the mapping function for Kuskokwim
    message(paste("Mapping", year, watershed, "with specific parameters"))
    map_result <- All_Map(year, sensitivity_threshold, min_error, min_stream_order)
    message(paste("Completed standard mapping for", year, watershed))
    
    # Run quartile mapping if requested
    if (run_quartiles) {
      message(paste("Running quartile analysis for", year, watershed))
      for (quartile_type in quartile_types) {
        message(paste("  Processing", quartile_type, "quartiles"))
        # Use the ALL_Map_func_Quartile from Map Functions QUARTILE.R instead
        quartile_result <- ALL_Map_func_Quartile(
          year = year,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          filter_by = quartile_type,
          HUC = 8
        )
        message(paste("  Completed", quartile_type, "quartile mapping"))
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
  
  # Return basic info about the processed dataset
  return(list(
    year = year,
    watershed = watershed,
    sensitivity_threshold = sensitivity_threshold,
    min_error = min_error,
    min_stream_order = min_stream_order,
    quartile_analysis = run_quartiles
  ))
}

#===============================================================================
# 4. MAIN EXECUTION
#===============================================================================

run_analysis <- function(datasets = NULL, run_quartiles = FALSE, quartile_types = c("DOY_Q", "CPUE_Q")) {
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
  
  # Create output directories for quartile analysis if needed
  if (run_quartiles) {
    dir.create(here("Basin Maps/Quartile_Maps"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here("Basin Maps/Quartile_Maps/HUC"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here("Basin Maps/Quartile_Maps/Tribs"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
  }
  
  # Step 5: Process each dataset
  results <- list()
  for(dataset in datasets) {
    tryCatch({
      results[[dataset]] <- process_dataset(dataset, run_quartiles, quartile_types)
    }, error = function(e) {
      message(paste("Error processing dataset", dataset, ":", e$message))
    })
  }
  
  # Step 6: Return summary of processing
  message("Analysis complete.")
  return(results)
}

#===============================================================================
# 5. ADDITIONAL UTILITY FUNCTIONS
#===============================================================================

# Function to generate a summary report of all processed data
generate_summary_report <- function(results) {
  if(length(results) == 0) {
    message("No results to summarize.")
    return(NULL)
  }
  
  # Convert results to a data frame
  summary_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      year = x$year,
      watershed = x$watershed,
      sensitivity_threshold = x$sensitivity_threshold,
      min_error = x$min_error,
      min_stream_order = x$min_stream_order,
      quartile_analysis = ifelse(is.null(x$quartile_analysis), FALSE, x$quartile_analysis)
    )
  }))
  
  # Add row names
  rownames(summary_df) <- names(results)
  
  # Display summary
  message("Summary of processed datasets:")
  print(summary_df)
  
  # Return the summary data frame
  return(summary_df)
}

# Function to process specific datasets
process_specific_datasets <- function(years, watersheds, run_quartiles = FALSE, 
                                      quartile_types = c("DOY_Q", "CPUE_Q")) {
  # Create all combinations of years and watersheds
  datasets <- c()
  for (year in years) {
    for (watershed in watersheds) {
      datasets <- c(datasets, paste(year, watershed, sep = "_"))
    }
  }
  
  message(paste("Processing specific datasets:", paste(datasets, collapse=", ")))
  return(run_analysis(datasets, run_quartiles, quartile_types))
}

#===============================================================================
# 6. EXECUTION
#===============================================================================

# Main execution - process all available datasets
main <- function(run_quartiles = FALSE, quartile_types = c("DOY_Q", "CPUE_Q")) {
  # Step 1: Load required functions
  source_function_scripts()
  
  # Step 2: Clean previous outputs
  clean_previous_outputs()
  
  # Step 3: Update datasets
  data_dir <- update_data()
  
  # Step 4: Get available datasets
  datasets <- get_available_datasets(data_dir)
  message(paste("Found", length(datasets), "datasets to process:", paste(datasets, collapse=", ")))
  
  # Create quartile directories if needed
  if (run_quartiles) {
    dir.create(here("Basin Maps/Quartile_Maps"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here("Basin Maps/Quartile_Maps/HUC"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here("Basin Maps/Quartile_Maps/Tribs"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
  }
  
  # Step 5: Process each dataset
  results <- list()
  for(dataset in datasets) {
    message(paste("Starting processing of dataset:", dataset))
    tryCatch({
      results[[dataset]] <- process_dataset(dataset, run_quartiles, quartile_types)
      message(paste("Successfully processed dataset:", dataset))
    }, error = function(e) {
      message(paste("ERROR processing dataset", dataset, ":", e$message))
    })
  }
  
  # Step 6: Generate and print a summary
  summary <- generate_summary_report(results)
  
  # Step 7: Return results
  message("All processing complete!")
  return(results)
}

# Uncomment this line to process ALL available datasets automatically
main(run_quartiles = TRUE, quartile_types = c("DOY_Q", "CPUE_Q"))

# For testing with quartile analysis:
# test_quartiles <- function() {
#   message("Testing quartile analysis with a single dataset...")
#   test_year <- "2015"
#   test_watershed <- "Kusko"  # Change to "Yukon" to test Yukon watershed
#   message(paste("Testing with dataset:", test_year, test_watershed))
#   results <- process_specific_datasets(
#     years = c(test_year), 
#     watersheds = c(test_watershed),
#     run_quartiles = TRUE,
#     quartile_types = c("DOY_Q")  # Just use DOY quartiles for testing
#   )
#   return(results)
# }
# test_quartiles()