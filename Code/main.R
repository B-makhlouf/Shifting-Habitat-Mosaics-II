# main.R
# Main execution script for watershed mapping analysis

library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(tidyr)

# Source all required function files
source(here("code/utils/spatial_utils.R"))
source(here("code/utils/visualization.R"))
source(here("code/assignment.R"))
source(here("code/mapping.R"))
source(here("code/doy_analysis.R"))
source(here("code/cpue_analysis.R"))
source(here("code/cumulative_quartile_analysis.R"))  # New file
#source(here("code/cumulative_visualization.R"))      # New file

#' Clean previous output files
#'
#' @param base_dir Base directory to clean
#' @return Number of files removed
clean_previous_outputs <- function(base_dir = here("Basin Maps")) {
  message("Cleaning previous PDF outputs...")
  pdf_files <- list.files(base_dir, 
                          pattern = "\\.png$", 
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
#' @param run_quartiles Whether to run regular quartile analyses 
#' @param run_cumulative Whether to run cumulative quartile analyses
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
process_dataset <- function(dataset, run_quartiles = FALSE, 
                            run_cumulative = FALSE,
                            quartile_types = c("DOY", "CPUE")) {
  # Extract year and watershed
  parts <- strsplit(dataset, "_")[[1]]
  year <- parts[1]
  watershed <- parts[2]
  
  message(paste("Processing dataset:", year, watershed))
  
  # Set parameters based on watershed
  if (watershed == "Yukon") {
    sensitivity_threshold <- 0.8
    min_error <- 0.003
    min_stream_order <- 4
  } else if (watershed == "Kusko") {
    sensitivity_threshold <- 0.0001
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
  
  # Run cumulative quartile analysis if requested
  if (run_cumulative) {
    message("  Processing cumulative quartile analysis")
    
    # Run analysis for each requested type
    cumulative_types <- c()
    
    if ("DOY" %in% quartile_types) {
      message("    Processing cumulative DOY quartiles")
      Cumulative_DOY_Analysis(
        year = year,
        watershed = watershed,
        sensitivity_threshold = sensitivity_threshold,
        min_error = min_error,
        min_stream_order = min_stream_order,
        HUC = 8
      )
      cumulative_types <- c(cumulative_types, "DOY")
    }
    
    if ("CPUE" %in% quartile_types) {
      message("    Processing cumulative CPUE quartiles")
      Cumulative_CPUE_Analysis(
        year = year,
        watershed = watershed, 
        sensitivity_threshold = sensitivity_threshold,
        min_error = min_error,
        min_stream_order = min_stream_order,
        HUC = 8
      )
      cumulative_types <- c(cumulative_types, "CPUE")
    }
    
    # Create progression plots and dashboard if we have at least one type
    if (length(cumulative_types) > 0) {
      message("    Creating cumulative visualization dashboard")
      create_cumulative_dashboard(
        year = year,
        watershed = watershed,
        include_raw_production = TRUE
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
#' @param run_quartiles Whether to run regular quartile analyses
#' @param run_cumulative Whether to run cumulative quartile analyses
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
process_specific_datasets <- function(years, watersheds, 
                                      run_quartiles = FALSE, 
                                      run_cumulative = FALSE,
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
      process_dataset(dataset, run_quartiles, run_cumulative, quartile_types)
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
#' @param run_quartiles Whether to run regular quartile analyses
#' @param run_cumulative Whether to run cumulative quartile analyses
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
run_all_analysis <- function(run_quartiles = FALSE, 
                             run_cumulative = FALSE,
                             quartile_types = c("DOY", "CPUE")) {
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
      process_dataset(dataset, run_quartiles, run_cumulative, quartile_types)
      message(paste("Successfully processed dataset:", dataset))
    }, error = function(e) {
      message(paste("ERROR processing dataset", dataset, ":", e$message))
    })
  }
  
  message("All map generation complete!")
  return(invisible(NULL))
}

#' Run only cumulative quartile analysis on specific datasets
#'
#' @param years Vector of years to process
#' @param watersheds Vector of watersheds to process
#' @param quartile_types Types of quartile analyses to run
#' @return Invisibly returns NULL
run_only_cumulative_analysis <- function(years, watersheds, 
                                         quartile_types = c("DOY", "CPUE")) {
  message("Running ONLY cumulative quartile analysis")
  
  # Create all combinations of years and watersheds
  datasets <- c()
  for (year in years) {
    for (watershed in watersheds) {
      datasets <- c(datasets, paste(year, watershed, sep = "_"))
    }
  }
  
  # Clean directories related to cumulative analysis to avoid confusion
  clean_dirs <- c(
    "Basin Maps/DOY_Cumulative",
    "Basin Maps/CPUE_Cumulative",
    "Basin Maps/Cumulative_Progression",
    "Basin Maps/Cumulative_Dashboards"
  )
  
  for(dir in clean_dirs) {
    dir_path <- here(dir)
    if(dir.exists(dir_path)) {
      pdf_files <- list.files(dir_path, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
      if(length(pdf_files) > 0) {
        file.remove(pdf_files)
        message(paste("Cleaned", length(pdf_files), "PDFs from", dir))
      }
    } else {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      message(paste("Created directory:", dir))
    }
  }
  
  # Process each dataset for cumulative analysis only
  for (dataset in datasets) {
    parts <- strsplit(dataset, "_")[[1]]
    year <- parts[1]
    watershed <- parts[2]
    
    message(paste("Processing cumulative analysis for", year, watershed))
    
    # Set parameters based on watershed
    if (watershed == "Yukon") {
      sensitivity_threshold <- 0.00001
      min_error <- 0.003
      min_stream_order <- 4
    } else if (watershed == "Kusko") {
      sensitivity_threshold <- 0.00001
      min_error <- 0.0006
      min_stream_order <- 3
    } else {
      stop(paste("Unknown watershed:", watershed))
    }
    
    # Run cumulative analysis for each requested type
    for (type in quartile_types) {
      tryCatch({
        if (type == "DOY") {
          message(paste("  Running cumulative DOY analysis for", dataset))
          Cumulative_DOY_Analysis(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8
          )
        } else if (type == "CPUE") {
          message(paste("  Running cumulative CPUE analysis for", dataset))
          Cumulative_CPUE_Analysis(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8
          )
        }
      }, error = function(e) {
        message(paste("Error in cumulative", type, "analysis for", dataset, ":", e$message))
      })
    }
    
    # Create visualization dashboard
    message(paste("  Creating cumulative dashboard for", dataset))
    tryCatch({
      create_cumulative_dashboard(
        year = year,
        watershed = watershed,
        include_raw_production = TRUE
      )
    }, error = function(e) {
      message(paste("Error creating dashboard for", dataset, ":", e$message))
    })
  }
  
  message("All cumulative analysis complete!")
  return(invisible(NULL))
}

# Add this new function to enforce consistent histogram limits
enforce_histogram_limits <- function(gg_hist) {
  if (!is.null(gg_hist)) {
    # Create a function to convert DOY to date for the x-axis labels
    doy_to_date <- function(doy, year = 2024) {  # Use 2024 as default (leap year)
      as.Date(doy - 1, origin = paste0(year, "-01-01"))
    }
    
    # Create custom x-axis breaks and labels
    doy_breaks <- seq(140, 200, by = 10)
    
    gg_hist <- gg_hist + 
      scale_x_continuous(limits = c(140, 200), 
                         breaks = doy_breaks,
                         labels = function(x) {
                           # Create two-line labels with DOY and date
                           paste0(x, "\n", format(doy_to_date(x), "%b %d"))
                         },
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 0.1), 
                         breaks = seq(0, 0.1, by = 0.02),
                         expand = c(0, 0)) +
      coord_cartesian(xlim = c(140, 200), ylim = c(0, 0.1), expand = FALSE) +
      labs(x = "Day of Year (Date)") +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(hjust = 1)
      )
  }
  return(gg_hist)
}


# Example usage:
# 1. Run all analyses with no quartiles
# run_all_analysis()

# 2. Run all analyses with DOY quartiles, including cumulative
#run_all_analysis(run_quartiles = TRUE, run_cumulative = TRUE, quartile_types = c("DOY"))

# 3. Process specific datasets with both quartile analyses and cumulative
# process_specific_datasets(
#   years = c("2015", "2016"), 
#   watersheds = c("Yukon", "Kusko"),
#   run_quartiles = TRUE,
#   run_cumulative = TRUE,
#   quartile_types = c("DOY", "CPUE")
# )

# 4. Run only the cumulative analysis for specific datasets
# run_only_cumulative_analysis(
#   years = c("2015", "2016"), 
#   watersheds = c("Yukon", "Kusko"),
#   quartile_types = c("DOY", "CPUE")
# )

export_production_values <- function(years, watersheds, 
                                     output_dir = here("Analysis_Results")) {
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Initialize data frames to store combined results
  all_huc_data <- NULL
  all_trib_data <- NULL
  
  # Process each watershed and year
  for (watershed in watersheds) {
    for (year in years) {
      message(paste("Processing", watershed, "watershed for year", year))
      
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
      
      # Run analysis with return_values = TRUE to get data for export
      results <- create_basin_maps(
        year = year,
        watershed = watershed,
        sensitivity_threshold = sensitivity_threshold,
        min_error = min_error,
        min_stream_order = min_stream_order,
        HUC = 8,
        return_values = TRUE  # Important: return values instead of just creating maps
      )
      
      # Extract HUC data
      huc_data <- results$huc_data
      
      # Add year and watershed identifiers
      huc_data$year <- year
      huc_data$watershed <- watershed
      
      # Drop geometry for CSV export
      if (inherits(huc_data, "sf")) {
        huc_data_df <- st_drop_geometry(huc_data)
      } else {
        huc_data_df <- huc_data
      }
      
      # Extract tributary data
      trib_data <- results$stream_data
      trib_data$basin_assign_norm <- results$basin_assign_norm
      trib_data$year <- year
      trib_data$watershed <- watershed
      
      # Create simplified tributary dataframe
      if (inherits(trib_data, "sf")) {
        # Extract important attributes from trib_data
        if ("reachid" %in% colnames(trib_data)) {
          trib_id_col <- "reachid"
        } else if ("SEGMENT_ID" %in% colnames(trib_data)) {
          trib_id_col <- "SEGMENT_ID"
        } else {
          # Create an identifier based on geometry if no ID column exists
          trib_data$temp_id <- 1:nrow(trib_data)
          trib_id_col <- "temp_id"
        }
        
        # Get stream order if available
        if ("Str_Order" %in% colnames(trib_data)) {
          str_order_col <- "Str_Order"
        } else {
          trib_data$str_order <- NA
          str_order_col <- "str_order"
        }
        
        # Create a simplified dataframe with essential columns
        trib_data_df <- data.frame(
          segment_id = trib_data[[trib_id_col]],
          stream_order = trib_data[[str_order_col]],
          basin_assign_norm = trib_data$basin_assign_norm,
          year = trib_data$year,
          watershed = trib_data$watershed
        )
        
        # Add stream length if available
        if ("stream_length_m" %in% colnames(trib_data)) {
          trib_data_df$stream_length_m <- trib_data$stream_length_m
        } else if (inherits(trib_data, "sf")) {
          trib_data_df$stream_length_m <- as.numeric(st_length(trib_data))
        }
        
      } else {
        trib_data_df <- trib_data
      }
      
      # Add to combined dataframes
      if (is.null(all_huc_data)) {
        all_huc_data <- huc_data_df
      } else {
        all_huc_data <- bind_rows(all_huc_data, huc_data_df)
      }
      
      if (is.null(all_trib_data)) {
        all_trib_data <- trib_data_df
      } else {
        all_trib_data <- bind_rows(all_trib_data, trib_data_df)
      }
    }
  }
  
  # Save to CSV files
  huc_filepath <- file.path(output_dir, "all_huc_production_values.csv")
  trib_filepath <- file.path(output_dir, "all_tributary_production_values.csv")
  
  # Write CSV files with appropriate options
  write.csv(all_huc_data, huc_filepath, row.names = FALSE)
  write.csv(all_trib_data, trib_filepath, row.names = FALSE)
  
  message(paste("Saved HUC data to:", huc_filepath))
  message(paste("Saved tributary data to:", trib_filepath))
  
  return(list(
    huc_file = huc_filepath,
    trib_file = trib_filepath
  ))
}

export_production_values(
  years = c("2017", "2019", "2020", "2021"),
  watersheds = c("Kusko")
)

