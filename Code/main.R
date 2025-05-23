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
  # Remove incomplete datasets (be more specific about which datasets to exclude)
  dataset_names <- dataset_names[!(dataset_names == "2017_Yukon" | dataset_names == "2018_Yukon" | dataset_names == "2019_Yukon")]
  
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
    # Modified to extend to 210
    doy_breaks <- seq(140, 210, by = 10)
    
    gg_hist <- gg_hist + 
      scale_x_continuous(limits = c(140, 210), 
                         breaks = doy_breaks,
                         labels = function(x) {
                           # Create two-line labels with DOY and date
                           paste0(x, "\n", format(doy_to_date(x), "%b %d"))
                         },
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 0.1), 
                         breaks = seq(0, 0.1, by = 0.02),
                         expand = c(0, 0)) +
      # Modified x limit to 210 instead of 200
      coord_cartesian(xlim = c(140, 210), ylim = c(0, 0.1), expand = FALSE) +
      labs(x = "Day of Year (Date)") +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(hjust = 1)
      )
  }
  return(gg_hist)
}



#' Export production values for multiple watersheds and years to CSV files
#'
#' @param years Vector of years to process
#' @param watersheds Vector of watersheds to process
#' @param include_quartiles Whether to include DOY and CPUE quartile data
#' @param include_cumulative Whether to include cumulative quartile data
#' @param output_dir Directory to save output CSV files
#' @param cache_dir Directory to look for cached analysis results
#' @return List with paths to created CSV files
# Modified export_production_values function to include CPUE percentage for each subset

# Modified export_production_values function to include CPUE percentage for each subset

export_production_values <- function(years, watersheds, 
                                     include_quartiles = TRUE,
                                     include_cumulative = TRUE,
                                     output_dir = here("Analysis_Results"),
                                     cache_dir = here("Data/Processed")) {
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Initialize data frames to store combined results
  all_huc_data <- NULL
  all_trib_data <- NULL
  
  # Helper function to extract non-geometry data from SF objects
  extract_data <- function(sf_obj) {
    if (inherits(sf_obj, "sf")) {
      return(st_drop_geometry(sf_obj))
    } else {
      return(sf_obj)
    }
  }
  
  # Helper function to extract tributary data with consistent columns
  extract_trib_data <- function(trib_data, basin_assign, metadata) {
    # Early return if input is NULL
    if (is.null(trib_data)) return(NULL)
    
    # Add basin_assign_norm if provided
    if (!is.null(basin_assign)) {
      trib_data$basin_assign_norm <- basin_assign
    }
    
    # Add metadata
    for (field in names(metadata)) {
      trib_data[[field]] <- metadata[[field]]
    }
    
    # Process as SF object if it is one
    if (inherits(trib_data, "sf")) {
      # Determine ID column
      id_col <- NULL
      for (possible_id in c("reachid", "SEGMENT_ID", "REACH_ID", "id")) {
        if (possible_id %in% colnames(trib_data)) {
          id_col <- possible_id
          break
        }
      }
      
      # Create ID if none exists
      if (is.null(id_col)) {
        trib_data$segment_id <- 1:nrow(trib_data)
        id_col <- "segment_id"
      } else {
        # Rename to standard column name
        trib_data$segment_id <- trib_data[[id_col]]
      }
      
      # Get stream order
      order_col <- NULL
      for (possible_order in c("Str_Order", "STR_ORDER", "stream_order")) {
        if (possible_order %in% colnames(trib_data)) {
          order_col <- possible_order
          break
        }
      }
      
      # Default stream order if none exists
      if (is.null(order_col)) {
        trib_data$stream_order <- NA
      } else {
        # Rename to standard column name
        trib_data$stream_order <- trib_data[[order_col]]
      }
      
      # Add stream length if not present
      if (!"stream_length_m" %in% colnames(trib_data)) {
        trib_data$stream_length_m <- as.numeric(st_length(trib_data))
      }
      
      # Create final data frame with selected columns
      result <- data.frame(
        segment_id = trib_data$segment_id,
        stream_order = trib_data$stream_order,
        stream_length_m = trib_data$stream_length_m,
        basin_assign_norm = trib_data$basin_assign_norm
      )
      
      # Add metadata columns
      for (field in names(metadata)) {
        result[[field]] <- metadata[[field]]
      }
      
      return(result)
    } else {
      # Already a data frame, return as is
      return(trib_data)
    }
  }
  
  # Helper function to calculate CPUE percentage for a subset
  calculate_cpue_percentage <- function(subset_data, total_data) {
    if (nrow(subset_data) == 0 || nrow(total_data) == 0) {
      return(0)
    }
    return(sum(subset_data$dailyCPUEprop, na.rm = TRUE) / sum(total_data$dailyCPUEprop, na.rm = TRUE))
  }
  
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
        warning(paste("Unknown watershed:", watershed, "- skipping"))
        next
      }
      
      # Load the natal origins data to calculate CPUE percentages
      natal_data <- load_natal_data(year, watershed)
      total_cpue <- sum(natal_data$dailyCPUEprop, na.rm = TRUE)
      
      # Try to load cached assignment data
      assignment_file <- file.path(cache_dir, paste0(year, "_", watershed, "_assignments.rds"))
      
      if (file.exists(assignment_file)) {
        message("  Loading cached basin assignments")
        basin_assign_values <- tryCatch({
          readRDS(assignment_file)
        }, error = function(e) {
          message("  Error reading cached file: ", e$message)
          NULL
        })
      } else {
        basin_assign_values <- NULL
      }
      
      # If no cached data, run the analysis
      if (is.null(basin_assign_values)) {
        message("  Running annual analysis to get base data")
        
        # Run full analysis for this dataset
        tryCatch({
          results <- create_basin_maps(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8,
            return_values = TRUE
          )
          
          # Cache the results for future use
          basin_assign_values <- list(
            rescale = results$basin_assign_rescale,
            norm = results$basin_assign_norm
          )
          saveRDS(basin_assign_values, assignment_file)
          
          # Extract data for annual maps
          annual_huc_data <- results$huc_data
          annual_trib_data <- results$stream_data
          
        }, error = function(e) {
          message("  Error running annual analysis: ", e$message)
          return(NULL)
        })
      } else {
        # Need to load spatial data to process cached assignments
        message("  Loading spatial data to process cached assignments")
        
        tryCatch({
          spatial_data <- load_spatial_data(watershed, 8, min_stream_order)
          edges <- spatial_data$edges
          basin <- spatial_data$basin
          Huc <- spatial_data$Huc
          
          # Process HUC data with the cached assignment values
          annual_huc_data <- process_huc_data(
            edges, basin, Huc, 
            basin_assign_values$rescale, 8
          )
          
          # Add assignments to edges data
          annual_trib_data <- edges
          annual_trib_data$basin_assign_norm <- basin_assign_values$norm
          
        }, error = function(e) {
          message("  Error processing cached assignments: ", e$message)
          return(NULL)
        })
      }
      
      # Check if we have valid data to proceed
      if (is.null(annual_huc_data) || is.null(annual_trib_data)) {
        message("  No valid data for ", year, " ", watershed, " - skipping")
        next
      }
      
      # Calculate CPUE percentage for annual data (should be 100% or 1.0)
      annual_cpue_percentage <- calculate_cpue_percentage(natal_data, natal_data)
      
      # Process and add annual data
      annual_metadata <- list(
        year = year,
        watershed = watershed,
        analysis_type = "Annual",
        quartile = NA,
        cpue_percentage = annual_cpue_percentage
      )
      
      # Add annual HUC data
      annual_huc_df <- extract_data(annual_huc_data)
      for (field in names(annual_metadata)) {
        annual_huc_df[[field]] <- annual_metadata[[field]]
      }
      
      all_huc_data <- rbind(all_huc_data, annual_huc_df)
      
      # Add annual tributary data
      annual_trib_df <- extract_trib_data(
        annual_trib_data, 
        basin_assign_values$norm, 
        annual_metadata
      )
      
      all_trib_data <- rbind(all_trib_data, annual_trib_df)
      
      # Process DOY quartile data if requested
      if (include_quartiles) {
        message("  Processing DOY quartile data")
        
        # First get the DOY quartile subsets to calculate CPUE percentages
        doy_quartile_data <- divide_doy_quartiles(natal_data)
        doy_quartile_subsets <- doy_quartile_data$subsets
        
        tryCatch({
          doy_results <- DOY_Quartile_Analysis(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8,
            return_values = TRUE
          )
          
          # Process each DOY quartile
          for (q in 1:length(doy_results)) {
            if (is.null(doy_results[[q]])) next
            
            message(paste("    Processing DOY Q", q))
            
            # Calculate CPUE percentage for this quartile
            doy_cpue_percentage <- calculate_cpue_percentage(doy_quartile_subsets[[q]], natal_data)
            
            doy_metadata <- list(
              year = year,
              watershed = watershed,
              analysis_type = "DOY",
              quartile = paste0("Q", q),
              cpue_percentage = doy_cpue_percentage
            )
            
            # Add HUC data
            doy_huc_df <- extract_data(doy_results[[q]]$huc_result)
            for (field in names(doy_metadata)) {
              doy_huc_df[[field]] <- doy_metadata[[field]]
            }
            all_huc_data <- rbind(all_huc_data, doy_huc_df)
            
            # Add tributary data
            doy_trib_df <- extract_trib_data(
              annual_trib_data, 
              doy_results[[q]]$basin_assign_norm, 
              doy_metadata
            )
            all_trib_data <- rbind(all_trib_data, doy_trib_df)
          }
        }, error = function(e) {
          message("    Error processing DOY quartiles: ", e$message)
        })
        
        # Process CPUE quartile data
        message("  Processing CPUE quartile data")
        
        # Get the CPUE quartile subsets to calculate CPUE percentages
        cpue_quartile_data <- divide_cpue_quartiles(natal_data)
        cpue_quartile_subsets <- cpue_quartile_data$subsets
        
        tryCatch({
          cpue_results <- CPUE_Quartile_Analysis(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8,
            return_values = TRUE
          )
          
          # Process each CPUE quartile
          for (q in 1:length(cpue_results)) {
            if (is.null(cpue_results[[q]])) next
            
            message(paste("    Processing CPUE Q", q))
            
            # Calculate CPUE percentage for this quartile
            cpue_cpue_percentage <- calculate_cpue_percentage(cpue_quartile_subsets[[q]], natal_data)
            
            cpue_metadata <- list(
              year = year,
              watershed = watershed,
              analysis_type = "CPUE",
              quartile = paste0("Q", q),
              cpue_percentage = cpue_cpue_percentage
            )
            
            # Add HUC data
            cpue_huc_df <- extract_data(cpue_results[[q]]$huc_result)
            for (field in names(cpue_metadata)) {
              cpue_huc_df[[field]] <- cpue_metadata[[field]]
            }
            all_huc_data <- rbind(all_huc_data, cpue_huc_df)
            
            # Add tributary data
            cpue_trib_df <- extract_trib_data(
              annual_trib_data, 
              cpue_results[[q]]$basin_assign_norm, 
              cpue_metadata
            )
            all_trib_data <- rbind(all_trib_data, cpue_trib_df)
          }
        }, error = function(e) {
          message("    Error processing CPUE quartiles: ", e$message)
        })
      }
      
      # Process cumulative data if requested
      if (include_cumulative) {
        # Process cumulative DOY data
        message("  Processing cumulative DOY data")
        
        # Get DOY quartile subsets for cumulative calculation
        doy_quartile_data <- divide_doy_quartiles(natal_data)
        doy_quartile_subsets <- doy_quartile_data$subsets
        
        tryCatch({
          cum_doy_results <- Cumulative_DOY_Analysis(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8,
            return_values = TRUE
          )
          
          # Process each cumulative DOY quartile
          for (q in 1:length(cum_doy_results)) {
            if (is.null(cum_doy_results[[q]])) next
            
            message(paste("    Processing Cumulative DOY Q", q))
            
            # Calculate cumulative CPUE percentage
            # Combine quartiles 1 through q
            cum_subset <- do.call(rbind, doy_quartile_subsets[1:q])
            cum_doy_cpue_percentage <- calculate_cpue_percentage(cum_subset, natal_data)
            
            cum_doy_metadata <- list(
              year = year,
              watershed = watershed,
              analysis_type = "Cumulative_DOY",
              quartile = paste0("Q", q),
              cpue_percentage = cum_doy_cpue_percentage
            )
            
            # Add HUC data
            cum_doy_huc_df <- extract_data(cum_doy_results[[q]]$huc_result)
            for (field in names(cum_doy_metadata)) {
              cum_doy_huc_df[[field]] <- cum_doy_metadata[[field]]
            }
            all_huc_data <- rbind(all_huc_data, cum_doy_huc_df)
            
            # Add tributary data
            cum_doy_trib_df <- extract_trib_data(
              annual_trib_data, 
              cum_doy_results[[q]]$basin_assign_norm, 
              cum_doy_metadata
            )
            all_trib_data <- rbind(all_trib_data, cum_doy_trib_df)
          }
        }, error = function(e) {
          message("    Error processing cumulative DOY data: ", e$message)
        })
        
        # Process cumulative CPUE data
        message("  Processing cumulative CPUE data")
        
        # Get CPUE quartile subsets for cumulative calculation
        cpue_quartile_data <- divide_cpue_quartiles(natal_data)
        cpue_quartile_subsets <- cpue_quartile_data$subsets
        
        tryCatch({
          cum_cpue_results <- Cumulative_CPUE_Analysis(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8,
            return_values = TRUE
          )
          
          # Process each cumulative CPUE quartile
          for (q in 1:length(cum_cpue_results)) {
            if (is.null(cum_cpue_results[[q]])) next
            
            message(paste("    Processing Cumulative CPUE Q", q))
            
            # Calculate cumulative CPUE percentage
            # Combine quartiles 1 through q
            cum_subset <- do.call(rbind, cpue_quartile_subsets[1:q])
            cum_cpue_cpue_percentage <- calculate_cpue_percentage(cum_subset, natal_data)
            
            cum_cpue_metadata <- list(
              year = year,
              watershed = watershed,
              analysis_type = "Cumulative_CPUE",
              quartile = paste0("Q", q),
              cpue_percentage = cum_cpue_cpue_percentage
            )
            
            # Add HUC data
            cum_cpue_huc_df <- extract_data(cum_cpue_results[[q]]$huc_result)
            for (field in names(cum_cpue_metadata)) {
              cum_cpue_huc_df[[field]] <- cum_cpue_metadata[[field]]
            }
            all_huc_data <- rbind(all_huc_data, cum_cpue_huc_df)
            
            # Add tributary data
            cum_cpue_trib_df <- extract_trib_data(
              annual_trib_data, 
              cum_cpue_results[[q]]$basin_assign_norm, 
              cum_cpue_metadata
            )
            all_trib_data <- rbind(all_trib_data, cum_cpue_trib_df)
          }
        }, error = function(e) {
          message("    Error processing cumulative CPUE data: ", e$message)
        })
      }
    }
  }
  
  # Check if we have data to save
  if (is.null(all_huc_data) || nrow(all_huc_data) == 0) {
    warning("No HUC data collected - unable to save HUC CSV")
    huc_filepath <- NULL
  } else {
    # Save HUC data to CSV
    huc_filepath <- file.path(output_dir, "all_huc_production_values.csv")
    write.csv(all_huc_data, huc_filepath, row.names = FALSE)
    message(paste("Saved HUC data to:", huc_filepath))
  }
  
  if (is.null(all_trib_data) || nrow(all_trib_data) == 0) {
    warning("No tributary data collected - unable to save tributary CSV")
    trib_filepath <- NULL
  } else {
    # Save tributary data to CSV
    trib_filepath <- file.path(output_dir, "all_tributary_production_values.csv")
    write.csv(all_trib_data, trib_filepath, row.names = FALSE)
    message(paste("Saved tributary data to:", trib_filepath))
  }
  
  return(list(
    huc_file = huc_filepath,
    trib_file = trib_filepath
  ))
}

# Example usage:
# 1. Run all analyses with no quartiles
#run_all_analysis()

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



# # Add this function to your main.R if you want a one-step approach
# run_analysis_and_export <- function(years, watersheds, 
#                                     run_quartiles = FALSE, 
#                                     run_cumulative = FALSE,
#                                     export_cv = TRUE) {
#   # Run the analysis
#   run_all_analysis(run_quartiles = run_quartiles, 
#                    run_cumulative = run_cumulative)
#   
#   # Export production values
#   export_production_values(years = years, watersheds = watersheds)
#   
#   # Export CV analysis if requested
#   if (export_cv) {
#     export_cv_analysis(years = years, watersheds = watersheds, top_percent = 50)
#   }
#   
#   message("Analysis and export complete!")
# }
# 
# 
# export_production_values(
#   years = c("2017", "2019", "2020", "2021"),
#   watersheds = c("Kusko")
# )

# Then use it like:
# run_analysis_and_export(
#   years = c("2017", "2019", "2020", "2021"),
#   watersheds = c("Kusko"),
#   run_quartiles = TRUE,
#   run_cumulative = TRUE
# )

export_production_values(
  years = c("2017","2018", "2019", "2020", "2021"),
  watersheds = c("Kusko"),
  include_quartiles = TRUE,
  include_cumulative = FALSE
)
