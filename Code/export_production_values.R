# export_production_values.R
# Function to export production values to CSV files for both HUCs and tributaries

library(sf)
library(dplyr)
library(here)
library(readr)

#' Export production values for multiple years and watersheds to CSV files
#'
#' @param years Vector of years to process
#' @param watersheds Vector of watersheds to process
#' @param output_dir Directory to save output CSV files
#' @param run_analysis Whether to run the analysis again or use existing data
#' @return List with paths to created CSV files
export_production_values <- function(years, watersheds, 
                                     output_dir = here("Analysis_Results"),
                                     run_analysis = FALSE) {
  
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
      
      # Either run analysis or load cached results
      if (run_analysis) {
        # Run full analysis for this dataset
        message("  Running full analysis...")
        results <- create_basin_maps(
          year = year,
          watershed = watershed,
          sensitivity_threshold = sensitivity_threshold,
          min_error = min_error,
          min_stream_order = min_stream_order,
          HUC = 8,
          return_values = TRUE  # Important: return values instead of just creating maps
        )
        
        # Extract HUC and trib data
        huc_data <- results$huc_data
        trib_data <- results$stream_data
        basin_assign_norm <- results$basin_assign_norm
        
      } else {
        # Try to load previously processed data from expected locations
        message("  Loading cached data...")
        
        # Load spatial data
        spatial_data <- load_spatial_data(watershed, 8, min_stream_order)
        edges <- spatial_data$edges
        basin <- spatial_data$basin
        Huc <- spatial_data$Huc
        
        # Look for processed assignment files
        assignment_file <- file.path(here("Data/Processed"), 
                                     paste0(year, "_", watershed, "_assignments.rds"))
        
        if (file.exists(assignment_file)) {
          basin_assign_values <- readRDS(assignment_file)
          basin_assign_rescale <- basin_assign_values$rescale
          basin_assign_norm <- basin_assign_values$norm
          
          # Process HUC data
          huc_data <- process_huc_data(edges, basin, Huc, basin_assign_rescale, 8)
          
          # Add trib data
          trib_data <- edges
          trib_data$basin_assign_norm <- basin_assign_norm
          
        } else {
          message("  No cached data found. Running analysis...")
          # Run analysis if no cached data exists
          results <- create_basin_maps(
            year = year,
            watershed = watershed,
            sensitivity_threshold = sensitivity_threshold,
            min_error = min_error,
            min_stream_order = min_stream_order,
            HUC = 8,
            return_values = TRUE
          )
          
          # Extract HUC and trib data
          huc_data <- results$huc_data
          trib_data <- results$stream_data
          basin_assign_norm <- results$basin_assign_norm
          
          # Save the assignment data for future use
          dir.create(file.path(here("Data/Processed")), showWarnings = FALSE, recursive = TRUE)
          saveRDS(results[c("basin_assign_rescale", "basin_assign_norm")], assignment_file)
        }
      }
      
      # Add year and watershed identifiers to the data
      huc_data$year <- year
      huc_data$watershed <- watershed
      
      trib_data$year <- year
      trib_data$watershed <- watershed
      
      # Select non-geometry columns for HUC data
      if (inherits(huc_data, "sf")) {
        huc_data_df <- st_drop_geometry(huc_data)
      } else {
        huc_data_df <- huc_data
      }
      
      # Select relevant columns for tributary data
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
        
        # Create a simplified dataframe
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
  write_csv(all_huc_data, huc_filepath)
  write_csv(all_trib_data, trib_filepath)
  
  message(paste("Saved HUC data to:", huc_filepath))
  message(paste("Saved tributary data to:", trib_filepath))
  
  return(list(
    huc_file = huc_filepath,
    trib_file = trib_filepath
  ))
}
