# cumulative_quartile_analysis.R
# Functions for cumulative quartile analysis (DOY and CPUE)

library(sf)
library(dplyr)
library(here)
library(ggplot2)

# Source the required utility files
source(here("code/utils/spatial_utils.R"))
source(here("code/utils/visualization.R"))
source(here("code/assignment.R"))

#' Perform Cumulative DOY Quartile Analysis
#'
#' @param year Character or numeric representing the year
#' @param watershed Character: "Kusko" or "Yukon"
#' @param sensitivity_threshold Numeric threshold for assignment filtering
#' @param min_error Minimum error value to use
#' @param min_stream_order Minimum stream order to include
#' @param HUC HUC level (e.g., 8, 10)
#' @param return_values Whether to return the calculated values
#' @return If return_values is TRUE, a list with results; otherwise NULL
# Modified Cumulative DOY Analysis function to use PNG output

Cumulative_DOY_Analysis <- function(year, watershed, sensitivity_threshold, min_error, 
                                    min_stream_order = 3, HUC = 8, 
                                    return_values = FALSE) {
  
  # Generate identifier for output files
  identifier <- paste(year, watershed, sep = "_")
  
  # Load spatial data
  spatial_data <- load_spatial_data(watershed, HUC, min_stream_order)
  edges <- spatial_data$edges
  basin <- spatial_data$basin
  Huc <- spatial_data$Huc
  
  # Load natal origins data
  natal_data <- load_natal_data(year, watershed)
  
  # Divide data into DOY quartiles
  quartile_data <- divide_doy_quartiles(natal_data)
  quartile_subsets <- quartile_data$subsets
  subset_labels <- quartile_data$labels
  
  # Create visualization of the DOY splits
  create_split_visualization(natal_data, quartile_data$breaks, "DOY", identifier)
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Calculate error values
  error <- calculate_error(pid_isose, min_error)
  
  # Set up watershed-specific priors
  priors <- setup_watershed_priors(edges, min_stream_order, watershed, natal_data)
  
  # Create output directories
  dir.create(here("Basin Maps/DOY_Cumulative/HUC"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/DOY_Cumulative/HUC/RawProduction"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/DOY_Cumulative/Tribs"), showWarnings = FALSE, recursive = TRUE)
  
  # Return values storage
  if (return_values) {
    all_results <- list()
  }
  
  # Create cumulative quartile subsets
  cumulative_subsets <- list()
  cumulative_labels <- list()
  
  # First quartile is just Q1
  cumulative_subsets[[1]] <- quartile_subsets[[1]]
  cumulative_labels[[1]] <- paste0("Cumulative DOY: ", subset_labels[1])
  
  # For remaining quartiles, add all previous data
  for (q in 2:length(quartile_subsets)) {
    # Combine data from quartiles 1 to q
    cumulative_subsets[[q]] <- do.call(rbind, quartile_subsets[1:q])
    
    # Create label that shows cumulative range
    q1_min <- min(quartile_subsets[[1]]$DOY, na.rm = TRUE)
    current_max <- max(quartile_subsets[[q]]$DOY, na.rm = TRUE)
    cumulative_labels[[q]] <- paste0("Cumulative DOY: ", ceiling(q1_min), "-", floor(current_max))
  }
  
  # Process each cumulative subset
  for (q in 1:length(cumulative_subsets)) {
    current_subset <- cumulative_subsets[[q]]
    
    # Skip empty subsets
    if (nrow(current_subset) == 0) {
      message(paste("Skipping", cumulative_labels[q], "because it contains no data"))
      next
    }
    
    # Create unique ID for this subset
    subset_id <- paste0(watershed, "_", year, "_CumulativeDOY_Q", q)
    message(paste("Processing", cumulative_labels[[q]], "with", nrow(current_subset), "data points"))
    
    # Perform assignment
    assignment_matrix <- perform_assignment(
      current_subset, edges, watershed, priors, pid_iso, error, sensitivity_threshold
    )
    
    # Process assignments to get basin-scale values
    basin_results <- process_assignments(assignment_matrix)
    basin_assign_rescale <- basin_results$rescale
    basin_assign_norm <- basin_results$norm
    
    # Create improved histogram
    gg_hist <- create_cpue_histogram(natal_data, current_subset, cumulative_labels[[q]])
    
    # Process HUC data
    final_result <- process_huc_data(edges, basin, Huc, basin_assign_rescale, HUC)
    
    # Create HUC map with production per km (Using PNG instead of PDF)
    huc_filepath <- file.path(here("Basin Maps/DOY_Cumulative/HUC"), 
                              paste0(subset_id, "_HUC", HUC, "_.png"))
    
    create_huc_map(
      final_result = final_result,
      basin_assign_norm = basin_assign_norm,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      HUC = HUC,
      subset_label = cumulative_labels[[q]],
      output_filepath = huc_filepath
    )
    
    # Create HUC map with raw production proportion (Using PNG instead of PDF)
    raw_huc_filepath <- file.path(here("Basin Maps/DOY_Cumulative/HUC/RawProduction"), 
                                  paste0(subset_id, "_RawProd_HUC", HUC, "_.png"))
    
    create_raw_production_map(
      final_result = final_result,
      basin_assign_norm = basin_assign_norm,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      HUC = HUC,
      subset_label = cumulative_labels[[q]],
      output_filepath = raw_huc_filepath
    )
    
    # Create tributary map (Using PNG instead of PDF)
    trib_filepath <- file.path(here("Basin Maps/DOY_Cumulative/Tribs"), 
                               paste0(subset_id, "_.png"))
    
    create_tributary_map(
      basin = basin,
      edges = edges,
      basin_assign_norm = basin_assign_norm,
      StreamOrderPrior = priors$StreamOrderPrior,
      pid_prior = priors$pid_prior,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      min_error = min_error,
      subset_label = cumulative_labels[[q]],
      output_filepath = trib_filepath
    )
    
    # Store results if needed
    if (return_values) {
      all_results[[q]] <- list(
        subset = current_subset,
        label = cumulative_labels[[q]],
        basin_assign_rescale = basin_assign_rescale,
        basin_assign_norm = basin_assign_norm,
        huc_result = final_result
      )
    }
  }
  
  if (return_values) {
    return(all_results)
  } else {
    return(invisible(NULL))
  }
}

# Modified Cumulative CPUE Analysis function to use PNG output

Cumulative_CPUE_Analysis <- function(year, watershed, sensitivity_threshold, min_error, 
                                     min_stream_order = 3, HUC = 8, 
                                     return_values = FALSE) {
  
  # Generate identifier for output files
  identifier <- paste(year, watershed, sep = "_")
  
  # Load spatial data
  spatial_data <- load_spatial_data(watershed, HUC, min_stream_order)
  edges <- spatial_data$edges
  basin <- spatial_data$basin
  Huc <- spatial_data$Huc
  
  # Load natal origins data
  natal_data <- load_natal_data(year, watershed)
  
  # Divide data into CPUE quartiles
  quartile_data <- divide_cpue_quartiles(natal_data)
  quartile_subsets <- quartile_data$subsets
  subset_labels <- quartile_data$labels
  
  # Create visualization of the CPUE splits
  create_split_visualization(natal_data, quartile_data$breaks, "CPUE", identifier)
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Calculate error values
  error <- calculate_error(pid_isose, min_error)
  
  # Set up watershed-specific priors
  priors <- setup_watershed_priors(edges, min_stream_order, watershed, natal_data)
  
  # Create output directories
  dir.create(here("Basin Maps/CPUE_Cumulative/HUC"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/CPUE_Cumulative/HUC/RawProduction"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/CPUE_Cumulative/Tribs"), showWarnings = FALSE, recursive = TRUE)
  
  # Return values storage
  if (return_values) {
    all_results <- list()
  }
  
  # Create cumulative quartile subsets
  cumulative_subsets <- list()
  cumulative_labels <- list()
  
  # First quartile is just Q1
  cumulative_subsets[[1]] <- quartile_subsets[[1]]
  cumulative_labels[[1]] <- paste0("Cumulative: ", subset_labels[1])
  
  # For remaining quartiles, add all previous data
  for (q in 2:length(quartile_subsets)) {
    # Combine data from quartiles 1 to q
    cumulative_subsets[[q]] <- do.call(rbind, quartile_subsets[1:q])
    
    # Create label that shows cumulative progress
    current_proportion <- sum(sapply(quartile_subsets[1:q], function(x) sum(x$dailyCPUEprop))) / 
      sum(natal_data$dailyCPUEprop)
    cumulative_labels[[q]] <- paste0("Cumulative: First ", round(current_proportion * 100), "% of Run")
  }
  
  # Process each cumulative subset
  for (q in 1:length(cumulative_subsets)) {
    current_subset <- cumulative_subsets[[q]]
    
    # Skip empty subsets
    if (nrow(current_subset) == 0) {
      message(paste("Skipping", cumulative_labels[q], "because it contains no data"))
      next
    }
    
    # Create unique ID for this subset
    subset_id <- paste0(watershed, "_", year, "_CumulativeCPUE_Q", q)
    message(paste("Processing", cumulative_labels[[q]], "with", nrow(current_subset), "data points"))
    
    # Perform assignment
    assignment_matrix <- perform_assignment(
      current_subset, edges, watershed, priors, pid_iso, error, sensitivity_threshold
    )
    
    # Process assignments to get basin-scale values
    basin_results <- process_assignments(assignment_matrix)
    basin_assign_rescale <- basin_results$rescale
    basin_assign_norm <- basin_results$norm
    
    # Create improved histogram
    gg_hist <- create_cpue_histogram(natal_data, current_subset, cumulative_labels[[q]])
    
    # Process HUC data
    final_result <- process_huc_data(edges, basin, Huc, basin_assign_rescale, HUC)
    
    # Create HUC map with production per km (Using PNG instead of PDF)
    huc_filepath <- file.path(here("Basin Maps/CPUE_Cumulative/HUC"), 
                              paste0(subset_id, "_HUC", HUC, "_.png"))
    
    create_huc_map(
      final_result = final_result,
      basin_assign_norm = basin_assign_norm,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      HUC = HUC,
      subset_label = cumulative_labels[[q]],
      output_filepath = huc_filepath
    )
    
    # Create HUC map with raw production proportion (Using PNG instead of PDF)
    raw_huc_filepath <- file.path(here("Basin Maps/CPUE_Cumulative/HUC/RawProduction"), 
                                  paste0(subset_id, "_RawProd_HUC", HUC, "_.png"))
    
    create_raw_production_map(
      final_result = final_result,
      basin_assign_norm = basin_assign_norm,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      HUC = HUC,
      subset_label = cumulative_labels[[q]],
      output_filepath = raw_huc_filepath
    )
    
    # Create tributary map (Using PNG instead of PDF)
    trib_filepath <- file.path(here("Basin Maps/CPUE_Cumulative/Tribs"), 
                               paste0(subset_id, "_.png"))
    
    create_tributary_map(
      basin = basin,
      edges = edges,
      basin_assign_norm = basin_assign_norm,
      StreamOrderPrior = priors$StreamOrderPrior,
      pid_prior = priors$pid_prior,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      min_error = min_error,
      subset_label = cumulative_labels[[q]],
      output_filepath = trib_filepath
    )
    
    # Store results if needed
    if (return_values) {
      all_results[[q]] <- list(
        subset = current_subset,
        label = cumulative_labels[[q]],
        basin_assign_rescale = basin_assign_rescale,
        basin_assign_norm = basin_assign_norm,
        huc_result = final_result
      )
    }
  }
  
  if (return_values) {
    return(all_results)
  } else {
    return(invisible(NULL))
  }
}

# Example usage:
# Run for specific years and watersheds
# run_cumulative_quartile_analysis(
#   years = c("2015", "2016"), 
#   watersheds = c("Yukon", "Kusko"),
#   analysis_types = c("DOY", "CPUE")
# )