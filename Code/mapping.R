# mapping.R
# Core mapping functionality for watershed analysis

library(sf)
library(dplyr)
library(here)
library(ggplot2)

# Source the required utility files
source(here("code/utils/spatial_utils.R"))
source(here("code/utils/visualization.R"))
source(here("code/assignment.R"))

#' Create basin maps for natal origin distributions
#'
#' @param year Character or numeric representing the year
#' @param watershed Character: "Kusko" or "Yukon"
#' @param sensitivity_threshold Numeric threshold for assignment filtering
#' @param min_error Minimum error value to use
#' @param min_stream_order Minimum stream order to include
#' @param HUC HUC level (e.g., 8, 10)
#' @param return_values Whether to return the calculated values
#' @return If return_values is TRUE, a list with results; otherwise NULL
create_basin_maps <- function(year, watershed, sensitivity_threshold, min_error, 
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
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Calculate error values
  error <- calculate_error(pid_isose, min_error)
  
  # Set up watershed-specific priors
  priors <- setup_watershed_priors(edges, min_stream_order, watershed, natal_data)
  
  # Create CPUE histogram
  gg_hist <- ggplot(natal_data, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "gray20", linewidth = 2) +
    theme_minimal() +
    ggtitle("Prop. CPUE by DOY")
  
  if (nrow(natal_data) > 0) {
    gg_hist <- gg_hist +
      geom_ribbon(data = natal_data, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                  fill = "gray", alpha = 0.7, inherit.aes = FALSE)
  }
  
  # Perform assignment
  assignment_matrix <- perform_assignment(
    natal_data, edges, watershed, priors, pid_iso, error, sensitivity_threshold
  )
  
  # Process assignments to get basin-scale values
  basin_results <- process_assignments(assignment_matrix)
  basin_assign_rescale <- basin_results$rescale
  basin_assign_norm <- basin_results$norm
  
  # Process HUC data
  final_result <- process_huc_data(edges, basin, Huc, basin_assign_rescale, HUC)
  
  # Create output directories
  dir.create(here("Basin Maps/Annual_Maps/HUC"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/Annual_Maps/Tribs"), showWarnings = FALSE, recursive = TRUE)
  
  # Create HUC map
  huc_filepath <- file.path(here("Basin Maps/Annual_Maps/HUC"), 
                            paste0(identifier, "_HUC", HUC, "_", sensitivity_threshold, 
                                   "_StrOrd", min_stream_order, "_.pdf"))
  
  create_huc_map(
    final_result = final_result,
    basin_assign_norm = basin_assign_norm,
    gg_hist = gg_hist,
    year = year,
    watershed = watershed,
    sensitivity_threshold = sensitivity_threshold,
    min_stream_order = min_stream_order,
    HUC = HUC,
    subset_label = NULL,
    output_filepath = huc_filepath
  )
  
  # Create tributary map
  trib_filepath <- file.path(here("Basin Maps/Annual_Maps/Tribs"), 
                             paste0(identifier, "_", sensitivity_threshold,
                                    "_StrOrd", min_stream_order, "_.pdf"))
  
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
    subset_label = NULL,
    output_filepath = trib_filepath
  )
  
  if (return_values) {
    return(list(
      huc_data = final_result,
      stream_data = edges,
      basin_assign_norm = basin_assign_norm,
      basin_assign_rescale = basin_assign_rescale
    ))
  } else {
    return(invisible(NULL))
  }
}