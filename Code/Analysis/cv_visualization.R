# cv_visualization_top_producers.R
# Visualization of coefficient of variation across years for top producers only

library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(RColorBrewer)

# Source the required utility files
source(here("code/utils/spatial_utils.R"))
source(here("code/assignment.R"))

#' Process annual data for multiple years to calculate coefficient of variation
#' focusing only on top producers
#'
#' @param years Vector of years to analyze
#' @param watershed Character: "Kusko" or "Yukon"
#' @param top_percent Percent of top producers to include (default 50%)
#' @param sensitivity_threshold Numeric threshold for assignment filtering
#' @param min_error Minimum error value to use
#' @param min_stream_order Minimum stream order to include
#' @param HUC HUC level (e.g., 8, 10)
#' @return List containing processed data with CV calculations for top producers
analyze_annual_cv_top_producers <- function(years, watershed, 
                                            top_percent = 50,
                                            sensitivity_threshold = 0.7,
                                            min_error = NULL, 
                                            min_stream_order = NULL, 
                                            HUC = 8) {
  
  # Set default parameters based on watershed
  if(is.null(min_error)) {
    min_error <- ifelse(watershed == "Yukon", 0.003, 0.0006)
  }
  
  if(is.null(min_stream_order)) {
    min_stream_order <- ifelse(watershed == "Yukon", 5, 3)
  }
  
  # Load spatial data (only need to do this once)
  spatial_data <- load_spatial_data(watershed, HUC, min_stream_order)
  edges <- spatial_data$edges
  basin <- spatial_data$basin
  Huc <- spatial_data$Huc
  
  # Extract isoscape prediction and error values (same for all years)
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  error <- calculate_error(pid_isose, min_error)
  
  # Store results for each year
  huc_results_list <- list()
  edge_results_list <- list()
  
  # Process each year
  for(year in years) {
    message(paste("Processing", year, watershed))
    
    # Load natal origins data for this year
    tryCatch({
      natal_data <- load_natal_data(year, watershed)
      
      # Set up watershed-specific priors
      priors <- setup_watershed_priors(edges, min_stream_order, watershed, natal_data)
      
      # Perform assignment
      assignment_matrix <- perform_assignment(
        natal_data, edges, watershed, priors, pid_iso, error, sensitivity_threshold
      )
      
      # Process assignments to get basin-scale values
      basin_results <- process_assignments(assignment_matrix)
      basin_assign_rescale <- basin_results$rescale
      basin_assign_norm <- basin_results$norm
      
      # Process HUC data
      huc_result <- process_huc_data(edges, basin, Huc, basin_assign_rescale, HUC)
      
      # Store HUC result for this year
      huc_results_list[[year]] <- huc_result
      
      # Store edge data for this year
      edges_with_data <- edges
      edges_with_data$basin_assign_norm <- basin_assign_norm
      edge_results_list[[year]] <- edges_with_data
      
      message(paste("  Successfully processed", year))
      
    }, error = function(e) {
      message(paste("  Error processing", year, ":", e$message))
    })
  }
  
  # Check if we have data
  if(length(huc_results_list) < 2) {
    stop("Need at least 2 years of data to calculate CV")
  }
  
  # Calculate CV for HUCs
  # First, create a reference to the HUC data structure
  ref_huc <- huc_results_list[[1]]
  huc_col <- paste0("HUC", HUC)
  
  # Initialize matrices to store values across years
  n_hucs <- nrow(ref_huc)
  n_years <- length(huc_results_list)
  years_with_data <- names(huc_results_list)
  
  raw_prod_matrix <- matrix(NA, nrow = n_hucs, ncol = n_years)
  prod_per_km_matrix <- matrix(NA, nrow = n_hucs, ncol = n_years)
  
  # Fill matrices with values from each year
  for(i in 1:n_years) {
    year <- years_with_data[i]
    current_huc <- huc_results_list[[year]]
    
    # Match HUCs between reference and current year
    match_idx <- match(ref_huc[[huc_col]], current_huc[[huc_col]])
    
    # Extract values
    raw_prod_matrix[, i] <- current_huc$production_proportion[match_idx]
    prod_per_km_matrix[, i] <- current_huc$production_per_meter_norm[match_idx]
  }
  
  # Calculate mean production across years
  mean_raw_prod <- rowMeans(raw_prod_matrix, na.rm = TRUE)
  mean_prod_per_km <- rowMeans(prod_per_km_matrix, na.rm = TRUE)
  
  # Identify top producers based on mean production
  # Find threshold value for top_percent
  raw_prod_threshold <- quantile(mean_raw_prod, prob = 1 - (top_percent/100), na.rm = TRUE)
  prod_km_threshold <- quantile(mean_prod_per_km, prob = 1 - (top_percent/100), na.rm = TRUE)
  
  # Filter to include only top producers
  top_raw_prod <- mean_raw_prod >= raw_prod_threshold
  top_prod_per_km <- mean_prod_per_km >= prod_km_threshold
  
  # Calculate CV only for top producers
  cv_raw_prod <- rep(NA, n_hucs)
  cv_prod_per_km <- rep(NA, n_hucs)
  
  for(i in 1:n_hucs) {
    if(top_raw_prod[i] && sum(!is.na(raw_prod_matrix[i, ])) >= 2) {
      cv_raw_prod[i] <- sd(raw_prod_matrix[i, ], na.rm = TRUE) / mean_raw_prod[i]
    }
    
    if(top_prod_per_km[i] && sum(!is.na(prod_per_km_matrix[i, ])) >= 2) {
      cv_prod_per_km[i] <- sd(prod_per_km_matrix[i, ], na.rm = TRUE) / mean_prod_per_km[i]
    }
  }
  
  # Add CV and mean to HUC data
  huc_cv_data <- ref_huc
  huc_cv_data$mean_raw_prod <- mean_raw_prod
  huc_cv_data$mean_prod_per_km <- mean_prod_per_km
  huc_cv_data$cv_raw_prod <- cv_raw_prod
  huc_cv_data$cv_prod_per_km <- cv_prod_per_km
  huc_cv_data$is_top_raw_prod <- top_raw_prod
  huc_cv_data$is_top_prod_per_km <- top_prod_per_km
  
  # Calculate CV for tributary edges - for top producers only
  # First calculate mean values across years
  
  # Get all unique edge IDs
  all_edge_ids <- lapply(edge_results_list, function(edges) {
    if("reachid" %in% names(edges)) {
      return(edges$reachid)
    } else {
      # Create a unique ID based on geometry
      return(paste(st_coordinates(st_centroid(edges))[,1], 
                   st_coordinates(st_centroid(edges))[,2], sep = "_"))
    }
  })
  all_edge_ids <- unique(unlist(all_edge_ids))
  
  # Create matrix for edge data
  n_edges <- length(all_edge_ids)
  edge_matrix <- matrix(NA, nrow = n_edges, ncol = n_years)
  
  # Fill matrix with values from each year
  for(i in 1:n_years) {
    year <- years_with_data[i]
    current_edges <- edge_results_list[[year]]
    
    # Get identifier for current edges
    if("reachid" %in% names(current_edges)) {
      current_ids <- current_edges$reachid
    } else {
      current_ids <- paste(st_coordinates(st_centroid(current_edges))[,1], 
                           st_coordinates(st_centroid(current_edges))[,2], sep = "_")
    }
    
    # Match edges to all_edge_ids
    for(j in 1:n_edges) {
      match_idx <- which(current_ids == all_edge_ids[j])
      if(length(match_idx) > 0) {
        edge_matrix[j, i] <- current_edges$basin_assign_norm[match_idx[1]]
      }
    }
  }
  
  # Calculate mean value for each edge
  mean_edge_values <- rowMeans(edge_matrix, na.rm = TRUE)
  
  # Find threshold for top edges
  edge_threshold <- quantile(mean_edge_values, prob = 1 - (top_percent/100), na.rm = TRUE)
  top_edges <- mean_edge_values >= edge_threshold
  
  # Calculate CV only for top edges
  cv_edges <- rep(NA, n_edges)
  for(i in 1:n_edges) {
    if(top_edges[i] && sum(!is.na(edge_matrix[i, ])) >= 2) {
      cv_edges[i] <- sd(edge_matrix[i, ], na.rm = TRUE) / mean_edge_values[i]
    }
  }
  
  # We need to match the CV values back to the edges geometry
  # We'll use the first year as reference
  ref_edges <- edge_results_list[[1]]
  
  if("reachid" %in% names(ref_edges)) {
    ref_ids <- ref_edges$reachid
  } else {
    ref_ids <- paste(st_coordinates(st_centroid(ref_edges))[,1], 
                     st_coordinates(st_centroid(ref_edges))[,2], sep = "_")
  }
  
  # Match mean values and CV values to reference edges
  ref_edges$mean_basin_assign <- NA
  ref_edges$cv_basin_assign <- NA
  ref_edges$is_top_producer <- FALSE
  
  for(i in 1:length(ref_ids)) {
    match_idx <- which(all_edge_ids == ref_ids[i])
    if(length(match_idx) > 0) {
      ref_edges$mean_basin_assign[i] <- mean_edge_values[match_idx[1]]
      ref_edges$cv_basin_assign[i] <- cv_edges[match_idx[1]]
      ref_edges$is_top_producer[i] <- top_edges[match_idx[1]]
    }
  }
  
  return(list(
    huc_cv_data = huc_cv_data,
    edge_cv_data = ref_edges,
    watershed = watershed,
    years = years_with_data,
    top_percent = top_percent
  ))
}

#' Create and save CV visualization for top producers
#'
#' @param cv_data Output from analyze_annual_cv_top_producers function
#' @param output_dir Directory to save output files
#' @return List of file paths to saved figures
create_top_producer_cv_visualization <- function(cv_data, output_dir = here("Basin Maps/CV_Analysis")) {
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Extract data
  huc_cv_data <- cv_data$huc_cv_data
  edge_cv_data <- cv_data$edge_cv_data
  watershed <- cv_data$watershed
  years <- cv_data$years
  top_percent <- cv_data$top_percent
  
  # Generate output filenames
  huc_filename <- paste0(watershed, "_", min(years), "-", max(years), "_Top", top_percent, "_HUC_CV.pdf")
  trib_filename <- paste0(watershed, "_", min(years), "-", max(years), "_Top", top_percent, "_Tributary_CV.pdf")
  
  huc_filepath <- file.path(output_dir, huc_filename)
  trib_filepath <- file.path(output_dir, trib_filename)
  
  # Create HUC CV map for top producers
  # Using a reversed spectral palette: blue = low CV (consistent), red = high CV (variable)
  cv_colors <- rev(brewer.pal(11, "RdYlBu"))
  
  # Create figure for HUC map
  pdf(huc_filepath, width = 10, height = 8)
  
  # Create base map with all HUCs in light gray
  plot(st_geometry(huc_cv_data), 
       col = "gray90", 
       border = "white",
       main = paste0(watershed, " Basin: Coefficient of Variation in Annual Production\n",
                     "Top ", top_percent, "% Producers (", paste(years, collapse = ", "), ")"))
  
  # Overlay only top producers with CV colors
  top_hucs <- huc_cv_data[huc_cv_data$is_top_raw_prod == TRUE, ]
  
  if(nrow(top_hucs) > 0) {
    max_cv_huc <- max(top_hucs$cv_raw_prod, na.rm = TRUE)
    max_cv_huc <- min(max_cv_huc, 2) # Cap at 2 for better color scaling
    
    plot(st_geometry(top_hucs), 
         col = colorRampPalette(cv_colors)(100)[
           pmin(100, pmax(1, round(top_hucs$cv_raw_prod / max_cv_huc * 100)))
         ], 
         border = "white",
         add = TRUE)
    
    # Add legend
    legend_breaks <- seq(0, max_cv_huc, length.out = 5)
    legend_colors <- colorRampPalette(cv_colors)(length(legend_breaks))
    
    legend("bottomright", 
           legend = sprintf("%.2f", legend_breaks), 
           fill = legend_colors, 
           title = "Coefficient of Variation\n(lower = more consistent)",
           bty = "n")
  } else {
    text(mean(par("usr")[1:2]), mean(par("usr")[3:4]), "No top producers found")
  }
  
  dev.off()
  
  # Create Tributary CV map for top producers
  # Filter to only include top producers with CV values
  top_edges <- edge_cv_data[edge_cv_data$is_top_producer == TRUE & !is.na(edge_cv_data$cv_basin_assign), ]
  
  # Create figure for tributary map
  pdf(trib_filepath, width = 10, height = 8)
  
  # Plot basin as background
  plot(st_geometry(huc_cv_data), 
       col = "gray90", 
       border = "gray80",
       main = paste0(watershed, " Basin: Coefficient of Variation in Annual Production\n",
                     "Top ", top_percent, "% Producers (", paste(years, collapse = ", "), ")"))
  
  # Overlay only top producer tributaries with CV colors
  if(nrow(top_edges) > 0) {
    max_cv_edge <- max(top_edges$cv_basin_assign, na.rm = TRUE)
    max_cv_edge <- min(max_cv_edge, 2) # Cap at 2 for better color scaling
    
    # Scale edge width by stream order
    stream_order_lwd <- top_edges$Str_Order
    linewidths <- rep(1, length(stream_order_lwd))
    linewidths <- ifelse(stream_order_lwd == 9, 5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 8, 4, linewidths)
    linewidths <- ifelse(stream_order_lwd == 7, 3, linewidths)
    linewidths <- ifelse(stream_order_lwd == 6, 2, linewidths)
    linewidths <- ifelse(stream_order_lwd == 5, 1.8, linewidths)
    linewidths <- ifelse(stream_order_lwd == 4, 1.5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 3, 1, linewidths)
    
    plot(st_geometry(top_edges), 
         col = colorRampPalette(cv_colors)(100)[
           pmin(100, pmax(1, round(top_edges$cv_basin_assign / max_cv_edge * 100)))
         ],
         lwd = linewidths,
         add = TRUE)
    
    # Add legend
    legend_breaks <- seq(0, max_cv_edge, length.out = 5)
    legend_colors <- colorRampPalette(cv_colors)(length(legend_breaks))
    
    legend("bottomright", 
           legend = sprintf("%.2f", legend_breaks), 
           col = legend_colors, 
           lwd = 3,
           title = "Coefficient of Variation\n(lower = more consistent)",
           bty = "n")
  } else {
    text(mean(par("usr")[1:2]), mean(par("usr")[3:4]), "No top producer tributaries found")
  }
  
  dev.off()
  
  message("HUC CV map for top producers saved to: ", huc_filepath)
  message("Tributary CV map for top producers saved to: ", trib_filepath)
  
  return(list(
    huc_filepath = huc_filepath,
    trib_filepath = trib_filepath
  ))
}

# Example usage:
 cv_data <- analyze_annual_cv_top_producers(
   years = c("2017", "2019", "2020", "2021"),
   watershed = "Kusko",
   top_percent = 50
 )
# 
create_top_producer_cv_visualization(cv_data)
