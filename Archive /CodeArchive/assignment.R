################################################################################
# ASSIGNMENT.R - BAYESIAN ASSIGNMENT FUNCTIONS
################################################################################
# PURPOSE: Core functions for performing Bayesian assignment of fish to watersheds
# Based on isotope signatures, genetic data, and spatial priors
################################################################################

library(sf)
library(dplyr)
library(here)

################################################################################
# MAIN ASSIGNMENT FUNCTION
################################################################################

#' Perform Bayesian assignment for natal origins data
#' 
#' @param natal_data Data frame with natal origins data (must have natal_iso, COratio columns)
#' @param edges SF object with stream edges
#' @param watershed Character: "Kusko" or "Yukon"
#' @param priors List of prior values from setup_watershed_priors()
#' @param pid_iso Vector of isotope predictions for each edge
#' @param error Vector of error values for each edge
#' @param sensitivity_threshold Threshold for assignment filtering (default 0.7)
#' @return Matrix of assignments for each data point to each stream edge
perform_assignment <- function(natal_data, edges, watershed, priors, 
                               pid_iso, error, sensitivity_threshold) {
  
  # Initialize assignment matrix (edges × fish)
  assignment_matrix <- matrix(NA, nrow = length(pid_iso), ncol = nrow(natal_data))
  
  # Process each fish individually
  for (i in 1:nrow(natal_data)) {
    iso_o <- as.numeric(natal_data$natal_iso[i])
    
    if (watershed == "Kusko") {
      # KUSKOKWIM ASSIGNMENT: Uses isotope + spatial priors + management unit filtering
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * priors$PresencePrior 
      
      # Create data frame with assignment values and management unit names
      assign_df <- data.frame(
        assign = assign, 
        mgmt_river = edges$mgmt_river,
        stringsAsFactors = FALSE
      )
      
      # Remove rows with NA or empty management units
      assign_df <- assign_df %>% 
        filter(!is.na(mgmt_river) & mgmt_river != "")
      
      # Find the top 5% of assignment values
      top_5_percent <- quantile(assign_df$assign, 0.95, na.rm = TRUE)
      
      # Filter to values above the top 5%
      top_assigns <- assign_df %>% 
        filter(assign >= top_5_percent)
      
      # Find the management unit with the most segments in the top 5%
      top_mgmt_unit <- top_assigns %>% 
        group_by(mgmt_river) %>% 
        summarise(count = n(), .groups = "drop") %>% 
        arrange(desc(count)) %>% 
        slice(1) %>% 
        pull(mgmt_river)
      
      # Get management units for all edges
      mgmt_units <- edges$mgmt_river
      
      # Find indices for the top management unit vs all others
      top_indices <- which(mgmt_units == top_mgmt_unit)
      non_top_indices <- which(mgmt_units != top_mgmt_unit | 
                                 is.na(mgmt_units) | 
                                 mgmt_units == "")
      
      # Zero out assignments for non-top management units
      assign[non_top_indices] <- 0
      
    } else if (watershed == "Yukon") {
      # YUKON ASSIGNMENT: Uses isotope + spatial + genetic priors
      gen.prior <- rep(0, length = length(pid_iso))
      gen.prior[priors$LYsites] <- as.numeric(natal_data$Lower[i])
      gen.prior[priors$MYsites] <- as.numeric(natal_data$Middle[i])
      gen.prior[priors$UYsites] <- as.numeric(natal_data$Upper[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * gen.prior
    }
    
    # NORMALIZE AND THRESHOLD assignments
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # WEIGHT by CPUE (daily catch per unit effort)
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  return(assignment_matrix)
}

################################################################################
# ASSIGNMENT PROCESSING FUNCTIONS
################################################################################

#' Process assignment matrix to get basin-scale summary values
#' 
#' @param assignment_matrix Matrix from perform_assignment()
#' @return List containing sum, rescaled, and normalized basin assignments
process_assignments <- function(assignment_matrix) {
  # Sum across all fish for each stream edge
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  
  # Rescale to proportions (sums to 1)
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE)
  
  # Normalize to 0-1 scale (max value = 1)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
  
  return(list(
    sum = basin_assign_sum,
    rescale = basin_assign_rescale,
    norm = basin_assign_norm
  ))
}