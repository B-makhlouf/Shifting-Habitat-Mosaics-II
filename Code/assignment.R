# assignment.R
# Functions for Bayesian assignment in watershed analysis

library(sf)
library(dplyr)
library(here)

#' Perform Bayesian assignment for a given dataset
#'
#' @param natal_data Data frame with natal origins data
#' @param edges SF object with stream edges
#' @param watershed Character: "Kusko" or "Yukon"
#' @param priors List of prior values from setup_watershed_priors()
#' @param pid_iso Vector of isotope predictions
#' @param error Vector of error values
#' @param sensitivity_threshold Threshold for assignment filtering
#' @return Matrix of assignments for each data point
perform_assignment <- function(natal_data, edges, watershed, priors, 
                               pid_iso, error, sensitivity_threshold) {
  
  # Initialize assignment matrix
  assignment_matrix <- matrix(NA, nrow = length(pid_iso), ncol = nrow(natal_data))
  
  # Process each data point
  for (i in 1:nrow(natal_data)) {
    iso_o <- as.numeric(natal_data$natal_iso[i])
    
    if (watershed == "Kusko") {
      # Kusko assignment
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * priors$PresencePrior 
      
      # Make a data frame with assign and the name column from edges 
      assign_df <- data.frame(assign = assign, name = edges$Name)
      
      # Find the top 5% of values 
      top_5_percent <- quantile(assign_df$assign, 0.95, na.rm = TRUE)
      
      # filter the values above the top 5% 
      assign_df <- assign_df %>% filter(assign >= top_5_percent)
      
      # find the name with the most segments in the top 5% 
      top_name <- assign_df %>% group_by(name) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% 
        slice(1) %>% 
        pull(name)
      
      hucs<- edges$Name
      
      # find the indices of the top name in the edges data frame
      top_indices <- which(hucs == top_name)
      non_top_indices <- which(hucs != top_name)
      
      assign[non_top_indices] <- 0 
      
    } else if (watershed == "Yukon") {
      # Yukon assignment with genetic priors
      gen.prior <- rep(0, length = length(pid_iso))
      gen.prior[priors$LYsites] <- as.numeric(natal_data$Lower[i])
      gen.prior[priors$MYsites] <- as.numeric(natal_data$Middle[i])
      gen.prior[priors$UYsites] <- as.numeric(natal_data$Upper[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * gen.prior
    }
    
    # Normalize and threshold
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # Weight by CPUE
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  return(assignment_matrix)
}

#' Process assignment matrix to get basin-scale values
#'
#' @param assignment_matrix Matrix of assignments from perform_assignment()
#' @return List containing sum, rescaled, and normalized basin assignments
process_assignments <- function(assignment_matrix) {
  # Calculate basin-scale values
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
  
  return(list(
    sum = basin_assign_sum,
    rescale = basin_assign_rescale,
    norm = basin_assign_norm
  ))
}

#' Divide data into quartiles based on DOY
#'
#' @param natal_data Data frame with natal origins data
#' @return List containing quartile subsets, breaks, and labels
#' Divide data into quartiles based on DOY with CPUE distribution statistics
#'
#' @param natal_data Data frame with natal origins data
#' @return List containing quartile subsets, breaks, labels, and CPUE statistics
divide_doy_quartiles <- function(natal_data) {
  # Get full DOY range
  full_doy_range <- range(natal_data$DOY, na.rm = TRUE)
  
  # Create breaks at even intervals
  doy_breaks <- seq(full_doy_range[1], full_doy_range[2], length.out = 5)
  
  # Calculate total CPUE for the year
  total_annual_cpue <- sum(natal_data$dailyCPUEprop, na.rm = TRUE)
  
  # Create quartile subsets and calculate statistics
  quartile_subsets <- list()
  quartile_stats <- list()
  
  for (i in 1:4) {
    # Create quartile subset
    quartile_subsets[[i]] <- natal_data %>% 
      filter(DOY >= doy_breaks[i] & DOY < doy_breaks[i+1])
    
    # Calculate CPUE statistics for this quartile
    if (nrow(quartile_subsets[[i]]) > 0) {
      quartile_cpue <- sum(quartile_subsets[[i]]$dailyCPUEprop, na.rm = TRUE)
      quartile_stats[[i]] <- list(
        quartile = paste0("Q", i),
        n_days = nrow(quartile_subsets[[i]]),
        doy_start = floor(doy_breaks[i]),
        doy_end = floor(doy_breaks[i+1]),
        total_cpue = quartile_cpue,
        percent_of_annual_cpue = (quartile_cpue / total_annual_cpue) * 100,
        mean_daily_cpue = mean(quartile_subsets[[i]]$dailyCPUEprop, na.rm = TRUE),
        peak_daily_cpue = max(quartile_subsets[[i]]$dailyCPUEprop, na.rm = TRUE)
      )
    } else {
      quartile_stats[[i]] <- list(
        quartile = paste0("Q", i),
        n_days = 0,
        doy_start = floor(doy_breaks[i]),
        doy_end = floor(doy_breaks[i+1]),
        total_cpue = 0,
        percent_of_annual_cpue = 0,
        mean_daily_cpue = 0,
        peak_daily_cpue = 0
      )
    }
  }
  
  # Create labels with CPUE percentages
  subset_labels <- sapply(1:4, function(i) {
    sprintf("DOY Q%d: %d-%d (%.1f%% of annual CPUE)", 
            i, 
            quartile_stats[[i]]$doy_start, 
            quartile_stats[[i]]$doy_end,
            quartile_stats[[i]]$percent_of_annual_cpue)
  })
  
  return(list(
    subsets = quartile_subsets,
    breaks = doy_breaks,
    labels = subset_labels,
    stats = quartile_stats  # NEW: Add statistics
  ))
}

#' Divide data into quartiles based on CPUE with timing statistics
#'
#' @param natal_data Data frame with natal origins data
#' @return List containing quartile subsets, breaks, labels, and timing statistics
divide_cpue_quartiles <- function(natal_data) {
  # Calculate cumulative CPUE proportions
  natal_data <- natal_data %>% 
    arrange(DOY) %>% 
    mutate(
      CumCPUE = cumsum(dailyCPUEprop),
      TotalCPUE = sum(dailyCPUEprop),
      CumProp = CumCPUE / TotalCPUE  # Cumulative proportion of run
    )
  
  # Find DOY values at 25%, 50%, and 75% of total run
  cpue_breaks <- sapply(c(0.25, 0.5, 0.75), function(p) {
    natal_data$DOY[which.min(abs(natal_data$CumProp - p))]
  })
  
  # Add min and max DOY to breaks
  all_breaks <- c(min(natal_data$DOY), cpue_breaks, max(natal_data$DOY))
  
  # Create quartile subsets and calculate statistics
  quartile_subsets <- list()
  quartile_stats <- list()
  
  # Q1: Start to 25% of run
  quartile_subsets[[1]] <- natal_data %>% filter(DOY <= cpue_breaks[1])
  
  # Q2: 25% to 50% of run
  quartile_subsets[[2]] <- natal_data %>% filter(DOY > cpue_breaks[1] & DOY <= cpue_breaks[2])
  
  # Q3: 50% to 75% of run
  quartile_subsets[[3]] <- natal_data %>% filter(DOY > cpue_breaks[2] & DOY <= cpue_breaks[3])
  
  # Q4: 75% to end of run
  quartile_subsets[[4]] <- natal_data %>% filter(DOY > cpue_breaks[3])
  
  # Calculate statistics for each quartile
  for (i in 1:4) {
    if (nrow(quartile_subsets[[i]]) > 0) {
      doy_range <- range(quartile_subsets[[i]]$DOY)
      quartile_cpue <- sum(quartile_subsets[[i]]$dailyCPUEprop, na.rm = TRUE)
      
      quartile_stats[[i]] <- list(
        quartile = paste0("Q", i),
        n_days = nrow(quartile_subsets[[i]]),
        doy_start = min(quartile_subsets[[i]]$DOY),
        doy_end = max(quartile_subsets[[i]]$DOY),
        duration_days = doy_range[2] - doy_range[1] + 1,
        total_cpue = quartile_cpue,
        percent_of_annual_cpue = 25,  # By definition, each CPUE quartile is 25%
        mean_daily_cpue = mean(quartile_subsets[[i]]$dailyCPUEprop, na.rm = TRUE),
        peak_daily_cpue = max(quartile_subsets[[i]]$dailyCPUEprop, na.rm = TRUE)
      )
    } else {
      quartile_stats[[i]] <- list(
        quartile = paste0("Q", i),
        n_days = 0,
        doy_start = NA,
        doy_end = NA,
        duration_days = 0,
        total_cpue = 0,
        percent_of_annual_cpue = 0,
        mean_daily_cpue = 0,
        peak_daily_cpue = 0
      )
    }
  }
  
  # Create labels with timing information
  subset_labels <- sapply(1:4, function(i) {
    if (!is.na(quartile_stats[[i]]$doy_start)) {
      sprintf("CPUE Q%d: DOY %d-%d (%d days, 25%% of CPUE)", 
              i, 
              quartile_stats[[i]]$doy_start, 
              quartile_stats[[i]]$doy_end,
              quartile_stats[[i]]$duration_days)
    } else {
      sprintf("CPUE Q%d: No data", i)
    }
  })
  
  return(list(
    subsets = quartile_subsets,
    breaks = all_breaks,
    labels = subset_labels,
    stats = quartile_stats  # NEW: Add statistics
  ))
}