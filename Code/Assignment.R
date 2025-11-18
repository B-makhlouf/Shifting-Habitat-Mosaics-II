################################################################################
# ASSIGNMENT.R - ALL BAYESIAN ASSIGNMENT LOGIC
################################################################################
# Contains all functions for performing Bayesian assignment of salmon to 
# natal tributaries based on isotopes, genetics, and spatial priors
# ANNUAL ANALYSIS ONLY
# CORRECTED: Paths updated for repository 05
# MODIFIED: Added CSV export functionality for assignment results
################################################################################

library(sf)
library(dplyr)
library(readr)

################################################################################
# CONFIGURATION
################################################################################

# File paths - UPDATE THESE FOR YOUR SYSTEM
PATHS <- list(
  # Kuskokwim data
  kusko_edges = "/Users/benjaminmakhlouf/Spatial Data/KuskoUSGS_HUC.shp",
  kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  
  # Yukon data
  yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp",
  yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
  yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
  yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
  yukon_uy_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp",
  
  # Natal origins data
  natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
  
  # Output directories
  output_kusko = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko",
  output_yukon = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
)

# Watershed parameters
PARAMS <- list(
  Kusko = list(
    min_stream_order = 3,
    min_error = 0.0006,
    sensitivity_threshold = 0.01
  ),
  Yukon = list(
    min_stream_order = 4,
    min_error = 0.003,
    sensitivity_threshold = 0.01
  )
)

################################################################################
# DATA LOADING FUNCTIONS
################################################################################

#' Load spatial data for a watershed
load_spatial_data <- function(watershed, min_stream_order = NULL) {
  
  if (is.null(min_stream_order)) {
    min_stream_order <- PARAMS[[watershed]]$min_stream_order
  }
  
  if (watershed == "Kusko") {
    edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  } else if (watershed == "Yukon") {
    edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  } else {
    stop("Watershed must be 'Kusko' or 'Yukon'")
  }
  
  # Transform and filter
  edges <- st_transform(edges, st_crs(basin))
  edges <- edges[edges$Str_Order >= min_stream_order, ]
  
  return(list(edges = edges, basin = basin))
}

#' Load natal origins data
load_natal_data <- function(year, watershed) {
  file_path <- file.path(PATHS$natal_data_dir, 
                         paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv"))
  
  if (!file.exists(file_path)) {
    stop("Natal data file not found: ", file_path)
  }
  
  natal_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Clean data based on watershed
  if (watershed == "Yukon") {
    clean_data <- natal_data %>%
      filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  } else {
    clean_data <- natal_data %>%
      filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  
  return(clean_data)
}

################################################################################
# ERROR CALCULATION
################################################################################

#' Calculate error values for Bayesian assignment
calculate_error <- function(pid_isose, min_error) {
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  return(error)
}

################################################################################
# PRIOR SETUP
################################################################################

#' Set up watershed-specific priors
setup_priors <- function(edges, watershed, natal_data = NULL, min_stream_order = NULL) {
  
  if (is.null(min_stream_order)) {
    min_stream_order <- PARAMS[[watershed]]$min_stream_order
  }
  
  StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
  
  if (watershed == "Kusko") {
    # KUSKOKWIM PRIORS
    pid_prior <- edges$UniPh2oNoE
    PresencePrior <- ifelse((edges$Str_Order %in% c(6, 7, 8)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    return(list(
      pid_prior = pid_prior,
      StreamOrderPrior = StreamOrderPrior,
      PresencePrior = PresencePrior,
      NewHabitatPrior = NewHabitatPrior
    ))
    
  } else if (watershed == "Yukon") {
    # YUKON PRIORS (includes genetic data)
    pid_prior <- edges$PriorSl2
    PresencePrior <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    # Load genetic groups
    ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
    uy.gen <- st_read(PATHS$yukon_uy_gen, quiet = TRUE)
    
    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
    edges$GenLMU[edges$reachid %in% uy.gen$reachid] <- "upper"
    
    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
    UYsites <- which(edges$GenLMU == "upper")
    
    return(list(
      pid_prior = pid_prior,
      StreamOrderPrior = StreamOrderPrior,
      PresencePrior = PresencePrior,
      NewHabitatPrior = NewHabitatPrior,
      LYsites = LYsites,
      MYsites = MYsites,
      UYsites = UYsites
    ))
  }
}

################################################################################
# CORE BAYESIAN ASSIGNMENT
################################################################################

#' Perform Bayesian assignment of fish to tributaries
perform_assignment <- function(natal_data, edges, watershed, priors, 
                               pid_iso, error, sensitivity_threshold = NULL) {
  
  if (is.null(sensitivity_threshold)) {
    sensitivity_threshold <- PARAMS[[watershed]]$sensitivity_threshold
  }
  
  n_basins <- length(pid_iso)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(NA, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    if (watershed == "Kusko") {
      # KUSKOKWIM ASSIGNMENT
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * priors$PresencePrior * priors$NewHabitatPrior
      
    } else if (watershed == "Yukon") {
      # YUKON ASSIGNMENT (includes genetics)
      gen_prior <- rep(0, length = length(pid_iso))
      gen_prior[priors$LYsites] <- as.numeric(natal_data$Lower[i])
      gen_prior[priors$MYsites] <- as.numeric(natal_data$Middle[i])
      gen_prior[priors$UYsites] <- as.numeric(natal_data$Upper[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * gen_prior * 
        priors$PresencePrior * priors$NewHabitatPrior
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

################################################################################
# POST-PROCESSING
################################################################################

#' Process assignment matrix to get basin-scale values
process_assignments <- function(assignment_matrix) {
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
  
  return(list(
    sum = basin_assign_sum,
    rescale = basin_assign_rescale,
    norm = basin_assign_norm
  ))
}

################################################################################
# EXPORT FUNCTIONS
################################################################################

#' Export assignment results to CSV
export_results <- function(edges, basin_results, year, watershed) {
  
  # Determine output directory
  output_dir <- if (watershed == "Kusko") PATHS$output_kusko else PATHS$output_yukon
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(paste("  Created output directory:", output_dir, "\n"))
  }
  
  # Prepare data frame with spatial and assignment data
  # Drop geometry to make it a regular data frame
  edges_df <- edges
  if ("sf" %in% class(edges_df)) {
    edges_df <- st_drop_geometry(edges_df)
  }
  
  # Create output data frame
  output_data <- data.frame(
    reachid = edges_df$reachid,
    Str_Order = edges_df$Str_Order,
    iso_pred = edges_df$iso_pred,
    assignment_sum = basin_results$sum,
    assignment_rescale = basin_results$rescale,
    assignment_norm = basin_results$norm
  )
  
  # Add genetic information for Yukon
  if (watershed == "Yukon" && "GenLMU" %in% names(edges_df)) {
    output_data$GenLMU <- edges_df$GenLMU
  }
  
  # Create filename
  filename <- paste0(year, "_", watershed, "_Assignment_Results.csv")
  filepath <- file.path(output_dir, filename)
  
  # Write CSV
  write_csv(output_data, filepath)
  
  cat(paste("  ✓ Results exported to:", filepath, "\n"))
  
  # Print summary statistics
  cat(paste("  Summary:\n"))
  cat(paste("    - Total segments:", nrow(output_data), "\n"))
  cat(paste("    - Sum of rescaled assignments:", round(sum(basin_results$rescale), 4), "\n"))
  cat(paste("    - Max normalized assignment:", round(max(basin_results$norm), 4), "\n"))
  cat(paste("    - Segments with assignment > 0:", sum(basin_results$sum > 0), "\n"))
  
  return(filepath)
}

################################################################################
# HIGH-LEVEL WRAPPER FUNCTION
################################################################################

#' Run complete annual analysis for a single year and watershed
run_annual_analysis <- function(year, watershed) {
  cat(paste("\nProcessing", watershed, year, "...\n"))
  
  # Load data
  spatial_data <- load_spatial_data(watershed)
  natal_data <- load_natal_data(year, watershed)
  
  cat(paste("  Loaded", nrow(natal_data), "fish observations\n"))
  cat(paste("  Loaded", nrow(spatial_data$edges), "stream segments\n"))
  
  # Setup
  pid_iso <- spatial_data$edges$iso_pred
  pid_isose <- spatial_data$edges$isose_pred
  error <- calculate_error(pid_isose, PARAMS[[watershed]]$min_error)
  priors <- setup_priors(spatial_data$edges, watershed, natal_data)
  
  # Assignment
  cat("  Performing Bayesian assignment...\n")
  assignment_matrix <- perform_assignment(natal_data, spatial_data$edges, watershed, 
                                          priors, pid_iso, error)
  
  # Process results
  basin_results <- process_assignments(assignment_matrix)
  
  cat(paste("  Total annual production:", round(sum(basin_results$sum), 2), "\n"))
  
  # Export results (automatic)
  export_filepath <- export_results(spatial_data$edges, basin_results, year, watershed)
  
  cat("  ✓ Analysis complete\n")
  
  return(list(
    spatial_data = spatial_data,
    natal_data = natal_data,
    basin_results = basin_results,
    priors = priors,
    export_filepath = export_filepath
  ))
}

################################################################################
# BATCH PROCESSING FUNCTION
################################################################################

#' Run analysis for multiple years and watersheds
run_batch_analysis <- function(years, watersheds = c("Kusko", "Yukon")) {
  cat("\n================================================================================\n")
  cat("BATCH PROCESSING: Assignment Analysis\n")
  cat("================================================================================\n")
  
  results <- list()
  
  for (watershed in watersheds) {
    for (year in years) {
      tryCatch({
        key <- paste(year, watershed, sep = "_")
        results[[key]] <- run_annual_analysis(year, watershed)
      }, error = function(e) {
        cat(paste("  ✗ Error processing", watershed, year, ":", e$message, "\n"))
      })
    }
  }
  
  cat("\n================================================================================\n")
  cat("BATCH PROCESSING COMPLETE\n")
  cat(paste("Successfully processed:", length(results), "datasets\n"))
  cat("================================================================================\n")
  
  return(results)
}

################################################################################
# INITIALIZATION MESSAGE
################################################################################

cat("✓ Assignment.R loaded successfully\n")
cat("\nMain functions:\n")
cat("  - run_annual_analysis(year, watershed) - automatically exports results\n")
cat("  - run_batch_analysis(years, watersheds = c('Kusko', 'Yukon'))\n")
cat("  - export_results(edges, basin_results, year, watershed) - manual export\n")
cat("\nExample usage:\n")
cat("  # Single year analysis (auto-exports):\n")
cat("  results <- run_annual_analysis(2017, 'Kusko')\n\n")
cat("  # Batch processing (auto-exports all):\n")
cat("  all_results <- run_batch_analysis(2015:2021)\n\n")

results<- run_annual_analysis(2017, 'Kusko')
