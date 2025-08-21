################################################################################
# 00_SETUP_ANNUAL.R - MINIMAL SETUP FOR ANNUAL TRIBUTARY MAPS
################################################################################
# Essential configuration and functions for simple annual production mapping
# Creates total annual production maps by year - no quartiles or management units
################################################################################

# Load required libraries
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(glue)
  library(ggplot2)
  library(here)
})

################################################################################
# CONFIGURATION - UPDATE PATHS FOR YOUR SYSTEM
################################################################################

# Core configuration
CONFIG <- list(
  years = c(2017, 2018, 2019, 2020, 2021, 2022),
  watersheds = c("Kusko")  # Can add "Yukon" if needed
)

# IMPORTANT: Update BASE_DIR for your system
BASE_DIR <- "/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II"

# File paths structure
PATHS <- list(
  # Base directories
  base_dir = BASE_DIR,
  output_dir = file.path(BASE_DIR, "Analysis_Results"),
  maps_dir = file.path(BASE_DIR, "Maps"),
  
  # Data directories - using original data location
  natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
  
  # Spatial data paths
  kusko_edges = "/Users/benjaminmakhlouf/Spatial Data/KuskoUSGS_HUC_joined.shp",
  kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/YukonUSGS_HUC_joined.shp",
  yukon_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/Yukon_basin.shp"
)

# Watershed-specific parameters (EXACT VALUES FROM ORIGINAL)
WATERSHED_PARAMS <- list(
  Kusko = list(
    min_stream_order = 3,
    min_error = 0.0006,
    sensitivity_threshold = 0.7  # CRITICAL: Original uses 0.7, not 0.01!
  ),
  Yukon = list(
    min_stream_order = 4,
    min_error = 0.003,
    sensitivity_threshold = 0.7  # CRITICAL: Original uses 0.7, not 0.01!
  )
)

################################################################################
# ESSENTIAL DATA LOADING FUNCTIONS
################################################################################

#' Load spatial data for a watershed
load_spatial_data <- function(watershed) {
  params <- WATERSHED_PARAMS[[watershed]]
  
  if (watershed == "Kusko") {
    edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  } else if (watershed == "Yukon") {
    edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  } else {
    stop("Watershed must be 'Kusko' or 'Yukon'")
  }
  
  # Transform CRS and filter by stream order
  edges <- st_transform(edges, st_crs(basin))
  edges <- edges[edges$Str_Order >= params$min_stream_order, ]
  
  return(list(edges = edges, basin = basin))
}

#' Load natal origins data for a specific year and watershed
load_natal_data <- function(year, watershed) {
  file_path <- file.path(PATHS$natal_data_dir, 
                         paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv"))
  
  if (!file.exists(file_path)) {
    stop("Natal data file not found: ", file_path)
  }
  
  natal_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Clean data based on watershed requirements
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
# CORE CALCULATION FUNCTIONS
################################################################################

#' Calculate error values for Bayesian assignment
calculate_error <- function(pid_isose, min_error) {
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  return(error)
}

#' Set up watershed-specific priors (EXACT ORIGINAL ALGORITHM)
setup_priors <- function(edges, watershed, natal_data = NULL) {
  params <- WATERSHED_PARAMS[[watershed]]
  StreamOrderPrior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  
  if (watershed == "Kusko") {
    # KUSKOKWIM PRIORS (EXACT ORIGINAL)
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
    # YUKON PRIORS (EXACT ORIGINAL)
    pid_prior <- edges$PriorSl2
    PresencePrior <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    # Load Yukon genetic groups (simplified for this example)
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

#' Perform Bayesian assignment for entire annual dataset (EXACT ORIGINAL ALGORITHM)
perform_assignment <- function(natal_data, edges, watershed, priors, pid_iso, error, sensitivity_threshold) {
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(NA, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    if (watershed == "Kusko") {
      # KUSKOKWIM ASSIGNMENT: Uses isotope + spatial priors (EXACT ORIGINAL)
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * priors$PresencePrior * priors$NewHabitatPrior
      
    } else if (watershed == "Yukon") {
      # YUKON ASSIGNMENT: Uses isotope + spatial + genetic priors (EXACT ORIGINAL)
      gen_prior <- rep(0, length = length(pid_iso))
      gen_prior[priors$LYsites] <- as.numeric(natal_data$Lower[i])
      gen_prior[priors$MYsites] <- as.numeric(natal_data$Middle[i])
      gen_prior[priors$UYsites] <- as.numeric(natal_data$Upper[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * gen_prior
    }
    
    # NORMALIZE AND THRESHOLD assignments (EXACT ORIGINAL ALGORITHM)
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # Weight by CPUE (EXACT ORIGINAL)
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  return(assignment_matrix)
}

#' Create output directories
create_output_dirs <- function() {
  dir.create(PATHS$output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(PATHS$maps_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("✓ Annual tributary mapping setup complete.\n")
cat("✓ All outputs will be saved to: /Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II\n")
cat("✓ Input data will be read from: /Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE\n")
cat("✓ Update spatial data paths in PATHS list if needed for your system.\n")
cat("✓ Next: source('01_visualization_annual.R') and '02_annual_tributary_maps.R'\n")