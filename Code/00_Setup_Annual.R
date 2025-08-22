################################################################################
# 00_SETUP_ANNUAL.R - MINIMAL SETUP FOR ANNUAL TRIBUTARY MAPS
################################################################################
# Essential configuration and functions for simple annual production mapping
# Creates total annual production maps by year - no quartiles or management units
# UPDATED: Now includes full Yukon watershed support
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

# Core configuration - different years available for each watershed
CONFIG <- list(
  # Watershed-specific available years (based on data availability)
  kusko_years = c(2017, 2018, 2019, 2020, 2021, 2022),
  yukon_years = c(2015, 2016),  # Yukon has different available years
  watersheds = c("Kusko", "Yukon")
)

# Helper function to get available years for a watershed
get_watershed_years <- function(watershed) {
  if (watershed == "Kusko") {
    return(CONFIG$kusko_years)
  } else if (watershed == "Yukon") {
    return(CONFIG$yukon_years)
  } else {
    stop("Unknown watershed: ", watershed)
  }
}

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
  
  # Spatial data paths - UPDATED for both watersheds
  kusko_edges = "/Users/benjaminmakhlouf/Spatial Data/KuskoUSGS_HUC.shp",
  kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp",
  yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
  
  # Yukon genetic data paths (needed for Yukon assignments)
  yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
  yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
  yukon_uy_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp"
)

# Watershed-specific parameters (EXACT VALUES FROM ORIGINAL)
WATERSHED_PARAMS <- list(
  Kusko = list(
    min_stream_order = 3,
    min_error = 0.0006,
    sensitivity_threshold = 0.7
  ),
  Yukon = list(
    min_stream_order = 4,  # Original uses 4, not 5!
    min_error = 0.003,     
    sensitivity_threshold = 0.8  # Original Yukon uses 0.8, not 0.7!
  )
)

################################################################################
# ESSENTIAL DATA LOADING FUNCTIONS
################################################################################

#' Load spatial data for a watershed (UPDATED for both watersheds)
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

#' Load natal origins data for a specific year and watershed (UPDATED for both watersheds)
load_natal_data <- function(year, watershed) {
  file_path <- file.path(PATHS$natal_data_dir, 
                         paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv"))
  
  if (!file.exists(file_path)) {
    stop("Natal data file not found: ", file_path)
  }
  
  natal_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Clean data based on watershed requirements (EXACT ORIGINAL LOGIC)
  if (watershed == "Yukon") {
    # Yukon requires Lower, Middle, Upper genetic columns
    clean_data <- natal_data %>%
      filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  } else {
    # Kusko only needs natal_iso and dailyCPUEprop
    clean_data <- natal_data %>%
      filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  
  return(clean_data)
}

################################################################################
# CORE CALCULATION FUNCTIONS
################################################################################

#' Calculate error values for Bayesian assignment (EXACT ORIGINAL)
calculate_error <- function(pid_isose, min_error) {
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  return(error)
}

#' Set up watershed-specific priors (UPDATED for both watersheds - EXACT ORIGINAL ALGORITHM)
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
    # YUKON PRIORS (EXACT ORIGINAL) - includes genetic data
    pid_prior <- edges$PriorSl2
    PresencePrior <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    # Load Yukon genetic groups (EXACT ORIGINAL METHOD)
    ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    ly.gen_reachid <- ly.gen$reachid
    my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
    my.gen_reachid <- my.gen$reachid
    uy.gen <- st_read(PATHS$yukon_uy_gen, quiet = TRUE)
    uy.gen_reachid <- uy.gen$reachid
    
    # Create genetic management unit assignments (EXACT ORIGINAL LOGIC)
    edges$GenLMU <- 0  # Initialize with 0, not character
    edges$GenLMU[edges$reachid %in% ly.gen_reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen_reachid] <- "middle"
    edges$GenLMU[edges$reachid %in% uy.gen_reachid] <- "upper"
    
    # Find sites for each genetic group (EXACT ORIGINAL)
    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
    UYsites <- which(edges$GenLMU == "upper")
    
    # Debug: Print genetic group sizes
    cat("  Genetic groups - Lower:", length(LYsites), "Middle:", length(MYsites), "Upper:", length(UYsites), "\n")
    
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

#' Perform Bayesian assignment for entire annual dataset (UPDATED for both watersheds - EXACT ORIGINAL ALGORITHM)
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
      
      # Debug: Check genetic values for first fish
      if (i == 1) {
        cat("  First fish genetic values - Lower:", natal_data$Lower[i], "Middle:", natal_data$Middle[i], "Upper:", natal_data$Upper[i], "\n")
        cat("  Non-zero genetic priors:", sum(gen_prior > 0), "out of", length(gen_prior), "\n")
      }
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        priors$pid_prior * priors$StreamOrderPrior * gen_prior * priors$PresencePrior * priors$NewHabitatPrior
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
cat("✓ Now supports both Kusko and Yukon watersheds.\n")
cat("✓ Available years:\n")
cat("  - Kusko:", paste(CONFIG$kusko_years, collapse = ", "), "\n")
cat("  - Yukon:", paste(CONFIG$yukon_years, collapse = ", "), "\n")
cat("✓ All outputs will be saved to: /Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II\n")
cat("✓ Input data will be read from: /Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE\n")
cat("✓ Update spatial data paths in PATHS list if needed for your system.\n")
cat("✓ Next: source('01_visualization_annual.R') and '02_annual_tributary_maps.R'\n")