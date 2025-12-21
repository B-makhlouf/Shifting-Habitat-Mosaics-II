################################################################################
# RUN ANALYSIS.R - AUTOMATED EXECUTION WITH SCENARIO-BASED DIRECTORY STRUCTURE
# FIXED: Sources complete visualization module with all functions
################################################################################

# Load required libraries first
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(RColorBrewer)
  library(scales)
  library(grid)
  library(tidyr)
})

# Source scripts in correct order
# 1. Visualization first (defines create_annual_map and histogram functions)
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/00_Visualization.R")

# 2. Assignment functions
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/00_Assignment_noCA.R")


# Define BASE output directories (without Production subfolder - that gets created by function)
BASE_KUSKO_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Kusko_Annual"
BASE_YUKON_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual"
BASE_NUSHAGAK_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Nushagak_Annual"


################################################################################
# EXAMPLE 1: FULL YEAR ANALYSIS
################################################################################

# # KUSKOKWIM FULL YEAR
# for (year in c(2017, 2018, 2019, 2020, 2021)) {
#   cat("\n--- Kuskokwim", year, "---\n")
#   tryCatch({
#     results <- run_annual_analysis(year, "Kusko")
#     create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko", filter_type = "none")
#   }, error = function(e) {
#     cat("ERROR processing Kusko", year, ":", e$message, "\n")
#   })
# }

# YUKON FULL YEAR
for (year in c(2015, 2016, 2018, 2021)) {
  cat("\n--- Yukon", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon")
    create_annual_map(results, BASE_YUKON_DIR, year, "Yukon", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}


################################################################################
# EXAMPLE 2: HALF YEAR (50% CUMULATIVE CPUE CUTOFF)
################################################################################

# YUKON - UP TO 50% CPUE
for (year in c(2015, 2016, 2018, 2021)) {
  cat("\n--- Yukon", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon", filter_type = "cpue_50_cutoff")
    create_annual_map(results, BASE_YUKON_DIR, year, "Yukon", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}



