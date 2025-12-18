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

# Verify functions are loaded
if (!exists("create_annual_map")) {
  stop("ERROR: create_annual_map function not found after sourcing!")
}
if (!exists("create_cpue_histogram_genetic")) {
  stop("ERROR: create_cpue_histogram_genetic function not found after sourcing!")
}
if (!exists("run_annual_analysis")) {
  stop("ERROR: run_annual_analysis function not found after sourcing!")
}

cat("✓ All functions loaded successfully\n\n")

# Define BASE output directories (without Production subfolder - that gets created by function)
BASE_KUSKO_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Kusko_Annual"
BASE_YUKON_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual"
BASE_NUSHAGAK_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Nushagak_Annual"

cat("=== RUNNING ANNUAL TRIBUTARY MAPPING ANALYSIS ===\n")
cat("Maps will be organized by scenario:\n")
cat("  Kuskokwim: ", BASE_KUSKO_DIR, "/Production/{scenario}/\n", sep = "")
cat("  Yukon:     ", BASE_YUKON_DIR, "/Production/{scenario}/\n", sep = "")
cat("  Nushagak:  ", BASE_NUSHAGAK_DIR, "/Production/{scenario}/\n\n", sep = "")

################################################################################
# EXAMPLE 1: FULL YEAR ANALYSIS (ALL WATERSHEDS)
################################################################################

cat("\n=== EXAMPLE 1: FULL YEAR ANALYSIS ===\n")

# KUSKOKWIM FULL YEAR
for (year in c(2017, 2018, 2019, 2020, 2021)) {
  cat("\n--- Kuskokwim", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko")
    create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

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

# NUSHAGAK FULL YEAR
for (year in c(2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Nushagak", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak")
    create_annual_map(results, BASE_NUSHAGAK_DIR, year, "Nushagak", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 2: HALF YEAR (50% CUMULATIVE CPUE CUTOFF)
################################################################################

cat("\n=== EXAMPLE 2: HALF YEAR (50% CUMULATIVE CPUE CUTOFF) ===\n")

# KUSKOKWIM - UP TO 50% CPUE
for (year in c(2017, 2018, 2019, 2020, 2021)) {
  cat("\n--- Kuskokwim", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko", filter_type = "cpue_50_cutoff")
    create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

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

# NUSHAGAK - UP TO 50% CPUE
for (year in c(2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Nushagak", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak", filter_type = "cpue_50_cutoff")
    create_annual_map(results, BASE_NUSHAGAK_DIR, year, "Nushagak", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 3: CPUE PERCENTILE FILTERING (TOP 50%)
################################################################################

cat("\n=== EXAMPLE 3: CPUE PERCENTILE (TOP 50%) ===\n")

# KUSKOKWIM - TOP 50% CPUE DAYS
for (year in c(2017, 2018, 2019, 2020, 2021)) {
  cat("\n--- Kuskokwim", year, "(Top 50% CPUE days) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko", 
                                   filter_type = "cpue_percentile",
                                   cpue_lower = 50, cpue_upper = 100)
    create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko",
                      filter_type = "cpue_percentile",
                      cpue_lower = 50, cpue_upper = 100)
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON - TOP 50% CPUE DAYS
for (year in c(2015, 2016, 2018, 2021)) {
  cat("\n--- Yukon", year, "(Top 50% CPUE days) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon",
                                   filter_type = "cpue_percentile",
                                   cpue_lower = 50, cpue_upper = 100)
    create_annual_map(results, BASE_YUKON_DIR, year, "Yukon",
                      filter_type = "cpue_percentile",
                      cpue_lower = 50, cpue_upper = 100)
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

# NUSHAGAK - TOP 50% CPUE DAYS
for (year in c(2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Nushagak", year, "(Top 50% CPUE days) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak",
                                   filter_type = "cpue_percentile",
                                   cpue_lower = 50, cpue_upper = 100)
    create_annual_map(results, BASE_NUSHAGAK_DIR, year, "Nushagak",
                      filter_type = "cpue_percentile",
                      cpue_lower = 50, cpue_upper = 100)
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 4: DATE RANGE FILTERING (PEAK SEASON)
################################################################################

cat("\n=== EXAMPLE 4: DATE RANGE (PEAK SEASON DOY 160-183) ===\n")

# KUSKOKWIM - PEAK SEASON ONLY
for (year in c(2017, 2018, 2019, 2020, 2021)) {
  cat("\n--- Kuskokwim", year, "(DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko",
                                   filter_type = "date_range",
                                   date_start = 160, date_end = 183)
    create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko",
                      filter_type = "date_range",
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON - PEAK SEASON
for (year in c(2015, 2016, 2018, 2021)) {
  cat("\n--- Yukon", year, "(DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon",
                                   filter_type = "date_range",
                                   date_start = 160, date_end = 183)
    create_annual_map(results, BASE_YUKON_DIR, year, "Yukon",
                      filter_type = "date_range",
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

# NUSHAGAK - PEAK SEASON
for (year in c(2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Nushagak", year, "(DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak",
                                   filter_type = "date_range",
                                   date_start = 160, date_end = 183)
    create_annual_map(results, BASE_NUSHAGAK_DIR, year, "Nushagak",
                      filter_type = "date_range",
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 5: COMBINED FILTERS (TOP 50% CPUE + DATE RANGE)
################################################################################

cat("\n=== EXAMPLE 5: COMBINED FILTERS (TOP 50% CPUE + DOY 160-183) ===\n")

# KUSKOKWIM - TOP 50% CPUE DURING PEAK SEASON
for (year in c(2017, 2018, 2019, 2020, 2021)) {
  cat("\n--- Kuskokwim", year, "(Top 50% CPUE + DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko",
                                   filter_type = "both",
                                   cpue_lower = 50, cpue_upper = 100,
                                   date_start = 160, date_end = 183)
    create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko",
                      filter_type = "both",
                      cpue_lower = 50, cpue_upper = 100,
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON - COMBINED FILTERS
for (year in c(2015, 2016, 2018, 2021)) {
  cat("\n--- Yukon", year, "(Top 50% CPUE + DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon",
                                   filter_type = "both",
                                   cpue_lower = 50, cpue_upper = 100,
                                   date_start = 160, date_end = 183)
    create_annual_map(results, BASE_YUKON_DIR, year, "Yukon",
                      filter_type = "both",
                      cpue_lower = 50, cpue_upper = 100,
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

# NUSHAGAK - COMBINED FILTERS
for (year in c(2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Nushagak", year, "(Top 50% CPUE + DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak",
                                   filter_type = "both",
                                   cpue_lower = 50, cpue_upper = 100,
                                   date_start = 160, date_end = 183)
    create_annual_map(results, BASE_NUSHAGAK_DIR, year, "Nushagak",
                      filter_type = "both",
                      cpue_lower = 50, cpue_upper = 100,
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# SUMMARY AND REPORT
################################################################################

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Maps organized by scenario in Production/ subdirectories:\n\n")

# Function to summarize maps by watershed
summarize_maps <- function(base_dir, watershed_name) {
  prod_dir <- file.path(base_dir, "Production")
  if (dir.exists(prod_dir)) {
    scenarios <- list.dirs(prod_dir, recursive = FALSE, full.names = FALSE)
    cat(paste0("\n", watershed_name, " - ", length(scenarios), " scenario(s):\n"))
    for (scenario in scenarios) {
      maps <- list.files(file.path(prod_dir, scenario), pattern = "\\.png$")
      cat(paste0("  ", scenario, ": ", length(maps), " maps\n"))
    }
  }
}

summarize_maps(BASE_KUSKO_DIR, "Kuskokwim")
summarize_maps(BASE_YUKON_DIR, "Yukon")
summarize_maps(BASE_NUSHAGAK_DIR, "Nushagak")

cat("\nBase directories:\n")
cat(paste0("  Kuskokwim: ", BASE_KUSKO_DIR, "\n"))
cat(paste0("  Yukon:     ", BASE_YUKON_DIR, "\n"))
cat(paste0("  Nushagak:  ", BASE_NUSHAGAK_DIR, "\n"))

cat("\nDone!\n")