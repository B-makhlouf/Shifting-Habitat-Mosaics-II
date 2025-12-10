################################################################################
# RUN ANALYSIS.R - AUTOMATED EXECUTION WITH FILTER TYPE PARAMETER
# Updated to use matching filename conventions for both CSVs and maps
################################################################################
# This script demonstrates how to use the new filter_type parameter with
# both run_annual_analysis() and create_annual_map() to generate consistent
# filenames for CSVs and PNGs
################################################################################

# Source both files
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Assignment_noCA.R")
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Visualization.R")

# Define output directories
BASE_MAPS_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps"
KUSKO_OUTPUT <- file.path(BASE_MAPS_DIR, "Kusko_Annual")
YUKON_OUTPUT <- file.path(BASE_MAPS_DIR, "Yukon_Annual")

# Create directories if they don't exist
dir.create(KUSKO_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(YUKON_OUTPUT, recursive = TRUE, showWarnings = FALSE)

cat("=== RUNNING ANNUAL TRIBUTARY MAPPING ANALYSIS ===\n")
cat("Output directories:\n")
cat("  Kuskokwim:", KUSKO_OUTPUT, "\n")
cat("  Yukon:    ", YUKON_OUTPUT, "\n\n")

################################################################################
# EXAMPLE 1: FULL YEAR ANALYSIS (BOTH WATERSHEDS)
################################################################################

cat("\n=== EXAMPLE 1: FULL YEAR ANALYSIS ===\n")

# KUSKOKWIM FULL YEAR
for (year in c(2017, 2018)) {
  cat("\n--- Kuskokwim", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko")
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON FULL YEAR
# for (year in c(2015, 2016)) {
#   cat("\n--- Yukon", year, "---\n")
#   tryCatch({
#     results <- run_annual_analysis(year, "Yukon")
#     create_annual_map(results, YUKON_OUTPUT, year, "Yukon", filter_type = "none")
#   }, error = function(e) {
#     cat("ERROR processing Yukon", year, ":", e$message, "\n")
#   })
# }

################################################################################
# EXAMPLE 2: 50% CUMULATIVE CPUE CUTOFF
################################################################################

cat("\n=== EXAMPLE 2: 50% CUMULATIVE CPUE CUTOFF ===\n")

# KUSKOKWIM - UP TO 50% CPUE
for (year in c(2017, 2018)) {
  cat("\n--- Kuskokwim", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko", filter_type = "cpue_50_cutoff")
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON - UP TO 50% CPUE
for (year in c(2015, 2016, 2017, 2018, 2019, 2021)) {
  cat("\n--- Yukon", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon", filter_type = "cpue_50_cutoff")
    create_annual_map(results, YUKON_OUTPUT, year, "Yukon", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 3: CPUE PERCENTILE FILTERING (TOP 50%)
################################################################################

cat("\n=== EXAMPLE 3: CPUE PERCENTILE (TOP 50%) ===\n")

# KUSKOKWIM - TOP 50% CPUE DAYS
for (year in c(2017, 2018)) {
  cat("\n--- Kuskokwim", year, "(Top 50% CPUE days) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko", 
                                   filter_type = "cpue_percentile",
                                   cpue_lower = 50, cpue_upper = 100)
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko",
                      filter_type = "cpue_percentile",
                      cpue_lower = 50, cpue_upper = 100)
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 4: DATE RANGE FILTERING (PEAK SEASON)
################################################################################

cat("\n=== EXAMPLE 4: DATE RANGE (PEAK SEASON DOY 160-183) ===\n")

# KUSKOKWIM - PEAK SEASON ONLY
for (year in c(2017, 2018)) {
  cat("\n--- Kuskokwim", year, "(DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko",
                                   filter_type = "date_range",
                                   date_start = 160, date_end = 183)
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko",
                      filter_type = "date_range",
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 5: COMBINED FILTERS (TOP 50% CPUE + DATE RANGE)
################################################################################

cat("\n=== EXAMPLE 5: COMBINED FILTERS (TOP 50% CPUE + DOY 160-183) ===\n")

# KUSKOKWIM - TOP 50% CPUE DURING PEAK SEASON
for (year in c(2017, 2018)) {
  cat("\n--- Kuskokwim", year, "(Top 50% CPUE + DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko",
                                   filter_type = "both",
                                   cpue_lower = 50, cpue_upper = 100,
                                   date_start = 160, date_end = 183)
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko",
                      filter_type = "both",
                      cpue_lower = 50, cpue_upper = 100,
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

################################################################################
# SUMMARY
################################################################################

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Maps and CSVs saved to:\n")
cat("  Kuskokwim:", KUSKO_OUTPUT, "\n")
cat("  Yukon:    ", YUKON_OUTPUT, "\n\n")

# List generated files
cat("Kuskokwim files:\n")
kusko_files <- list.files(KUSKO_OUTPUT, full.names = FALSE)
if (length(kusko_files) > 0) {
  for (f in kusko_files) cat("  ✓", f, "\n")
} else {
  cat("  (no files found)\n")
}

cat("\nYukon files:\n")
yukon_files <- list.files(YUKON_OUTPUT, full.names = FALSE)
if (length(yukon_files) > 0) {
  for (f in yukon_files) cat("  ✓", f, "\n")
} else {
  cat("  (no files found)\n")
}

cat("\nDone!\n")

################################################################################
# QUICK REFERENCE FOR ADDING YOUR OWN FILTER COMBINATIONS
################################################################################

# To add a new filter combination, follow this template:
#
# for (year in c(YOUR_YEARS)) {
#   cat("\n--- WATERSHED", year, "(YOUR_FILTER_DESCRIPTION) ---\n")
#   tryCatch({
#     results <- run_annual_analysis(year, "WATERSHED",
#                                    filter_type = "FILTER_TYPE",
#                                    FILTER_PARAMETERS)
#     create_annual_map(results, OUTPUT_DIR, year, "WATERSHED",
#                       filter_type = "FILTER_TYPE",
#                       FILTER_PARAMETERS)
#   }, error = function(e) {
#     cat("ERROR processing WATERSHED", year, ":", e$message, "\n")
#   })
# }
#
# Available filter_type options:
# - "none"             → Full year analysis
# - "cpue_50_cutoff"   → Up to 50% cumulative CPUE
# - "cpue_percentile"  → By CPUE percentile (requires cpue_lower, cpue_upper)
# - "date_range"       → By day of year (requires date_start, date_end)
# - "both"             → Combine percentile and date range (requires all four params)