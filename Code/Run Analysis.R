################################################################################
# RUN ANALYSIS.R - AUTOMATED EXECUTION WITH NUSHAGAK SUPPORT
# Updated to include Nushagak watershed alongside Kusko and Yukon
# Demonstrates various filter combinations for all three watersheds
################################################################################

# Source the updated assignment and visualization scripts
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Assignment_noCA.R")  # UPDATE PATH
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Visualization.R")    # UPDATE PATH

# Define output directories for all three watersheds
BASE_MAPS_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps"
KUSKO_OUTPUT <- file.path(BASE_MAPS_DIR, "Kusko_Annual")
YUKON_OUTPUT <- file.path(BASE_MAPS_DIR, "Yukon_Annual")
NUSHAGAK_OUTPUT <- file.path(BASE_MAPS_DIR, "Nushagak_Annual")  # NEW

# Create directories if they don't exist
dir.create(KUSKO_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(YUKON_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(NUSHAGAK_OUTPUT, recursive = TRUE, showWarnings = FALSE)  # NEW

cat("=== RUNNING ANNUAL TRIBUTARY MAPPING ANALYSIS ===\n")
cat("Output directories:\n")
cat("  Kuskokwim: ", KUSKO_OUTPUT, "\n")
cat("  Yukon:     ", YUKON_OUTPUT, "\n")
cat("  Nushagak:  ", NUSHAGAK_OUTPUT, "\n\n")  # NEW

################################################################################
# EXAMPLE 1: FULL YEAR ANALYSIS (ALL WATERSHEDS)
################################################################################

cat("\n=== EXAMPLE 1: FULL YEAR ANALYSIS ===\n")

# KUSKOKWIM FULL YEAR
for (year in c(2017, 2018, 2019)) {
  cat("\n--- Kuskokwim", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko")
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON FULL YEAR
for (year in c(2015, 2016, 2017, 2018)) {
  cat("\n--- Yukon", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon")
    create_annual_map(results, YUKON_OUTPUT, year, "Yukon", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

# NUSHAGAK FULL YEAR (NEW)
for (year in c(2018, 2019, 2020, 2021, 2022)) {  # TODO: Update with actual available years
  cat("\n--- Nushagak", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak")
    create_annual_map(results, NUSHAGAK_OUTPUT, year, "Nushagak", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 2: 50% CUMULATIVE CPUE CUTOFF (ALL WATERSHEDS)
################################################################################

cat("\n=== EXAMPLE 2: 50% CUMULATIVE CPUE CUTOFF ===\n")

# KUSKOKWIM - UP TO 50% CPUE
for (year in c(2017, 2018, 2019)) {
  cat("\n--- Kuskokwim", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko", filter_type = "cpue_50_cutoff")
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# YUKON - UP TO 50% CPUE
for (year in c(2015, 2016, 2017, 2018)) {
  cat("\n--- Yukon", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon", filter_type = "cpue_50_cutoff")
    create_annual_map(results, YUKON_OUTPUT, year, "Yukon", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

# NUSHAGAK - UP TO 50% CPUE (NEW)
for (year in c(2018, 2019, 2020, 2021, 2022)) {  # TODO: Update with actual available years
  cat("\n--- Nushagak", year, "(50% CPUE cutoff) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak", filter_type = "cpue_50_cutoff")
    create_annual_map(results, NUSHAGAK_OUTPUT, year, "Nushagak", filter_type = "cpue_50_cutoff")
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
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

# NUSHAGAK - TOP 50% CPUE DAYS (NEW)
for (year in c(2020, 2021)) {  # TODO: Update with actual available years
  cat("\n--- Nushagak", year, "(Top 50% CPUE days) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak", 
                                   filter_type = "cpue_percentile",
                                   cpue_lower = 50, cpue_upper = 100)
    create_annual_map(results, NUSHAGAK_OUTPUT, year, "Nushagak",
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

# NUSHAGAK - PEAK SEASON (NEW)
# TODO: Adjust DOY dates if Nushagak peak season differs from Kusko
for (year in c(2020, 2021)) {  # TODO: Update with actual available years
  cat("\n--- Nushagak", year, "(DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak",
                                   filter_type = "date_range",
                                   date_start = 160, date_end = 183)
    create_annual_map(results, NUSHAGAK_OUTPUT, year, "Nushagak",
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

# NUSHAGAK - COMBINED FILTERS (NEW)
for (year in c(2020, 2021)) {  # TODO: Update with actual available years
  cat("\n--- Nushagak", year, "(Top 50% CPUE + DOY 160-183) ---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Nushagak",
                                   filter_type = "both",
                                   cpue_lower = 50, cpue_upper = 100,
                                   date_start = 160, date_end = 183)
    create_annual_map(results, NUSHAGAK_OUTPUT, year, "Nushagak",
                      filter_type = "both",
                      cpue_lower = 50, cpue_upper = 100,
                      date_start = 160, date_end = 183)
  }, error = function(e) {
    cat("ERROR processing Nushagak", year, ":", e$message, "\n")
  })
}

################################################################################
# EXAMPLE 6: MULTI-WATERSHED COMPARISON
################################################################################

cat("\n=== EXAMPLE 6: MULTI-WATERSHED COMPARISON (SINGLE YEAR) ===\n")

# Process 2020 for all three watersheds (if data available)
comparison_year <- 2020

for (watershed in c("Kusko", "Yukon", "Nushagak")) {
  output_dir <- ifelse(watershed == "Kusko", KUSKO_OUTPUT,
                       ifelse(watershed == "Yukon", YUKON_OUTPUT, NUSHAGAK_OUTPUT))
  
  cat(paste("\n---", watershed, comparison_year, "---\n"))
  tryCatch({
    results <- run_annual_analysis(comparison_year, watershed)
    create_annual_map(results, output_dir, comparison_year, watershed, filter_type = "none")
  }, error = function(e) {
    cat(paste("Skipping", watershed, "- not available for", comparison_year, "\n"))
  })
}

################################################################################
# SUMMARY AND REPORT
################################################################################

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Maps and CSVs saved to:\n")
cat("  Kuskokwim:", KUSKO_OUTPUT, "\n")
cat("  Yukon:    ", YUKON_OUTPUT, "\n")
cat("  Nushagak: ", NUSHAGAK_OUTPUT, "\n\n")

# List generated files by watershed
for (watershed in c("Kusko", "Yukon", "Nushagak")) {
  output_dir <- ifelse(watershed == "Kusko", KUSKO_OUTPUT,
                       ifelse(watershed == "Yukon", YUKON_OUTPUT, NUSHAGAK_OUTPUT))
  
  files <- list.files(output_dir, full.names = FALSE)
  
  cat(paste0("\n", watershed, " files (", length(files), " total):\n"))
  if (length(files) > 0) {
    # Count by file type
    png_files <- sum(grepl("\\.png$", files))
    csv_files <- sum(grepl("\\.csv$", files))
    cat(paste("  PNG maps: ", png_files, "\n"))
    cat(paste("  CSV data: ", csv_files, "\n"))
    
    # Show recent files
    if (png_files > 0) {
      cat("  Recent PNG files:\n")
      png_list <- files[grepl("\\.png$", files)]
      for (f in tail(png_list, 3)) {
        cat(paste("    -", f, "\n"))
      }
    }
  } else {
    cat("  (no files found)\n")
  }
}

cat("\nDone!\n")

################################################################################
# QUICK REFERENCE FOR ADDING YOUR OWN FILTER COMBINATIONS
################################################################################

# To add a new filter combination for Nushagak (or any watershed), follow this template:
#
# for (year in c(YOUR_YEARS)) {
#   cat("\n--- WATERSHED", year, "(YOUR_FILTER_DESCRIPTION) ---\n")
#   tryCatch({
#     results <- run_annual_analysis(year, "Nushagak",
#                                    filter_type = "FILTER_TYPE",
#                                    FILTER_PARAMETERS)
#     create_annual_map(results, OUTPUT_DIR, year, "Nushagak",
#                       filter_type = "FILTER_TYPE",
#                       FILTER_PARAMETERS)
#   }, error = function(e) {
#     cat("ERROR processing Nushagak", year, ":", e$message, "\n")
#   })
# }
#
# Available filter_type options:
# - "none"             → Full annual analysis
# - "cpue_50_cutoff"   → Up to 50% cumulative CPUE
# - "cpue_percentile"  → By CPUE percentile (requires cpue_lower, cpue_upper)
# - "date_range"       → By day of year (requires date_start, date_end)
# - "both"             → Combine percentile and date range (requires all four params)

################################################################################
# TROUBLESHOOTING NOTES FOR NUSHAGAK
################################################################################

# If Nushagak analyses fail:
# 1. Check that data files exist in correct directory:
#    /Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/.../
#    Should have: YYYY_Nushagak_Natal_Origins_Genetics_CPUE.csv
#
# 2. Verify shapefile paths are correct (all component files: .shp, .shx, .dbf, .prj)
#
# 3. Check that PARAMS and PATHS are updated in Assignment_noCA.R
#
# 4. Run a single year test first:
#    results <- run_annual_analysis(2020, "Nushagak", verbose = TRUE)
#    This will show detailed output for debugging
#
# 5. If columns not found, inspect the data:
#    nushagak_data <- read_csv("path_to_file.csv")
#    names(nushagak_data)

print("✓ Run Analysis.R with Nushagak support loaded")
print("Uncomment sections above to run analyses")