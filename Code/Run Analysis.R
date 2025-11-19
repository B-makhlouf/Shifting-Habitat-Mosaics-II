################################################################################
# RUN ANALYSIS.R - AUTOMATED EXECUTION SCRIPT
################################################################################
# Automatically runs annual tributary mapping analysis and saves maps to 
# watershed-specific directories
################################################################################

# Source both files
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Assignment.R")
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
# KUSKOKWIM ANALYSIS
################################################################################

cat("=== PROCESSING KUSKOKWIM WATERSHED ===\n")

for (year in c(2017, 2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Kuskokwim", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko")
    create_annual_map(results, KUSKO_OUTPUT, year, "Kusko")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

################################################################################
# YUKON ANALYSIS
################################################################################

cat("\n=== PROCESSING YUKON WATERSHED ===\n")

for (year in c(2015,2016, 2018, 2021)) {
  cat("\n--- Yukon", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Yukon")
    create_annual_map(results, YUKON_OUTPUT, year, "Yukon")
  }, error = function(e) {
    cat("ERROR processing Yukon", year, ":", e$message, "\n")
  })
}

################################################################################
# SUMMARY
################################################################################

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Maps saved to:\n")
cat("  Kuskokwim:", KUSKO_OUTPUT, "\n")
cat("  Yukon:    ", YUKON_OUTPUT, "\n")

# List generated files
cat("\nKuskokwim maps:\n")
kusko_files <- list.files(KUSKO_OUTPUT, pattern = "*.png", full.names = FALSE)
if (length(kusko_files) > 0) {
  for (f in kusko_files) cat("  ✓", f, "\n")
} else {
  cat("  (no files found)\n")
}

cat("\nYukon maps:\n")
yukon_files <- list.files(YUKON_OUTPUT, pattern = "*.png", full.names = FALSE)
if (length(yukon_files) > 0) {
  for (f in yukon_files) cat("  ✓", f, "\n")
} else {
  cat("  (no files found)\n")
}

cat("\nDone!\n")