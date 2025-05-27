# run_management_analysis.R
# Script to run the management unit analysis

library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(RColorBrewer)

# Source the required files
# Make sure to save the modified spatial_utils.R first!
source(here("code/utils/spatial_utils.R"))  # Modified version with management units
source(here("code/assignment.R"))
source(here("code/doy_analysis.R"))

# Source the new management analysis function
source(here("code/export_management_analysis.R"))  # Save the management export function as this file

# Run the management unit analysis for Kuskokwim watershed
cat("Starting management unit analysis for Kuskokwim watershed...\n")

# Years to analyze
years_to_analyze <- c("2017", "2019", "2020", "2021")

# Run the complete analysis
results <- run_management_analysis(
  years = years_to_analyze,
  create_plot = TRUE
)

# Print results summary
cat("\nAnalysis complete! Files created:\n")
if (!is.null(results$mgmt_file)) {
  cat("- Management unit data:", results$mgmt_file, "\n")
}
if (!is.null(results$huc_file)) {
  cat("- HUC data:", results$huc_file, "\n")
}
if (!is.null(results$trib_file)) {
  cat("- Tributary data:", results$trib_file, "\n")
}
if (!is.null(results$summary_file)) {
  cat("- Summary data:", results$summary_file, "\n")
}
if (!is.null(results$plot_file)) {
  cat("- Visualization:", results$plot_file, "\n")
}

# Preview the management unit data
if (!is.null(results$mgmt_file) && file.exists(results$mgmt_file)) {
  cat("\nPreview of management unit results:\n")
  mgmt_data <- read.csv(results$mgmt_file)
  
  # Show summary statistics
  summary_stats <- mgmt_data %>%
    group_by(mgmt_river) %>%
    summarise(
      years_present = n_distinct(year),
      total_contribution = sum(percent_of_total_run),
      avg_contribution_per_quartile = mean(percent_of_total_run),
      max_contribution = max(percent_of_total_run),
      .groups = "drop"
    ) %>%
    arrange(desc(total_contribution))
  
  print(summary_stats)
  
  # Show a sample of the detailed data
  cat("\nSample of detailed data (first 10 rows):\n")
  print(head(mgmt_data %>% 
               select(year, quartile, mgmt_river, percent_of_total_run, quartile_production_proportion), 
             10))
}

cat("\nManagement unit analysis completed successfully!\n")