################################################################################
# TRIBUTARY GROUP PRODUCTION ANALYSIS (ENHANCED)
# Analyzes production within tributary groups (focal reach + all upstream tributaries)
# 
# ENHANCED: Toggle between full year and half year (50% CPUE cutoff) production data
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(readxl)

#==============================================================================
# CONFIGURATION
#==============================================================================

DATA_TYPE <- "half_year"  # "full_year" or "half_year"

UPSTREAM_RELATIONSHIPS <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/UpstreamReaches_Relationships.csv"
PROD_DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
DATA_OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/TribGroupProdByYear/Yukon"
FIGURE_OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/UpstreamReachesbyStrOrd/Yukon/ProdByYear"

YEARS <- c(2015, 2016, 2017, 2018, 2019, 2021)

dir.create(DATA_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Validate data type
if (!(DATA_TYPE %in% c("full_year", "half_year"))) {
  stop("DATA_TYPE must be 'full_year' or 'half_year'")
}

type_label <- ifelse(DATA_TYPE == "full_year", "", "_HalfYear")
file_pattern <- ifelse(DATA_TYPE == "full_year", 
                       "_Yukon_Assignment_Results\\.csv$",
                       "CPUE50pct_.*_Yukon_Assignment_Results\\.csv$")

#==============================================================================
# LOAD DATA
#==============================================================================

if (!file.exists(UPSTREAM_RELATIONSHIPS)) {
  stop("Upstream relationships file not found: ", UPSTREAM_RELATIONSHIPS)
}

upstream_df <- read_csv(UPSTREAM_RELATIONSHIPS, show_col_types = FALSE) %>%
  rename(focal_reach = original_reachid, 
         tributary_reach = upstream_reachid,
         stream_order = reachbase)

# Create tributary groups
focal_reaches_only <- upstream_df %>%
  distinct(focal_reach, stream_order) %>%
  mutate(tributary_reach = focal_reach)

groups <- bind_rows(focal_reaches_only, upstream_df) %>%
  distinct() %>%
  rename(reach_in_group = tributary_reach)

#==============================================================================
# PROCESS EACH YEAR
#==============================================================================

all_results <- data.frame()

for (year in YEARS) {
  
  # Load basin run data
  basin_data <- read_xlsx("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx")
  basin_total_run <- basin_data %>%
    filter(River == "Yukon", Year == year) %>%
    pull(Total_Run)
  
  # Load production data
  all_files <- list.files(PROD_DATA_DIR, full.names = TRUE)
  matching_files <- all_files[grepl(paste0(year), basename(all_files)) & 
                                grepl(file_pattern, basename(all_files))]
  
  if (length(matching_files) == 0) next
  
  prod_data <- read_csv(matching_files[1], show_col_types = FALSE)
  
  if (!all(c("reachid", "assignment_individuals") %in% names(prod_data))) next
  
  # Calculate production for each tributary group
  unique_focal <- groups %>% distinct(focal_reach, stream_order)
  
  for (i in 1:nrow(unique_focal)) {
    focal_id <- unique_focal$focal_reach[i]
    so <- unique_focal$stream_order[i]
    
    group_reaches <- groups %>%
      filter(focal_reach == focal_id) %>%
      pull(reach_in_group)
    
    group_individuals <- sum(
      prod_data$assignment_individuals[prod_data$reachid %in% group_reaches],
      na.rm = TRUE
    )
    
    n_reaches_total <- length(group_reaches)
    n_reaches_with_prod <- sum(
      prod_data$reachid %in% group_reaches & prod_data$assignment_individuals > 0,
      na.rm = TRUE
    )
    
    all_results <- rbind(all_results, data.frame(
      year = year,
      stream_order = so,
      focal_reach = focal_id,
      group_individuals = group_individuals,
      n_reaches_in_group = n_reaches_total,
      n_reaches_with_production = n_reaches_with_prod,
      basin_total_run = basin_total_run,
      stringsAsFactors = FALSE
    ))
  }
}

if (nrow(all_results) == 0) {
  stop("No data found. Check paths and file availability.")
}

#==============================================================================
# EXPORT RESULTS
#==============================================================================

# Long format (detailed)
detailed_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_Individuals_LongFormat", type_label, ".csv"))
write_csv(all_results, detailed_file)

# Timeseries (wide format)
timeseries_pivot <- all_results %>%
  select(focal_reach, stream_order, year, group_individuals) %>%
  pivot_wider(
    names_from = year,
    values_from = group_individuals,
    names_prefix = "Year_"
  ) %>%
  mutate(
    focal_reach = as.integer(focal_reach),
    stream_order = as.integer(stream_order)
  ) %>%
  arrange(stream_order, focal_reach)

timeseries_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_Individuals_Timeseries", type_label, ".csv"))
write_csv(timeseries_pivot, timeseries_file)

# Summary by stream order
summary_by_order <- all_results %>%
  group_by(stream_order, year) %>%
  summarise(
    n_groups = n_distinct(focal_reach),
    total_individuals = sum(group_individuals),
    mean_individuals_per_group = mean(group_individuals),
    sd_individuals_per_group = sd(group_individuals),
    max_individuals_group = max(group_individuals),
    .groups = 'drop'
  )

summary_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_SummaryByStreamOrder", type_label, ".csv"))
write_csv(summary_by_order, summary_file)

#==============================================================================
# TIMESERIES DATA PREPARATION & CV ANALYSIS
#==============================================================================

plot_data <- timeseries_pivot %>%
  pivot_longer(
    cols = starts_with("Year_"),
    names_to = "year",
    values_to = "individuals"
  ) %>%
  mutate(year = as.numeric(gsub("Year_", "", year))) %>%
  group_by(focal_reach) %>%
  mutate(
    individuals_z = (individuals - mean(individuals, na.rm = TRUE)) / sd(individuals, na.rm = TRUE)
  ) %>%
  ungroup()

avg_by_year_z <- plot_data %>%
  group_by(year) %>%
  summarise(avg_individuals_z = mean(individuals_z, na.rm = TRUE), .groups = 'drop')

stream_orders <- sort(unique(plot_data$stream_order))

# CV by tributary group
cv_analysis <- all_results %>%
  filter(group_individuals > 0) %>%
  group_by(focal_reach, stream_order) %>%
  summarise(
    n_years_with_individuals = n(),
    mean_individuals = mean(group_individuals, na.rm = TRUE),
    sd_individuals = sd(group_individuals, na.rm = TRUE),
    cv = sd_individuals / mean_individuals,
    .groups = 'drop'
  ) %>%
  filter(!is.na(cv))

# Basin-wide CV
basin_cv <- all_results %>%
  distinct(year, basin_total_run) %>%
  summarise(
    mean_basin_run = mean(basin_total_run, na.rm = TRUE),
    sd_basin_run = sd(basin_total_run, na.rm = TRUE),
    cv_basin = sd_basin_run / mean_basin_run,
    .groups = "drop"
  ) %>%
  pull(cv_basin)

# CV summary by stream order
cv_summary <- cv_analysis %>%
  group_by(stream_order) %>%
  summarise(
    n_groups = n(),
    mean_cv = mean(cv, na.rm = TRUE),
    median_cv = median(cv, na.rm = TRUE),
    sd_cv = sd(cv, na.rm = TRUE),
    min_cv = min(cv, na.rm = TRUE),
    max_cv = max(cv, na.rm = TRUE),
    .groups = 'drop'
  )

# Export CV results
cv_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_CoefficientOfVariation", type_label, ".csv"))
write_csv(cv_analysis, cv_file)

cv_summary_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_CV_Summary", type_label, ".csv"))
write_csv(cv_summary, cv_summary_file)

#==============================================================================
# COMBINED VISUALIZATION (3 TIMESERIES + BOXPLOT)
#==============================================================================

# Prepare CV data for boxplot
cv_data_for_plot <- cv_analysis %>%
  mutate(stream_order_char = as.character(stream_order))

# Create combined figure
png_file <- file.path(FIGURE_OUTPUT_DIR, paste0("TributaryGroups_Combined_Analysis", type_label, ".png"))
png(png_file, width = 16, height = 12, units = "in", res = 300, bg = "white")

# Set up layout: Stream Orders 5, 7, 6 in rows 1-3 of column 1; Boxplot spans all rows in column 2
layout_matrix <- matrix(
  c(1, 4,
    2, 4,
    3, 4),
  nrow = 3,
  byrow = TRUE
)

layout(layout_matrix, widths = c(3, 2))

# Reorder stream orders for plotting: 5, 7, 6 (so 6 is in third row)
stream_orders_plot <- c(5, 7, 6)[c(5, 7, 6) %in% stream_orders]

# Create timeseries plots for each stream order
for (i in seq_along(stream_orders_plot)) {
  so <- stream_orders_plot[i]
  data_subset <- plot_data %>% filter(stream_order == so)
  
  par(
    bg = "#2d3a42",
    fg = "#ffffff",
    col.main = "#ffffff",
    col.lab = "#ffffff",
    col.axis = "#ffffff",
    mar = c(4, 5, 3, 1),
    mgp = c(3, 0.8, 0),
    family = "sans",
    lwd = 1.5
  )
  
  y_range <- range(c(data_subset$individuals_z, avg_by_year_z$avg_individuals_z), na.rm = TRUE)
  
  plot(
    range(data_subset$year),
    y_range,
    type = "n",
    main = paste("Stream Order", so),
    xlab = if(i == length(stream_orders)) "Year" else "",
    ylab = "Z-normalized Individuals",
    las = 1,
    bty = "n",
    axes = FALSE,
    cex.main = 1.3,
    cex.lab = 1.0
  )
  
  axis(1, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", 
       labels = if(i == 3) TRUE else FALSE)
  axis(2, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", las = 1)
  
  abline(h = axTicks(2), col = "#4a5f67", lwd = 0.5, lty = 1)
  
  # Plot individual groups
  focal_reaches <- sort(unique(data_subset$focal_reach))
  for (focal in focal_reaches) {
    focal_data <- data_subset %>% filter(focal_reach == focal) %>% arrange(year)
    lines(focal_data$year, focal_data$individuals_z, 
          col = rgb(94, 179, 214, 80, maxColorValue = 255),
          lwd = 0.8)
  }
  
  # Plot trend line
  trend_data <- avg_by_year_z %>% arrange(year)
  lines(trend_data$year, trend_data$avg_individuals_z, 
        type = "l", col = "#1dd4d4", lwd = 3.5)
  
  abline(h = 0, lty = 2, col = "#4a5f67", lwd = 1.2)
}

# Create boxplot on the right
par(
  bg = "#2d3a42",
  fg = "#ffffff",
  col.main = "#ffffff",
  col.lab = "#ffffff",
  col.axis = "#ffffff",
  mar = c(4, 4, 3, 2),
  mgp = c(3, 0.8, 0),
  family = "sans"
)

boxplot(
  cv ~ stream_order_char,
  data = cv_data_for_plot,
  names = c("SO 5", "SO 6", "SO 7"),
  main = "CV by Stream Order",
  ylab = "Coefficient of Variation",
  xlab = "",
  col = "#ff5555",
  border = "#ffffff",
  las = 1,
  cex.main = 1.2,
  cex.lab = 0.9,
  cex.axis = 0.85,
  outline = TRUE,
  pch = 19,
  col.lab = "#ffffff",
  col.axis = "#ffffff"
)

# Add basin CV reference line
abline(h = basin_cv, lty = 2, col = "white", lwd = 4)
legend("topright", legend = paste("Basin CV =", round(basin_cv, 3)), 
       lty = 2, col = "#1dd4d4", bty = "n", cex = 0.8, text.col = "#ffffff")

dev.off()

cat("✓ Saved combined figure:", basename(png_file), "\n")

#==============================================================================
# COEFFICIENT OF VARIATION ANALYSIS
#==============================================================================

# CV by tributary group
cv_analysis <- all_results %>%
  filter(group_individuals > 0) %>%
  group_by(focal_reach, stream_order) %>%
  summarise(
    n_years_with_individuals = n(),
    mean_individuals = mean(group_individuals, na.rm = TRUE),
    sd_individuals = sd(group_individuals, na.rm = TRUE),
    cv = sd_individuals / mean_individuals,
    .groups = 'drop'
  ) %>%
  filter(!is.na(cv))

# Basin-wide CV
basin_cv <- all_results %>%
  distinct(year, basin_total_run) %>%
  summarise(
    mean_basin_run = mean(basin_total_run, na.rm = TRUE),
    sd_basin_run = sd(basin_total_run, na.rm = TRUE),
    cv_basin = sd_basin_run / mean_basin_run,
    .groups = "drop"
  ) %>%
  pull(cv_basin)

# CV summary by stream order
cv_summary <- cv_analysis %>%
  group_by(stream_order) %>%
  summarise(
    n_groups = n(),
    mean_cv = mean(cv, na.rm = TRUE),
    median_cv = median(cv, na.rm = TRUE),
    sd_cv = sd(cv, na.rm = TRUE),
    min_cv = min(cv, na.rm = TRUE),
    max_cv = max(cv, na.rm = TRUE),
    .groups = 'drop'
  )

# Export CV results
cv_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_CoefficientOfVariation", type_label, ".csv"))
write_csv(cv_analysis, cv_file)

cv_summary_file <- file.path(DATA_OUTPUT_DIR, paste0("TributaryGroups_CV_Summary", type_label, ".csv"))
write_csv(cv_summary, cv_summary_file)

#==============================================================================
# SUMMARY
#==============================================================================

cat("\n=== TRIBUTARY GROUP ANALYSIS COMPLETE ===\n")
cat("Data type:", DATA_TYPE, "\n\n")
cat("Outputs saved:\n")
cat("Data files:\n")
cat("  -", basename(detailed_file), "\n")
cat("  -", basename(timeseries_file), "\n")
cat("  -", basename(summary_file), "\n")
cat("  -", basename(cv_file), "\n")
cat("  -", basename(cv_summary_file), "\n\n")
cat("Figure:\n")
cat("  -", basename(png_file), "\n\n")
cat("Directories:\n")
cat("  Data: ", DATA_OUTPUT_DIR, "\n")
cat("  Figure: ", FIGURE_OUTPUT_DIR, "\n")
cat("\nBasin-wide CV:", round(basin_cv, 4), "\n")