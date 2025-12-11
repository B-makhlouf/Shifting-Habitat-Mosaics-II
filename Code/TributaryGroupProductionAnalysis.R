################################################################################
# TRIBUTARY GROUP PRODUCTION ANALYSIS (ENHANCED)
# Analyzes production within tributary groups (focal reach + all upstream tributaries)
# 
# ENHANCED: Toggle between full year and half year (50% CPUE cutoff) production data
# 
# The focal reaches (with reachbase values 5, 6, 7) are the lowest order section.
# All upstream tributaries feed into each focal reach.
# We sum: focal reach + all tributaries upstream of it = one "group"
#
# Groups are identified by reachbase (stream order of the focal/lowest reach)
################################################################################

library(readr)
library(dplyr)
library(tidyr)

#------------------------------------------------------------------------------
# CONFIGURATION - TOGGLE HERE
#------------------------------------------------------------------------------

# CHOOSE DATA TYPE:
# "full_year" = all production data
# "half_year" = 50% CPUE cutoff data (CPUE50pct_*.csv files)
DATA_TYPE <- "half_year"  # <-- TOGGLE THIS

# Paths
UPSTREAM_RELATIONSHIPS <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/UpstreamReaches_Relationships.csv"
PROD_DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Analysis_Results"

# Years to analyze
YEARS <- c(2015, 2016,2017, 2018, 2019,2021)

# Create output directory
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
# VALIDATE DATA TYPE AND SET UP FILENAMES
#------------------------------------------------------------------------------

if (!(DATA_TYPE %in% c("full_year", "half_year"))) {
  stop("DATA_TYPE must be 'full_year' or 'half_year'")
}

# Set up filename pattern based on data type
if (DATA_TYPE == "full_year") {
  file_pattern <- "_Yukon_Assignment_Results\\.csv$"
  data_label <- "Full Year"
  output_suffix <- "FullYear"
} else if (DATA_TYPE == "half_year") {
  file_pattern <- "CPUE50pct_.*_Yukon_Assignment_Results\\.csv$"
  data_label <- "Half Year (50% CPUE Cutoff)"
  output_suffix <- "HalfYear"
}

cat("=== TRIBUTARY GROUP PRODUCTION ANALYSIS ===\n")
cat("Data Type: ", data_label, "\n")
cat("(Focal reach + all upstream tributaries per group)\n\n")

#------------------------------------------------------------------------------
# LOAD AND PREPARE UPSTREAM RELATIONSHIPS
#------------------------------------------------------------------------------

cat("Step 1: Loading upstream reach relationships...\n")

if (!file.exists(UPSTREAM_RELATIONSHIPS)) {
  stop("Upstream relationships file not found: ", UPSTREAM_RELATIONSHIPS)
}

upstream_df <- read_csv(UPSTREAM_RELATIONSHIPS, show_col_types = FALSE)

# Rename columns for clarity:
# original_reachid = focal reach (lowest order in group)
# upstream_reachid = tributary that feeds into it
# reachbase = stream order of the focal reach

upstream_df <- upstream_df %>%
  rename(focal_reach = original_reachid, 
         tributary_reach = upstream_reachid,
         stream_order = reachbase)

# Add the focal reach itself to each group
# (Each focal reach is part of its own group)
focal_reaches_only <- upstream_df %>%
  distinct(focal_reach, stream_order) %>%
  mutate(tributary_reach = focal_reach)

# Combine: focal reach + all tributaries upstream of it
groups <- bind_rows(focal_reaches_only, upstream_df) %>%
  distinct() %>%
  rename(reach_in_group = tributary_reach)

cat("  Loaded relationships\n")
cat("  Total focal reaches (groups):", n_distinct(groups$focal_reach), "\n")
cat("  Unique stream orders:", paste(sort(unique(groups$stream_order)), collapse = ", "), "\n\n")

#------------------------------------------------------------------------------
# PROCESS EACH YEAR
#------------------------------------------------------------------------------

cat("Step 2: Processing production data by year...\n")
cat("Looking for files matching pattern:", file_pattern, "\n\n")

all_results <- data.frame()
files_found <- list()

for (year in YEARS) {
  cat("  Year", year, ":\n")
  
  # Find production file for this year
  all_files <- list.files(PROD_DATA_DIR, full.names = TRUE)
  
  # Filter by year and pattern
  matching_files <- all_files[grepl(paste0(year), basename(all_files)) & 
                                grepl(file_pattern, basename(all_files))]
  
  if (length(matching_files) == 0) {
    cat("    ✗ No matching file found\n")
    next
  }
  
  if (length(matching_files) > 1) {
    cat("    ⚠ Multiple files found, using first:\n")
    for (f in matching_files) {
      cat("      -", basename(f), "\n")
    }
    prod_file <- matching_files[1]
  } else {
    prod_file <- matching_files[1]
  }
  
  cat("    ✓ File:", basename(prod_file), "\n")
  files_found[[as.character(year)]] <- basename(prod_file)
  
  # Load production data
  prod_data <- read_csv(prod_file, show_col_types = FALSE)
  
  # Validate required columns
  required_cols <- c("reachid", "assignment_rescale")
  if (!all(required_cols %in% names(prod_data))) {
    cat("    ✗ Missing required columns:", 
        paste(setdiff(required_cols, names(prod_data)), collapse = ", "), "\n")
    next
  }
  
  # Calculate total production for this year
  total_prod <- sum(prod_data$assignment_rescale, na.rm = TRUE)
  cat("    Loaded", nrow(prod_data), "reaches\n")
  cat("    Total production:", round(total_prod, 2), "\n")
  
  # Get unique focal reaches to loop through
  unique_focal <- groups %>% distinct(focal_reach, stream_order)
  
  # For each focal reach, sum production in its group
  for (i in 1:nrow(unique_focal)) {
    focal_id <- unique_focal$focal_reach[i]
    so <- unique_focal$stream_order[i]
    
    # Get all reaches in this group (focal + upstream tributaries)
    group_reaches <- groups %>%
      filter(focal_reach == focal_id) %>%
      pull(reach_in_group)
    
    # Sum production for all reaches in this group
    group_prod_sum <- sum(
      prod_data$assignment_rescale[prod_data$reachid %in% group_reaches],
      na.rm = TRUE
    )
    
    # Count reaches in group with production
    n_reaches_total <- length(group_reaches)
    n_reaches_with_prod <- sum(
      prod_data$reachid %in% group_reaches & prod_data$assignment_rescale > 0,
      na.rm = TRUE
    )
    
    # Calculate percentage
    pct_prod <- (group_prod_sum / total_prod) * 100
    
    # Add to results
    all_results <- rbind(all_results, data.frame(
      year = year,
      stream_order = so,
      focal_reach = focal_id,
      group_production = group_prod_sum,
      pct_of_total = pct_prod,
      n_reaches_in_group = n_reaches_total,
      n_reaches_with_production = n_reaches_with_prod,
      stringsAsFactors = FALSE
    ))
  }
  
  cat("\n")
}

#------------------------------------------------------------------------------
# CHECK IF WE HAVE DATA
#------------------------------------------------------------------------------

if (nrow(all_results) == 0) {
  cat("\n✗ NO DATA FOUND\n")
  cat("Please check:\n")
  cat("  1. PROD_DATA_DIR path is correct\n")
  cat("  2. Files exist in directory\n")
  cat("  3. DATA_TYPE is set correctly\n")
  cat("  4. YEARS are available in the data directory\n\n")
  
  cat("Available files in", PROD_DATA_DIR, ":\n")
  all_files <- list.files(PROD_DATA_DIR, full.names = FALSE)
  if (length(all_files) > 0) {
    for (f in all_files) {
      cat("  -", f, "\n")
    }
  } else {
    cat("  (directory is empty)\n")
  }
  
  stop("Analysis stopped - no data to process")
}

#------------------------------------------------------------------------------
# PIVOT FOR TIMESERIES: ONE ROW PER FOCAL REACH ACROSS ALL YEARS
#------------------------------------------------------------------------------

cat("Step 3: Pivoting for timeseries analysis...\n\n")

# Pivot so each focal reach has one row with columns for each year
timeseries_pivot <- all_results %>%
  select(focal_reach, stream_order, year, pct_of_total) %>%
  pivot_wider(
    names_from = year,
    values_from = pct_of_total,
    names_prefix = "Year_"
  ) %>%
  mutate(
    focal_reach = as.integer(focal_reach),
    stream_order = as.integer(stream_order)
  ) %>%
  arrange(stream_order, focal_reach)

# Also create a version with absolute production values
timeseries_prod <- all_results %>%
  select(focal_reach, stream_order, year, group_production) %>%
  pivot_wider(
    names_from = year,
    values_from = group_production,
    names_prefix = "Year_"
  ) %>%
  mutate(
    focal_reach = as.integer(focal_reach),
    stream_order = as.integer(stream_order)
  ) %>%
  arrange(stream_order, focal_reach)

cat("Timeseries pivot created:\n")
cat("  Rows:", nrow(timeseries_pivot), "(one per focal reach/group)\n")
cat("  Columns:", ncol(timeseries_pivot), "(focal_reach, stream_order, + years)\n\n")

cat("Sample of production % by group across years:\n")
print(head(timeseries_pivot))

#------------------------------------------------------------------------------
# SUMMARY STATISTICS BY STREAM ORDER
#------------------------------------------------------------------------------

cat("\n\nStep 4: Summary statistics by stream order...\n\n")

summary_by_order <- all_results %>%
  group_by(stream_order, year) %>%
  summarise(
    n_groups = n_distinct(focal_reach),
    total_pct = sum(pct_of_total),
    mean_pct_per_group = mean(pct_of_total),
    sd_pct_per_group = sd(pct_of_total),
    max_pct_group = max(pct_of_total),
    .groups = 'drop'
  )

cat("Production by stream order and year:\n")
print(summary_by_order)

#------------------------------------------------------------------------------
# EXPORT RESULTS
#------------------------------------------------------------------------------

cat("\n\nStep 5: Exporting results...\n\n")

# Create descriptive filename suffix based on data type
type_label <- ifelse(DATA_TYPE == "full_year", "", "_HalfYear")

# Export detailed long-format results (original format - easier to plot/analyze)
detailed_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_Production_LongFormat", type_label, ".csv"))
write_csv(all_results, detailed_file)
cat("✓ Long format (by year & focal reach): \n")
cat("  ", detailed_file, "\n")
cat("  Use for: plotting, stats, detailed inspection\n\n")

# Export timeseries format - one row per focal reach, columns are years (PERCENTAGE)
timeseries_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_Production_Timeseries", type_label, ".csv"))
write_csv(timeseries_pivot, timeseries_file)
cat("✓ Timeseries format (% production): \n")
cat("  ", timeseries_file, "\n")
cat("  Use for: creating timeseries plots of each group's % production\n\n")

# Export timeseries format - absolute production values
timeseries_prod_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_Production_Timeseries_Absolute", type_label, ".csv"))
write_csv(timeseries_prod, timeseries_prod_file)
cat("✓ Timeseries format (absolute):      \n")
cat("  ", timeseries_prod_file, "\n")
cat("  Use for: creating timeseries plots of each group's total production\n\n")

# Export summary statistics
summary_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_SummaryByStreamOrder", type_label, ".csv"))
write_csv(summary_by_order, summary_file)
cat("✓ Summary by stream order:            \n")
cat("  ", summary_file, "\n")
cat("  Use for: understanding patterns by stream order\n\n")

#------------------------------------------------------------------------------
# TIMESERIES PLOTS BY STREAM ORDER (Z-NORMALIZED)
#------------------------------------------------------------------------------

cat("\nStep 6: Creating timeseries plots (z-normalized)...\n\n")

# Convert to long format for plotting
plot_data <- timeseries_prod %>%
  pivot_longer(
    cols = starts_with("Year_"),
    names_to = "year",
    values_to = "production"
  ) %>%
  mutate(year = as.numeric(gsub("Year_", "", year)))

# Z-normalize each timeseries (focal reach)
plot_data <- plot_data %>%
  group_by(focal_reach) %>%
  mutate(
    production_z = (production - mean(production, na.rm = TRUE)) / sd(production, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate average z-normalized production by year
avg_by_year <- plot_data %>%
  group_by(year) %>%
  summarise(avg_production_z = mean(production_z, na.rm = TRUE), .groups = 'drop')

# Modern color palette inspired by reference
bg_color <- "#2d3a42"     # dark slate background
text_color <- "#ffffff"
grid_color <- "#4a5f67"
line_color <- "#5eb3d6"   # light teal for individual lines
trend_color <- "#1dd4d4"  # bright cyan for trend

# Create one plot per stream order
stream_orders <- sort(unique(plot_data$stream_order))

for (so in stream_orders) {
  data_subset <- plot_data %>% filter(stream_order == so)
  
  # Set up plot with dark modern styling
  par(
    bg = bg_color,
    fg = text_color,
    col.main = text_color,
    col.lab = text_color,
    col.axis = text_color,
    mar = c(5, 5, 4, 2),
    mgp = c(3, 0.8, 0),
    family = "sans",
    lwd = 1.5
  )
  
  y_range <- range(c(data_subset$production_z, avg_by_year$avg_production_z), na.rm = TRUE)
  
  plot(
    range(data_subset$year),
    y_range,
    type = "n",
    main = paste("Stream Order", so),
    xlab = "Year",
    ylab = "Z-normalized Production",
    las = 1,
    bty = "n",
    axes = FALSE,
    cex.main = 1.4,
    cex.lab = 1.1
  )
  
  # Add clean axes
  axis(1, lwd = 1.5, col = grid_color, col.ticks = grid_color, 
       col.axis = text_color, family = "sans", cex.axis = 1)
  axis(2, lwd = 1.5, col = grid_color, col.ticks = grid_color, 
       col.axis = text_color, las = 1, family = "sans", cex.axis = 1)
  
  # Add subtle horizontal gridlines only
  abline(h = axTicks(2), col = grid_color, lwd = 0.5, lty = 1)
  
  # Plot individual timeseries as subtle background lines
  focal_reaches <- sort(unique(data_subset$focal_reach))
  for (focal in focal_reaches) {
    focal_data <- data_subset %>% filter(focal_reach == focal) %>% arrange(year)
    lines(focal_data$year, focal_data$production_z, 
          col = rgb(94, 179, 214, 80, maxColorValue = 255),  # transparent teal
          lwd = 0.8, type = "l")
  }
  
  # Prominent average trend line
  trend_data <- avg_by_year %>% arrange(year)
  lines(trend_data$year, trend_data$avg_production_z, 
        type = "l", col = trend_color, lwd = 3.5)
  
  # Zero reference line
  abline(h = 0, lty = 2, col = grid_color, lwd = 1.2)
}

#------------------------------------------------------------------------------
# SUMMARY AND INSTRUCTIONS
#------------------------------------------------------------------------------

cat("\n=== COMPLETE ===\n\n")

cat("Data Type Used:", data_label, "\n\n")

cat("Files processed:\n")
for (year in YEARS) {
  if (as.character(year) %in% names(files_found)) {
    cat("  Year", year, ":", files_found[[as.character(year)]], "\n")
  }
}