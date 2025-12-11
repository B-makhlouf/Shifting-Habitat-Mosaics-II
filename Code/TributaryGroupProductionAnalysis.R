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
# CONFIGURATION
#------------------------------------------------------------------------------

DATA_TYPE <- "full_year"  # "full_year" or "half_year"

UPSTREAM_RELATIONSHIPS <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/UpstreamReaches_Relationships.csv"
PROD_DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Analysis_Results"

YEARS <- c(2015, 2016, 2018, 2021)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
# SET UP FILENAMES
#------------------------------------------------------------------------------

if (!(DATA_TYPE %in% c("full_year", "half_year"))) {
  stop("DATA_TYPE must be 'full_year' or 'half_year'")
}

if (DATA_TYPE == "full_year") {
  file_pattern <- "_Yukon_Assignment_Results\\.csv$"
} else {
  file_pattern <- "CPUE50pct_.*_Yukon_Assignment_Results\\.csv$"
}

#------------------------------------------------------------------------------
# LOAD UPSTREAM RELATIONSHIPS
#------------------------------------------------------------------------------

if (!file.exists(UPSTREAM_RELATIONSHIPS)) {
  stop("Upstream relationships file not found: ", UPSTREAM_RELATIONSHIPS)
}

upstream_df <- read_csv(UPSTREAM_RELATIONSHIPS, show_col_types = FALSE)

upstream_df <- upstream_df %>%
  rename(focal_reach = original_reachid, 
         tributary_reach = upstream_reachid,
         stream_order = reachbase)

focal_reaches_only <- upstream_df %>%
  distinct(focal_reach, stream_order) %>%
  mutate(tributary_reach = focal_reach)

groups <- bind_rows(focal_reaches_only, upstream_df) %>%
  distinct() %>%
  rename(reach_in_group = tributary_reach)

#------------------------------------------------------------------------------
# PROCESS EACH YEAR
#------------------------------------------------------------------------------

all_results <- data.frame()

for (year in YEARS) {
  
  all_files <- list.files(PROD_DATA_DIR, full.names = TRUE)
  
  matching_files <- all_files[grepl(paste0(year), basename(all_files)) & 
                                grepl(file_pattern, basename(all_files))]
  
  if (length(matching_files) == 0) {
    next
  }
  
  prod_file <- matching_files[1]
  prod_data <- read_csv(prod_file, show_col_types = FALSE)
  
  required_cols <- c("reachid", "assignment_individuals")
  if (!all(required_cols %in% names(prod_data))) {
    next
  }
  
  total_individuals <- sum(prod_data$assignment_individuals, na.rm = TRUE)
  
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
      stringsAsFactors = FALSE
    ))
  }
}

#------------------------------------------------------------------------------
# CHECK FOR DATA
#------------------------------------------------------------------------------

if (nrow(all_results) == 0) {
  stop("No data found. Check paths and file availability.")
}

#------------------------------------------------------------------------------
# PIVOT FOR TIMESERIES
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# SUMMARY STATISTICS BY STREAM ORDER
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# EXPORT RESULTS
#------------------------------------------------------------------------------

type_label <- ifelse(DATA_TYPE == "full_year", "", "_HalfYear")

detailed_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_Individuals_LongFormat", type_label, ".csv"))
write_csv(all_results, detailed_file)

timeseries_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_Individuals_Timeseries", type_label, ".csv"))
write_csv(timeseries_pivot, timeseries_file)

summary_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_SummaryByStreamOrder", type_label, ".csv"))
write_csv(summary_by_order, summary_file)

#------------------------------------------------------------------------------
# TIMESERIES PLOTS BY STREAM ORDER (Z-NORMALIZED)
#------------------------------------------------------------------------------

plot_data <- timeseries_pivot %>%
  pivot_longer(
    cols = starts_with("Year_"),
    names_to = "year",
    values_to = "individuals"
  ) %>%
  mutate(year = as.numeric(gsub("Year_", "", year)))

plot_data <- plot_data %>%
  group_by(focal_reach) %>%
  mutate(
    individuals_z = (individuals - mean(individuals, na.rm = TRUE)) / sd(individuals, na.rm = TRUE)
  ) %>%
  ungroup()

avg_by_year <- plot_data %>%
  group_by(year) %>%
  summarise(avg_individuals_z = mean(individuals_z, na.rm = TRUE), .groups = 'drop')

bg_color <- "#2d3a42"
text_color <- "#ffffff"
grid_color <- "#4a5f67"
line_color <- "#5eb3d6"
trend_color <- "#1dd4d4"

stream_orders <- sort(unique(plot_data$stream_order))

for (so in stream_orders) {
  data_subset <- plot_data %>% filter(stream_order == so)
  
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
  
  y_range <- range(c(data_subset$individuals_z, avg_by_year$avg_individuals_z), na.rm = TRUE)
  
  plot(
    range(data_subset$year),
    y_range,
    type = "n",
    main = paste("Stream Order", so),
    xlab = "Year",
    ylab = "Z-normalized Individuals",
    las = 1,
    bty = "n",
    axes = FALSE,
    cex.main = 1.4,
    cex.lab = 1.1
  )
  
  axis(1, lwd = 1.5, col = grid_color, col.ticks = grid_color, 
       col.axis = text_color, family = "sans", cex.axis = 1)
  axis(2, lwd = 1.5, col = grid_color, col.ticks = grid_color, 
       col.axis = text_color, las = 1, family = "sans", cex.axis = 1)
  
  abline(h = axTicks(2), col = grid_color, lwd = 0.5, lty = 1)
  
  focal_reaches <- sort(unique(data_subset$focal_reach))
  for (focal in focal_reaches) {
    focal_data <- data_subset %>% filter(focal_reach == focal) %>% arrange(year)
    lines(focal_data$year, focal_data$individuals_z, 
          col = rgb(94, 179, 214, 80, maxColorValue = 255),
          lwd = 0.8, type = "l")
  }
  
  trend_data <- avg_by_year %>% arrange(year)
  lines(trend_data$year, trend_data$avg_individuals_z, 
        type = "l", col = trend_color, lwd = 3.5)
  
  abline(h = 0, lty = 2, col = grid_color, lwd = 1.2)
}

#------------------------------------------------------------------------------
# COEFFICIENT OF VARIATION ANALYSIS
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# BOXPLOT VISUALIZATION - CV BY STREAM ORDER
#------------------------------------------------------------------------------

bg_color <- "#ffffff"
text_color <- "#333333"
grid_color <- "#e0e0e0"
box_color <- "#ff5555"
outlier_color <- "#2c3e50"

cv_data_for_plot <- cv_analysis %>%
  mutate(stream_order_char = as.character(stream_order))

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

bp <- boxplot(
  cv ~ stream_order,
  data = cv_data_for_plot,
  names = paste("Stream Order", c(5, 6, 7)),
  main = "Coefficient of Variation by Stream Order",
  xlab = "Stream Order",
  ylab = "Coefficient of Variation",
  bty = "l",
  axes = FALSE,
  outline = TRUE,
  lwd = 1.5,
  medlwd = 3,
  whisklwd = 1.5,
  staplelwd = 1.5,
  cex.main = 1.4,
  cex.lab = 1.1,
  ylim = c(0, max(cv_data_for_plot$cv) * 1.1),
  col = box_color,
  border = outlier_color
)

axis(1, lwd = 1.5, col = text_color, col.ticks = text_color, 
     col.axis = text_color, family = "sans", cex.axis = 1, at = 1:3)
axis(2, lwd = 1.5, col = text_color, col.ticks = text_color, 
     col.axis = text_color, las = 1, family = "sans", cex.axis = 1)

abline(h = axTicks(2), col = grid_color, lwd = 0.8, lty = 1)

for (i in 1:3) {
  lines(c(i - 0.3, i + 0.3), c(bp$stats[3, i], bp$stats[3, i]), 
        col = "#000000", lwd = 4.5)
}

for (i in 1:3) {
  stream_order_val <- c(5, 6, 7)[i]
  points_data <- cv_data_for_plot %>% filter(stream_order == stream_order_val)
  
  points(
    jitter(rep(i, nrow(points_data)), amount = 0.15),
    points_data$cv,
    col = rgb(44, 62, 80, 80, maxColorValue = 255),
    pch = 16,
    cex = 1.2
  )
}

#------------------------------------------------------------------------------
# EXPORT CV RESULTS
#------------------------------------------------------------------------------

cv_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_CoefficientOfVariation", type_label, ".csv"))
write_csv(cv_analysis, cv_file)

cv_summary_file <- file.path(OUTPUT_DIR, paste0("TributaryGroups_CV_Summary", type_label, ".csv"))
write_csv(cv_summary, cv_summary_file)