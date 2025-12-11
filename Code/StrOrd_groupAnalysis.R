################################################################################
# TRIBUTARY GROUP PRODUCTION TIMESERIES PLOTS BY STREAM ORDER
# Separate plot for each stream order (5, 6, 7)
# Each plot shows all tributary groups within that stream order
# No legend (each group is a line)
################################################################################

library(ggplot2)
library(readr)
library(dplyr)

#------------------------------------------------------------------------------
# SETUP
#------------------------------------------------------------------------------

OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Analysis_Results"
PLOT_OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures"

# Create directories
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== TRIBUTARY GROUP PRODUCTION TIMESERIES PLOTS (BY STREAM ORDER) ===\n\n")

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------

cat("Step 1: Loading production data...\n")

detailed_file <- file.path(OUTPUT_DIR, "TributaryGroups_Production_LongFormat.csv")

if (!file.exists(detailed_file)) {
  stop("Production data not found. Run TributaryGroups_Production_Analysis.R first.")
}

prod_data <- read_csv(detailed_file, show_col_types = FALSE)

cat("  Loaded", nrow(prod_data), "rows\n")
cat("  Focal reaches:", n_distinct(prod_data$focal_reach), "\n")
cat("  Stream orders:", paste(sort(unique(prod_data$stream_order)), collapse = ", "), "\n")
cat("  Years:", paste(sort(unique(prod_data$year)), collapse = ", "), "\n\n")

#------------------------------------------------------------------------------
# SETUP COLOR PALETTE BY STREAM ORDER
#------------------------------------------------------------------------------

cat("Step 2: Setting up plots...\n\n")

# Define colors for each stream order
color_palette <- c(
  "5" = "#fee5d9",
  "6" = "#fcae91",
  "7" = "#fb6a4a"
)

# Get unique stream orders
unique_orders <- sort(unique(prod_data$stream_order))

#------------------------------------------------------------------------------
# CREATE SEPARATE PLOT FOR EACH STREAM ORDER
#------------------------------------------------------------------------------

cat("Step 3: Creating plots by stream order...\n\n")

for (order in unique_orders) {
  
  cat("  Creating plot for Stream Order", order, "...\n")
  
  # Filter data for this stream order
  order_data <- prod_data %>%
    filter(stream_order == order)
  
  n_groups <- n_distinct(order_data$focal_reach)
  
  # Get year range
  year_range <- range(order_data$year, na.rm = TRUE)
  
  # Create the plot
  p <- ggplot(order_data, aes(x = year, y = pct_of_total, 
                              group = focal_reach)) +
    
    # Line plot for each group
    geom_line(color = color_palette[as.character(order)], 
              linewidth = 0.7, alpha = 0.7) +
    geom_point(color = color_palette[as.character(order)], 
               size = 2, alpha = 0.6) +
    
    # X-axis setup
    scale_x_continuous(
      breaks = sort(unique(prod_data$year)),
      labels = sort(unique(prod_data$year))
    ) +
    
    # Y-axis setup
    scale_y_continuous(
      name = "% of Basin Production",
      limits = c(0, NA)
    ) +
    
    # Labels and theme
    labs(
      title = paste0("Stream Order ", order, " - Tributary Group Production Timeseries"),
      subtitle = paste0(n_groups, " groups (focal reaches) | Each line = one group + its upstream tributaries"),
      x = "Year",
      y = "% of Basin Production"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, color = "gray50", hjust = 0, margin = margin(b = 10)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # Save as PNG
  plot_file <- file.path(PLOT_OUTPUT_DIR, 
                         paste0("TributaryGroups_StreamOrder", order, "_Timeseries.png"))
  ggsave(plot_file, p, width = 12, height = 8, dpi = 300)
  cat("    ✓ PNG: ", basename(plot_file), "\n")
  
  # Save as PDF
  pdf_file <- file.path(PLOT_OUTPUT_DIR, 
                        paste0("TributaryGroups_StreamOrder", order, "_Timeseries.pdf"))
  ggsave(pdf_file, p, width = 12, height = 8)
  cat("    ✓ PDF: ", basename(pdf_file), "\n")
}

cat("\n")

#------------------------------------------------------------------------------
# SUMMARY STATISTICS BY STREAM ORDER
#------------------------------------------------------------------------------

cat("Step 4: Summary statistics...\n\n")

summary_stats <- prod_data %>%
  group_by(stream_order) %>%
  summarise(
    n_groups = n_distinct(focal_reach),
    min_pct = min(pct_of_total, na.rm = TRUE),
    max_pct = max(pct_of_total, na.rm = TRUE),
    mean_pct = mean(pct_of_total, na.rm = TRUE),
    median_pct = median(pct_of_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(stream_order)

print(summary_stats)

cat("\n=== COMPLETE ===\n")

cat("\nOutput files:\n")
for (order in unique_orders) {
  cat("  - TributaryGroups_StreamOrder", order, "_Timeseries.png\n")
  cat("  - TributaryGroups_StreamOrder", order, "_Timeseries.pdf\n")
}

cat("\nInterpretation:\n")
cat("- Each plot shows one stream order (focal reach order)\n")
cat("- Each line = one tributary group (focal reach + upstream tributaries)\n")
cat("- Y-axis = % of basin production that year\n")
cat("- Look for patterns: stable groups, increasing/decreasing trends\n")