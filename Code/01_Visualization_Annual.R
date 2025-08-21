################################################################################
# 01_VISUALIZATION_ANNUAL.R - VISUALIZATION FUNCTIONS FOR ANNUAL TRIBUTARY MAPS
################################################################################
# Visualization functions matching original tributary map style exactly
# Uses base R plotting system with YlOrRd palette and specific formatting
################################################################################

# Check if setup is loaded
if (!exists("CONFIG")) {
  stop("Please run source('00_setup_annual.R') first")
}

# Load required libraries for visualization
suppressPackageStartupMessages({
  library(RColorBrewer)
  library(grid)
})

################################################################################
# ANNUAL TRIBUTARY MAP VISUALIZATION FUNCTIONS
################################################################################

#' Create DOY histogram for tributary maps (matching original style)
create_doy_histogram <- function(full_dataset, current_subset, title = NULL) {
  if (is.null(title)) title <- "Annual DOY Distribution"
  
  highlight_color <- "tomato"
  background_color <- "gray70"
  
  doy_to_date <- function(doy, year = 2024) {
    as.Date(doy - 1, origin = paste0(year, "-01-01"))
  }
  
  doy_breaks <- seq(140, 210, by = 10)
  
  ggplot() + 
    # Background curve (full year)
    geom_line(data = full_dataset, aes(x = DOY, y = dailyCPUEprop), 
              color = "gray40", linewidth = 1, alpha = 0.5) +
    geom_ribbon(data = full_dataset, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = background_color, alpha = 0.3) +
    
    # Highlighted annual data (should be same as full_dataset for annual maps)
    geom_line(data = current_subset, aes(x = DOY, y = dailyCPUEprop), 
              color = "black", linewidth = 2) +
    geom_ribbon(data = current_subset, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = highlight_color, alpha = 0.7) +
    
    scale_x_continuous(
      limits = c(140, 210),
      breaks = doy_breaks,
      labels = function(x) paste0(x, "\n", format(doy_to_date(x), "%b %d"))
    ) +
    scale_y_continuous(limits = c(0, 0.1)) +
    coord_cartesian(xlim = c(140, 210), ylim = c(0, 0.1), expand = FALSE) +
    
    labs(
      title = title,
      subtitle = "Annual distribution",
      x = "Day of Year (Date)", 
      y = "Daily CPUE Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
}

#' Create tributary map (matching original style exactly)
create_annual_tributary_map <- function(edges, basin, year, watershed, 
                                        map_filename, basin_assign_sum, natal_data = NULL) {
  
  # Calculate normalized values exactly like original
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
  
  # Create histogram if natal data provided
  gg_hist <- NULL
  if (!is.null(natal_data)) {
    gg_hist <- create_doy_histogram(natal_data, natal_data, 
                                    paste("Annual Distribution", year))
  }
  
  # Open PNG file
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  # Use YlOrRd palette with 9 colors expanded to 10 (matching original exactly)
  pallete <- brewer.pal(9, "YlOrRd")
  pallete_expanded <- colorRampPalette(pallete)(10)
  
  # Color coding with bins at every 0.1 (matching original exactly)
  colcode <- rep("gray60", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'white'
  colcode[basin_assign_norm > 0 & basin_assign_norm <= 0.1] <- pallete_expanded[1]
  colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- pallete_expanded[2]
  colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- pallete_expanded[3]
  colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- pallete_expanded[4]
  colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- pallete_expanded[5]
  colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- pallete_expanded[6]
  colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- pallete_expanded[7]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- pallete_expanded[8]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- pallete_expanded[9]
  colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- pallete_expanded[10]
  
  # Set linewidths based on stream order (matching original exactly)
  stream_order_lwd <- edges$Str_Order
  linewidths <- rep(1, length(stream_order_lwd))
  
  if (watershed == "Yukon") {
    linewidths <- ifelse(stream_order_lwd == 9, 3.7, linewidths)
    linewidths <- ifelse(stream_order_lwd == 8, 2.5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 7, 1.7, linewidths)
    linewidths <- ifelse(stream_order_lwd == 6, 1.5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 5, 1, linewidths)
    linewidths <- ifelse(stream_order_lwd == 4, 1, linewidths)
    linewidths <- ifelse(stream_order_lwd == 3, 1, linewidths)
  } else {
    # Kuskokwim linewidths
    linewidths <- ifelse(stream_order_lwd == 9, 5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 8, 4, linewidths)
    linewidths <- ifelse(stream_order_lwd == 7, 3, linewidths)
    linewidths <- ifelse(stream_order_lwd == 6, 2, linewidths)
    linewidths <- ifelse(stream_order_lwd == 5, 1.8, linewidths)
    linewidths <- ifelse(stream_order_lwd == 4, 1.5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 3, 1, linewidths)
  }
  
  # Generate title (matching original format)
  plot_title <- paste0("Annual Production\nYear:", year, " River:", watershed)
  
  # Set plot margins (matching original)
  par(mar = c(8, 4, 4, 2), bg = "white")
  
  # Plot basin and edges (matching original exactly)
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title, bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add legend (matching original exactly)
  legend("topleft", 
         legend = c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                    "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"), 
         col = pallete_expanded, 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n",
         bg = "white")
  
  # Add histogram overlay if provided
  if (!is.null(gg_hist)) {
    # Modify the histogram specifically for grid viewport use (from original code)
    limited_hist <- gg_hist +
      scale_x_continuous(limits = c(140, 200)) +
      scale_y_continuous(limits = c(0, 0.1)) +
      coord_cartesian(xlim = c(140, 200), ylim = c(0, 0.1), expand = FALSE) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )
    
    # Create viewport with explicit scaling (from original code)
    vp_hist <- viewport(
      x = 0.5, y = 0.05, 
      width = 0.7, height = 0.2, 
      just = c("center", "bottom")
    )
    
    # Print the modified histogram (from original code)
    print(limited_hist, vp = vp_hist)
  }
  
  dev.off()
  
  # Reset par to default
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(glue("  ✓ Saved annual tributary map: {basename(map_filename)}\n"))
}

#' Create multi-year comparison plot (simple bar chart)
create_multiyear_comparison <- function(annual_summary_data, watershed, output_dir) {
  
  if (nrow(annual_summary_data) == 0) return()
  
  # Create annual production comparison plot using base R
  comparison_filename <- file.path(output_dir, paste0("Annual_Comparison_", watershed, ".png"))
  
  png(comparison_filename, width = 12, height = 8, units = "in", res = 300, bg = "white")
  
  # Set margins
  par(mar = c(5, 5, 4, 2), bg = "white")
  
  # Create bar plot
  barplot(annual_summary_data$total_production, 
          names.arg = annual_summary_data$year,
          main = paste(watershed, "- Annual Production Comparison"),
          xlab = "Year",
          ylab = "Total Production",
          col = "#2166ac",
          border = "black")
  
  # Add values on top of bars
  text(x = seq_along(annual_summary_data$year) * 1.2 - 0.5,
       y = annual_summary_data$total_production + max(annual_summary_data$total_production) * 0.02,
       labels = round(annual_summary_data$total_production, 0),
       pos = 3, cex = 0.8)
  
  dev.off()
  
  # Reset par
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(glue("✓ Saved annual comparison: {basename(comparison_filename)}\n"))
}

#' Create simple summary statistics table
create_annual_summary_table <- function(annual_summary_data, watershed, output_dir) {
  
  if (nrow(annual_summary_data) == 0) return()
  
  # Add some summary statistics
  summary_stats <- annual_summary_data %>%
    summarise(
      mean_production = mean(total_production, na.rm = TRUE),
      sd_production = sd(total_production, na.rm = TRUE),
      min_production = min(total_production, na.rm = TRUE),
      max_production = max(total_production, na.rm = TRUE),
      cv_production = sd_production / mean_production * 100
    ) %>%
    mutate(
      watershed = watershed,
      years_analyzed = paste(range(annual_summary_data$year), collapse = "-"),
      n_years = nrow(annual_summary_data)
    ) %>%
    select(watershed, years_analyzed, n_years, everything())
  
  # Save summary table
  summary_filename <- file.path(output_dir, paste0("Annual_Summary_Stats_", watershed, ".csv"))
  write_csv(summary_stats, summary_filename)
  cat(glue("✓ Saved summary statistics: {basename(summary_filename)}\n"))
  
  # Also save the annual data
  annual_filename <- file.path(output_dir, paste0("Annual_Production_Data_", watershed, ".csv"))
  write_csv(annual_summary_data, annual_filename)
  cat(glue("✓ Saved annual data: {basename(annual_filename)}\n"))
}

cat("✓ Annual tributary mapping visualization functions loaded.\n")
cat("Functions available:\n")
cat("  - create_annual_tributary_map() - matches original style exactly\n")
cat("  - create_multiyear_comparison()\n")
cat("  - create_annual_summary_table()\n")