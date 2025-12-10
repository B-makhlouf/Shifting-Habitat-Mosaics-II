################################################################################
# CONSOLIDATED SALMON VISUALIZATION - WITH GENETIC COMPOSITION COLORING
# UPDATED: Now handles all stream orders (below-threshold streams show as white/gray)
################################################################################

library(ggplot2); library(RColorBrewer); library(scales); library(grid); library(sf); library(dplyr); library(tidyr)

#------------------------------------------------------------------------------
# HISTOGRAM CREATION FUNCTIONS
#------------------------------------------------------------------------------

#' Create CPUE histogram with genetic composition coloring (for Yukon)
#' Matches the QC script approach with filtered data underline
create_cpue_histogram_genetic <- function(natal_data, year, watershed) {
  
  if (watershed != "Yukon") {
    # For Kusko, use simple tomato-colored histogram (no genetic data)
    return(create_cpue_histogram_simple(natal_data, year))
  }
  
  # For Yukon: Create data with genetic composition by DOY
  doy_breaks <- seq(150, 190, by = 10)
  
  # Calculate by DOY (matching QC script exactly)
  daily_genetic <- natal_data %>%
    group_by(DOY) %>%
    summarise(
      cpue = first(dailyCPUEprop),
      has_genetics = sum(!is.na(Lower) & !is.na(Middle), na.rm = TRUE) > 0,
      mean_Lower = mean(Lower[!is.na(Lower)], na.rm = TRUE),
      mean_Middle = mean(Middle[!is.na(Middle)], na.rm = TRUE),
      mean_Upper = mean(Upper[!is.na(Upper)], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      mean_Lower = ifelse(is.na(mean_Lower), 0, mean_Lower),
      mean_Middle = ifelse(is.na(mean_Middle), 0, mean_Middle),
      mean_Upper = ifelse(is.na(mean_Upper), 0, mean_Upper)
    )
  
  # Create stacked data for ggplot (all three groups: Lower, Middle, Upper)
  stacked_data <- daily_genetic %>%
    filter(has_genetics) %>%
    select(DOY, cpue, mean_Lower, mean_Middle, mean_Upper) %>%
    pivot_longer(
      cols = starts_with("mean_"),
      names_to = "genetic_group",
      values_to = "proportion",
      names_prefix = "mean_"
    ) %>%
    mutate(
      genetic_group = factor(genetic_group, levels = c("Lower", "Middle", "Upper")),
      cpue_segment = cpue * proportion
    )
  
  # Define genetic group colors
  genetic_colors <- c("Lower" = "#1b9e77", "Middle" = "#d95f02", "Upper" = "#7570b3")
  
  # Get DOY range of actual data to show red underline
  doy_range <- range(natal_data$DOY, na.rm = TRUE)
  
  # Get max y value for scaling
  max_cpue <- max(daily_genetic$cpue, na.rm = TRUE)
  
  # Create histogram with red underline for filtered data range
  gg_hist <- ggplot(daily_genetic, aes(x = DOY)) +
    # Gray bars for days WITHOUT genetics
    geom_col(data = filter(daily_genetic, !has_genetics),
             aes(y = cpue), fill = "gray70", alpha = 0.8, width = 0.8) +
    
    # Stacked colored bars for days WITH genetics
    geom_col(data = stacked_data,
             aes(y = cpue_segment, fill = genetic_group), alpha = 0.85, width = 0.8) +
    

    # Color scale for genetic groups
    scale_fill_manual(values = genetic_colors, name = "Genetic Group") +
    
    # X-axis fixed from 150 to 190
    scale_x_continuous(
      limits = c(150, 190),
      breaks = doy_breaks,
      labels = doy_breaks
    ) +
    
    # Y-axis limits (extended above to show line)
    scale_y_continuous(limits = c(0, max_cpue * 1.3)) +
    
    # Coordinates
    coord_cartesian(xlim = c(150, 190), expand = FALSE) +
    
    # Labels and theme
    labs(
      title = NULL,
      x = "Day of Year",
      y = "Daily CPUE Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95", size = 0.2),
      plot.margin = margin(2, 2, 2, 2, "mm"),
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      legend.margin = margin(0, 0, 0, 0)
    )
  
  return(gg_hist)
}

#' Simple CPUE histogram (for Kusko or when genetic data unavailable)
create_cpue_histogram_simple <- function(natal_data, year) {
  
  doy_to_date <- function(doy) as.Date(doy - 1, origin = "2024-01-01")
  doy_breaks <- seq(140, 210, by = 10)
  
  gg_hist <- ggplot(natal_data, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "black", linewidth = 2) +
    geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), fill = "tomato", alpha = 0.7) +
    scale_x_continuous(
      limits = c(140, 200),
      breaks = doy_breaks,
      labels = function(x) paste0(x, "\n", format(doy_to_date(x), "%b %d"))
    ) +
    scale_y_continuous(limits = c(0, 0.1)) +
    coord_cartesian(xlim = c(140, 200), ylim = c(0, 0.1), expand = FALSE) +
    labs(
      title = paste("Annual Distribution", year),
      x = "Day of Year (Date)",
      y = "Daily CPUE Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  return(gg_hist)
}

#------------------------------------------------------------------------------
# MAIN MAPPING FUNCTION - UPDATED
#------------------------------------------------------------------------------
create_annual_map <- function(analysis_results, output_dir, year, watershed) {
  
  cat(paste("\n=== Creating map for", watershed, year, "===\n"))
  
  # 1. EXTRACT DATA FROM ANALYSIS RESULTS
  edges <- analysis_results$edges
  basin <- analysis_results$basin
  natal_data <- analysis_results$natal_data
  basin_assign_norm <- analysis_results$results$assignment_norm
  
  # 2. SETUP COLOR PALETTE
  palette <- brewer.pal(9, "YlOrRd")
  palette_expanded <- colorRampPalette(palette)(10)
  
  # 3. COLOR CODING (watershed-specific bins)
  # White for zero assignments, gray for NA/missing data
  colcode <- rep("gray90", length(basin_assign_norm))
  colcode[is.na(basin_assign_norm)] <- 'gray80'  # For any NA values
  # Stream order less then threshold will be slighly lighter grey 
  
  
  if (watershed == "Yukon") {
    # YUKON: Assign colors to bins (0 = white, >0 gets colors)
    colcode[edges$Str_Order < 3] <- 'gray80'
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.7] <- 'gray75'
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette_expanded[1]
    colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette_expanded[2]
    colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette_expanded[3]
    colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette_expanded[4]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette_expanded[5]
    colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette_expanded[6]
    colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette_expanded[7]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[8]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[9]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette_expanded[10]
    # colcode[basin_assign_norm > 0 & basin_assign_norm <=.4 ] <- palette_expanded[2]
    # colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.7] <- palette_expanded[5]
    # colcode[basin_assign_norm > 0.7 ] <- palette_expanded[9]
    # LEGEND: Use the EXACT SAME colors that were assigned above
    legend_labels <- c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
    legend_colors <- c(palette_expanded[1], palette_expanded[4], palette_expanded[5], 
                       palette_expanded[7], palette_expanded[8], palette_expanded[9], 
                       palette_expanded[10])
    
  } else {
    # KUSKO: 0.1 intervals (10 bins)
    colcode[edges$Str_Order <= 2] <- 'gray70'
    # colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette_expanded[1]
    # colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette_expanded[2]
    # colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette_expanded[3]
    # colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette_expanded[4]
    # colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette_expanded[5]
    # colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette_expanded[6]
    # colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette_expanded[7]
    # colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[8]
    # colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[9]
    # colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette_expanded[10]
    
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.2] <- palette_expanded[2]
    colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.4] <- palette_expanded[4]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.6] <- palette_expanded[6]
    colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.8] <- palette_expanded[8]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 1.0] <- palette_expanded[9]

    
    
    legend_labels <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                       "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
    legend_colors <- palette_expanded
  }
  
  # 4. LINE WIDTHS (watershed-specific stream order emphasis)
  stream_order <- edges$Str_Order
  stream_order[is.na(stream_order)] <- 1
  
  if (watershed == "Yukon") {
    # Conservative Yukon linewidths
    linewidths <- ifelse(stream_order >= 9, 3.7,
                         ifelse(stream_order >= 8, 2.5,
                                ifelse(stream_order >= 7, 2.3,
                                       ifelse(stream_order >= 6, 2.0,
                                              ifelse(stream_order >= 5, 1.8,
                                                     ifelse(stream_order >= 4, .8, 
                                                            ifelse(stream_order >= 3, 0.7, 0.2)))))))
    
    linewidths[basin_assign_norm > 0.7] <- linewidths[basin_assign_norm > 0.7] * 1.1
    
    
  } else {
    # Dramatic Kusko linewidths
    linewidths <- ifelse(stream_order >= 9, 5,
                         ifelse(stream_order >= 8, 6,
                                ifelse(stream_order >= 7, 6,
                                       ifelse(stream_order >= 6, 3.5,
                                              ifelse(stream_order >= 5, 3.0,
                                                     ifelse(stream_order >= 4, 2.7,
                                                            ifelse(stream_order >= 3, 2.0, 1.0)))))))
    
    linewidths[basin_assign_norm > 0.7] <- linewidths[basin_assign_norm > 0.7] * 1.1
    
  }
  
  # 5. CREATE CPUE HISTOGRAM (with genetic coloring for Yukon)
  gg_hist <- create_cpue_histogram_genetic(natal_data, year, watershed)
  
  # 6. CREATE OUTPUT FILE
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("Annual_Production_", year, "_", watershed, ".png"))
  
  png(file = output_file, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  # 7. PLOT BASE MAP
  par(mar = c(8, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = paste0("Annual Production\nYear: ", year, " River: ", watershed), bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # 8. ADD LEGEND
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5, 
         title = "Relative posterior density", bty = "n", bg = "white")
  
  # 9. OVERLAY HISTOGRAM
  vp_hist <- viewport(x = 0.5, y = 0.05, width = 0.7, height = 0.2, just = c("center", "bottom"))
  print(gg_hist, vp = vp_hist)
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("  ✓ Saved:", basename(output_file), "\n"))
  cat(paste("  ✓ Map includes ALL stream orders (white = zero assignment, colors = assignment values)\n"))
  
  return(output_file)
}

#------------------------------------------------------------------------------
# DOCUMENTATION
#------------------------------------------------------------------------------
cat("\n✓ UPDATED Visualization.R loaded with genetic composition coloring\n")
cat("New features:\n")
cat("  - For Yukon: CPUE bars colored by genetic composition (Lower=green, Middle=orange)\n")
cat("  - For Kusko: Simple tomato-colored CPUE histogram (no genetic data)\n")
cat("  - Gray bars for days without genetic data (Yukon only)\n")
cat("  - Genetic group legend integrated into histogram\n")
cat("  - Now handles all stream orders (below-threshold appear as gray)\n\n")
cat("Usage: create_annual_map(analysis_results, output_dir, year, watershed)\n")
cat("Example: create_annual_map(results, '/path/to/output', 2017, 'Kusko')\n\n")

# Uncomment to run:
# map_file <- create_annual_map(results, "/Users/benjaminmakhlouf/Desktop/Maps", 2017, "Kusko")