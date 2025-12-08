################################################################################
# CONSOLIDATED SALMON VISUALIZATION - UPDATED FOR ALL STREAM ORDERS
# UPDATED: Now handles all stream orders (below-threshold streams show as white/gray)
################################################################################

library(ggplot2); library(RColorBrewer); library(scales); library(grid); library(sf)

#------------------------------------------------------------------------------
# MAIN FUNCTION - UPDATED
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
  colcode[edges$Str_Order < 5] <- 'gray75'
  
  if (watershed == "Yukon") {
    # YUKON: Assign colors to bins (0 = white, >0 gets colors)
    
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.2] <- palette_expanded[2]
    colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette_expanded[3]
    colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette_expanded[4]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette_expanded[5]
    colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette_expanded[6]
    colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette_expanded[7]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[8]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[9]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette_expanded[10]
    
    # LEGEND: Use the EXACT SAME colors that were assigned above
    legend_labels <- c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
    legend_colors <- c(palette_expanded[1], palette_expanded[4], palette_expanded[5], 
                       palette_expanded[7], palette_expanded[8], palette_expanded[9], 
                       palette_expanded[10])
    
  } else {
    # KUSKO: 0.1 intervals (10 bins)
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
                                ifelse(stream_order >= 7, 1.7,
                                       ifelse(stream_order >= 6, 1.5,
                                              ifelse(stream_order >= 5, 1.2,
                                                     ifelse(stream_order >= 4, .8, 
                                                            ifelse(stream_order >= 3, 0.4, 0.2)))))))
    
    production_boost <- 1 + (pmax(basin_assign_norm - 0.7, 0) / 0.3) * 0.3
    production_boost[basin_assign_norm < 0.7] <- 1  # No boost below 0.7
    linewidths <- linewidths * production_boost
    
  } else {
    # Dramatic Kusko linewidths
    linewidths <- ifelse(stream_order >= 9, 5,
                         ifelse(stream_order >= 8, 4,
                                ifelse(stream_order >= 7, 3,
                                       ifelse(stream_order >= 6, 2,
                                              ifelse(stream_order >= 5, 1.8,
                                                     ifelse(stream_order >= 4, 1.5,
                                                            ifelse(stream_order >= 3, 0.8, 0.5)))))))
  }
  
  # 5. CREATE CPUE HISTOGRAM (for overlay)
  doy_to_date <- function(doy) as.Date(doy - 1, origin = "2024-01-01")
  doy_breaks <- seq(140, 210, by = 10)
  
  gg_hist <- ggplot(natal_data, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "black", linewidth = 2) +
    geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), fill = "tomato", alpha = 0.7) +
    scale_x_continuous(limits = c(140, 200), breaks = doy_breaks,
                       labels = function(x) paste0(x, "\n", format(doy_to_date(x), "%b %d"))) +
    scale_y_continuous(limits = c(0, 0.1)) +
    coord_cartesian(xlim = c(140, 200), ylim = c(0, 0.1), expand = FALSE) +
    labs(title = paste("Annual Distribution", year), x = "Day of Year (Date)", y = "Daily CPUE Proportion") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(0, 0, 0, 0))
  
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
# RUN
#------------------------------------------------------------------------------
cat("\n✓ UPDATED Visualization script loaded.\n")
cat("  - Now handles all stream orders in maps\n")
cat("  - Below-threshold streams appear as white (zero assignment)\n")
cat("  - Run: create_annual_map(analysis_results, output_dir, year, watershed)\n")
cat("Example: create_annual_map(results, '/path/to/output', 2017, 'Kusko')\n\n")

# Uncomment to run:
# map_file <- create_annual_map(results, "/Users/benjaminmakhlouf/Desktop/Maps", 2017, "Kusko")