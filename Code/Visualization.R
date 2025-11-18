################################################################################
# VISUALIZATION.R - ALL MAPPING AND PLOTTING FUNCTIONS
################################################################################
# Contains all functions for creating tributary maps and visualizations of 
# salmon natal origin assignments
# ANNUAL ANALYSIS ONLY
################################################################################

library(ggplot2)
library(RColorBrewer)
library(scales)
library(grid)

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Get line width based on stream order and watershed
get_line_width <- function(stream_order, watershed) {
  stream_order[is.na(stream_order)] <- 1
  
  if (watershed == "Yukon") {
    # Conservative Yukon linewidths
    ifelse(stream_order >= 9, 3.7,
           ifelse(stream_order >= 8, 2.5,
                  ifelse(stream_order >= 7, 1.7,
                         ifelse(stream_order >= 6, 1.5,
                                ifelse(stream_order >= 5, 1,
                                       ifelse(stream_order >= 4, 1, 1))))))
  } else {
    # Dramatic Kusko linewidths
    ifelse(stream_order >= 9, 5,
           ifelse(stream_order >= 8, 4,
                  ifelse(stream_order >= 7, 3,
                         ifelse(stream_order >= 6, 2,
                                ifelse(stream_order >= 5, 1.8,
                                       ifelse(stream_order >= 4, 1.5, 1))))))
  }
}

#' Get color coding based on normalized values and watershed
get_color_code <- function(basin_assign_norm, watershed) {
  pallete <- brewer.pal(9, "YlOrRd")
  pallete_expanded <- colorRampPalette(pallete)(10)
  
  colcode <- rep("gray60", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'white'
  
  if (watershed == "Yukon") {
    # Yukon: 0.2 intervals
    colcode[basin_assign_norm > 0 & basin_assign_norm <= 0.2] <- pallete_expanded[1]
    colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.4] <- pallete_expanded[4]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.6] <- pallete_expanded[5]
    colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- pallete_expanded[7]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- pallete_expanded[8]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- pallete_expanded[9]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- pallete_expanded[10]
  } else {
    # Kusko: 0.1 intervals
    for (i in 1:10) {
      lower <- (i-1)/10
      upper <- i/10
      colcode[basin_assign_norm > lower & basin_assign_norm <= upper] <- pallete_expanded[i]
    }
  }
  
  return(colcode)
}

#' Create legend labels based on watershed
get_legend_labels <- function(watershed) {
  if (watershed == "Yukon") {
    return(c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.7", 
             "0.7-0.8", "0.8-0.9", "0.9-1.0"))
  } else {
    return(c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
             "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"))
  }
}

#' Get legend colors based on watershed
get_legend_colors <- function(watershed) {
  pallete <- brewer.pal(9, "YlOrRd")
  pallete_expanded <- colorRampPalette(pallete)(10)
  
  if (watershed == "Yukon") {
    return(c(pallete_expanded[1], pallete_expanded[4], pallete_expanded[5], 
             pallete_expanded[7], pallete_expanded[8], pallete_expanded[9], 
             pallete_expanded[10]))
  } else {
    return(pallete_expanded)
  }
}

################################################################################
# HISTOGRAM FUNCTIONS
################################################################################

#' Create DOY histogram
create_cpue_histogram <- function(natal_data, title = NULL) {
  if (is.null(title)) title <- "Annual CPUE Distribution"
  
  doy_to_date <- function(doy, year = 2024) {
    as.Date(doy - 1, origin = paste0(year, "-01-01"))
  }
  
  doy_breaks <- seq(140, 210, by = 10)
  
  ggplot(natal_data, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "black", linewidth = 2) +
    geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), 
                fill = "tomato", alpha = 0.7) +
    scale_x_continuous(
      limits = c(140, 210),
      breaks = doy_breaks,
      labels = function(x) paste0(x, "\n", format(doy_to_date(x), "%b %d"))
    ) +
    scale_y_continuous(limits = c(0, 0.1)) +
    coord_cartesian(xlim = c(140, 210), ylim = c(0, 0.1), expand = FALSE) +
    labs(title = title,
         x = "Day of Year (Date)", y = "Daily CPUE Proportion") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
}

################################################################################
# TRIBUTARY MAP FUNCTION
################################################################################

#' Create tributary map (base R plotting)
create_tributary_map <- function(basin, edges, basin_assign_norm, year, watershed, 
                                 output_filepath, gg_hist = NULL, priors = NULL) {
  
  # Open PNG file
  png(file = output_filepath, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  # Get colors and linewidths
  colcode <- get_color_code(basin_assign_norm, watershed)
  linewidths <- get_line_width(edges$Str_Order, watershed)
  
  # Reset colors for zero-prior segments if priors provided
  if (!is.null(priors)) {
    colcode[priors$StreamOrderPrior == 0] <- 'gray60'
    colcode[priors$pid_prior == 0] <- 'gray60'
  }
  
  # Generate title
  plot_title <- paste0("Annual Production\nYear: ", year, " River: ", watershed)
  
  # Set plot margins
  par(mar = c(8, 4, 4, 2), bg = "white")
  
  # Plot basin and edges
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = plot_title, bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, 
       add = TRUE, lwd = linewidths)
  
  # Add legend
  legend("topleft", 
         legend = get_legend_labels(watershed), 
         col = get_legend_colors(watershed), 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n",
         bg = "white")
  
  # Add histogram overlay if provided
  if (!is.null(gg_hist)) {
    limited_hist <- gg_hist +
      scale_x_continuous(limits = c(140, 200)) +
      scale_y_continuous(limits = c(0, 0.1)) +
      coord_cartesian(xlim = c(140, 200), ylim = c(0, 0.1), expand = FALSE) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )
    
    vp_hist <- viewport(x = 0.5, y = 0.05, width = 0.7, height = 0.2, 
                        just = c("center", "bottom"))
    print(limited_hist, vp = vp_hist)
  }
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("  ✓ Saved tributary map:", basename(output_filepath), "\n"))
}

################################################################################
# HIGH-LEVEL VISUALIZATION FUNCTION
################################################################################

#' Create annual tributary map with histogram
create_annual_map <- function(analysis_results, output_dir, year, watershed) {
  
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Extract data
  edges <- analysis_results$spatial_data$edges
  basin <- analysis_results$spatial_data$basin
  basin_results <- analysis_results$basin_results
  natal_data <- analysis_results$natal_data
  priors <- analysis_results$priors
  
  # Create histogram
  gg_hist <- create_cpue_histogram(natal_data, paste("Annual Distribution", year))
  
  # Create output filename
  output_file <- file.path(output_dir, 
                           paste0("Annual_Production_", year, "_", watershed, ".png"))
  
  # Create tributary map
  create_tributary_map(basin, edges, basin_results$norm, year, watershed, 
                       output_file, gg_hist, priors)
  
  cat("✓ Annual map created\n")
  
  return(output_file)
}

cat("✓ Visualization.R loaded successfully\n")
cat("Main function:\n")
cat("  - create_annual_map(analysis_results, output_dir, year, watershed)\n")
cat("\nExample usage:\n")
cat("  results <- run_annual_analysis(2017, 'Kusko')\n")
cat("  create_annual_map(results, '/path/to/output', 2017, 'Kusko')\n")