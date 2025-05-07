# Modified visualization.R for PNG output
# Visualization utilities for the watershed mapping project with PNG output

library(ggplot2)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(viridis)

#' Create a CPUE histogram highlighting a specific subset
#'
#' @param full_dataset Full dataset containing DOY and dailyCPUEprop
#' @param current_subset Subset of data to highlight
#' @param title Optional custom title for the plot
#' @return ggplot object with the histogram
create_cpue_histogram <- function(full_dataset, current_subset, title = NULL) {
  # Set default title if not provided
  if (is.null(title)) {
    title <- "CPUE Distribution"
  }
  
  # Create a custom color for highlighting the subset
  highlight_color <- "tomato"
  background_color <- "gray70"
  
  # Create the histogram with FIXED axis limits and coordinate system
  ggplot() + 
    # First plot the full dataset as background with reduced opacity
    geom_line(data = full_dataset, aes(x = DOY, y = dailyCPUEprop), 
              color = "gray40", linewidth = 1, alpha = 0.5) +
    geom_ribbon(data = full_dataset, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = background_color, alpha = 0.3) +
    
    # Then overlay the current subset with higher opacity and different color
    geom_line(data = current_subset, aes(x = DOY, y = dailyCPUEprop), 
              color = "black", linewidth = 2) +
    geom_ribbon(data = current_subset, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = highlight_color, alpha = 0.7) +
    
    # Add vertical lines to show the subset boundaries
    geom_vline(xintercept = c(
      min(current_subset$DOY), 
      max(current_subset$DOY)
    ), linetype = "dashed", color = "darkred") +
    
    # Set fixed axis ranges - MORE EXPLICIT approach
    scale_x_continuous(limits = c(145, 200)) +
    scale_y_continuous(limits = c(0, 0.1)) +
    # Force the coordinate system to respect our limits without expansion
    coord_cartesian(xlim = c(145, 200), ylim = c(0, 0.1), expand = FALSE) +
    
    # Add labels and theme
    labs(
      title = title,
      subtitle = "Current subset highlighted in red",
      x = "Day of Year", 
      y = "Daily CPUE Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA), # White background
      panel.background = element_rect(fill = "white", color = NA), # White panel background
      # Make sure plot margins don't affect scaling
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
}

#' Create and save a HUC map as PNG
#'
#' @param final_result SF object with HUC polygons and metrics
#' @param basin_assign_norm Vector of normalized basin assignments
#' @param gg_hist ggplot object with histogram to overlay
#' @param year Year for the title
#' @param watershed Watershed name for the title
#' @param sensitivity_threshold Sensitivity threshold used
#' @param min_stream_order Minimum stream order used
#' @param HUC HUC level used
#' @param subset_label Optional label for the subset
#' @param output_filepath Path to save the PNG
#' @return Invisibly returns the output filepath
# Modified create_huc_map function to place DOY histogram below the map
# and remove the raw production proportion bar chart

create_huc_map <- function(final_result, basin_assign_norm, gg_hist, year, watershed, 
                           sensitivity_threshold, min_stream_order, HUC, 
                           subset_label = NULL, output_filepath) {
  
  # Update file extension from pdf to png if needed
  output_filepath <- sub("\\.pdf$", ".png", output_filepath)
  
  huc_col <- paste0("HUC", HUC)
  name_col <- "Name"
  
  # Create bar plot of production proportion per meter by HUC
  bargraph_per_meter <- ggplot(final_result, 
                               aes(x = !!sym(name_col), y = production_per_meter_norm)) +
    geom_col(aes(fill = production_per_meter_norm), alpha = 0.9) +
    # Use YlOrRd color scale
    scale_fill_gradientn(
      colors = (brewer.pal(9, "YlOrRd")),
      name = "Production per km",
      limits = c(0, 1)
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("Production per km by", "HUC", HUC, "(Alphabetical)"),
         x = "",
         y = "Production per km (normalized)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 7),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5, "mm"),  # Add right margin
      plot.background = element_rect(fill = "white", color = NA), # White background
      panel.background = element_rect(fill = "white", color = NA) # White panel background
    )
  
  # Create main map plot
  main_plot <- ggplot() +
    geom_sf(
      data = final_result,
      aes(fill = production_per_meter_norm),
      color = "white",
      size = 0.1
    ) +
    scale_fill_gradientn(
      colors = (brewer.pal(9, "YlOrRd")),
      name = "Relative production\nper river km",
      na.value = "grey95",
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      guide = guide_colorbar(
        barwidth = 1,
        barheight = 15,
        frame.colour = "grey40",
        ticks.colour = "grey40",
        show.limits = TRUE
      )
    ) +
    coord_sf(datum = NA) +
    labs(
      title = paste0(ifelse(is.null(subset_label), "", paste(subset_label, ": ")), 
                     "Watershed:", watershed),
      subtitle = paste("Year", year, "- Sensitivity:", sensitivity_threshold, 
                       "- Min Stream Order:", min_stream_order)
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "grey30"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey50"),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold", color = "grey30"),
      legend.text = element_text(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA), # White panel background
      plot.background = element_rect(fill = "white", color = NA), # White plot background
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
  
  # If provided, make sure the gg_hist has white background
  if (!is.null(gg_hist)) {
    gg_hist <- gg_hist + 
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # Save plots to a PNG with higher resolution
  png(file = output_filepath, width = 12, height = 10, units = "in", res = 300, bg = "white")
  
  # Set up the plotting layout with proper spacing for two plots (map and bargraph) vertically
  # and the DOY histogram at the bottom
  grid.newpage()
  
  # Create a 2-row, 2-column layout
  # Row 1: Main map (col 1) and production per meter bar chart (col 2)
  # Row 2: DOY histogram spans both columns
  pushViewport(viewport(layout = grid.layout(2, 2, 
                                             heights = unit(c(0.7, 0.3), "npc"),
                                             widths = unit(c(0.6, 0.4), "npc"))))
  
  # Plot main map in upper left
  print(main_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  # Plot production per meter bar chart in upper right
  print(bargraph_per_meter, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  
  # If we have a histogram to include, add it at the bottom spanning both columns
  if (!is.null(gg_hist)) {
    print(gg_hist, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
  }
  
  dev.off()
  
  message(paste("Created PNG map:", output_filepath))
  invisible(output_filepath)
}

# For consistency, also modify the create_raw_production_map function to follow the same layout
create_raw_production_map <- function(final_result, basin_assign_norm, gg_hist, year, watershed, 
                                      sensitivity_threshold, min_stream_order, HUC, 
                                      subset_label = NULL, output_filepath) {
  
  # Update file extension from pdf to png if needed
  output_filepath <- sub("\\.pdf$", ".png", output_filepath)
  
  # Find the maximum production proportion for scaling
  max_prod <- max(final_result$production_proportion, na.rm = TRUE)
  
  # Create main map plot
  main_plot <- ggplot() +
    geom_sf(
      data = final_result,
      aes(fill = production_proportion),
      color = "white",
      size = 0.1
    ) +
    scale_fill_gradientn(
      colors = (brewer.pal(9, "YlOrRd")),
      name = "Raw proportion of\ntotal production",
      na.value = "grey95",
      limits = c(0, max_prod),
      labels = scales::percent_format(accuracy = 1),
      guide = guide_colorbar(
        barwidth = 1,
        barheight = 15,
        frame.colour = "grey40",
        ticks.colour = "grey40",
        show.limits = TRUE
      )
    ) +
    coord_sf(datum = NA) +
    labs(
      title = paste0(ifelse(is.null(subset_label), "", paste(subset_label, ": ")), 
                     "Raw Production Proportion - Watershed:", watershed),
      subtitle = paste("Year", year, "- Sensitivity:", sensitivity_threshold, 
                       "- Min Stream Order:", min_stream_order)
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey30"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold", color = "grey30"),
      legend.text = element_text(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA), # White panel background
      plot.background = element_rect(fill = "white", color = NA), # White plot background
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
  
  # If provided, make sure the gg_hist has white background
  if (!is.null(gg_hist)) {
    gg_hist <- gg_hist + 
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # Save plots to a PNG file
  png(file = output_filepath, width = 12, height = 8, units = "in", res = 300, bg = "white")
  
  # Set up the plotting layout - no bar chart, just map with histogram below
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.7, 0.3), "npc"))))
  
  # Plot main map
  print(main_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  # If we have a histogram to include, add it below the map
  if (!is.null(gg_hist)) {
    print(gg_hist, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  }
  
  dev.off()
  
  message(paste("Created PNG raw production map:", output_filepath))
  invisible(output_filepath)
}

#' Create and save a tributary map as PNG
#'
#' @param basin SF object with basin boundary
#' @param edges SF object with stream edges
#' @param basin_assign_norm Vector of normalized basin assignments
#' @param StreamOrderPrior Vector of stream order priors
#' @param pid_prior Vector of watershed priors
#' @param gg_hist ggplot object with histogram to overlay
#' @param year Year for the title
#' @param watershed Watershed name for the title
#' @param sensitivity_threshold Sensitivity threshold used
#' @param min_stream_order Minimum stream order used
#' @param min_error Minimum error used
#' @param subset_label Optional label for the subset
#' @param output_filepath Path to save the PNG
#' @return Invisibly returns the output filepath
create_tributary_map <- function(basin, edges, basin_assign_norm, StreamOrderPrior, 
                                 pid_prior, gg_hist, year, watershed, 
                                 sensitivity_threshold, min_stream_order, min_error,
                                 subset_label = NULL, output_filepath) {
  
  # Update file extension from pdf to png if needed
  output_filepath <- sub("\\.pdf$", ".png", output_filepath)
  
  # Open PNG for tributary map - higher resolution
  png(file = output_filepath, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  # Use the YlOrRd palette with 9 colors expanded to 10
  pallete <- brewer.pal(9, "YlOrRd")
  pallete_expanded <- colorRampPalette(pallete)(10)
  
  # Color coding with bins at every 0.1
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
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  
  # Set linewidths based on stream order and probability
  stream_order_lwd <- edges$Str_Order
  linewidths <- rep(1, length(stream_order_lwd))
  linewidths <- ifelse(stream_order_lwd == 9, 5, linewidths)
  linewidths <- ifelse(stream_order_lwd == 8, 4, linewidths)
  linewidths <- ifelse(stream_order_lwd == 7, 3, linewidths)
  linewidths <- ifelse(stream_order_lwd == 6, 2, linewidths)
  linewidths <- ifelse(stream_order_lwd == 5, 1.8, linewidths)
  linewidths <- ifelse(stream_order_lwd == 4, 1.5, linewidths)
  linewidths <- ifelse(stream_order_lwd == 3, 1, linewidths)
  
  # Add a multiplier for segments with high probability
  high_prob_multiplier <- rep(1, length(basin_assign_norm))
  high_prob_multiplier[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- 1.5
  high_prob_multiplier[basin_assign_norm > 0.9] <- 1.9
  linewidths <- linewidths * high_prob_multiplier
  
  # Generate title
  plot_title <- paste0(
    ifelse(is.null(subset_label), "", paste0(subset_label, "\n")),
    "Year:", year, 
    " River:", watershed,
    "\nThreshold:", sensitivity_threshold, 
    " Min Stream Order:", min_stream_order,
    " Min Error:", min_error
  )
  
  # Adjust plot margins
  par(mar = c(8, 4, 4, 2), bg = "white") # Set white background
  
  # Plot the basin and edges
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title, bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add legend
  legend("topleft", 
         legend = c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                    "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"), 
         col = pallete_expanded, 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n",
         bg = "white") # Ensure white background for legend
  
  # Add histogram to the plot if provided
  if (!is.null(gg_hist)) {
    vp_hist <- viewport(x = 0.5, y = 0.05, width = 0.7, height = 0.2, just = c("center", "bottom"))
    print(gg_hist, vp = vp_hist)
  }
  
  dev.off()
  
  # Reset par to default
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  message(paste("Created PNG tributary map:", output_filepath))
  invisible(output_filepath)
}

#' Create visualization of data splits (DOY or CPUE quartiles)
#'
#' @param data Data frame containing the data
#' @param breaks Vector of break points
#' @param type Type of split ("DOY" or "CPUE")
#' @param identifier Identifier for the output file
#' @return Path to saved plot
create_split_visualization <- function(data, breaks, type, identifier) {
  # Create visualization of the splits
  split_plot <- ggplot(data, aes(x = DOY, y = dailyCPUEprop)) +
    geom_line(color = "black", linewidth = 1) +
    geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), fill = "grey70", alpha = 0.5) +
    geom_vline(xintercept = breaks, linetype = "dashed", color = "red") +
    labs(title = paste("Run Timing Split by", type, "Quartiles"),
         x = "Day of Year",
         y = "Daily CPUE Proportion") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA), # White background
      panel.background = element_rect(fill = "white", color = NA) # White panel background
    )
  
  # Create directory and save
  dir.create(here::here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
  output_path <- paste0(here::here("Basin Maps/Quartile_Splits/"), identifier, "_", type, "_quartiles.png")
  ggsave(output_path, split_plot, width = 8, height = 5, bg = "white")
  
  message(paste("Created PNG quartile visualization:", output_path))
  return(output_path)
}

# Function to create a raw production proportion map as PNG
create_raw_production_map <- function(final_result, basin_assign_norm, gg_hist, year, watershed, 
                                      sensitivity_threshold, min_stream_order, HUC, 
                                      subset_label = NULL, output_filepath) {
  
  # Update file extension from pdf to png if needed
  output_filepath <- sub("\\.pdf$", ".png", output_filepath)
  
  huc_col <- paste0("HUC", HUC)
  name_col <- "Name"
  
  # Find the maximum production proportion for scaling
  max_prod <- max(final_result$production_proportion, na.rm = TRUE)
  
  # Create bar plot of raw production proportion by HUC
  bargraph <- ggplot(final_result, 
                     aes(x = reorder(!!sym(name_col), production_proportion), 
                         y = production_proportion)) +
    geom_col(aes(fill = production_proportion), alpha = 0.9) +
    # Use YlOrRd color scale
    scale_fill_gradientn(
      colors = (brewer.pal(9, "YlOrRd")),
      name = "Raw proportion",
      limits = c(0, max_prod)
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max_prod * 1.05), expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("Raw Production Proportion by", "HUC", HUC, "(Ordered)"),
         x = "",
         y = "Proportion of total production") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(5, 25, 5, 5, "mm"),  # Add right margin
      plot.background = element_rect(fill = "white", color = NA), # White background
      panel.background = element_rect(fill = "white", color = NA) # White panel background
    )
  
  # Create main map plot
  main_plot <- ggplot() +
    geom_sf(
      data = final_result,
      aes(fill = production_proportion),
      color = "white",
      size = 0.1
    ) +
    scale_fill_gradientn(
      colors = (brewer.pal(9, "YlOrRd")),
      name = "Raw proportion of\ntotal production",
      na.value = "grey95",
      limits = c(0, max_prod),
      labels = scales::percent_format(accuracy = 1),
      guide = guide_colorbar(
        barwidth = 1,
        barheight = 15,
        frame.colour = "grey40",
        ticks.colour = "grey40",
        show.limits = TRUE
      )
    ) +
    coord_sf(datum = NA) +
    labs(
      title = paste0(ifelse(is.null(subset_label), "", paste(subset_label, ": ")), 
                     "Raw Production Proportion - Watershed:", watershed),
      subtitle = paste("Year", year, "- Sensitivity:", sensitivity_threshold, 
                       "- Min Stream Order:", min_stream_order)
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey30"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold", color = "grey30"),
      legend.text = element_text(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA), # White panel background
      plot.background = element_rect(fill = "white", color = NA), # White plot background
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
  
  # Save plots to a PNG file
  png(file = output_filepath, width = 12, height = 8, units = "in", res = 300, bg = "white")
  
  # Set up the plotting layout with proper spacing
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2, widths = unit(c(0.6, 0.4), "npc"))))
  
  # Plot main map in left panel
  print(main_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  # Plot bar chart in right panel 
  print(bargraph, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  
  # If we have a histogram to include, add it
  if (!is.null(gg_hist)) {
    print(gg_hist, vp = viewport(x = 0.3, y = 0.85, width = 0.5, height = 0.25))
  }
  
  dev.off()
  
  message(paste("Created PNG raw production map:", output_filepath))
  invisible(output_filepath)
}