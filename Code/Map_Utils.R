# Map_Utils.R - A new utility file with shared mapping functions

library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(here)
library(grid)
library(gridExtra)
library(tidyverse)

#========================================================================
# UTILITY FUNCTIONS FOR MAPPING
#========================================================================

# Function to create HUC polygon maps
create_huc_map <- function(final_result, basin_assign_norm, gg_hist, year, watershed, 
                           sensitivity_threshold, min_stream_order, HUC, subset_label, 
                           output_filepath) {
  
  huc_col <- paste0("HUC", HUC)
  name_col <- "Name"
  
  # Create bar plot of production proportion by HUC
  bargraph <- ggplot(final_result, 
                     aes(x = !!sym(name_col),  # Use NAME directly
                         y = production_per_meter_norm)) +
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
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(5, 25, 5, 5, "mm")  # Add right margin
    )
  
  # Create main map plot
  main_plot <- ggplot() +
    geom_sf(
      data = final_result,
      aes(fill = production_per_meter_norm),
      color = "white",
      size = 0.1
    ) +
    # Using YlOrRd color palette
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
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey30"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold", color = "grey30"),
      legend.text = element_text(color = "grey30"),
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
  
  # Save plots to a PDF
  pdf(file = output_filepath, width = 12, height = 8)
  
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
}

# Function to create tributary maps
create_tributary_map <- function(basin, edges, basin_assign_norm, StreamOrderPrior, pid_prior, gg_hist,
                                 year, watershed, sensitivity_threshold, min_stream_order, min_error,
                                 subset_label, output_filepath) {
  
  # Open PDF for tributary map
  pdf(file = output_filepath, width = 9, height = 8)
  
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
  linewidths <- ifelse(stream_order_lwd == 9, 2, linewidths)
  linewidths <- ifelse(stream_order_lwd == 8, 1.7, linewidths)
  linewidths <- ifelse(stream_order_lwd == 7, 1.5, linewidths)
  linewidths <- ifelse(stream_order_lwd == 6, 1.2, linewidths)
  linewidths <- ifelse(stream_order_lwd == 5, 1, linewidths)
  linewidths <- ifelse(stream_order_lwd == 4, 0.8, linewidths)
  linewidths <- ifelse(stream_order_lwd == 3, 0.6, linewidths)
  
  # Add a multiplier for segments with high probability
  high_prob_multiplier <- rep(1, length(basin_assign_norm))
  high_prob_multiplier[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- 1.3
  high_prob_multiplier[basin_assign_norm > 0.9] <- 1.5
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
  par(mar = c(8, 4, 4, 2))
  
  # Plot the basin and edges
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add legend
  legend("topleft", 
         legend = c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                    "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"), 
         col = pallete_expanded, 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n")
  
  # Add histogram to the plot if provided
  if (!is.null(gg_hist)) {
    vp_hist <- viewport(x = 0.5, y = 0.05, width = 0.7, height = 0.2, just = c("center", "bottom"))
    print(gg_hist, vp = vp_hist)
  }
  
  dev.off()
  
  # Reset par to default
  par(mar = c(5, 4, 4, 2) + 0.1)
}

# Function to create CPUE histogram
create_cpue_histogram <- function(full_dataset, current_subset) {
  # Create a custom color for highlighting the subset
  highlight_color <- "tomato"
  background_color <- "gray70"
  
  # Create the improved histogram
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
    
    # Add labels and theme
    labs(
      title = "CPUE Distribution",
      subtitle = "Current subset highlighted in red",
      x = "Day of Year", 
      y = "Daily CPUE Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    )
}

# Function to process HUC calculations
process_huc_data <- function(edges, basin, Huc, basin_assign_rescale, HUC = 8) {
  huc_col <- paste0("HUC", HUC)
  name_col <- "Name"
  
  edges <- st_transform(edges, st_crs(Huc))
  edges$basin_assign_rescale <- basin_assign_rescale
  basin <- st_transform(basin, st_crs(Huc))
  
  # Identify which HUCs are within the Basin
  basin_buffer <- st_buffer(basin, dist = 0)
  hucs_in_basin <- Huc[st_intersects(Huc, basin_buffer, sparse = FALSE)[,1], ]
  
  # Calculate the area of intersection
  intersection_areas <- st_intersection(hucs_in_basin, basin_buffer) %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry() %>%
    group_by(!!sym(huc_col)) %>%
    summarize(int_area = sum(area))
  
  # Original areas of the HUCs
  hucs_areas <- hucs_in_basin %>%
    mutate(total_area = st_area(.)) %>%
    st_drop_geometry() %>%
    select(!!sym(huc_col), total_area)
  
  # Calculate percentage of overlap
  overlap_percentage <- intersection_areas %>%
    left_join(hucs_areas, by = huc_col) %>%
    mutate(pct_overlap = as.numeric(int_area / total_area))
  
  # Filter to significant overlap
  significant_hucs <- overlap_percentage %>%
    filter(pct_overlap > 0.1) %>%
    pull(!!sym(huc_col))
  
  # Spatial join and calculate stream length
  Combined_edges_HUC <- st_join(edges, Huc, join = st_intersects)
  edges$stream_length_m <- as.numeric(st_length(edges))
  
  # Summarize production by HUC polygon
  summary_huc <- Combined_edges_HUC %>%
    group_by(!!sym(huc_col)) %>%
    summarise(
      total_production = sum(basin_assign_rescale, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(production_proportion = total_production / sum(total_production, na.rm = TRUE))
  
  # Summarize stream length by HUC
  stream_length_by_huc <- edges %>%
    st_join(Huc, join = st_intersects) %>%
    st_drop_geometry() %>%
    group_by(!!sym(huc_col)) %>%
    summarise(total_stream_length = sum(stream_length_m, na.rm = TRUE))
  
  # Merge production and stream length
  final_result <- Huc %>%
    filter(!!sym(huc_col) %in% significant_hucs) %>%
    left_join(st_drop_geometry(summary_huc), by = huc_col) %>%
    left_join(stream_length_by_huc, by = huc_col) %>%
    replace_na(list(
      total_production = 0, 
      production_proportion = 0, 
      total_stream_length = 0
    )) %>%
    mutate(
      production_per_meter = ifelse(total_stream_length > 0,
                                    total_production / total_stream_length,
                                    NA_real_),
      production_per_meter_norm = production_per_meter / max(production_per_meter, na.rm = TRUE)
    )
  
  return(final_result)
}