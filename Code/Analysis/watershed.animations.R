# watershed_animation.R
# Animation script for watershed mapping visualization

# Install required packages if needed
# install.packages(c("gganimate", "gifski", "transformr", "cowplot"))

library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)
library(here)
library(RColorBrewer)
library(scales)
library(cowplot)

# Source your existing utility functions (adjust paths as needed)
source(here("code/utils/spatial_utils.R"))
source(here("code/utils/visualization.R"))
source(here("code/assignment.R"))

#' Create animations of watershed maps showing changes across run quartiles
#'
#' @param year Character or numeric representing the year
#' @param watershed Character: "Kusko" or "Yukon"
#' @param analysis_type Type of quartile analysis ("CPUE" or "DOY")
#' @param sensitivity_threshold Numeric threshold for assignment filtering
#' @param min_error Minimum error value to use
#' @param min_stream_order Minimum stream order to include
#' @param HUC HUC level to use
#' @param output_dir Directory to save animations
#' @param fps Frames per second for the animation
#' @param combined Whether to create a combined animation with map and histogram
#' @return Path to the created animation files
animate_watershed_maps <- function(year, watershed, 
                                   analysis_type = "CPUE", # "CPUE" or "DOY"
                                   sensitivity_threshold = NULL,
                                   min_error = NULL,
                                   min_stream_order = NULL,
                                   HUC = 8,
                                   output_dir = here("Basin Maps/Animations"),
                                   fps = 5,
                                   combined = TRUE) {
  
  # Set default parameters based on watershed if not provided
  if (is.null(sensitivity_threshold)) {
    sensitivity_threshold <- 0.7  # Same for both watersheds
  }
  
  if (is.null(min_error)) {
    min_error <- ifelse(watershed == "Kusko", 0.0006, 0.003)
  }
  
  if (is.null(min_stream_order)) {
    min_stream_order <- ifelse(watershed == "Kusko", 3, 5)
  }
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  message(paste("Processing", watershed, "watershed for year", year, "using", analysis_type, "quartiles"))
  
  # Load spatial data
  message("Loading spatial data...")
  spatial_data <- load_spatial_data(watershed, HUC, min_stream_order)
  edges <- spatial_data$edges
  basin <- spatial_data$basin
  Huc <- spatial_data$Huc
  
  # Load natal origins data
  message("Loading natal origins data...")
  natal_data <- load_natal_data(year, watershed)
  
  # Divide data into quartiles based on analysis type
  message(paste("Dividing data into", analysis_type, "quartiles..."))
  if(analysis_type == "CPUE") {
    quartile_data <- divide_cpue_quartiles(natal_data)
  } else {
    quartile_data <- divide_doy_quartiles(natal_data)
  }
  quartile_subsets <- quartile_data$subsets
  subset_labels <- quartile_data$labels
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Calculate error values
  error <- calculate_error(pid_isose, min_error)
  
  # Set up watershed-specific priors
  priors <- setup_watershed_priors(edges, min_stream_order, watershed, natal_data)
  
  # Prepare data frame to store results for each frame
  all_huc_results <- list()
  all_basin_assign_norm <- list()
  
  # Process each quartile subset
  for (q in 1:length(quartile_subsets)) {
    current_subset <- quartile_subsets[[q]]
    
    # Skip empty subsets
    if (nrow(current_subset) == 0) {
      message(paste("Skipping", subset_labels[q], "because it contains no data"))
      next
    }
    
    message(paste("Processing", subset_labels[q], "with", nrow(current_subset), "data points"))
    
    # Perform assignment
    assignment_matrix <- perform_assignment(
      current_subset, edges, watershed, priors, pid_iso, error, sensitivity_threshold
    )
    
    # Process assignments to get basin-scale values
    basin_results <- process_assignments(assignment_matrix)
    basin_assign_rescale <- basin_results$rescale
    basin_assign_norm <- basin_results$norm
    
    # Store normalized basin assignments for tributary map
    all_basin_assign_norm[[q]] <- basin_assign_norm
    
    # Process HUC data
    final_result <- process_huc_data(edges, basin, Huc, basin_assign_rescale, HUC)
    
    # Add frame information to the HUC data
    final_result$frame <- q
    final_result$frame_label <- subset_labels[q]
    
    # Store results
    all_huc_results[[q]] <- final_result
  }
  
  # Combine all frames into one data frame
  all_huc_data <- do.call(rbind, all_huc_results)
  
  # Create CPUE histogram data for the animation
  message("Preparing CPUE histogram data for animation...")
  
  histogram_data <- data.frame(
    DOY = integer(),
    dailyCPUEprop = numeric(),
    frame = integer(), 
    highlight = logical(),
    stringsAsFactors = FALSE
  )
  
  # Add full run data for each frame, with appropriate highlighting
  for (q in 1:length(quartile_subsets)) {
    frame_data <- natal_data %>%
      select(DOY, dailyCPUEprop) %>%
      mutate(
        frame = q,
        highlight = DOY %in% quartile_subsets[[q]]$DOY
      )
    
    histogram_data <- rbind(histogram_data, frame_data)
  }
  
  # Set up common theme elements
  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    )
  
  message("Creating animations...")
  
  # 1. HUC Map Animation
  map_plot <- ggplot() +
    geom_sf(
      data = all_huc_data,
      aes(fill = production_proportion),
      color = "white",
      size = 0.1
    ) +
    scale_fill_gradientn(
      colors = (brewer.pal(9, "YlOrRd")),
      name = "Proportion of total\nproduction",
      limits = c(0, max(all_huc_data$production_proportion, na.rm = TRUE)),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = paste("Watershed:", watershed, "- Year:", year),
      subtitle = "{closest_state}"
    ) +
    base_theme +
    # Animation transitions
    transition_states(
      frame_label,
      transition_length = 2,
      state_length = 3
    )
  
  # 2. CPUE Histogram Animation
  histogram_plot <- ggplot(histogram_data, aes(x = DOY, y = dailyCPUEprop)) +
    geom_line(color = "gray40", linewidth = 1) +
    geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop, fill = highlight),
                alpha = 0.7) +
    scale_fill_manual(values = c("gray70", "tomato"), guide = "none") +
    labs(
      title = paste(analysis_type, "Distribution"),
      x = "Day of Year", 
      y = "Daily CPUE Proportion"
    ) +
    base_theme +
    # Animation transitions
    transition_states(
      frame,
      transition_length = 2,
      state_length = 3
    )
  
  # 3. Stream/Tributary Map Animation (if edge data is available)
  if (!is.null(edges) && length(all_basin_assign_norm) > 0) {
    # Prepare edges data for animation
    all_edges_data <- NULL
    
    for (q in 1:length(all_basin_assign_norm)) {
      if (length(all_basin_assign_norm[[q]]) > 0) {
        # Create a copy of edges for this frame
        frame_edges <- edges
        
        # Add normalized basin assignments to edges
        frame_edges$basin_assign_norm <- all_basin_assign_norm[[q]]
        
        # Add frame information
        frame_edges$frame <- q
        frame_edges$frame_label <- subset_labels[[q]]
        
        # Convert values to color codes for visualization
        frame_edges$color_code <- "gray60"  # Default color
        
        # Color coding with bins at every 0.1
        for (i in 1:10) {
          lower <- (i-1)/10
          upper <- i/10
          
          if (i == 1) {
            frame_edges$color_code[frame_edges$basin_assign_norm > 0 & 
                                     frame_edges$basin_assign_norm <= upper] <- 
              brewer.pal(9, "YlOrRd")[1]
          } else {
            frame_edges$color_code[frame_edges$basin_assign_norm > lower & 
                                     frame_edges$basin_assign_norm <= upper] <- 
              brewer.pal(9, "YlOrRd")[i-1]
          }
        }
        
        # Reset color for segments with StreamOrderPrior = 0 or pid_prior = 0
        frame_edges$color_code[priors$StreamOrderPrior == 0] <- 'gray60'
        frame_edges$color_code[priors$pid_prior == 0] <- 'gray60'
        
        # Combine data
        if (is.null(all_edges_data)) {
          all_edges_data <- frame_edges
        } else {
          all_edges_data <- rbind(all_edges_data, frame_edges)
        }
      }
    }
    
    # Create tributary map animation if we have data
    if (!is.null(all_edges_data)) {
      trib_plot <- ggplot() +
        # Plot the basin as background
        geom_sf(data = basin, fill = 'gray80', color = 'gray60', size = 0.3) +
        # Plot the edges with color based on basin_assign_norm
        geom_sf(data = all_edges_data, aes(color = color_code, size = Str_Order)) +
        scale_color_identity() +
        scale_size_continuous(range = c(0.5, 2.5), guide = "none") +
        labs(
          title = paste("Tributary Analysis:", watershed, "Watershed -", year),
          subtitle = "{closest_state}"
        ) +
        base_theme +
        theme(legend.position = "none") +
        # Animation transitions
        transition_states(
          frame_label,
          transition_length = 2,
          state_length = 3
        )
    }
  }
  
  # Output filenames
  base_filename <- paste0(watershed, "_", year, "_", analysis_type)
  huc_filename <- paste0(base_filename, "_HUC_Map.gif")
  hist_filename <- paste0(base_filename, "_Histogram.gif")
  trib_filename <- paste0(base_filename, "_Tributary_Map.gif")
  combined_filename <- paste0(base_filename, "_Combined.gif")
  
  # Animation parameters
  anim_params <- list(
    fps = fps,
    renderer = gifski_renderer(),
    width = 800,
    height = 600
  )
  
  # Save individual animations
  message("Rendering HUC map animation...")
  anim_map <- animate(map_plot, 
                      fps = anim_params$fps, 
                      width = anim_params$width, 
                      height = anim_params$height,
                      renderer = anim_params$renderer)
  anim_save(huc_filename, animation = anim_map, path = output_dir)
  
  message("Rendering histogram animation...")
  anim_hist <- animate(histogram_plot, 
                       fps = anim_params$fps, 
                       width = anim_params$width, 
                       height = anim_params$height * 0.4,
                       renderer = anim_params$renderer)
  anim_save(hist_filename, animation = anim_hist, path = output_dir)
  
  if (exists("trib_plot")) {
    message("Rendering tributary map animation...")
    anim_trib <- animate(trib_plot, 
                         fps = anim_params$fps, 
                         width = anim_params$width, 
                         height = anim_params$height,
                         renderer = anim_params$renderer)
    anim_save(trib_filename, animation = anim_trib, path = output_dir)
  }
  
  # Create combined animation if requested
  if (combined) {
    message("Creating combined animation (this may take some time)...")
    
    # Use cowplot to create a combined plot
    if (exists("trib_plot")) {
      # Include tributary map if available
      combined_plot <- plot_grid(
        map_plot, trib_plot, histogram_plot,
        ncol = 1,
        rel_heights = c(4, 4, 2)
      )
    } else {
      # Map and histogram only
      combined_plot <- plot_grid(
        map_plot, histogram_plot,
        ncol = 1,
        rel_heights = c(4, 2)
      )
    }
    
    # Add a title to the combined plot
    title <- ggdraw() + 
      draw_label(
        paste(watershed, "Watershed -", year, "-", analysis_type, "Analysis"),
        fontface = 'bold',
        size = 16
      )
    
    combined_plot_with_title <- plot_grid(
      title, combined_plot,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
    
    # Animate the combined plot
    anim_combined <- animate(
      combined_plot_with_title,
      fps = anim_params$fps,
      width = anim_params$width,
      height = anim_params$height * 1.7,
      renderer = anim_params$renderer
    )
    
    anim_save(combined_filename, animation = anim_combined, path = output_dir)
  }
  
  message(paste("All animations created in", output_dir))
  
  # Return paths to created files
  output_files <- c(
    HUC_Map = file.path(output_dir, huc_filename),
    Histogram = file.path(output_dir, hist_filename)
  )
  
  if (exists("trib_plot")) {
    output_files <- c(output_files, 
                      Tributary_Map = file.path(output_dir, trib_filename))
  }
  
  if (combined) {
    output_files <- c(output_files,
                      Combined = file.path(output_dir, combined_filename))
  }
  
  return(output_files)
}

# Example usage:
# Just uncomment and run one of these examples

# Example 1: Create animation for Kuskokwim watershed using CPUE quartiles
# animate_watershed_maps("2016", "Kusko", analysis_type = "CPUE")

# Example 2: Create animation for Yukon watershed using DOY quartiles
# animate_watershed_maps("2015", "Yukon", analysis_type = "DOY")

# Example 3: Create animations for both watersheds and both analysis types
# watersheds <- c("Kusko", "Yukon")
# years <- c("2015", "2016")
# analysis_types <- c("CPUE", "DOY")
# 
# for (year in years) {
#   for (watershed in watersheds) {
#     for (analysis_type in analysis_types) {
#       try({
#         animate_watershed_maps(year, watershed, analysis_type)
#       })
#     }
#   }
# }