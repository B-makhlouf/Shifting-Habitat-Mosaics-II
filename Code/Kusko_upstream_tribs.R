# Load required libraries
library(sf)
library(dplyr)

# Read the management areas CSV
mgmt_areas <- read.csv("/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Data/DownstreamReachesKuskoMngmt.csv")

# Load the Kuskokwim shapefile and basin
kuskokwim_shapefile <- sf::st_read('/Users/benjaminmakhlouf/Spatial Data/Shapefiles/AYK Shapefiles/kusko_edges_20210717.shp', quiet = TRUE)
kusk_basin <- sf::st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp", quiet = TRUE)

# Load nodes for network relationships
KuskoNodes <- read.csv("/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Data/Upstream_reaches/kusko/kusko_noderelationships.csv")
KuskoNetwork <- KuskoNodes %>% rename(child_s = fromnode, parent_s = tonode)

# Check available columns in shapefile to identify stream order column
print("Available columns in shapefile:")
print(names(kuskokwim_shapefile))

# Function to determine line width based on stream order
get_line_width <- function(stream_order) {
  # Handle NA values
  stream_order[is.na(stream_order)] <- 1
  
  # Define line widths based on stream order with very dramatic differences
  # Higher order streams get much thicker lines
  ifelse(stream_order >= 7, 8.0,      # Major rivers - VERY thick
         ifelse(stream_order >= 6, 6.0,  # Large rivers - thick
                ifelse(stream_order >= 5, 4.5,  # Large tributaries
                       ifelse(stream_order >= 4, 3.5,  # Medium-large tributaries
                              ifelse(stream_order >= 3, 2.5,  # Medium tributaries
                                     ifelse(stream_order >= 2, 1.5,  # Small tributaries
                                            1.0))))))        # Headwater streams
}

# Function to get background line width (lighter/thinner for all streams)
get_background_line_width <- function(stream_order) {
  # Handle NA values
  stream_order[is.na(stream_order)] <- 1
  
  # Background streams with very dramatic differences for major rivers
  ifelse(stream_order >= 7, 5.0,      # Major rivers - thick even in background
         ifelse(stream_order >= 6, 3.5,  # Large rivers
                ifelse(stream_order >= 5, 2.5,  # Large tributaries
                       ifelse(stream_order >= 4, 2.0,  # Medium-large tributaries
                              ifelse(stream_order >= 3, 1.5,  # Medium tributaries
                                     ifelse(stream_order >= 2, 1.0,  # Small tributaries
                                            0.3))))))        # Headwater streams
}

# Function to find upstream reaches (simplified version of your function)
find_upstream_reaches <- function(ReachID) {
  # Select the reach ID from the shapefile that corresponds to the ReachID value
  TribStartRID <- kuskokwim_shapefile$rid[which(kuskokwim_shapefile$reachid == ReachID)]
  
  if(length(TribStartRID) == 0) {
    return(c())
  }
  
  # Create an empty list called tributary_indices
  tributary_indices <- c()
  
  # Find the first child reach ID value for the starting tributary
  StartChild <- KuskoNetwork$child_s[which(KuskoNetwork$rid == TribStartRID)]
  
  if(length(StartChild) == 0) {
    return(c())
  }
  
  # Concatenate these together
  tributary_indices <- c(tributary_indices, StartChild)
  
  # Go find all child reach ID values which have a parent value of the specified starting child value
  ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s == StartChild)]
  
  # As long as there are some "child" tributaries
  while (length(ChildList) > 0) {
    # Add the new child values to the existing tributary_indices
    tributary_indices <- c(tributary_indices, ChildList)
    
    # Find the next set of child values
    ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s %in% ChildList)]
  }
  
  # Find the indices of elements in KuskoNetwork$child_s that match values in tributary_indices
  indices_in_child_s <- match(tributary_indices, KuskoNetwork$child_s)
  
  # Extract the values from KuskoNetwork$rid corresponding to the matched indices
  rid_values <- KuskoNetwork$rid[indices_in_child_s]
  
  # Find the indices of the values obtained in Step 2 within kuskokwim_shapefile$rid
  indices_in_shapefile <- match(rid_values, kuskokwim_shapefile$rid)
  
  # Extract the values from kuskokwim_shapefile$reachid using the indices obtained in Step 3
  upstream_reach_ids <- kuskokwim_shapefile$reachid[indices_in_shapefile]
  
  # Remove NA values
  upstream_reach_ids <- upstream_reach_ids[!is.na(upstream_reach_ids)]
  
  return(upstream_reach_ids)
}

# Initialize management_river column with NA (shortened name for shapefile compatibility)
kuskokwim_shapefile$mgmt_river <- NA_character_

# Process each management area
for (i in 1:nrow(mgmt_areas)) {
  area_name <- mgmt_areas$Name[i]
  reach_id <- mgmt_areas$reachid[i]
  
  print(paste("Processing", area_name, "- ReachID:", reach_id))
  
  # Find all upstream reaches for this management area
  upstream_reaches <- find_upstream_reaches(reach_id)
  
  # Also include the management area reach itself
  all_reaches <- c(reach_id, upstream_reaches)
  
  # Update the shapefile attribute for these reaches
  kuskokwim_shapefile$mgmt_river[kuskokwim_shapefile$reachid %in% all_reaches] <- area_name
  
  print(paste("  - Assigned", length(all_reaches), "reaches to", area_name))
}

# Check how many reaches were assigned
assigned_count <- sum(!is.na(kuskokwim_shapefile$mgmt_river))
total_count <- nrow(kuskokwim_shapefile)

print(paste("Summary:"))
print(paste("  - Total reaches in shapefile:", total_count))
print(paste("  - Reaches assigned to management areas:", assigned_count))
print(paste("  - Unassigned reaches:", total_count - assigned_count))

# Display the distribution of management areas
management_summary <- table(kuskokwim_shapefile$mgmt_river, useNA = "ifany")
print("Management area distribution:")
print(management_summary)

# Create output directory if it doesn't exist
output_dir <- "/Users/benjaminmakhlouf/Spatial Data/Management_Units"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  print(paste("Created directory:", output_dir))
}

# Check if directory was created successfully
if (!dir.exists(output_dir)) {
  stop("Failed to create output directory. Please check permissions.")
}

# Save the updated shapefile with error handling
output_path <- file.path(output_dir, "kusko_edges_with_management.shp")

tryCatch({
  st_write(kuskokwim_shapefile, output_path, delete_dsn = TRUE, quiet = FALSE)
  print(paste("Updated shapefile saved to:", output_path))
}, error = function(e) {
  print(paste("Error saving shapefile:", e$message))
  # Try alternative approach - save to current working directory first
  temp_path <- "kusko_edges_with_management.shp"
  st_write(kuskokwim_shapefile, temp_path, delete_dsn = TRUE, quiet = FALSE)
  print(paste("Shapefile saved to current directory:", getwd()))
  print("You can manually move it to your desired location.")
})

# Also save a summary CSV of the management assignments
summary_df <- kuskokwim_shapefile %>% 
  st_drop_geometry() %>%
  select(reachid, rid, mgmt_river) %>%
  arrange(mgmt_river, reachid)

summary_path <- file.path(output_dir, "management_assignments_summary.csv")
write.csv(summary_df, summary_path, row.names = FALSE)

print(paste("Management assignments summary saved to:", summary_path))

#===============================================================================
# NOW CREATE THE FIGURES WITH STREAM ORDER LINE WIDTHS
#===============================================================================

# Set up plotting directory
plot_output_dir <- "/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/UpstreamReaches"
if (!dir.exists(plot_output_dir)) {
  dir.create(plot_output_dir, recursive = TRUE)
}

# Use Strahler as the stream order column
stream_order_col <- "Strahler"

if (!"Strahler" %in% names(kuskokwim_shapefile)) {
  warning("Strahler column not found in shapefile. Using default line widths.")
  kuskokwim_shapefile$Strahler <- 2  # Default stream order
} else {
  print(paste("Using stream order column:", stream_order_col))
  print(paste("Strahler order range:", min(kuskokwim_shapefile$Strahler, na.rm = TRUE), 
              "to", max(kuskokwim_shapefile$Strahler, na.rm = TRUE)))
}

# Generate colors for each management area
colors <- rainbow(nrow(mgmt_areas))

# Create comprehensive plot with stream order line widths
comprehensive_plot_path <- file.path(plot_output_dir, "Kusko_All_Management_Areas_StreamOrder.png")
png(filename = comprehensive_plot_path, width = 4000, height = 3000, res = 300)

# Plot basin background
plot(sf::st_geometry(kusk_basin), col = "gray90", border = "gray70", 
     main = "Kuskokwim Watershed - All Management Areas (Line Width by Stream Order)",
     sub = paste("Management Areas:", nrow(mgmt_areas)))

# Plot all streams in light gray with varying line widths based on Strahler order
strahler_orders <- kuskokwim_shapefile$Strahler

# Plot streams by Strahler order (from smallest to largest for proper layering)
unique_orders <- sort(unique(strahler_orders[!is.na(strahler_orders)]))
print(paste("Plotting background streams with Strahler orders:", paste(unique_orders, collapse = ", ")))

for (order in unique_orders) {
  order_indices <- which(strahler_orders == order & !is.na(strahler_orders))
  if (length(order_indices) > 0) {
    # Get the appropriate line width for this Strahler order
    order_line_width <- get_background_line_width(order)
    print(paste("Strahler order", order, "- plotting", length(order_indices), "segments with line width", order_line_width))
    plot(sf::st_geometry(kuskokwim_shapefile)[order_indices], 
         col = "gray70", add = TRUE, lwd = order_line_width)
  }
}

# Process each management area and add to plot with stream order line widths
legend_names <- c()
legend_colors <- c()

for (i in 1:nrow(mgmt_areas)) {
  area_name <- mgmt_areas$Name[i]
  reach_id <- mgmt_areas$reachid[i]
  color <- colors[i]
  
  # Find upstream reaches using your original function logic
  upstream_reaches <- find_upstream_reaches(reach_id)
  
  # Convert reachid to indices in shapefile for plotting
  if (length(upstream_reaches) > 0) {
    upstream_indices <- match(upstream_reaches, kuskokwim_shapefile$reachid)
    upstream_indices <- upstream_indices[!is.na(upstream_indices)]
    
    if (length(upstream_indices) > 0) {
      # Get Strahler orders for upstream reaches
      upstream_orders <- strahler_orders[upstream_indices]
      upstream_line_widths <- get_line_width(upstream_orders)
      
      # Plot streams by stream order for proper layering
      unique_upstream_orders <- sort(unique(upstream_orders[!is.na(upstream_orders)]))
      for (order in unique_upstream_orders) {
        order_mask <- which(upstream_orders == order)
        if (length(order_mask) > 0) {
          plot(sf::st_geometry(kuskokwim_shapefile)[upstream_indices[order_mask]], 
               col = color, add = TRUE, lwd = upstream_line_widths[order_mask[1]])
        }
      }
    }
  }
  
  legend_names <- c(legend_names, paste(area_name, "(", length(upstream_reaches), ")"))
  legend_colors <- c(legend_colors, color)
  
  print(paste("Processed", area_name, "- ReachID:", reach_id, "- Upstream reaches:", length(upstream_reaches)))
}

# Add legend
legend("topleft", 
       legend = legend_names, 
       col = legend_colors, 
       lwd = 2, 
       bty = "n",
       cex = 0.7)

# Add stream order legend
if ("Strahler" %in% names(kuskokwim_shapefile)) {
  max_order <- max(strahler_orders, na.rm = TRUE)
  min_order <- max(1, min(strahler_orders, na.rm = TRUE))
  example_orders <- seq(min_order, min(max_order, 7), by = 1)
  example_widths <- get_line_width(example_orders)
  
  legend("bottomright", 
         legend = paste("Strahler", example_orders),
         lwd = example_widths,
         col = "gray40",
         bty = "n",
         cex = 0.6,
         title = "Line Width")
}

dev.off()
print(paste("Comprehensive plot saved:", comprehensive_plot_path))

# Modified FindUpstreamReachID_Kusk function with stream order line widths
FindUpstreamReachID_Kusk <- function(ReachID, create_plot = FALSE, output_dir = "Output/Plots") {
  # Import necessary packages if not already loaded
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")
  
  # Select the reach ID from the shapefile that corresponds to the ReachID value passed to the function argument
  TribStartRID <- kuskokwim_shapefile$rid[which(kuskokwim_shapefile$reachid == ReachID)]
  
  # Create an empty list called tributary_indices
  tributary_indices <- c()
  
  # Find the parent reach ID value for the starting tributary
  StartParent <- KuskoNetwork$parent_s[which(KuskoNetwork$rid == TribStartRID)]
  
  # Find the first child reach ID value for the starting tributary
  StartChild <- KuskoNetwork$child_s[which(KuskoNetwork$rid == TribStartRID)]
  
  # Concatenate these together
  tributary_indices <- c(tributary_indices, StartChild)
  
  # Go find all child reach ID values which have a parent value of the specified starting child value
  ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s == StartChild)]
  
  # As long as there are some "child" tributaries
  while (length(ChildList) > 0) {
    # Add the new child values to the existing tributary_indices
    tributary_indices <- c(tributary_indices, ChildList)
    
    # Find the next set of child values
    ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s %in% ChildList)]
  }
  
  # Find the indices of elements in KuskoNetwork$child_s that match values in tributary_indices
  indices_in_child_s <- match(tributary_indices, KuskoNetwork$child_s)
  
  # Extract the values from KuskoNetwork$rid corresponding to the matched indices
  rid_values <- KuskoNetwork$rid[indices_in_child_s]
  
  # Find the indices of the values obtained in Step 2 within kuskokwim_shapefile$rid
  indices_in_shapefile <- match(rid_values, kuskokwim_shapefile$rid)
  
  # Extract the values from kuskokwim_shapefile$reachid using the indices obtained in Step 3
  upstream_reach_ids <- kuskokwim_shapefile$reachid[indices_in_shapefile]
  
  # Create plot if requested
  if (create_plot) {
    tryCatch({
      # Create output directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # Define plot filename
      plot_filename <- paste0("Kusko_ReachID_", ReachID, "_upstream_StreamOrder.png")
      plot_path <- file.path(output_dir, plot_filename)
      
      # Create a PNG file
      grDevices::png(filename = plot_path, width = 3000, height = 2000, res = 300)
      
      # First plot the basin as background
      plot(sf::st_geometry(kusk_basin), col = "gray80", border = "gray60", 
           main = paste("Kuskokwim Watershed - Upstream Reaches from ID:", ReachID, "(Line Width by Stream Order)"),
           sub = paste("Total upstream reaches:", length(upstream_reach_ids)))
      
      # Plot all streams in light gray with varying line widths based on Strahler order
      strahler_orders <- kuskokwim_shapefile$Strahler
      bg_line_widths <- get_background_line_width(strahler_orders)
      
      # Plot background streams by Strahler order for proper layering
      unique_orders <- sort(unique(strahler_orders[!is.na(strahler_orders)]))
      for (order in unique_orders) {
        order_indices <- which(strahler_orders == order & !is.na(strahler_orders))
        if (length(order_indices) > 0) {
          # Get the appropriate line width for this Strahler order
          order_line_width <- get_background_line_width(order)
          plot(sf::st_geometry(kuskokwim_shapefile)[order_indices], 
               col = "gray60", add = TRUE, lwd = order_line_width)
        }
      }
      
      # Then plot the upstream reaches in red with thicker lines based on Strahler order
      if (length(indices_in_shapefile) > 0) {
        upstream_orders <- strahler_orders[indices_in_shapefile]
        upstream_line_widths <- get_line_width(upstream_orders)
        
        # Plot upstream reaches by stream order for proper layering
        unique_upstream_orders <- sort(unique(upstream_orders[!is.na(upstream_orders)]))
        for (order in unique_upstream_orders) {
          order_mask <- which(upstream_orders == order)
          if (length(order_mask) > 0) {
            plot(sf::st_geometry(kuskokwim_shapefile)[indices_in_shapefile[order_mask]], 
                 col = "red", add = TRUE, lwd = upstream_line_widths[order_mask[1]])
          }
        }
      }
      
      # Add legend
      legend("topleft", legend = c("Upstream reaches", "Other streams"), 
             col = c("red", "gray60"), lwd = c(2, 1), bty = "n")
      
      # Add stream order legend
      if ("Strahler" %in% names(kuskokwim_shapefile)) {
        max_order <- max(strahler_orders, na.rm = TRUE)
        min_order <- max(1, min(strahler_orders, na.rm = TRUE))
        example_orders <- seq(min_order, min(max_order, 7), by = 1)
        example_widths <- get_line_width(example_orders)
        
        legend("bottomright", 
               legend = paste("Strahler", example_orders),
               lwd = example_widths,
               col = "gray40",
               bty = "n",
               cex = 0.7,
               title = "Stream Order")
      }
      
      # Close the PNG device
      grDevices::dev.off()
      
      message(paste("Plot saved to:", plot_path))
    }, error = function(e) {
      warning(paste("Error creating plot:", e$message))
    })
  }
  
  # Return the resulting vector upstream_reach_ids
  return(upstream_reach_ids)
}

# Create individual plots for each area with stream order line widths
for (i in 1:nrow(mgmt_areas)) {
  area_name <- mgmt_areas$Name[i]
  reach_id <- mgmt_areas$reachid[i]
  
  # Use the modified function to create individual plots
  upstream_ids <- FindUpstreamReachID_Kusk(reach_id, create_plot = TRUE, output_dir = plot_output_dir)
}