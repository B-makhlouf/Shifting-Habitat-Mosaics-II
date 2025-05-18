################################################################################
################ Kuskokwim 
################################################################################

#' Find upstream reaches and optionally create a plot
#' 
#' @param ReachID Reach ID value for which upstream reaches need to be determined
#' @param create_plot Logical: whether to create and save a plot
#' @param output_dir Character: directory to save plot (if create_plot is TRUE)
#' @return Vector of upstream reach IDs
FindUpstreamReachID_Kusk <- function(ReachID, create_plot = FALSE, output_dir = "Output/Plots") {
  # Import necessary packages if not already loaded
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")
  
  # Load the required shapefiles - we'll keep these lines exactly as they are in your script
  kuskokwim_shapefile <- sf::st_read('/Users/benjaminmakhlouf/Spatial Data/Shapefiles/AYK Shapefiles/kusko_edges_20210717.shp', quiet = TRUE)
  
  # For plotting, we need the basin too
  if(create_plot) {
    kusk_basin <- sf::st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp", quiet = TRUE)
  }
  
  # Load nodes for all tributaries
  KuskoNodes <- utils::read.csv("/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Data/Upstream_reaches/kusko/kusko_noderelationships.csv")
  
  # Rename fromnode to child_s and tonode to parent_s
  KuskoNetwork <- KuskoNodes %>% dplyr::rename(child_s = fromnode, parent_s = tonode)
  
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
      plot_filename <- paste0("Kusko_ReachID_", ReachID, "_upstream.png")
      plot_path <- file.path(output_dir, plot_filename)
      
      # Create a PNG file
      grDevices::png(filename = plot_path, width = 3000, height = 2000, res = 300)
      
      # First plot the basin as background
      plot(sf::st_geometry(kusk_basin), col = "gray80", border = "gray60", 
           main = paste("Kuskokwim Watershed - Upstream Reaches from ID:", ReachID),
           sub = paste("Total upstream reaches:", length(upstream_reach_ids)))
      
      # Then plot all streams in light gray
      plot(sf::st_geometry(kuskokwim_shapefile), col = "gray60", add = TRUE, lwd = 0.5)
      
      # Then plot the upstream reaches in red with thicker lines
      if (length(indices_in_shapefile) > 0) {
        plot(sf::st_geometry(kuskokwim_shapefile)[indices_in_shapefile], 
             col = "red", add = TRUE, lwd = 2)
      }
      
      # Add legend
      legend("topleft", legend = c("Upstream reaches", "Other streams"), 
             col = c("red", "gray60"), lwd = c(2, 0.5), bty = "n")
      
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

#' Create plot for upstream reaches and return the reach IDs
#' 
#' @param ReachID Reach ID value for which upstream reaches need to be determined
#' @param output_dir Directory to save the plot
#' @return Vector of upstream reach IDs
plot_upstream_reaches <- function(ReachID, output_dir = "Output/Plots") {
  FindUpstreamReachID_Kusk(ReachID, create_plot = TRUE, output_dir = output_dir)
}

upstream_ids <- FindUpstreamReachID_Kusk(7293, create_plot = TRUE)

