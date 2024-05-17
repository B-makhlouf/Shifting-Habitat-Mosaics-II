# Load required libraries
library(sf)
library(dplyr)
library(tidyverse)
install.packages("tidygraph")
library(tidygraph)
install.packages("igraph")
library(igraph)

# Read the shapefile
yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp")

# Ensure the geometry is valid
yuk_edges <- st_make_valid(yuk_edges)

# Create a function to generate common IDs for connected lines with the same Str_Order and Stream_Order
generate_common_id <- function(edges) {
  # Split the edges by the Str_Order attribute
  edges_list <- split(edges, edges$Str_Order)
  
  # Loop through each Str_Order group
  for (str_order in names(edges_list)) {
    edges_subset <- edges_list[[str_order]]
    
    # Create an adjacency list of touching segments
    adj_list <- st_touches(edges_subset)
    
    # Convert the adjacency list to a graph
    graph <- graph_from_adj_list(adj_list)
    
    # Find connected components
    components <- components(graph)
    
    # Assign a common ID to each component
    edges_subset$common_id <- paste(str_order, components$membership, sep="_")
    
    # Replace the subset in the original edges object
    edges_list[[str_order]] <- edges_subset
  }
  
  # Combine all edges back into a single sf object
  consolidated_edges <- do.call(rbind, edges_list)
  
  return(consolidated_edges)
}

# Apply the function to generate common IDs
yuk_edges_with_id <- generate_common_id(yuk_edges)

# Optionally, save the result to a new shapefile
st_write(yuk_edges_with_id, "/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/consolidated_tribs.shp")



