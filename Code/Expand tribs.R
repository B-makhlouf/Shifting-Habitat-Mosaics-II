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



