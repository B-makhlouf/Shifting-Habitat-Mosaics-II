library(readxl)
library(sf)
library(dplyr)
library(here)
library(tidyverse)
library(ggplot2)
library(patchwork)

# ---------------------------
# 1. Read base input data
# ---------------------------

# Shapefile with line geometries (same across years)
shp <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"))

# Escapement data
escapement <- read_xlsx(here("Data","AYKEscapement.xlsx"))

# Spatial scale polygons
sb5 <- st_read(here("Data","Spatial Data","SubBasinPolygons","Kusko_SubWs5.shp"))
sb6 <- st_read(here("Data","Spatial Data","SubBasinPolygons","Kusko_SubWs6.shp"))
sb7 <- st_read(here("Data","Spatial Data","SubBasinPolygons","Kusko_SubWs7.shp"))

# Ensure same CRS
sb6 <- st_transform(sb6, st_crs(sb5))
sb7 <- st_transform(sb7, st_crs(sb5))

