library(sf)
library(tidyverse)
library(here)
install.packages("RColorBrewer")
library(RColorBrewer)


### This is the script that will be used for mapping 

# For a given year, read in the full assignment matrix for that year
assignments<- read.csv(here("Outputs/Assignment Matrix/Yukon_2017_0.7_basin_assignments.csv"))

# Read in the shapefile for the edges
edges<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp")
background<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

# Merge the shapefile and the data frame into an sf object 
assignments$index <- 1:nrow(assignments)
edges$index <- 1:nrow(edges)

# Step 4: Merge the shapefile and the data frame by the index
merged_data <- edges %>%
  left_join(assignments, by = "index")

# Step 5: Ensure the merged object is an sf object
spatial_assignments <- st_as_sf(merged_data)

tidy_assignments <- assignments %>%
  pivot_longer(cols = starts_with("Q"), 
               names_to = "Time", 
               values_to = "Assignment")

# Create a ggplot map with edges colored by the assignment value for each quartile 
Q1_plot <- ggplot() +
  geom_sf(data = background, fill = "grey90") +
  geom_sf(data = spatial_assignments, aes(color = Q1), lwd = .3) +
  scale_color_gradient(
    low = "grey90",
    high = "firebrick4",
    na.value = "grey90"  # Set the color of 0 values to white
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey70"),  # Set background color to grey
    panel.grid = element_blank()  # Remove grid lines
  )

Q2_plot <- ggplot() +
  geom_sf(data = background, fill = "grey90") +
  geom_sf(data = spatial_assignments, aes(color = Q2), lwd = .3) +
  scale_color_gradient(
    low = "grey90",
    high = "firebrick4",
    na.value = "grey90"  # Set the color of 0 values to white
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey70"),  # Set background color to grey
    panel.grid = element_blank()  # Remove grid lines
  )

Q3_plot <- ggplot() +
  geom_sf(data = background, fill = "grey90") +
  geom_sf(data = spatial_assignments, aes(color = Q3), lwd = .3) +
  scale_color_gradient(
    low = "grey90",
    high = "firebrick4",
    na.value = "grey90"  # Set the color of 0 values to white
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey70"),  # Set background color to grey
    panel.grid = element_blank()  # Remove grid lines
  )

Q4_plot <- ggplot() +
  geom_sf(data = background, fill = "grey90") +
  geom_sf(data = spatial_assignments, aes(color = Q4), lwd = .3) +
  scale_color_gradient(
    low = "grey90",
    high = "firebrick4",
    na.value = "grey90"  # Set the color of 0 values to white
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey70"),  # Set background color to grey
    panel.grid = element_blank()  # Remove grid lines
  )

###################



# Export as a tif 
filename <- paste0("Yukon_2017_0.7_basin_assignments_Q1.tif")
filepath <- file.path(here("Figures", "Maps", filename))
ggsave(filepath, plot = Q1_plot, device = "tiff", width = 9, height = 6)

filename <- paste0("Yukon_2017_0.7_basin_assignments_Q2.tif")
filepath <- file.path(here("Figures", "Maps", filename))
ggsave(filepath, plot = Q2_plot, device = "tiff", width = 9, height = 6)

filename <- paste0("Yukon_2017_0.7_basin_assignments_Q3.tif")
filepath <- file.path(here("Figures", "Maps", filename))
ggsave(filepath, plot = Q3_plot, device = "tiff", width = 9, height = 6)

filename <- paste0("Yukon_2017_0.7_basin_assignments_Q4.tif")
filepath <- file.path(here("Figures", "Maps", filename))
ggsave(filepath, plot = Q4_plot, device = "tiff", width = 9, height = 6)
