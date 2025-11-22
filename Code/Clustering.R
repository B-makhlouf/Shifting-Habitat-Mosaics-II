library(sf)
library(ggplot2)
library(tidyverse)

# ============================================================================
# Read in the shapefile
# ============================================================================
shapefile <- st_read("/Users/benjaminmakhlouf/Spatial Data/UY_clusterlines.shp")

# Check the data
head(shapefile)
str(shapefile)

# ============================================================================
# Prepare data for k-means clustering
# ============================================================================

# Extract centroids and iso_pred values
data_cluster <- shapefile %>%
  mutate(
    x = st_coordinates(st_centroid(.))[, 1],
    y = st_coordinates(st_centroid(.))[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(x, y, iso_pred)

# Check for any missing values
print(paste("Missing isotope values:", sum(is.na(data_cluster$iso_pred))))
print(paste("Missing coordinates:", sum(is.na(data_cluster$x))))

# Remove any rows with missing values if needed
data_cluster <- na.omit(data_cluster)

# ============================================================================
# Scale the data for clustering
# ============================================================================
data_scaled <- data_cluster %>%
  mutate(
    x_scaled = scale(x)[, 1],
    y_scaled = scale(y)[, 1],
    iso_scaled = scale(iso_pred)[, 1]
  )

# ============================================================================
# Create weighted clustering data
# ============================================================================
# Adjust these weights based on how much you want spatial vs. isotope to matter
spatial_weight <- 0.3   # Higher = prioritize spatial proximity
isotope_weight <- 0.7   # Higher = prioritize isotope similarity

clustering_data <- data_scaled %>%
  select(x_scaled, y_scaled, iso_scaled) %>%
  mutate(
    x_scaled = x_scaled * spatial_weight,
    y_scaled = y_scaled * spatial_weight,
    iso_scaled = iso_scaled * isotope_weight
  ) %>%
  as.matrix()

# ============================================================================
# Run k-means clustering
# ============================================================================
# Adjust 'centers' based on how many clusters/groups you want
# You can try different values: 5, 10, 15, 20, etc.
set.seed(42)  # For reproducibility

k_value <- 5  # Adjust this based on desired number of groups
kmeans_result <- kmeans(clustering_data, centers = k_value, iter.max = 50, nstart = 10)

# Add cluster assignments back to original shapefile
shapefile$cluster <- kmeans_result$cluster

# Print cluster summary
print(table(shapefile$cluster))

# ============================================================================
# Create the map visualization
# ============================================================================

# Create directory if it doesn't exist
dir.create("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Maps/ClusteringMaps", 
           showWarnings = FALSE, recursive = TRUE)

# Create the plot
# For line features, use 'color' aesthetic instead of 'fill'
p <- ggplot(data = shapefile) +
  geom_sf(aes(color = factor(cluster)), size = 0.8, show.legend = TRUE) +
  scale_color_viridis_d(name = "Isotope Group", option = "turbo") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(title = "Spatially-Constrained Isotope Clustering")

# Save as PNG
png_path <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Maps/ClusteringMaps/isotope_clusters.png"
ggsave(png_path, plot = p, width = 14, height = 10, dpi = 300)

print(paste("Map saved to:", png_path))

# Optional: Also save as PDF for higher quality
pdf_path <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Maps/ClusteringMaps/isotope_clusters.pdf"
ggsave(pdf_path, plot = p, width = 14, height = 10)

print(paste("PDF also saved to:", pdf_path))

# ============================================================================
# Save the clustered shapefile (optional)
# ============================================================================
output_shp <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Maps/ClusteringMaps/UY_clusterlines_grouped.shp"
st_write(shapefile, output_shp, append = FALSE)

print(paste("Clustered shapefile saved to:", output_shp))

# ============================================================================
# Summary statistics
# ============================================================================
summary_stats <- shapefile %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_isotope = mean(iso_pred, na.rm = TRUE),
    sd_isotope = sd(iso_pred, na.rm = TRUE),
    min_isotope = min(iso_pred, na.rm = TRUE),
    max_isotope = max(iso_pred, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

# Save summary to CSV
csv_path <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Maps/ClusteringMaps/cluster_summary.csv"
write.csv(summary_stats, csv_path, row.names = FALSE)

print(paste("Cluster summary saved to:", csv_path))

# ============================================================================
# Create boxplot of isotope values by cluster
# ============================================================================
boxplot_data <- shapefile %>%
  st_drop_geometry() %>%
  select(cluster, iso_pred)

p_boxplot <- ggplot(boxplot_data, aes(x = factor(cluster), y = iso_pred, fill = factor(cluster))) +
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Distribution of Isotope Values by Cluster",
    x = "Cluster",
    y = "Isotope Value (iso_pred)",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# Save boxplot
boxplot_path <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Maps/ClusteringMaps/isotope_boxplot.png"
ggsave(boxplot_path, plot = p_boxplot, width = 12, height = 8, dpi = 300)

print(paste("Boxplot saved to:", boxplot_path))