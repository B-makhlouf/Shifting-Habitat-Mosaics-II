library(sf)
library(dplyr)
library(ggplot2)

# Load production for a single year (here 2018 based on your path)
kuskokwim_2018 <- read.csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko/2020_Kusko_Assignment_Results.csv")

# Load spatial attributes
shp <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_SlpDistkm.shp")

# Add production to shapefile (assumes same row order)
shp$Assignment_norm <- kuskokwim_2018$assignment_individuals

# Extract variables we need
dat <- shp %>%
  st_drop_geometry() %>%
  transmute(
    slope = Avg_Slop_1,
    dist_up = FlowLen_do,
    prod  = Assignment_norm
  ) %>%
  filter(!is.na(prod))


# Optional: adjust number of bins as needed (currently 30x30)
dat <- dat %>%
  mutate(
    slope_bin = cut(slope, breaks = 30),
    dist_bin = cut(dist_up, breaks = 30)
  )

# Summarize production within bins (this is the key step)
dat_sum <- dat %>%
  group_by(dist_bin, slope_bin) %>%
  summarize(prod = sum(prod, na.rm = TRUE), .groups = "drop")

# Heatmap plot
ggplot(dat_sum, aes(x = dist_bin, y = slope_bin, fill = prod)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Total Production by Distance–Slope Space (2018)",
    x = "Distance Upstream (km, binned)",
    y = "Channel Slope (binned)",
    fill = "Production"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
