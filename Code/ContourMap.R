library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(tidyr)
library(viridis)

# Load static spatial data once
shp <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_SlpDistkm.shp") %>% 
  st_drop_geometry()

# Collect data across years
all_data <- list()

for(yr in 2017:2021) {
  
  df <- read.csv(
    paste0("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko/", yr, "_Kusko_Assignment_Results.csv")
  )
  
  dat <- data.frame(
    slope = shp$Avg_Slop_1,
    dist_up = shp$FlowLen_do,
    prod = df$assignment_norm,
    year = factor(yr)
  ) %>%
    filter(!is.na(prod),
           slope > 0,
           slope <= 2.5,
           prod >= 0.7)
  
  all_data[[as.character(yr)]] <- dat
}

combined_data <- bind_rows(all_data)

# Plot with per-facet normalized density
p <- ggplot(combined_data, aes(x = dist_up, y = slope)) +
  geom_point(size = 1.3, alpha = 0.35, color = "steelblue") +
  stat_density_2d(
    aes(fill = after_stat(level / max(level))),   # normalize within panel
    geom = "polygon",
    color = NA,
    alpha = 0.45,
    contour_var = "ndensity"
  ) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(
    title = "Highest likelihood habitat (2017–2021)",
    fill = "Rel. density"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

print(p)
