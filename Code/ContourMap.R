library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(tidyr)
library(viridis)
library(here)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

PROD_THRESHOLD <- 0.9

cat("\n=== Running analysis with production threshold:", PROD_THRESHOLD, "===\n")

# ==============================================================================
# LOAD SPATIAL DATA
# ==============================================================================

kusko_shp <- st_read(here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp")) %>%
  st_drop_geometry()

yukon_shp <- st_read(here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges.shp")) %>%
  st_drop_geometry()

# ==============================================================================
# PROCESS KUSKOKWIM DATA
# ==============================================================================

kusko_years <- 2017:2021
kusko_list <- list()

for(yr in kusko_years) {
  prod_data <- read.csv(here("Outputs", "ProductionData", paste0(yr, "_Kusko_Assignment_Results.csv")))
  
  kusko_list[[as.character(yr)]] <- data.frame(
    river = "Kusko",
    year = factor(yr),
    prod = prod_data$assignment_norm,
    slope = kusko_shp$Channel_sl,
    dist_upstream = kusko_shp$Upstream_d,
    snap_temp = kusko_shp[[paste0("SnapTp", yr)]],
    snap_precip = kusko_shp[[paste0("SnapPr", yr)]]
  ) %>%
    filter(prod >= PROD_THRESHOLD, !is.na(prod), !is.na(slope), 
           !is.na(dist_upstream), !is.na(snap_temp), !is.na(snap_precip))
}

kusko_data <- bind_rows(kusko_list)

# ==============================================================================
# PROCESS YUKON DATA
# ==============================================================================

yukon_years <- c(2015, 2016, 2018, 2021)
yukon_list <- list()

for(yr in yukon_years) {
  prod_data <- read.csv(here("Outputs", "ProductionData", paste0(yr, "_Yukon_Assignment_Results.csv")))
  
  yukon_list[[as.character(yr)]] <- data.frame(
    river = "Yukon",
    year = factor(yr),
    prod = prod_data$assignment_norm,
    slope = yukon_shp$Channel_sl,
    dist_upstream = yukon_shp$DistUpstre,
    snap_temp = yukon_shp[[paste0("SnapTp", yr)]],
    snap_precip = yukon_shp[[paste0("SnapPr", yr)]]
  ) %>%
    filter(prod >= PROD_THRESHOLD, !is.na(prod), !is.na(slope), 
           !is.na(dist_upstream), !is.na(snap_temp), !is.na(snap_precip))
}

yukon_data <- bind_rows(yukon_list)

# ==============================================================================
# COMBINE DATA
# ==============================================================================

all_data <- bind_rows(kusko_data, yukon_data)

cat("\nData summary:\n")
cat("  Kuskokwim:", nrow(kusko_data), "rows\n")
cat("  Yukon:", nrow(yukon_data), "rows\n")
cat("  Total:", nrow(all_data), "rows\n")

# ==============================================================================
# PLOTTING FUNCTION
# ==============================================================================

create_contour_plot <- function(data, x_var, y_var, x_label, y_label, 
                                title, x_limits = NULL) {
  
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(size = 1.3, alpha = 0.35, color = "steelblue") +
    stat_density_2d(
      aes(fill = after_stat(level / max(level))),
      geom = "polygon",
      color = NA,
      alpha = 0.45,
      contour_var = "ndensity"
    ) +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    facet_wrap(~year, nrow = 1) +
    labs(title = title, x = x_label, y = y_label, fill = "Rel. density") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold")
    )
  
  if(!is.null(x_limits)) p <- p + scale_x_continuous(limits = x_limits)
  
  return(p)
}

# ==============================================================================
# GENERATE PLOTS: KUSKOKWIM
# ==============================================================================

kusko_subset <- all_data %>% filter(river == "Kusko")

# Plot 1: Distance upstream vs Slope
p1 <- create_contour_plot(
  data = kusko_subset,
  x_var = "dist_upstream",
  y_var = "slope",
  x_label = "Distance upstream",
  y_label = "Channel slope",
  title = paste0("Kuskokwim - Highest likelihood habitat (2017–2021) [threshold = ", PROD_THRESHOLD, "]")
)
print(p1)
ggsave(here("Figures", "ContourMaps", paste0("Kusko_Slope_Upstream_", PROD_THRESHOLD, ".png")),
       p1, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 2: Precipitation vs Temperature
p2 <- create_contour_plot(
  data = kusko_subset %>% filter(snap_precip <= 60),
  x_var = "snap_precip",
  y_var = "snap_temp",
  x_label = "SnapPr (Precipitation)",
  y_label = "SnapTp (Temperature)",
  title = paste0("Kuskokwim - Precipitation vs Temperature (2017–2021) [threshold = ", PROD_THRESHOLD, "]"),
  x_limits = c(0, 60)
)
print(p2)
ggsave(here("Figures", "ContourMaps", paste0("Kusko_SnapPr_SnapTp_", PROD_THRESHOLD, ".png")),
       p2, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 3: Temperature vs Slope
p3 <- create_contour_plot(
  data = kusko_subset,
  x_var = "snap_temp",
  y_var = "slope",
  x_label = "SnapTp (Temperature)",
  y_label = "Channel slope",
  title = paste0("Kuskokwim - Temperature vs Slope (2017–2021) [threshold = ", PROD_THRESHOLD, "]")
)
print(p3)
ggsave(here("Figures", "ContourMaps", paste0("Kusko_SnapTp_Slope_", PROD_THRESHOLD, ".png")),
       p3, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 4: Temperature vs Distance upstream
p4 <- create_contour_plot(
  data = kusko_subset,
  x_var = "snap_temp",
  y_var = "dist_upstream",
  x_label = "SnapTp (Temperature)",
  y_label = "Distance upstream",
  title = paste0("Kuskokwim - Temperature vs Distance Upstream (2017–2021) [threshold = ", PROD_THRESHOLD, "]")
)
print(p4)
ggsave(here("Figures", "ContourMaps", paste0("Kusko_SnapTp_Distance_", PROD_THRESHOLD, ".png")),
       p4, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 5: Precipitation vs Slope
p5 <- create_contour_plot(
  data = kusko_subset %>% filter(snap_precip <= 60),
  x_var = "snap_precip",
  y_var = "slope",
  x_label = "SnapPr (Precipitation)",
  y_label = "Channel slope",
  title = paste0("Kuskokwim - Precipitation vs Slope (2017–2021) [threshold = ", PROD_THRESHOLD, "]"),
  x_limits = c(0, 60)
)
print(p5)
ggsave(here("Figures", "ContourMaps", paste0("Kusko_SnapPr_Slope_", PROD_THRESHOLD, ".png")),
       p5, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 6: Precipitation vs Distance upstream
p6 <- create_contour_plot(
  data = kusko_subset %>% filter(snap_precip <= 60),
  x_var = "snap_precip",
  y_var = "dist_upstream",
  x_label = "SnapPr (Precipitation)",
  y_label = "Distance upstream",
  title = paste0("Kuskokwim - Precipitation vs Distance Upstream (2017–2021) [threshold = ", PROD_THRESHOLD, "]"),
  x_limits = c(0, 60)
)
print(p6)
ggsave(here("Figures", "ContourMaps", paste0("Kusko_SnapPr_Distance_", PROD_THRESHOLD, ".png")),
       p6, width = 12, height = 4, dpi = 300, bg = "white")

# ==============================================================================
# GENERATE PLOTS: YUKON
# ==============================================================================

yukon_subset <- all_data %>% filter(river == "Yukon")

# Plot 1: Distance upstream vs Slope
p7 <- create_contour_plot(
  data = yukon_subset,
  x_var = "dist_upstream",
  y_var = "slope",
  x_label = "Distance upstream",
  y_label = "Channel slope",
  title = paste0("Yukon - Highest likelihood habitat (2015, 2016, 2018, 2021) [threshold = ", PROD_THRESHOLD, "]")
)
print(p7)
ggsave(here("Figures", "ContourMaps", paste0("Yukon_Slope_Upstream_", PROD_THRESHOLD, ".png")),
       p7, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 2: Precipitation vs Temperature
p8 <- create_contour_plot(
  data = yukon_subset %>% filter(snap_precip <= 60),
  x_var = "snap_precip",
  y_var = "snap_temp",
  x_label = "SnapPr (Precipitation)",
  y_label = "SnapTp (Temperature)",
  title = paste0("Yukon - Precipitation vs Temperature (2015, 2016, 2018, 2021) [threshold = ", PROD_THRESHOLD, "]"),
  x_limits = c(0, 60)
)
print(p8)
ggsave(here("Figures", "ContourMaps", paste0("Yukon_SnapPr_SnapTp_", PROD_THRESHOLD, ".png")),
       p8, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 3: Temperature vs Slope
p9 <- create_contour_plot(
  data = yukon_subset,
  x_var = "snap_temp",
  y_var = "slope",
  x_label = "SnapTp (Temperature)",
  y_label = "Channel slope",
  title = paste0("Yukon - Temperature vs Slope (2015, 2016, 2018, 2021) [threshold = ", PROD_THRESHOLD, "]")
)
print(p9)
ggsave(here("Figures", "ContourMaps", paste0("Yukon_SnapTp_Slope_", PROD_THRESHOLD, ".png")),
       p9, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 4: Temperature vs Distance upstream
p10 <- create_contour_plot(
  data = yukon_subset,
  x_var = "snap_temp",
  y_var = "dist_upstream",
  x_label = "SnapTp (Temperature)",
  y_label = "Distance upstream",
  title = paste0("Yukon - Temperature vs Distance Upstream (2015, 2016, 2018, 2021) [threshold = ", PROD_THRESHOLD, "]")
)
print(p10)
ggsave(here("Figures", "ContourMaps", paste0("Yukon_SnapTp_Distance_", PROD_THRESHOLD, ".png")),
       p10, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 5: Precipitation vs Slope
p11 <- create_contour_plot(
  data = yukon_subset %>% filter(snap_precip <= 60),
  x_var = "snap_precip",
  y_var = "slope",
  x_label = "SnapPr (Precipitation)",
  y_label = "Channel slope",
  title = paste0("Yukon - Precipitation vs Slope (2015, 2016, 2018, 2021) [threshold = ", PROD_THRESHOLD, "]"),
  x_limits = c(0, 60)
)
print(p11)
ggsave(here("Figures", "ContourMaps", paste0("Yukon_SnapPr_Slope_", PROD_THRESHOLD, ".png")),
       p11, width = 12, height = 4, dpi = 300, bg = "white")

# Plot 6: Precipitation vs Distance upstream
p12 <- create_contour_plot(
  data = yukon_subset %>% filter(snap_precip <= 60),
  x_var = "snap_precip",
  y_var = "dist_upstream",
  x_label = "SnapPr (Precipitation)",
  y_label = "Distance upstream",
  title = paste0("Yukon - Precipitation vs Distance Upstream (2015, 2016, 2018, 2021) [threshold = ", PROD_THRESHOLD, "]"),
  x_limits = c(0, 60)
)
print(p12)
ggsave(here("Figures", "ContourMaps", paste0("Yukon_SnapPr_Distance_", PROD_THRESHOLD, ".png")),
       p12, width = 12, height = 4, dpi = 300, bg = "white")

# ==============================================================================
# COMPLETE
# ==============================================================================

cat("\n=== Analysis Complete ===\n")
cat("Production threshold:", PROD_THRESHOLD, "\n")
cat("Created 12 contour maps\n")