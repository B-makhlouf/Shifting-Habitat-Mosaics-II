################################################################################
# YUKON HUC PRODUCTION — STREAM LENGTH NORMALIZED (BEFORE COLOR SCALING)
# Normalizes production by km of stream in each HUC, then scales 0–1 for colors
################################################################################

library(tidyverse)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# -----------------------
# Read spatial data
# -----------------------
huc <- st_read("/Users/benjaminmakhlouf/Spatial Data/SMH2/YkKkHuc7.shp")
edges <- st_read("/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp")

if (st_crs(edges) != st_crs(huc)) {
  edges <- st_transform(edges, st_crs(huc))
}

sf::sf_use_s2(FALSE)

# -----------------------
# Settings
# -----------------------
YUKON_YEARS <- c(2015, 2016, 2018, 2021)
DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual/HUC"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# STEP 1 — Calculate stream length per HUC (one time)
# -----------------------

# Add stream length to edges
edges$stream_length_km <- st_length(edges) %>% as.numeric() / 1000

# Find HUC for each edge (using st_intersects)
edges_in_huc <- st_join(edges, huc, join = st_intersects)

# Calculate total stream length per HUC
huc_stream_lengths <- edges_in_huc %>%
  st_drop_geometry() %>%
  group_by(HYBAS_ID) %>%
  summarise(
    total_stream_length_km = sum(stream_length_km, na.rm = TRUE),
    n_reaches = n(),
    .groups = "drop"
  )

cat("HUC stream length summary:\n")
print(summary(huc_stream_lengths$total_stream_length_km))

# -----------------------
# STEP 2 — Production by year (WITH stream length normalization)
# -----------------------

all_huc_data <- list()

for (year in YUKON_YEARS) {
  
  prod_files <- list.files(DATA_DIR,
                           pattern = paste0(year, "_Yukon_Assignment_Results.*\\.csv$"),
                           full.names = TRUE)
  if (length(prod_files) == 0) next
  
  prod <- read.csv(prod_files[1])
  
  edges_prod <- edges %>%
    left_join(prod %>% select(reachid, assignment_rescale),
              by = "reachid")
  
  edges_prod$assignment_rescale[is.na(edges_prod$assignment_rescale)] <- 0
  
  edges_in_huc <- st_join(edges_prod, huc, join = st_intersects)
  
  # Calculate TOTAL production and stream length per HUC for this year
  huc_prod <- edges_in_huc %>%
    st_drop_geometry() %>%
    group_by(HYBAS_ID) %>%
    summarise(
      total_prod = sum(assignment_rescale, na.rm = TRUE),
      stream_length_km = sum(stream_length_km, na.rm = TRUE),
      n_reaches_year = n(),
      .groups = "drop"
    ) %>%
    # NORMALIZE PRODUCTION BY STREAM LENGTH (per km)
    mutate(
      prod_per_km = total_prod / stream_length_km
    )
  
  # Attach to polygons
  huc_year <- huc %>%
    left_join(huc_prod, by = "HYBAS_ID") %>%
    mutate(
      year = year,
      total_prod = replace_na(total_prod, 0),
      stream_length_km = replace_na(stream_length_km, 0),
      prod_per_km = replace_na(prod_per_km, 0)
    )
  
  # NOW scale 0–1 AFTER normalization by stream length
  huc_year <- huc_year %>%
    mutate(
      max_prod_per_km = max(prod_per_km, na.rm = TRUE)
    ) %>%
    mutate(
      prod_for_plot = ifelse(max_prod_per_km > 0, 
                             prod_per_km / max_prod_per_km, 
                             0)
    )
  
  all_huc_data[[as.character(year)]] <- huc_year
  
  # Print summary for this year
  cat("\n--- Year:", year, "---\n")
  cat("Production per km summary:\n")
  print(summary(huc_year$prod_per_km))
  cat("Max production per km:", max(huc_year$prod_per_km, na.rm = TRUE), "\n")
}

all_huc_combined <- bind_rows(all_huc_data)

# -----------------------
# STEP 3 — Make maps (0 = white, scale = 0–1 AFTER stream normalization)
# -----------------------

plots <- list()

for (year in YUKON_YEARS) {
  
  huc_year <- all_huc_combined %>% filter(year == !!year)
  
  p <- ggplot() +
    geom_sf(data = huc, fill = "#fbfbfb", color = "#dcdcdc", linewidth = 0.35) +
    
    geom_sf(data = huc_year,
            aes(fill = prod_for_plot),
            color = "#ffffff",
            linewidth = 0.28) +
    
    geom_sf(data = huc, fill = NA, color = "#2b2b2b", linewidth = 0.18) +
    
    scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      limits = c(0, 1),              # Forced 0–1 scale (AFTER stream normalization)
      values = seq(0, 1, length.out = 11),
      na.value = "#ffffff",
      name = "Production per km\n(relative)",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 0.8
      )
    ) +
    
    coord_sf(expand = FALSE) +
    theme_minimal(base_family = "Helvetica", base_size = 10) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = "gray70"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("Year: ", year),
      subtitle = "Production per km of stream (0 = white; normalized within year)"
    )
  
  plots[[as.character(year)]] <- p
}

# -----------------------
# STEP 4 — Save individual maps
# -----------------------
for (year in YUKON_YEARS) {
  out_png <- file.path(output_dir, paste0("Yukon_HUC_Production_", year, "_StreamNormalized.png"))
  ggsave(out_png, plots[[as.character(year)]],
         width = 11, height = 9, dpi = 300, bg = "white")
  cat("✓ Saved:", basename(out_png), "\n")
}

# -----------------------
# STEP 5 — Multi-year comparison
# -----------------------
comparison_plot <- wrap_plots(plots, nrow = 1) +
  plot_annotation(
    title = "Yukon HUC Production (Stream Length Normalized)",
    subtitle = "Production per km of stream (0 = white, 1 = darkest red)"
  )

ggsave(file.path(output_dir, "Yukon_HUC_Production_MultiYear_StreamNormalized.png"),
       comparison_plot, width = 20, height = 9, dpi = 300, bg = "white")

cat("\n✓ Saved: Yukon_HUC_Production_MultiYear_StreamNormalized.png\n")

# -----------------------
# STEP 6 — Export summary table
# -----------------------
summary_df <- all_huc_combined %>%
  st_drop_geometry() %>%
  select(year, HYBAS_ID, total_prod, stream_length_km, prod_per_km, prod_for_plot) %>%
  arrange(year, desc(prod_per_km))

write_csv(summary_df, file.path(output_dir, "Yukon_HUC_Production_StreamNormalized_Summary.csv"))

cat("\n✓ Saved: Yukon_HUC_Production_StreamNormalized_Summary.csv\n")

# Show which HUC has prod_for_plot = 1.0 (maximum) in each year
cat("\n=== Maximum production per km HUC by year ===\n")
max_by_year <- all_huc_combined %>%
  st_drop_geometry() %>%
  filter(prod_for_plot == 1.0) %>%
  select(year, HYBAS_ID, prod_per_km, prod_for_plot) %>%
  arrange(year)
print(max_by_year)

cat("\nTop 5 HUCs by production per km (all years combined):\n")
print(summary_df %>% group_by(HYBAS_ID) %>% 
        summarise(mean_prod_per_km = mean(prod_per_km), .groups = 'drop') %>%
        arrange(desc(mean_prod_per_km)) %>%
        head(5))

################################################################################
# END
################################################################################