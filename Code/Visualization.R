################################################################################
# YUKON HUC PRODUCTION - NORMALIZED BY STREAM LENGTH
# FIXED: Continuous color scale consistent across all years
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

# Ensure same CRS
if (st_crs(edges) != st_crs(huc)) {
  edges <- st_transform(edges, st_crs(huc))
}

sf::sf_use_s2(FALSE)

# -----------------------
# User settings
# -----------------------
YUKON_YEARS <- c(2015, 2016, 2018, 2021)
DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual/HUC"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# STEP 1: Compute per-reach lengths and stream length per HUC
# -----------------------
cat("=== STEP 1: Calculating stream length by HUC ===\n")

edges <- edges %>%
  mutate(stream_length_m = as.numeric(st_length(geometry)))

edges_in_huc_temp <- st_join(edges, huc, join = st_intersects)

huc_stream_length <- edges_in_huc_temp %>%
  st_drop_geometry() %>%
  group_by(HYBAS_ID) %>%
  summarize(
    total_stream_length_m = sum(stream_length_m, na.rm = TRUE),
    n_reaches = n(),
    .groups = "drop"
  ) %>%
  mutate(total_stream_length_km = total_stream_length_m / 1000)

cat("✓ Calculated stream length for", nrow(huc_stream_length), "HUCs\n\n")

# -----------------------
# STEP 2: Process each year's production and normalize by stream length
# -----------------------
cat("=== STEP 2: Processing annual production data ===\n")

all_huc_data <- list()

for (year in YUKON_YEARS) {
  cat("\nProcessing", year, "...\n")
  prod_files <- list.files(DATA_DIR, pattern = paste0(year, "_Yukon_Assignment_Results.*\\.csv$"), full.names = TRUE)
  if (length(prod_files) == 0) {
    cat("  ✗ No assignment results file found for", year, "\n")
    next
  }
  prod_file <- prod_files[1]
  cat("  Reading:", basename(prod_file), "\n")
  prod <- read.csv(prod_file)
  
  # Join production to reaches
  edges_prod <- edges %>%
    left_join(prod %>% select(reachid, assignment_rescale), by = "reachid")
  
  # Replace NA with 0
  edges_prod$assignment_rescale[is.na(edges_prod$assignment_rescale)] <- 0
  
  # Spatial join to HUC polygons
  edges_in_huc <- st_join(edges_prod, huc, join = st_intersects)
  
  # Sum production by HUC
  huc_prod <- edges_in_huc %>%
    st_drop_geometry() %>%
    group_by(HYBAS_ID) %>%
    summarize(
      total_prod = sum(assignment_rescale, na.rm = TRUE),
      n_reaches_year = n(),
      .groups = "drop"
    )
  
  # Attach to HUC polygons and compute per-km production
  huc_year <- huc %>%
    left_join(huc_prod, by = "HYBAS_ID") %>%
    left_join(huc_stream_length, by = "HYBAS_ID") %>%
    mutate(
      year = year,
      total_prod = replace_na(total_prod, 0),
      n_reaches_year = replace_na(n_reaches_year, 0),
      total_stream_length_km = replace_na(total_stream_length_km, 0),
      prod_per_km = ifelse(total_stream_length_km > 0, total_prod / total_stream_length_km, 0)
    ) %>%
    mutate(
      prod_per_km_normalized = prod_per_km / sum(prod_per_km, na.rm = TRUE),
      prod_per_km_scaled = prod_per_km_normalized / max(prod_per_km_normalized, na.rm = TRUE)
    )
  
  all_huc_data[[as.character(year)]] <- huc_year
  cat("  ✓ Processed:", nrow(huc_year), "HUCs\n")
}

all_huc_combined <- bind_rows(all_huc_data)

# -----------------------
# STEP 3: Color palette
# -----------------------
cat("\n=== STEP 3: Setting up color scale ===\n")

palette <- brewer.pal(9, "YlOrRd")

cat("✓ Color palette configured.\n\n")

# -----------------------
# STEP 4: Create maps with continuous color scale
# -----------------------
cat("=== STEP 4: Creating maps ===\n")

plots <- list()

for (year in YUKON_YEARS) {
  huc_year <- all_huc_combined %>% filter(year == !!year)
  
  p <- ggplot() +
    # Base layer: all HUCs light gray
    geom_sf(data = huc, fill = "#fbfbfb", color = "#dcdcdc", linewidth = 0.35) +
    
    # Main data layer with continuous color based on prod_per_km_scaled
    geom_sf(data = huc_year, 
            aes(fill = prod_per_km_scaled), 
            color = "#ffffff", 
            linewidth = 0.28) +
    
    # Border layer
    geom_sf(data = huc, fill = NA, color = "#2b2b2b", linewidth = 0.18) +
    
    # Continuous color scale from 0 to 1
    scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      limits = c(0, 1),
      name = "Relative posterior density",
      na.value = "#ffffff",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 0.8,
        label = TRUE
      )
    ) +
    
    coord_sf(expand = FALSE) +
    
    theme_minimal(base_family = "Helvetica", base_size = 10) +
    theme(
      # Title and subtitle
      plot.title = element_text(
        size = 14, 
        face = "bold", 
        hjust = 0.5, 
        margin = margin(t = 5, b = 3)
      ),
      plot.subtitle = element_text(
        size = 9, 
        color = "#555555", 
        hjust = 0.5, 
        margin = margin(b = 5)
      ),
      
      # Legend
      legend.position = "bottom",
      legend.background = element_rect(
        fill = "white", 
        color = "#888888", 
        linewidth = 0.5
      ),
      legend.margin = margin(8, 8, 8, 8),
      legend.title = element_text(
        size = 10, 
        face = "bold",
        margin = margin(b = 6)
      ),
      legend.text = element_text(size = 9),
      
      # Panel and plot background
      plot.background = element_rect(fill = "#f6f6f6", color = NA),
      panel.background = element_rect(fill = "#f6f6f6", color = NA),
      panel.border = element_rect(color = "#e0e0e0", fill = NA, linewidth = 0.5),
      panel.grid = element_blank(),
      
      # Axes
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      # Margins
      plot.margin = margin(5, 5, 5, 5)
    ) +
    
    labs(
      title = paste0("Year: ", year),
      subtitle = "Yukon HUC Production"
    )
  
  plots[[as.character(year)]] <- p
  cat("  ✓ Created map for", year, "\n")
}

# -----------------------
# STEP 5: Save individual maps
# -----------------------
cat("\n=== STEP 5: Saving maps ===\n")

for (year in YUKON_YEARS) {
  out_png <- file.path(output_dir, paste0("Yukon_HUC_Production_", year, ".png"))
  ggsave(out_png, 
         plots[[as.character(year)]], 
         width = 11, 
         height = 9, 
         dpi = 300, 
         bg = "white")
  cat("✓ Saved:", out_png, "\n")
}

# -----------------------
# STEP 6: Multi-year comparison
# -----------------------
cat("\n=== STEP 6: Creating multi-year comparison ===\n")

comparison_plot <- wrap_plots(plots, nrow = 1) +
  plot_annotation(
    title = "Yukon HUC Production - Normalized by Stream Network Length",
    subtitle = "Production per km across years (0-1 continuous scale)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0.5, margin = margin(b = 10))
    )
  )

comparison_file <- file.path(output_dir, "Yukon_HUC_Production_MultiYear_Comparison.png")
ggsave(comparison_file, 
       comparison_plot, 
       width = 20, 
       height = 9, 
       dpi = 300, 
       bg = "white")
cat("✓ Saved comparison:", comparison_file, "\n")

# -----------------------
# STEP 7: Summary statistics
# -----------------------
cat("\n=== SUMMARY STATISTICS ===\n\n")
cat("Top 10 HUCs by Production (Scaled 0-1):\n")
top_hucs <- all_huc_combined %>%
  st_drop_geometry() %>%
  select(year, HYBAS_ID, total_prod, total_stream_length_km, prod_per_km_scaled) %>%
  arrange(desc(prod_per_km_scaled)) %>%
  head(10)
print(top_hucs)

cat("\nSummary Statistics by Year:\n")
summary_stats <- all_huc_combined %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    mean_production = mean(prod_per_km_scaled, na.rm = TRUE),
    median_production = median(prod_per_km_scaled, na.rm = TRUE),
    max_production = max(prod_per_km_scaled, na.rm = TRUE),
    min_production = min(prod_per_km_scaled, na.rm = TRUE),
    n_hucs = n(), 
    .groups = "drop"
  )
print(summary_stats)

cat("\n✓ Analysis complete. Maps saved to:\n")
cat("  ", output_dir, "\n")