################################################################################
# YUKON HUC PRODUCTION - NORMALIZED BY STREAM LENGTH
# FIXED: Improved legend handling and refined visual styling
# Legend now appears consistently and correctly
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
YUKON_YEARS <- c(2015, 2016, 2021)
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
# STEP 3: Color bins & palette (EXACT from tributary maps)
# -----------------------
cat("\n=== STEP 3: Setting up color scales ===\n")

palette <- brewer.pal(9, "YlOrRd")
palette_expanded <- colorRampPalette(palette)(10)

# 0.1 interval bins (matching Kusko style - 10 bins)
bin_breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
bin_labels <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")

color_values <- palette_expanded  # All 10 colors for 10 bins

cat("✓ Color palette configured.\n\n")

# -----------------------
# STEP 4: Create maps with FIXED legend
# -----------------------
cat("=== STEP 4: Creating maps ===\n")

plots <- list()

for (year in YUKON_YEARS) {
  huc_year <- all_huc_combined %>% filter(year == !!year)
  
  # Create bin categories with 0.1 intervals
  huc_year <- huc_year %>%
    mutate(
      prod_bin = case_when(
        prod_per_km_scaled == 0 ~ "No production",
        prod_per_km_scaled <= 0.1 ~ "0.0-0.1",
        prod_per_km_scaled <= 0.2 ~ "0.1-0.2",
        prod_per_km_scaled <= 0.3 ~ "0.2-0.3",
        prod_per_km_scaled <= 0.4 ~ "0.3-0.4",
        prod_per_km_scaled <= 0.5 ~ "0.4-0.5",
        prod_per_km_scaled <= 0.6 ~ "0.5-0.6",
        prod_per_km_scaled <= 0.7 ~ "0.6-0.7",
        prod_per_km_scaled <= 0.8 ~ "0.7-0.8",
        prod_per_km_scaled <= 0.9 ~ "0.8-0.9",
        TRUE ~ "0.9-1.0"
      ),
      # Convert to factor with specified order
      prod_bin = factor(prod_bin, 
                        levels = c("No production", bin_labels))
    )
  
  # Create manual color palette including white for "No production"
  color_map <- c(
    "No production" = "#ffffff",
    "0.0-0.1" = palette_expanded[1],
    "0.1-0.2" = palette_expanded[2],
    "0.2-0.3" = palette_expanded[3],
    "0.3-0.4" = palette_expanded[4],
    "0.4-0.5" = palette_expanded[5],
    "0.5-0.6" = palette_expanded[6],
    "0.6-0.7" = palette_expanded[7],
    "0.7-0.8" = palette_expanded[8],
    "0.8-0.9" = palette_expanded[9],
    "0.9-1.0" = palette_expanded[10]
  )
  
  # Build map with WORKING legend
  p <- ggplot() +
    # Base layer: all HUCs light gray
    geom_sf(data = huc, fill = "#fbfbfb", color = "#dcdcdc", linewidth = 0.35) +
    
    # Main data layer with fill color
    geom_sf(data = huc_year, 
            aes(fill = prod_bin), 
            color = "#ffffff", 
            linewidth = 0.28) +
    
    # Border layer
    geom_sf(data = huc, fill = NA, color = "#2b2b2b", linewidth = 0.18) +
    
    # FIXED: scale_fill_manual with consistent breaks across all maps
    scale_fill_manual(
      name = "Relative Posterior Density",
      values = color_map,
      breaks = c("No production", bin_labels),  # Force consistent order
      drop = FALSE,  # Keep all levels even if not used in this year
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        ncol = 1,
        label.hjust = 0,
        keyheight = unit(6, "mm"),
        keywidth = unit(6, "mm"),
        override.aes = list(color = NA)  # Removes borders, keeps fill colors visible
      )
    ) +
    
    coord_sf(expand = FALSE) +
    
    theme_minimal(base_family = "Helvetica", base_size = 10) +
    theme(
      # Title and subtitle
      plot.title = element_text(
        size = 16, 
        face = "bold", 
        hjust = 0.5, 
        margin = margin(t = 10, b = 5)
      ),
      plot.subtitle = element_text(
        size = 11, 
        color = "#555555", 
        hjust = 0.5, 
        margin = margin(b = 10)
      ),
      
      # Legend - positioned inside plot at top-left
      legend.position = "left",
      legend.justification = "top",
      legend.background = element_rect(
        fill = "white", 
        color = "#888888", 
        linewidth = 0.8
      ),
      legend.margin = margin(8, 10, 8, 10),
      legend.title = element_text(
        size = 10, 
        face = "bold",
        margin = margin(b = 6)
      ),
      legend.text = element_text(size = 9),
      legend.key = element_rect(
        fill = NA, 
        color = "#cccccc",  # Light gray border around each key
        linewidth = 0.3
      ),
      
      # Panel and plot background
      plot.background = element_rect(fill = "#f6f6f6", color = NA),
      panel.background = element_rect(fill = "#f6f6f6", color = NA),
      panel.border = element_rect(color = "#e0e0e0", fill = NA, linewidth = 0.5),
      panel.grid = element_blank(),
      
      # Axes
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      # Margins
      plot.margin = margin(10, 10, 10, 10)
    ) +
    
    labs(
      title = "HUC Production",
      subtitle = paste0("Year: ", year, " | River: Yukon | Normalized by Stream Network Length")
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
         width = 12, 
         height = 10, 
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
    subtitle = "Production per km across years (matching tributary map color scheme)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0.5, margin = margin(b = 10))
    )
  )

comparison_file <- file.path(output_dir, "Yukon_HUC_Production_MultiYear_Comparison.png")
ggsave(comparison_file, 
       comparison_plot, 
       width = 18, 
       height = 10, 
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