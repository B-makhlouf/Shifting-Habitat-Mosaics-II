################################################################################
# ATTRIBUTE HISTOGRAMS — Watershed Slope & Distance Upstream
#
# Plots the full distribution of WtrshdSlp and DistUpstre from the analysis
# shapefiles (all reaches, no assignment filter).
#
#   Left panel  : Watershed Slope on a log₁₀ x-axis
#   Right panel : Distance Upstream in km
#
# One figure per basin, saved to Figures/Contours/Presfigures/Thresh/
#
# USAGE (from project root):
#   source("Code/Analysis/01_DensityContours/AttributeHistograms.R")
#   Rscript Code/Analysis/01_DensityContours/AttributeHistograms.R
################################################################################

library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)
library(scales)

# ==============================================================================
# Config
# ==============================================================================
fig_dir <- here("Figures", "Contours", "Presfigures", "Thresh")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

N_BINS <- 60   # histogram bin count for both variables

# ==============================================================================
# Load shapefiles
# ==============================================================================
cat("Loading shapefiles...\n")
yukon_attr <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre) %>%
  filter(!is.na(WtrshdSlp), !is.na(DistUpstre), WtrshdSlp > 0)

kusko_attr <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre) %>%
  filter(!is.na(WtrshdSlp), !is.na(DistUpstre), WtrshdSlp > 0)

cat(sprintf("  Yukon reaches: %d\n", nrow(yukon_attr)))
cat(sprintf("  Kusko reaches: %d\n", nrow(kusko_attr)))

# ==============================================================================
# Shared theme
# ==============================================================================
base_theme <- theme_bw() +
  theme(
    axis.text        = element_text(size = 20, color = "grey30"),
    axis.title       = element_text(size = 24, color = "grey20"),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 26, face = "bold", hjust = 0.5,
                                    margin = margin(b = 8)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(14, 14, 14, 14)
  )

# ==============================================================================
# Panel builders
# ==============================================================================

# Watershed Slope — log₁₀ x-axis, bins evenly spaced in log space
slope_hist <- function(df, basin) {
  log_vals <- log10(df$WtrshdSlp)
  bin_lim  <- quantile(log_vals, c(0.001, 0.999), na.rm = TRUE)
  breaks   <- seq(bin_lim[1], bin_lim[2], length.out = N_BINS + 1)
  # Back-convert break positions to original scale for x-axis labels
  lab_pos  <- pretty(bin_lim, n = 6)
  lab_vals <- signif(10^lab_pos, 3)

  ggplot(data.frame(log_slope = log_vals), aes(x = log_slope)) +
    geom_histogram(breaks = breaks,
                   fill = "#4477AA", color = "white", linewidth = 0.2) +
    scale_x_continuous(breaks = lab_pos, labels = lab_vals) +
    scale_y_continuous(labels = label_comma()) +
    labs(x = "Watershed Slope (log₁₀ scale)", y = "Number of reaches") +
    base_theme +
    ggtitle(sprintf("%s — Watershed Slope", basin))
}

# Distance Upstream — linear x-axis in km
dist_hist <- function(df, basin) {
  km_vals <- df$DistUpstre / 1e3   # convert m → km

  ggplot(data.frame(dist_km = km_vals), aes(x = dist_km)) +
    geom_histogram(bins = N_BINS,
                   fill = "#EE6677", color = "white", linewidth = 0.2) +
    scale_x_continuous(labels = label_comma()) +
    scale_y_continuous(labels = label_comma()) +
    labs(x = "Distance Upstream (km)", y = "Number of reaches") +
    base_theme +
    ggtitle(sprintf("%s — Distance Upstream", basin))
}

# ==============================================================================
# Produce and save one figure per basin
# ==============================================================================
basins <- list(
  list(name = "Yukon", df = yukon_attr),
  list(name = "Kusko", df = kusko_attr)
)

for (b in basins) {
  cat(sprintf("Building %s histograms...\n", b$name))
  p_slope <- slope_hist(b$df, b$name)
  p_dist  <- dist_hist(b$df,  b$name)

  fig <- (p_slope | p_dist) +
    plot_annotation(
      theme = theme(
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  fname <- file.path(fig_dir, sprintf("%s_attribute_histograms.png", b$name))
  ggsave(fname, fig, width = 16, height = 6, dpi = 150)
  cat("  Saved:", fname, "\n")
}

cat("\nDone.\n")
