################################################################################
# CONTOUR_FLIPPED SENSITIVITY SWEEP  —  ggplot native density
#
# Identical to Contours_sensitivity_flat.R in every respect (same threshold
# sweep, same source/wrap of 00_FullBasinRelativeProdMaps.R, same folder
# structure) except contour panels are drawn with ggplot's built-in
# geom_density_2d_filled() instead of ks::kde + custom quantile breaks.
#
# Output structure:
#   Figures/ContourSensitivity_Sweep/
#     Yukon/WtrshdSlp_log/   t0.0.png … t0.9.png
#     Yukon/DistUpstre/      t0.0.png … t0.9.png
#     Kusko/WtrshdSlp_log/   t0.0.png … t0.9.png
#     Kusko/DistUpstre/      t0.0.png … t0.9.png
#
# USAGE (from project root):
#   Rscript Code/Analysis/01_DensityContours/CONTOUR_flipped_ggplot.R
#   source("Code/Analysis/01_DensityContours/CONTOUR_flipped_ggplot.R")
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(here)
library(scales)

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------
THRESHOLDS  <- seq(0.0, 0.9, by = 0.1)
YUKON_YEARS <- c(2015, 2016, 2021)
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)

csv_root   <- here("Outputs", "SensitivitySweep")

sweep_root <- here("Figures", "ContourSensitivity_Sweep")
fig_dirs <- list(
  y_log  = file.path(sweep_root, "GGplot", "Yukon", "WtrshdSlp_log"),
  y_dist = file.path(sweep_root, "GGplot", "Yukon", "DistUpstre"),
  k_log  = file.path(sweep_root, "GGplot", "Kusko", "WtrshdSlp_log"),
  k_dist = file.path(sweep_root, "GGplot", "Kusko", "DistUpstre")
)
invisible(lapply(fig_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ------------------------------------------------------------------------------
# Step 1: source the production script once
# ------------------------------------------------------------------------------
cat("\n=================================================\n")
cat("  Sourcing 00_FullBasinRelativeProdMaps.R (initial run with default params)...\n")
cat("=================================================\n")

source(here("Code", "Analysis", "00_ProvenanceEstimates", "00_FullBasinRelativeProdMaps.R"))

KUSKO_PARAMS_ORIG <- KUSKO_PARAMS
YUKON_PARAMS_ORIG <- YUKON_PARAMS
PATHS_ORIG        <- PATHS

cat("\nInitial run complete. Starting sensitivity sweep...\n")

# ------------------------------------------------------------------------------
# Load GEO shapefiles for habitat attributes (WtrshdSlp, DistUpstre)
# ------------------------------------------------------------------------------
cat("Loading GEO shapefiles for habitat attributes...\n")
yukon_geo <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  quiet = TRUE
)
kusko_geo <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
)

yukon_attr <- yukon_geo %>% st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)
kusko_attr <- kusko_geo %>% st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

# ------------------------------------------------------------------------------
# Fixed axis limits (computed from full spatial data, independent of threshold)
# ------------------------------------------------------------------------------
YUKON_X_LIM_DIST <- range(yukon_attr$DistUpstre, na.rm = TRUE)
KUSKO_X_LIM_DIST <- range(kusko_attr$DistUpstre, na.rm = TRUE)

yukon_log_slp       <- log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0])
kusko_log_slp       <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
YUKON_X_LIM_LOG     <- quantile(yukon_log_slp, c(0.01, 0.99), na.rm = TRUE)
KUSKO_X_LIM_LOG     <- quantile(kusko_log_slp, c(0.01, 0.99), na.rm = TRUE)
YUKON_X_BREAKS_ORIG <- scales::log_breaks(n = 8)(10^YUKON_X_LIM_LOG)
KUSKO_X_BREAKS_ORIG <- scales::log_breaks(n = 8)(10^KUSKO_X_LIM_LOG)

Y_LIM        <- c(0, 1)
dist_y_scale <- scale_y_continuous(
  breaks = seq(1e6, 3e6, by = 1e6), labels = 1:3
)

# ------------------------------------------------------------------------------
# Shared theme
# ------------------------------------------------------------------------------
black_bg_ann <- function(title_text) {
  plot_annotation(
    title = title_text,
    theme = theme(
      plot.background = element_rect(fill = "black", color = "black"),
      plot.title      = element_text(color = "white", size = 14, hjust = 0.5,
                                     margin = margin(b = 6))
    )
  )
}

base_theme <- theme_grey() +
  theme(
    axis.text        = element_text(size = 14, color = "grey30"),
    axis.title       = element_text(size = 13, color = "grey20"),
    panel.grid.major = element_line(color = scales::alpha("grey50", 0.3), linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title     = element_text(size = 12),
    legend.text      = element_text(size = 11),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(14, 14, 14, 14)
  )

# ------------------------------------------------------------------------------
# Panel helper — ggplot native density
# ------------------------------------------------------------------------------
make_gg_panel <- function(df, x_col, y_col, x_lim, y_lim,
                           x_lab, y_lab, yr, x_scale = NULL) {
  if (nrow(df) < 5) {
    return(
      ggplot() +
        annotate("text", x = mean(x_lim), y = mean(y_lim),
                 label = sprintf("n = %d\n(too few)", nrow(df)),
                 color = "firebrick", size = 5, hjust = 0.5) +
        coord_cartesian(xlim = x_lim, ylim = y_lim) +
        labs(x = x_lab, y = y_lab) +
        base_theme + ggtitle(yr)
    )
  }

  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                      weight = assignment_norm)) +
    geom_density_2d_filled(contour_var = "ndensity") +
    scale_fill_viridis_d("Level", direction = 1) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = x_lab, y = y_lab) +
    base_theme + ggtitle(yr)

  if (!is.null(x_scale)) p <- p + x_scale
  p
}

# ------------------------------------------------------------------------------
# Sweep loop
# ------------------------------------------------------------------------------
cat("\n=================================================\n")
cat("  Threshold sweep:", paste(sprintf("%.1f", THRESHOLDS), collapse = ", "), "\n")
cat("=================================================\n")

for (thresh in THRESHOLDS) {

  thresh_label <- sprintf("%.1f", thresh)
  cat(sprintf("\n========== THRESHOLD: %s ==========\n", thresh_label))

  # ── Override assignment thresholds ──────────────────────────────────────────
  KUSKO_PARAMS$sensitivity_threshold <- thresh
  YUKON_PARAMS$sensitivity_threshold <- thresh

  # ── Redirect CSV output ──────────────────────────────────────────────────────
  csv_dir_k <- file.path(csv_root, sprintf("t%s", thresh_label), "Kusko")
  csv_dir_y <- file.path(csv_root, sprintf("t%s", thresh_label), "Yukon")
  PATHS$out_kusko      <- csv_dir_k
  PATHS$out_yukon_full <- csv_dir_y
  PATHS$map_kusko      <- file.path(csv_root, sprintf("t%s", thresh_label), "maps", "Kusko")
  PATHS$map_yukon_full <- file.path(csv_root, sprintf("t%s", thresh_label), "maps", "Yukon")

  # ── Re-run assignments ───────────────────────────────────────────────────────
  cat("  Running Kusko assignments...\n")
  for (yr in KUSKO_YEARS)
    tryCatch(run_kusko(yr),
             error = function(e) cat("  ERROR Kusko", yr, ":", e$message, "\n"))

  cat("  Running Yukon assignments...\n")
  for (yr in YUKON_YEARS)
    tryCatch(run_yukon(yr),
             error = function(e) cat("  ERROR Yukon", yr, ":", e$message, "\n"))

  # ── Load fresh CSVs ──────────────────────────────────────────────────────────
  yr_data <- setNames(lapply(YUKON_YEARS, function(yr) {
    read_csv(
      file.path(csv_dir_y, sprintf("%d_Yukon_Full_Assignment_Results.csv", yr)),
      show_col_types = FALSE
    ) %>%
      dplyr::select(reachid, assignment_norm) %>%
      left_join(yukon_attr, by = "reachid") %>%
      filter(assignment_norm > 0, !is.na(WtrshdSlp), !is.na(DistUpstre))
  }), YUKON_YEARS)

  kusko_yr_data <- setNames(lapply(KUSKO_YEARS, function(yr) {
    read_csv(
      file.path(csv_dir_k, sprintf("%d_Kusko_Assignment_Results.csv", yr)),
      show_col_types = FALSE
    ) %>%
      dplyr::select(reachid, assignment_norm) %>%
      left_join(kusko_attr, by = "reachid") %>%
      filter(assignment_norm > 0, !is.na(WtrshdSlp), !is.na(DistUpstre))
  }), KUSKO_YEARS)

  n_y <- sapply(yr_data,       nrow)
  n_k <- sapply(kusko_yr_data, nrow)
  cat("  Yukon n:", paste(YUKON_YEARS, n_y, sep = "=", collapse = "  "), "\n")
  cat("  Kusko n:", paste(KUSKO_YEARS, n_k, sep = "=", collapse = "  "), "\n")

  # ── YUKON: WtrshdSlp (log) ──────────────────────────────────────────────────
  cat("  Yukon WtrshdSlp (log)...\n")
  log_data_y <- setNames(lapply(YUKON_YEARS, function(yr)
    yr_data[[as.character(yr)]] %>%
      filter(WtrshdSlp > 0) %>% mutate(log_slope = log10(WtrshdSlp))
  ), YUKON_YEARS)

  yukon_log_y_scale <- scale_y_continuous(
    breaks = log10(YUKON_X_BREAKS_ORIG), labels = YUKON_X_BREAKS_ORIG,
    limits = YUKON_X_LIM_LOG
  )
  plots_y_log <- lapply(YUKON_YEARS, function(yr)
    make_gg_panel(log_data_y[[as.character(yr)]],
                  x_col = "assignment_norm", y_col = "log_slope",
                  x_lim = Y_LIM, y_lim = YUKON_X_LIM_LOG,
                  x_lab = "Assignment (normalized)",
                  y_lab = "Watershed Slope (log₁₀ scale)",
                  yr = yr, x_scale = yukon_log_y_scale))
  ggsave(
    file.path(fig_dirs$y_log, sprintf("t%s.png", thresh_label)),
    wrap_plots(plots_y_log, ncol = 1, guides = "collect") +
      black_bg_ann(sprintf("Yukon — WtrshdSlp (log)  |  threshold = %s  |  n = %s",
                           thresh_label, paste(n_y, collapse = " / "))),
    width = 9, height = 21, dpi = 150
  )

  # ── YUKON: DistUpstre ───────────────────────────────────────────────────────
  cat("  Yukon DistUpstre...\n")
  plots_y_dist <- lapply(YUKON_YEARS, function(yr)
    make_gg_panel(yr_data[[as.character(yr)]],
                  x_col = "assignment_norm", y_col = "DistUpstre",
                  x_lim = Y_LIM, y_lim = YUKON_X_LIM_DIST,
                  x_lab = "Assignment (normalized)",
                  y_lab = "Distance Upstream (km × 1000)",
                  yr = yr, x_scale = dist_y_scale))
  ggsave(
    file.path(fig_dirs$y_dist, sprintf("t%s.png", thresh_label)),
    wrap_plots(plots_y_dist, ncol = 1, guides = "collect") +
      black_bg_ann(sprintf("Yukon — DistUpstre  |  threshold = %s  |  n = %s",
                           thresh_label, paste(n_y, collapse = " / "))),
    width = 9, height = 21, dpi = 150
  )

  # ── KUSKO: WtrshdSlp (log) ──────────────────────────────────────────────────
  cat("  Kusko WtrshdSlp (log)...\n")
  kusko_log_data <- setNames(lapply(KUSKO_YEARS, function(yr)
    kusko_yr_data[[as.character(yr)]] %>%
      filter(WtrshdSlp > 0) %>% mutate(log_slope = log10(WtrshdSlp))
  ), KUSKO_YEARS)

  kusko_log_y_scale <- scale_y_continuous(
    breaks = log10(KUSKO_X_BREAKS_ORIG), labels = KUSKO_X_BREAKS_ORIG,
    limits = KUSKO_X_LIM_LOG
  )
  plots_k_log <- lapply(KUSKO_YEARS, function(yr)
    make_gg_panel(kusko_log_data[[as.character(yr)]],
                  x_col = "assignment_norm", y_col = "log_slope",
                  x_lim = Y_LIM, y_lim = KUSKO_X_LIM_LOG,
                  x_lab = "Assignment (normalized)",
                  y_lab = "Watershed Slope (log₁₀ scale)",
                  yr = yr, x_scale = kusko_log_y_scale))
  ggsave(
    file.path(fig_dirs$k_log, sprintf("t%s.png", thresh_label)),
    wrap_plots(plots_k_log, ncol = 1, guides = "collect") +
      black_bg_ann(sprintf("Kusko — WtrshdSlp (log)  |  threshold = %s  |  n = %s",
                           thresh_label, paste(n_k, collapse = " / "))),
    width = 9, height = 42, dpi = 150
  )

  # ── KUSKO: DistUpstre ───────────────────────────────────────────────────────
  cat("  Kusko DistUpstre...\n")
  plots_k_dist <- lapply(KUSKO_YEARS, function(yr)
    make_gg_panel(kusko_yr_data[[as.character(yr)]],
                  x_col = "assignment_norm", y_col = "DistUpstre",
                  x_lim = Y_LIM, y_lim = KUSKO_X_LIM_DIST,
                  x_lab = "Assignment (normalized)",
                  y_lab = "Distance Upstream (km × 1000)",
                  yr = yr, x_scale = dist_y_scale))
  ggsave(
    file.path(fig_dirs$k_dist, sprintf("t%s.png", thresh_label)),
    wrap_plots(plots_k_dist, ncol = 1, guides = "collect") +
      black_bg_ann(sprintf("Kusko — DistUpstre  |  threshold = %s  |  n = %s",
                           thresh_label, paste(n_k, collapse = " / "))),
    width = 9, height = 42, dpi = 150
  )

  cat(sprintf("  Threshold %s complete.\n", thresh_label))
}

# ------------------------------------------------------------------------------
# Restore original params and paths
# ------------------------------------------------------------------------------
KUSKO_PARAMS <- KUSKO_PARAMS_ORIG
YUKON_PARAMS <- YUKON_PARAMS_ORIG
PATHS        <- PATHS_ORIG

cat("\n=================================================\n")
cat("  Sweep complete!\n")
cat("  Figures saved under:", sweep_root, "\n")
cat("=================================================\n")
