################################################################################
# CONTOUR_FLIPPED SENSITIVITY SWEEP  —  assignment threshold
#
# Varies the sensitivity_threshold parameter (0.0 – 0.9 by 0.1) used inside
# the Bayesian assignment loop of 00_FullBasinRelativeProdMaps.R, re-runs the
# full assignment for every year at each threshold, then produces flipped
# contour figures (habitat variable × assignment_norm) from the fresh CSVs.
#
# HOW THE WRAPPER WORKS
#   1. Source 00_FullBasinRelativeProdMaps.R once.
#      This loads all spatial data and defines run_kusko() / run_yukon() in
#      the global env.  The initial run executes with the current params.R
#      defaults and writes to the default output paths — that is the one
#      unavoidable side-effect of not modifying the source script.
#   2. For each threshold in the sweep:
#      - Overwrite KUSKO_PARAMS$sensitivity_threshold and
#        YUKON_PARAMS$sensitivity_threshold in the global env.
#        (params.R is NOT re-sourced here, so these stay overridden.)
#      - Redirect PATHS$out_kusko / PATHS$out_yukon_full to a
#        threshold-specific subfolder under Outputs/SensitivitySweep/.
#      - Call run_kusko(yr) / run_yukon(yr) for every year.
#      - Read the new CSVs, join shapefile attributes, build contour figures.
#
# OUTPUT FIGURES (flat folders, one file per threshold):
#   Figures/ContourSensitivity_Sweep/
#     Yukon/WtrshdSlp_log/   t0.0.png … t0.9.png
#     Yukon/DistUpstre/      t0.0.png … t0.9.png
#     Kusko/WtrshdSlp_log/   t0.0.png … t0.9.png
#     Kusko/DistUpstre/      t0.0.png … t0.9.png
#
# USAGE (from project root):
#   Rscript Code/Analysis/01_DensityContours/Contours_sensitivity_flat.R
#   source("Code/Analysis/01_DensityContours/Contours_sensitivity_flat.R")
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(here)
library(ks)
library(scales)

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------
THRESHOLDS  <- seq(0.0, 0.9, by = 0.1)
QUANTILES   <- c(0, 0.2, 0.4, 0.6, 0.8)
YUKON_YEARS <- c(2015, 2016, 2021)
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)

# Where per-threshold assignment CSVs are written
csv_root <- here("Outputs", "SensitivitySweep")

# Where contour figures are saved
sweep_root <- here("Figures", "ContourSensitivity_Sweep")
fig_dirs <- list(
  y_log  = file.path(sweep_root, "Quartiles", "Yukon", "WtrshdSlp_log"),
  y_dist = file.path(sweep_root, "Quartiles", "Yukon", "DistUpstre"),
  k_log  = file.path(sweep_root, "Quartiles", "Kusko", "WtrshdSlp_log"),
  k_dist = file.path(sweep_root, "Quartiles", "Kusko", "DistUpstre")
)
invisible(lapply(fig_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ------------------------------------------------------------------------------
# Step 1: source the production script once.
# This loads KUSKO_EDGES, YUKON_EDGES, KUSKO_BASIN, YUKON_BASIN, daily_gen_wide,
# run_kusko(), run_yukon(), KUSKO_PARAMS, YUKON_PARAMS, and PATHS into global env.
# The initial run writes to the default output paths — see note in header.
# ------------------------------------------------------------------------------
cat("\n=================================================\n")
cat("  Sourcing 00_FullBasinRelativeProdMaps.R (initial run with default params)...\n")
cat("=================================================\n")

source(here("Code", "Analysis", "00_ProvenanceEstimates", "00_FullBasinRelativeProdMaps.R"))

# Save originals so we can restore after the sweep
KUSKO_PARAMS_ORIG <- KUSKO_PARAMS
YUKON_PARAMS_ORIG <- YUKON_PARAMS
PATHS_ORIG        <- PATHS

cat("\nInitial run complete. Starting sensitivity sweep...\n")

# ------------------------------------------------------------------------------
# Pre-compute fixed axis limits from the GEO shapefiles (Yukon_GEO2 / Kusko_GEO),
# which carry WtrshdSlp and DistUpstre. These are separate from the
# geomorphAdded shapefiles used by 00_FullBasinRelativeProdMaps.R.
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

yukon_attr <- yukon_geo %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

kusko_attr <- kusko_geo %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

YUKON_X_LIM_DIST <- range(yukon_attr$DistUpstre, na.rm = TRUE)
KUSKO_X_LIM_DIST <- range(kusko_attr$DistUpstre, na.rm = TRUE)

yukon_log_slp        <- log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0])
kusko_log_slp        <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
YUKON_X_LIM_LOG      <- quantile(yukon_log_slp, c(0.01, 0.99), na.rm = TRUE)
KUSKO_X_LIM_LOG      <- quantile(kusko_log_slp, c(0.01, 0.99), na.rm = TRUE)
YUKON_X_BREAKS_ORIG  <- scales::log_breaks(n = 8)(10^YUKON_X_LIM_LOG)
KUSKO_X_BREAKS_ORIG  <- scales::log_breaks(n = 8)(10^KUSKO_X_LIM_LOG)

Y_LIM        <- c(0, 1)     # assignment_norm always 0–1 by definition
dist_y_scale <- scale_y_continuous(
  breaks = seq(1e6, 3e6, by = 1e6),
  labels = 1:3
)

# ------------------------------------------------------------------------------
# Shared plot theme
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
# Panel helpers (match CONTOUR_flipped.R exactly)
# ------------------------------------------------------------------------------
make_contour_panel <- function(df, x_col, y_col, x_lim, y_lim,
                                x_lab, y_lab, yr, x_scale = NULL) {
  if (nrow(df) < 5) {
    return(
      ggplot() +
        annotate("text", x = mean(x_lim), y = mean(y_lim),
                 label = sprintf("n = %d\n(too few for KDE)", nrow(df)),
                 color = "firebrick", size = 5, hjust = 0.5) +
        coord_cartesian(xlim = x_lim, ylim = y_lim) +
        labs(x = x_lab, y = y_lab) +
        base_theme + ggtitle(yr)
    )
  }
  x <- df[[x_col]]
  y <- df[[y_col]]
  w <- df[["assignment_norm"]] / sum(df[["assignment_norm"]]) * nrow(df)

  fit     <- ks::kde(x = cbind(x, y), w = w, gridsize = c(200, 200))
  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)

  pt_dens <- predict(fit, x = cbind(x, y))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w / sum(w))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))

  p <- ggplot() +
    geom_contour_filled(data = grid_df,
                        aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = breaks) +
    geom_point(data = df, aes(x = .data[[x_col]], y = .data[[y_col]]),
               alpha = 0.0, color = "grey50", size = 0.8) +
    scale_fill_viridis_d("Quantiles",
                         labels = scales::percent(rev(QUANTILES[-1])),
                         direction = 1) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = x_lab, y = y_lab) +
    base_theme + ggtitle(yr)

  if (!is.null(x_scale)) p <- p + x_scale
  p
}

make_log_contour_panel <- function(df, log_col, y_col, x_lim_log, y_lim,
                                    x_breaks_orig, yr) {
  if (nrow(df) < 5) {
    return(
      ggplot() +
        annotate("text", x = mean(y_lim), y = mean(x_lim_log),
                 label = sprintf("n = %d\n(too few for KDE)", nrow(df)),
                 color = "firebrick", size = 5, hjust = 0.5) +
        coord_cartesian(xlim = y_lim, ylim = x_lim_log) +
        labs(x = "Assignment (normalized)", y = "Watershed Slope (log₁₀ scale)") +
        base_theme + ggtitle(yr)
    )
  }
  x <- df[["assignment_norm"]]   # assignment_norm on x-axis
  y <- df[[log_col]]             # log_slope on y-axis
  w <- x / sum(x) * nrow(df)

  H   <- ks::Hpi(x = cbind(x, y))
  H   <- (H + t(H)) / 2
  fit <- ks::kde(x = cbind(x, y), H = H, w = w, gridsize = c(200, 200))
  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)

  pt_dens <- predict(fit, x = cbind(x, y))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w / sum(w))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))

  ggplot() +
    geom_contour_filled(data = grid_df,
                        aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = breaks) +
    geom_point(data = df, aes(x = assignment_norm, y = .data[[log_col]]),
               alpha = 0.0, color = "grey50", size = 0.8) +
    scale_fill_viridis_d("Quantiles",
                         labels = scales::percent(rev(QUANTILES[-1])),
                         direction = 1) +
    scale_y_continuous(breaks = log10(x_breaks_orig), labels = x_breaks_orig,
                       limits = x_lim_log) +
    coord_cartesian(xlim = y_lim, ylim = x_lim_log) +
    labs(x = "Assignment (normalized)", y = "Watershed Slope (log₁₀ scale)") +
    base_theme + ggtitle(yr)
}

# ------------------------------------------------------------------------------
# Step 2: sweep loop
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

  # ── Redirect CSV output to threshold-specific subfolder ─────────────────────
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

  # ── Load fresh CSVs and join attributes ─────────────────────────────────────
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
      filter(WtrshdSlp > 0) %>%
      mutate(log_slope = log10(WtrshdSlp))
  ), YUKON_YEARS)

  plots_y_log <- lapply(YUKON_YEARS, function(yr)
    make_log_contour_panel(log_data_y[[as.character(yr)]],
                           log_col = "log_slope", y_col = "assignment_norm",
                           x_lim_log = YUKON_X_LIM_LOG, y_lim = Y_LIM,
                           x_breaks_orig = YUKON_X_BREAKS_ORIG, yr = yr))
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
    make_contour_panel(yr_data[[as.character(yr)]],
                       x_col = "assignment_norm", y_col = "DistUpstre",
                       x_lim = Y_LIM, y_lim = YUKON_X_LIM_DIST,
                       x_lab = "Assignment (normalized)",
                       y_lab = "Distance Upstream (km × 1000)", yr = yr,
                       x_scale = dist_y_scale))
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
      filter(WtrshdSlp > 0) %>%
      mutate(log_slope = log10(WtrshdSlp))
  ), KUSKO_YEARS)

  plots_k_log <- lapply(KUSKO_YEARS, function(yr)
    make_log_contour_panel(kusko_log_data[[as.character(yr)]],
                           log_col = "log_slope", y_col = "assignment_norm",
                           x_lim_log = KUSKO_X_LIM_LOG, y_lim = Y_LIM,
                           x_breaks_orig = KUSKO_X_BREAKS_ORIG, yr = yr))
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
    make_contour_panel(kusko_yr_data[[as.character(yr)]],
                       x_col = "assignment_norm", y_col = "DistUpstre",
                       x_lim = Y_LIM, y_lim = KUSKO_X_LIM_DIST,
                       x_lab = "Assignment (normalized)",
                       y_lab = "Distance Upstream (km × 1000)", yr = yr,
                       x_scale = dist_y_scale))
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
cat("  Assignment CSVs saved under:", csv_root, "\n")
cat("  Figures saved under:        ", sweep_root, "\n")
cat("=================================================\n")
