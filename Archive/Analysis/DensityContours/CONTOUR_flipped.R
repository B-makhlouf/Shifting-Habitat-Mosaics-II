################################################################################
# DENSITY CONTOURS (FLIPPED): contour-only, no basin maps
# Production-weighted quantile contour panels, one per year.
#
# SECTION 1 — assignment_norm (x) vs WtrshdSlp (y), assignment_norm > 0.6
#   Yukon:     Yukon_WtrshdSlp_vs_AssignNorm.png
#              Yukon_WtrshdSlp_vs_AssignNorm_logSlope.png
#   Kusko:     Kusko_WtrshdSlp_vs_AssignNorm.png
#              Kusko_WtrshdSlp_vs_AssignNorm_logSlope.png
#
# SECTION 2 — assignment_norm (x) vs DistUpstre (y), assignment_norm > 0.6
#   Yukon:     Yukon_DistUpstre_vs_AssignNorm.png
#   Kusko:     Kusko_DistUpstre_vs_AssignNorm.png
#
# YUKON_EDGES is loaded automatically from Yukon_GEO2.shp if not present.
# KUSKO_EDGES is loaded automatically from Kusko_GEO.shp if not present.
################################################################################

local({
  for (cl in rev(sys.calls())) {
    nm <- tryCatch(as.character(cl[[1]]), error = function(e) "")
    if (nm == "source" && length(cl) >= 2) {
      p <- tryCatch(normalizePath(as.character(cl[[2]])), error = function(e) NULL)
      if (!is.null(p) && file.exists(p)) {
        root <- normalizePath(file.path(dirname(p), "../../.."))
        if (file.exists(file.path(root, "Shifting-Habitat-Mosaics-II.Rproj")))
          setwd(root)
        break
      }
    }
  }
})

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
YUKON_YEARS  <- c(2015, 2016, 2021)
KUSKO_YEARS  <- c(2017, 2018, 2019, 2020, 2021, 2022)
QUANTILES    <- c(0, 0.2, 0.4, 0.6, 0.8)
ASSIGN_MIN   <- 0.6          # x-axis lower bound (assignment_norm)

prod_dir       <- here("Outputs", "ProductionData", "Yukon_full")
kusko_prod_dir <- here("Outputs", "ProductionData", "Kusko")

# ------------------------------------------------------------------------------
# Shared theme
# ------------------------------------------------------------------------------
black_bg <- plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = "black")))
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

# Helper: build one production-weighted quantile contour panel
make_contour_panel <- function(df, x_col, y_col, x_lim, y_lim,
                               x_lab, y_lab, yr,
                               x_scale = NULL) {
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

# Helper: log-KDE variant (log_col is log-transformed slope, placed on y-axis)
make_log_contour_panel <- function(df, log_col, y_col, x_lim_log, y_lim,
                                   x_breaks_orig, yr) {
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

# ==============================================================================
# SECTION 1: WtrshdSlp vs assignment_norm
# ==============================================================================

# ------------------------------------------------------------------------------
# Yukon — load edges + data
# ------------------------------------------------------------------------------
YUKON_EDGES <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  quiet = TRUE
)

yukon_attr <- YUKON_EDGES %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

yr_data <- setNames(lapply(YUKON_YEARS, function(yr) {
  read_csv(
    file.path(prod_dir, sprintf("%d_Yukon_Full_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_norm) %>%
    left_join(yukon_attr, by = "reachid") %>%
    filter(assignment_norm > ASSIGN_MIN,
           !is.na(WtrshdSlp), !is.na(DistUpstre), !is.na(assignment_norm))
}), YUKON_YEARS)

Y_LIM  <- c(ASSIGN_MIN, 1)
X_LIM  <- c(0, 45)

out_dir_slp <- here("Figures", "Contours", "Yukon_WtrshdSlp_AssignNorm")
dir.create(out_dir_slp, recursive = TRUE, showWarnings = FALSE)

# Linear slope panels
yukon_slp_plots <- lapply(YUKON_YEARS, function(yr)
  make_contour_panel(yr_data[[as.character(yr)]],
                     x_col = "assignment_norm", y_col = "WtrshdSlp",
                     x_lim = Y_LIM, y_lim = X_LIM,
                     x_lab = "Assignment (normalized)",
                     y_lab = "Watershed Slope", yr = yr))

ggsave(
  file.path(out_dir_slp, "Yukon_WtrshdSlp_vs_AssignNorm.png"),
  wrap_plots(yukon_slp_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 21, dpi = 300
)
cat("\nYukon WtrshdSlp vs AssignNorm saved.\n")

# Log-slope panels
yukon_log_data <- setNames(lapply(YUKON_YEARS, function(yr)
  yr_data[[as.character(yr)]] %>%
    filter(WtrshdSlp > 0) %>%
    mutate(log_slope = log10(WtrshdSlp))
), YUKON_YEARS)

YUKON_X_LIM_LOG      <- quantile(unlist(lapply(yukon_log_data, `[[`, "log_slope")), c(0.01, 0.99), na.rm = TRUE)
yukon_x_breaks_orig  <- scales::log_breaks(n = 8)(10^YUKON_X_LIM_LOG)

yukon_log_plots <- lapply(YUKON_YEARS, function(yr)
  make_log_contour_panel(yukon_log_data[[as.character(yr)]],
                         log_col = "log_slope", y_col = "assignment_norm",
                         x_lim_log = YUKON_X_LIM_LOG, y_lim = Y_LIM,
                         x_breaks_orig = yukon_x_breaks_orig, yr = yr))

ggsave(
  file.path(out_dir_slp, "Yukon_WtrshdSlp_vs_AssignNorm_logSlope.png"),
  wrap_plots(yukon_log_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 21, dpi = 300
)
cat("Yukon log-slope vs AssignNorm saved.\n")

# ------------------------------------------------------------------------------
# Kuskokwim — load edges + data
# ------------------------------------------------------------------------------
KUSKO_EDGES <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
)

kusko_attr <- KUSKO_EDGES %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

kusko_yr_data <- setNames(lapply(KUSKO_YEARS, function(yr) {
  read_csv(
    file.path(kusko_prod_dir, sprintf("%d_Kusko_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_norm) %>%
    left_join(kusko_attr, by = "reachid") %>%
    filter(assignment_norm > ASSIGN_MIN,
           !is.na(WtrshdSlp), !is.na(DistUpstre), !is.na(assignment_norm))
}), KUSKO_YEARS)

KUSKO_Y_LIM <- c(ASSIGN_MIN, 1)
KUSKO_X_LIM <- c(0, 25)

kusko_out_dir_slp <- here("Figures", "Contours", "Kusko_WtrshdSlp_AssignNorm")
dir.create(kusko_out_dir_slp, recursive = TRUE, showWarnings = FALSE)

kusko_slp_plots <- lapply(KUSKO_YEARS, function(yr)
  make_contour_panel(kusko_yr_data[[as.character(yr)]],
                     x_col = "assignment_norm", y_col = "WtrshdSlp",
                     x_lim = KUSKO_Y_LIM, y_lim = KUSKO_X_LIM,
                     x_lab = "Assignment (normalized)",
                     y_lab = "Watershed Slope", yr = yr))

ggsave(
  file.path(kusko_out_dir_slp, "Kusko_WtrshdSlp_vs_AssignNorm.png"),
  wrap_plots(kusko_slp_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 42, dpi = 300
)
cat("\nKusko WtrshdSlp vs AssignNorm saved.\n")

# Log-slope panels
kusko_log_data <- setNames(lapply(KUSKO_YEARS, function(yr)
  kusko_yr_data[[as.character(yr)]] %>%
    filter(WtrshdSlp > 0) %>%
    mutate(log_slope = log10(WtrshdSlp))
), KUSKO_YEARS)

KUSKO_X_LIM_LOG     <- quantile(unlist(lapply(kusko_log_data, `[[`, "log_slope")), c(0.01, 0.99), na.rm = TRUE)
kusko_x_breaks_orig <- scales::log_breaks(n = 8)(10^KUSKO_X_LIM_LOG)

kusko_log_plots <- lapply(KUSKO_YEARS, function(yr)
  make_log_contour_panel(kusko_log_data[[as.character(yr)]],
                         log_col = "log_slope", y_col = "assignment_norm",
                         x_lim_log = KUSKO_X_LIM_LOG, y_lim = KUSKO_Y_LIM,
                         x_breaks_orig = kusko_x_breaks_orig, yr = yr))

ggsave(
  file.path(kusko_out_dir_slp, "Kusko_WtrshdSlp_vs_AssignNorm_logSlope.png"),
  wrap_plots(kusko_log_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 42, dpi = 300
)
cat("Kusko log-slope vs AssignNorm saved.\n")

# ==============================================================================
# SECTION 2: assignment_norm (x) vs DistUpstre (y)
# ==============================================================================

dist_y_scale <- scale_y_continuous(
  breaks = seq(1e6, 3e6, by = 1e6),
  labels = 1:3
)

# ------------------------------------------------------------------------------
# Yukon — DistUpstre vs assignment_norm
# ------------------------------------------------------------------------------
YUKON_DIST_X_LIM <- range(unlist(lapply(yr_data, `[[`, "DistUpstre")), na.rm = TRUE)

out_dir_dist <- here("Figures", "Contours", "Yukon_DistUpstre_AssignNorm")
dir.create(out_dir_dist, recursive = TRUE, showWarnings = FALSE)

yukon_dist_plots <- lapply(YUKON_YEARS, function(yr)
  make_contour_panel(yr_data[[as.character(yr)]],
                     x_col = "assignment_norm", y_col = "DistUpstre",
                     x_lim = Y_LIM, y_lim = YUKON_DIST_X_LIM,
                     x_lab = "Assignment (normalized)",
                     y_lab = "Distance Upstream (km × 1000)", yr = yr,
                     x_scale = dist_y_scale))

ggsave(
  file.path(out_dir_dist, "Yukon_DistUpstre_vs_AssignNorm.png"),
  wrap_plots(yukon_dist_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 21, dpi = 300
)
cat("\nYukon DistUpstre vs AssignNorm saved.\n")

# ------------------------------------------------------------------------------
# Kuskokwim — DistUpstre vs assignment_norm
# ------------------------------------------------------------------------------
KUSKO_DIST_X_LIM <- range(unlist(lapply(kusko_yr_data, `[[`, "DistUpstre")), na.rm = TRUE)

kusko_out_dir_dist <- here("Figures", "Contours", "Kusko_DistUpstre_AssignNorm")
dir.create(kusko_out_dir_dist, recursive = TRUE, showWarnings = FALSE)

kusko_dist_plots <- lapply(KUSKO_YEARS, function(yr)
  make_contour_panel(kusko_yr_data[[as.character(yr)]],
                     x_col = "assignment_norm", y_col = "DistUpstre",
                     x_lim = KUSKO_Y_LIM, y_lim = KUSKO_DIST_X_LIM,
                     x_lab = "Assignment (normalized)",
                     y_lab = "Distance Upstream (km × 1000)", yr = yr,
                     x_scale = dist_y_scale))

ggsave(
  file.path(kusko_out_dir_dist, "Kusko_DistUpstre_vs_AssignNorm.png"),
  wrap_plots(kusko_dist_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 42, dpi = 300
)
cat("\nKusko DistUpstre vs AssignNorm saved.\n")
cat("All contour figures complete.\n")
