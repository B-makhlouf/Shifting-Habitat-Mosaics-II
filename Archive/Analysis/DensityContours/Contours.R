################################################################################
# DENSITY CONTOURS: WtrshdSlp vs DistUpstre
# Production-weighted quantile contour panels, one per year.
#
# YUKON Outputs:
#   1. Yukon_WtrshdSlp_vs_DistUpstre.png  — original 3-panel contour figure
#   2. Yukon_Top50_Contours_SixPanel.png  — 6-panel figure: top-50% basin maps
#      (left column) paired with contour plots (right column), one row per year
#
# KUSKOKWIM Outputs:
#   3. Kusko_WtrshdSlp_vs_DistUpstre.png  — 6-panel contour figure (2017–2022)
#
# YUKON_EDGES is loaded automatically from Yukon_edges_geomorphAdded.shp if not present.
# YUKON_BASIN will be loaded automatically if not present (required for maps).
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

# -------------------------------------------------6----------------------------
# Config
# ------------------------------------------------------------------------------
YUKON_YEARS       <- c(2015, 2016, 2021)
if (!exists("CONTOUR_THRESHOLD")) CONTOUR_THRESHOLD <- 0.75
QUANTILES         <- c( 0, 0.2, 0.4, 0.6, 0.8)
prod_dir          <- here("Outputs", "ProductionData", "Yukon_full")
if (!exists("out_dir")) out_dir <- here("Figures", "Contours", "Yukon_WtrshdSlp_DistUpstre")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Output directory for individual presentation figures (white background)
pres_out_dir <- here("Figures", "Contours", "Presfigures", "Contours")
dir.create(pres_out_dir, recursive = TRUE, showWarnings = FALSE)

# Theme override for individual presentation exports: white panel background
white_panel_theme <- theme(
  panel.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = scales::alpha("grey80", 0.5), linewidth = 0.3)
)

# Load Yukon edges if not already in environment
YUKON_EDGES <- sf::st_read(here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"))

# Pull attributes from shapefile
yukon_attr <- YUKON_EDGES %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

# Load all years' data
yr_data <- setNames(lapply(YUKON_YEARS, function(yr) {
  read_csv(
    file.path(prod_dir, sprintf("%d_Yukon_Full_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_norm) %>%
    left_join(yukon_attr, by = "reachid") %>%
    filter(
      assignment_norm >= CONTOUR_THRESHOLD,
      !is.na(WtrshdSlp),
      !is.na(DistUpstre)
    )
}), YUKON_YEARS)

# Global axis limits
X_LIM <- c(0, 45)
Y_LIM <- range(unlist(lapply(yr_data, `[[`, "DistUpstre")), na.rm = TRUE)

# Shared plot elements
black_bg <- plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = "black")))
ax_labs  <- labs(x = "Watershed Slope", y = "Distance Upstream (km × 1000)")
y_scale  <- scale_y_continuous(breaks = seq(1e6, 3e6, by = 1e6), labels = 1:3)
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
# Build one panel per year
# ------------------------------------------------------------------------------
plots <- lapply(YUKON_YEARS, function(yr) {
  df <- yr_data[[as.character(yr)]]

  w       <- df$assignment_norm / sum(df$assignment_norm) * nrow(df)
  fit     <- ks::kde(x = cbind(df$WtrshdSlp, df$DistUpstre), w = w, gridsize = c(200, 200))
  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)

  pt_dens <- predict(fit, x = cbind(df$WtrshdSlp, df$DistUpstre))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w / sum(w))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))

  ggplot() +
    geom_contour_filled(data = grid_df, aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = breaks) +
    geom_point(data = df, aes(x = WtrshdSlp, y = DistUpstre),
               alpha = 0.0, color = "grey50", size = 0.8) +
    scale_fill_viridis_d("Quantiles", labels = scales::percent(rev(QUANTILES[-1])), direction = 1) +
    coord_cartesian(xlim = X_LIM, ylim = Y_LIM) +
    ax_labs + y_scale + base_theme + ggtitle(yr)
})

# Save individual Yukon contour panels (white background)
invisible(lapply(seq_along(YUKON_YEARS), function(i) {
  ggsave(
    file.path(pres_out_dir, sprintf("Yukon_%d_WtrshdSlp_vs_DistUpstre.png", YUKON_YEARS[i])),
    plots[[i]] + white_panel_theme,
    width = 9, height = 7, dpi = 300
  )
}))

ggsave(
  file.path(out_dir, "Yukon_WtrshdSlp_vs_DistUpstre.png"),
  wrap_plots(plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 21, dpi = 300
)

# ------------------------------------------------------------------------------
# Six-panel figure: top-50% basin maps (left) + contour panels (right)
#
# top50prod.R uses base-R graphics, so each map is rendered into a temporary
# PNG at the target cell size (9 x 7 in), read back as a raster, and wrapped
# as a patchwork-compatible grob via wrap_elements().
# Requires the 'png' package: install.packages("png")
# ------------------------------------------------------------------------------
source(here("Code", "Analysis", "01_DensityContours", "top50prod.R"))

if (!requireNamespace("png", quietly = TRUE))
  stop("Package 'png' needed for six-panel figure. Run: install.packages('png')")
library(grid)

map_panels <- lapply(YUKON_YEARS, function(yr) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 9, height = 7, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  draw_top50_map(yr, YUKON_EDGES, YUKON_BASIN, threshold = CONTOUR_THRESHOLD)
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  img <- png::readPNG(tmp)
  wrap_elements(
    grid::rasterGrob(img, interpolate = TRUE,
                     width  = unit(1, "npc"),
                     height = unit(1, "npc"))
  )
})

# Interleave: [map_yr1, contour_yr1, map_yr2, contour_yr2, map_yr3, contour_yr3]
combined_panels <- unlist(
  lapply(seq_along(YUKON_YEARS), function(i) {
    list(map_panels[[i]], plots[[i]])
  }),
  recursive = FALSE
)

six_panel_fig <- wrap_plots(combined_panels, ncol = 2) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = "white"))
  )

ggsave(
  file.path(out_dir, "Yukon_Top50_Contours_SixPanel.png"),
  six_panel_fig,
  width = 18, height = 21, dpi = 300
)

cat("\nYukon DONE\n")

# ------------------------------------------------------------------------------
# Yukon contour-only figure: log10(WtrshdSlp) on x-axis
# KDE is computed on the log-transformed values so density estimation is
# correct in log space (not just a cosmetic axis rescale).
# Rows with WtrshdSlp == 0 are dropped (log undefined).
# ------------------------------------------------------------------------------
yukon_log_data <- setNames(lapply(YUKON_YEARS, function(yr) {
  yr_data[[as.character(yr)]] %>%
    filter(WtrshdSlp > 0) %>%
    mutate(log_slope = log10(WtrshdSlp))
}), YUKON_YEARS)

YUKON_X_LIM_LOG <- range(unlist(lapply(yukon_log_data, `[[`, "log_slope")), na.rm = TRUE)

yukon_x_breaks_orig <- scales::log_breaks(n = 8)(10^YUKON_X_LIM_LOG)

yukon_log_plots <- lapply(YUKON_YEARS, function(yr) {
  df <- yukon_log_data[[as.character(yr)]]

  w       <- df$assignment_norm / sum(df$assignment_norm) * nrow(df)
  H       <- ks::Hpi(x = cbind(df$log_slope, df$DistUpstre))
  H       <- (H + t(H)) / 2                    # force exact symmetry
  fit     <- ks::kde(x = cbind(df$log_slope, df$DistUpstre), H = H, w = w, gridsize = c(200, 200))
  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)

  pt_dens <- predict(fit, x = cbind(df$log_slope, df$DistUpstre))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w / sum(w))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))

  ggplot() +
    geom_contour_filled(data = grid_df, aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = breaks) +
    geom_point(data = df, aes(x = log_slope, y = DistUpstre),
               alpha = 0.0, color = "grey50", size = 0.8) +
    scale_fill_viridis_d("Quantiles", labels = scales::percent(rev(QUANTILES[-1])), direction = 1) +
    scale_x_continuous(breaks = log10(yukon_x_breaks_orig), labels = yukon_x_breaks_orig,
                       limits = YUKON_X_LIM_LOG) +
    coord_cartesian(xlim = YUKON_X_LIM_LOG, ylim = Y_LIM) +
    labs(x = "Watershed Slope (log₁₀ scale)", y = "Distance Upstream (km × 1000)") +
    y_scale + base_theme + ggtitle(yr)
})

# Save individual Yukon log-slope panels (white background)
invisible(lapply(seq_along(YUKON_YEARS), function(i) {
  ggsave(
    file.path(pres_out_dir, sprintf("Yukon_%d_WtrshdSlp_vs_DistUpstre_logSlope.png", YUKON_YEARS[i])),
    yukon_log_plots[[i]] + white_panel_theme,
    width = 9, height = 7, dpi = 300
  )
}))

ggsave(
  file.path(out_dir, "Yukon_WtrshdSlp_vs_DistUpstre_logSlope.png"),
  wrap_plots(yukon_log_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 21, dpi = 300
)

cat("Yukon log-slope contours saved.\n")

# ==============================================================================
# KUSKOKWIM DENSITY CONTOURS: WtrshdSlp vs DistUpstre
# Production-weighted quantile contour panels, one per year (2017–2022).
# ==============================================================================

# ------------------------------------------------------------------------------
# Kuskokwim config
# ------------------------------------------------------------------------------
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
kusko_prod_dir <- here("Outputs", "ProductionData", "Kusko")
if (!exists("kusko_out_dir")) kusko_out_dir <- here("Figures", "Contours", "Kusko_WtrshdSlp_DistUpstre")
dir.create(kusko_out_dir, recursive = TRUE, showWarnings = FALSE)

# Load Kuskokwim edges if not already in environment

  KUSKO_EDGES <- sf::st_read(
    here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
    quiet = TRUE
  )


# Pull WtrshdSlp and DistUpstre from the Kuskokwim shapefile
kusko_attr <- KUSKO_EDGES %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

# Load all years' data
kusko_yr_data <- setNames(lapply(KUSKO_YEARS, function(yr) {
  read_csv(
    file.path(kusko_prod_dir, sprintf("%d_Kusko_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_norm) %>%
    left_join(kusko_attr, by = "reachid") %>%
    filter(
      assignment_norm >= CONTOUR_THRESHOLD,
      !is.na(WtrshdSlp),
      !is.na(DistUpstre)
    )
}), KUSKO_YEARS)

# Global axis limits for Kuskokwim
KUSKO_X_LIM <- c(0, 25)
KUSKO_Y_LIM <- range(unlist(lapply(kusko_yr_data, `[[`, "DistUpstre")), na.rm = TRUE)

# ------------------------------------------------------------------------------
# Build one contour panel per Kuskokwim year
# ------------------------------------------------------------------------------
kusko_plots <- lapply(KUSKO_YEARS, function(yr) {
  df <- kusko_yr_data[[as.character(yr)]]

  w       <- df$assignment_norm / sum(df$assignment_norm) * nrow(df)
  fit     <- ks::kde(x = cbind(df$WtrshdSlp, df$DistUpstre), w = w, gridsize = c(200, 200))
  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)

  pt_dens <- predict(fit, x = cbind(df$WtrshdSlp, df$DistUpstre))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w / sum(w))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))

  ggplot() +
    geom_contour_filled(data = grid_df, aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = breaks) +
    geom_point(data = df, aes(x = WtrshdSlp, y = DistUpstre),
               alpha = 0.0, color = "grey50", size = 0.8) +
    scale_fill_viridis_d("Quantiles", labels = scales::percent(rev(QUANTILES[-1])), direction = 1) +
    coord_cartesian(xlim = KUSKO_X_LIM, ylim = KUSKO_Y_LIM) +
    ax_labs + y_scale + base_theme + ggtitle(yr)
})

# Save individual Kuskokwim contour panels (white background)
invisible(lapply(seq_along(KUSKO_YEARS), function(i) {
  ggsave(
    file.path(pres_out_dir, sprintf("Kusko_%d_WtrshdSlp_vs_DistUpstre.png", KUSKO_YEARS[i])),
    kusko_plots[[i]] + white_panel_theme,
    width = 9, height = 7, dpi = 300
  )
}))

ggsave(
  file.path(kusko_out_dir, "Kusko_WtrshdSlp_vs_DistUpstre.png"),
  wrap_plots(kusko_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 42, dpi = 300
)

cat("\nKuskokwim DONE\n")
cat("  Saved to:", kusko_out_dir, "\n")

# ------------------------------------------------------------------------------
# Kuskokwim contour-only figure: log10(WtrshdSlp) on x-axis
# ------------------------------------------------------------------------------
kusko_log_data <- setNames(lapply(KUSKO_YEARS, function(yr) {
  kusko_yr_data[[as.character(yr)]] %>%
    filter(WtrshdSlp > 0) %>%
    mutate(log_slope = log10(WtrshdSlp))
}), KUSKO_YEARS)

KUSKO_X_LIM_LOG <- range(unlist(lapply(kusko_log_data, `[[`, "log_slope")), na.rm = TRUE)

kusko_x_breaks_orig <- scales::log_breaks(n = 8)(10^KUSKO_X_LIM_LOG)

kusko_log_plots <- lapply(KUSKO_YEARS, function(yr) {
  df <- kusko_log_data[[as.character(yr)]]

  w       <- df$assignment_norm / sum(df$assignment_norm) * nrow(df)
  H       <- ks::Hpi(x = cbind(df$log_slope, df$DistUpstre))
  H       <- (H + t(H)) / 2                    # force exact symmetry
  fit     <- ks::kde(x = cbind(df$log_slope, df$DistUpstre), H = H, w = w, gridsize = c(200, 200))
  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)

  pt_dens <- predict(fit, x = cbind(df$log_slope, df$DistUpstre))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w / sum(w))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))

  ggplot() +
    geom_contour_filled(data = grid_df, aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = breaks) +
    geom_point(data = df, aes(x = log_slope, y = DistUpstre),
               alpha = 0.0, color = "grey50", size = 0.8) +
    scale_fill_viridis_d("Quantiles", labels = scales::percent(rev(QUANTILES[-1])), direction = 1) +
    scale_x_continuous(breaks = log10(kusko_x_breaks_orig), labels = kusko_x_breaks_orig,
                       limits = KUSKO_X_LIM_LOG) +
    coord_cartesian(xlim = KUSKO_X_LIM_LOG, ylim = KUSKO_Y_LIM) +
    labs(x = "Watershed Slope (log₁₀ scale)", y = "Distance Upstream (km × 1000)") +
    y_scale + base_theme + ggtitle(yr)
})

# Save individual Kuskokwim log-slope panels (white background)
invisible(lapply(seq_along(KUSKO_YEARS), function(i) {
  ggsave(
    file.path(pres_out_dir, sprintf("Kusko_%d_WtrshdSlp_vs_DistUpstre_logSlope.png", KUSKO_YEARS[i])),
    kusko_log_plots[[i]] + white_panel_theme,
    width = 9, height = 7, dpi = 300
  )
}))

ggsave(
  file.path(kusko_out_dir, "Kusko_WtrshdSlp_vs_DistUpstre_logSlope.png"),
  wrap_plots(kusko_log_plots, ncol = 1, guides = "collect") + black_bg,
  width = 9, height = 42, dpi = 300
)

cat("Kuskokwim log-slope contours saved.\n")

# ------------------------------------------------------------------------------
# Twelve-panel figure: top-50% basin maps (left) + contour panels (right)
# one row per year, 6 years => 12 panels total (same style as Yukon six-panel)
# ------------------------------------------------------------------------------
source(here("Code", "Analysis", "01_DensityContours", "kusko_top50prod.R"))

if (!requireNamespace("png", quietly = TRUE))
  stop("Package 'png' needed for twelve-panel figure. Run: install.packages('png')")

kusko_map_panels <- lapply(KUSKO_YEARS, function(yr) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 9, height = 7, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  draw_kusko_top50_map(yr, KUSKO_EDGES, KUSKO_BASIN, threshold = CONTOUR_THRESHOLD)
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  img <- png::readPNG(tmp)
  wrap_elements(
    grid::rasterGrob(img, interpolate = TRUE,
                     width  = unit(1, "npc"),
                     height = unit(1, "npc"))
  )
})

# Interleave: [map_yr1, contour_yr1, map_yr2, contour_yr2, ...]
kusko_combined_panels <- unlist(
  lapply(seq_along(KUSKO_YEARS), function(i) {
    list(kusko_map_panels[[i]], kusko_plots[[i]])
  }),
  recursive = FALSE
)

kusko_twelve_panel_fig <- wrap_plots(kusko_combined_panels, ncol = 2) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = "white"))
  )

ggsave(
  file.path(kusko_out_dir, "Kusko_Top50_Contours_TwelvePanel.png"),
  kusko_twelve_panel_fig,
  width = 18, height = 42, dpi = 300
)

cat("\nKuskokwim twelve-panel figure saved.\n")
