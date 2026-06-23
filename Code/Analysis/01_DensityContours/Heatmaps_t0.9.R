################################################################################
# HEATMAP FIGURES — threshold = 0.9, binned production
#
# Same data pipeline as Contours_t0.9.R.  Both axes are discretised into
# N_X_BINS × N_Y_BINS rectangular cells; each cell is coloured by the *sum*
# of assignment_norm within it, normalised to [0, 1] within each panel so
# the colour scale is relative production.
#
# Reads:   Outputs/SensitivitySweep/t0.9/
# Writes:
#   Figures/Heatmaps/Yukon_heatmap_t0.9.png
#   Figures/Heatmaps/Kusko_heatmap_t0.9.png
#   Figures/Heatmaps/Presfigures/<basin>_<yr>_heatmap_t0.9.png
#
# USAGE (from project root):
#   source("Code/Analysis/01_DensityContours/Heatmaps_t0.9.R")
#   Rscript Code/Analysis/01_DensityContours/Heatmaps_t0.9.R
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(here)
library(scales)

# ==============================================================================
# Config
# ==============================================================================
YUKON_YEARS <- c(2015, 2016, 2021)
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
THRESHOLD   <- "0.9"
N_X_BINS    <- 12    # bins along the habitat axis (slope / distance)
N_Y_BINS    <- 10    # bins along the assignment axis (0–1 → each bin = 0.1)

csv_root     <- here("Outputs", "SensitivitySweep", paste0("t", THRESHOLD))
fig_dir      <- here("Figures", "Heatmaps")
pres_fig_dir <- here("Figures", "Heatmaps", "Presfigures")
dir.create(fig_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(pres_fig_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Load shapefiles
# ==============================================================================
cat("Loading shapefiles...\n")
yukon_attr <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

kusko_attr <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

# ==============================================================================
# Load CSVs
# ==============================================================================
cat("Loading t0.9 CSVs...\n")
yukon_data <- setNames(lapply(YUKON_YEARS, function(yr) {
  read_csv(
    file.path(csv_root, "Yukon",
              sprintf("%d_Yukon_Full_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_norm) %>%
    left_join(yukon_attr, by = "reachid") %>%
    filter(assignment_norm > 0, !is.na(WtrshdSlp), !is.na(DistUpstre))
}), YUKON_YEARS)

kusko_data <- setNames(lapply(KUSKO_YEARS, function(yr) {
  read_csv(
    file.path(csv_root, "Kusko",
              sprintf("%d_Kusko_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_norm) %>%
    left_join(kusko_attr, by = "reachid") %>%
    filter(assignment_norm > 0, !is.na(WtrshdSlp), !is.na(DistUpstre))
}), KUSKO_YEARS)

# ==============================================================================
# Fixed axis limits
# ==============================================================================
Y_LIM <- c(0, 1)

YUKON_DIST_LIM <- range(yukon_attr$DistUpstre, na.rm = TRUE)
KUSKO_DIST_LIM <- range(kusko_attr$DistUpstre, na.rm = TRUE)

yukon_log_all <- log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0])
kusko_log_all <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
YUKON_LOG_LIM <- quantile(yukon_log_all, c(0.01, 0.99), na.rm = TRUE)
KUSKO_LOG_LIM <- quantile(kusko_log_all, c(0.01, 0.99), na.rm = TRUE)

# ==============================================================================
# Binning helper
# ------------------------------------------------------------------------------
# Returns a data frame of bin midpoints (x, y) and normalised production
# (bin_norm ∈ [0, 1]) plus the cell half-widths for geom_tile.
# ==============================================================================
make_bin_df <- function(x_vals, y_vals, weights,
                        x_lim, y_lim,
                        n_x = N_X_BINS, n_y = N_Y_BINS) {

  x_breaks <- seq(x_lim[1], x_lim[2], length.out = n_x + 1)
  y_breaks <- seq(y_lim[1], y_lim[2], length.out = n_y + 1)
  x_mids   <- (x_breaks[-length(x_breaks)] + x_breaks[-1]) / 2
  y_mids   <- (y_breaks[-length(y_breaks)] + y_breaks[-1]) / 2

  xi <- findInterval(x_vals, x_breaks, rightmost.closed = TRUE)
  yi <- findInterval(y_vals, y_breaks, rightmost.closed = TRUE)
  ok <- xi >= 1L & xi <= n_x & yi >= 1L & yi <= n_y

  if (!any(ok)) return(NULL)

  bin_df <- data.frame(xi = xi[ok], yi = yi[ok], w = weights[ok]) %>%
    group_by(xi, yi) %>%
    summarise(production = sum(w), .groups = "drop") %>%
    mutate(
      x        = x_mids[xi],
      y        = y_mids[yi],
      bin_norm = production / max(production)
    )

  attr(bin_df, "x_hw") <- (x_breaks[2] - x_breaks[1]) / 2   # half-width
  attr(bin_df, "y_hw") <- (y_breaks[2] - y_breaks[1]) / 2
  bin_df
}

# ==============================================================================
# Shared aesthetics
# ==============================================================================

# Spectral fill: low production = cool purple/blue, high = warm red/orange
fill_scale <- scale_fill_distiller(
  palette   = "Spectral",
  direction = -1,
  limits    = c(0, 1),
  breaks    = seq(0, 1, 0.25),
  labels    = c("0", "0.25", "0.50", "0.75", "1.0"),
  name      = "Relative\nproduction",
  na.value  = "grey92"
)

base_theme <- theme_minimal(base_size = 20) +
  theme(
    axis.text         = element_text(size = 18, color = "grey20"),
    axis.title        = element_text(size = 22, color = "grey10"),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    panel.grid        = element_blank(),
    plot.title        = element_text(size = 26, face = "bold", hjust = 0.5,
                                     margin = margin(b = 10)),
    legend.title      = element_text(size = 18),
    legend.text       = element_text(size = 16),
    legend.key.height = unit(2, "cm"),
    plot.background   = element_rect(fill = "white", color = NA),
    plot.margin       = margin(14, 14, 14, 14)
  )

base_theme_white <- base_theme   # identical here; kept for symmetry with original

plain_ann <- function(title_text) {
  plot_annotation(
    title = title_text,
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title      = element_text(color = "grey10", size = 28,
                                     hjust = 0.5, margin = margin(b = 12))
    )
  )
}

# ==============================================================================
# X-axis label helpers
# ==============================================================================

# Log-slope axis: breaks in log10 space, labels as original slope values
make_log_x_scale <- function(x_lim, n_x = N_X_BINS) {
  x_breaks <- seq(x_lim[1], x_lim[2], length.out = n_x + 1)
  x_mids   <- (x_breaks[-length(x_breaks)] + x_breaks[-1]) / 2
  # Label every other midpoint to avoid crowding
  show      <- seq(1, n_x, by = max(1, round(n_x / 6)))
  scale_x_continuous(
    breaks = x_mids[show],
    labels = signif(10^x_mids[show], 2),
    expand = c(0, 0)
  )
}

# Distance axis: breaks in metres, labels in km (×1000)
make_dist_x_scale <- function(x_lim, n_x = N_X_BINS) {
  x_breaks <- seq(x_lim[1], x_lim[2], length.out = n_x + 1)
  x_mids   <- (x_breaks[-length(x_breaks)] + x_breaks[-1]) / 2
  show      <- seq(1, n_x, by = max(1, round(n_x / 6)))
  scale_x_continuous(
    breaks = x_mids[show],
    labels = round(x_mids[show] / 1e6, 2),
    expand = c(0, 0)
  )
}

# Y-axis: assignment_norm in 0–1, label every other bin midpoint
make_y_scale <- function(n_y = N_Y_BINS) {
  y_breaks <- seq(0, 1, length.out = n_y + 1)
  y_mids   <- (y_breaks[-length(y_breaks)] + y_breaks[-1]) / 2
  show      <- seq(1, n_y, by = max(1, round(n_y / 5)))
  scale_y_continuous(
    breaks = y_mids[show],
    labels = round(y_mids[show], 2),
    expand = c(0, 0)
  )
}

# ==============================================================================
# Panel builders
# ==============================================================================

hm_log_panel <- function(df, x_lim_log, y_lim, yr, thm = base_theme) {
  df <- df %>% filter(WtrshdSlp > 0) %>% mutate(log_slope = log10(WtrshdSlp))

  if (nrow(df) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = sprintf("n = %d\n(too few)", nrow(df)),
                 color = "firebrick", size = 5, hjust = 0.5) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(x = "Watershed Slope", y = "Assignment (normalized)") +
        thm + ggtitle(yr)
    )
  }

  bd   <- make_bin_df(df$log_slope, df$assignment_norm, df$assignment_norm,
                      x_lim_log, y_lim)
  x_hw <- attr(bd, "x_hw")
  y_hw <- attr(bd, "y_hw")

  ggplot(bd, aes(x = x, y = y, fill = bin_norm)) +
    geom_tile(width = 2 * x_hw, height = 2 * y_hw, color = "white", linewidth = 0.3) +
    fill_scale +
    make_log_x_scale(x_lim_log) +
    make_y_scale() +
    coord_cartesian(xlim = x_lim_log, ylim = y_lim) +
    labs(x = "Watershed Slope", y = "Assignment (normalized)") +
    thm + ggtitle(yr)
}

hm_dist_panel <- function(df, x_lim, y_lim, yr, thm = base_theme) {
  if (nrow(df) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = sprintf("n = %d\n(too few)", nrow(df)),
                 color = "firebrick", size = 5, hjust = 0.5) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(x = "Distance Upstream (km × 1000)", y = "Assignment (normalized)") +
        thm + ggtitle(yr)
    )
  }

  bd   <- make_bin_df(df$DistUpstre, df$assignment_norm, df$assignment_norm,
                      x_lim, y_lim)
  x_hw <- attr(bd, "x_hw")
  y_hw <- attr(bd, "y_hw")

  ggplot(bd, aes(x = x, y = y, fill = bin_norm)) +
    geom_tile(width = 2 * x_hw, height = 2 * y_hw, color = "white", linewidth = 0.3) +
    fill_scale +
    make_dist_x_scale(x_lim) +
    make_y_scale() +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = "Distance Upstream (km × 1000)", y = "Assignment (normalized)") +
    thm + ggtitle(yr)
}

# ==============================================================================
# Composite panel builder
# ==============================================================================
build_panels <- function(yr_data, years, log_lim, dist_lim, y_lim,
                         thm = base_theme) {
  panels <- vector("list", length(years) * 2)
  for (i in seq_along(years)) {
    yr <- years[i]
    df <- yr_data[[as.character(yr)]]
    cat(sprintf("    %s — %d rows\n", yr, nrow(df)))
    panels[[2 * i - 1]] <- hm_log_panel(df,  log_lim,  y_lim, yr, thm)
    panels[[2 * i]]     <- hm_dist_panel(df, dist_lim, y_lim, yr, thm)
  }
  panels
}

# ==============================================================================
# Per-year Presfigures
# ==============================================================================
save_year_figs <- function(yr_data, years, basin, log_lim, dist_lim, y_lim) {
  for (yr in years) {
    df     <- yr_data[[as.character(yr)]]
    p_log  <- hm_log_panel(df,  log_lim,  y_lim, yr, thm = base_theme_white)
    p_dist <- hm_dist_panel(df, dist_lim, y_lim, yr, thm = base_theme_white)

    fig <- (p_log | p_dist) +
      plot_annotation(
        title = sprintf("%s — %d", basin, yr),
        theme = theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title      = element_text(color = "grey10", size = 28,
                                         hjust = 0.5, margin = margin(b = 12))
        )
      )

    fname <- file.path(pres_fig_dir,
                       sprintf("%s_%d_heatmap_t%s.png", basin, yr, THRESHOLD))
    ggsave(fname, fig, width = 18, height = 7, dpi = 150)
    cat("  Saved:", fname, "\n")
  }
}

save_fig <- function(panels, n_years, basin) {
  fig <- wrap_plots(panels, ncol = 2, guides = "collect") +
    plain_ann(basin)
  fname <- file.path(fig_dir,
                     sprintf("%s_heatmap_t%s.png", basin, THRESHOLD))
  ggsave(fname, fig, width = 18, height = 7 * n_years, dpi = 150)
  cat("  Saved:", fname, "\n")
}

# ==============================================================================
# Run
# ==============================================================================
cat("\n=== Yukon ===\n")
yukon_panels <- build_panels(yukon_data, YUKON_YEARS,
                             YUKON_LOG_LIM, YUKON_DIST_LIM, Y_LIM)
save_fig(yukon_panels, length(YUKON_YEARS), "Yukon")
cat("  Per-year figures...\n")
save_year_figs(yukon_data, YUKON_YEARS, "Yukon",
               YUKON_LOG_LIM, YUKON_DIST_LIM, Y_LIM)

cat("\n=== Kusko ===\n")
kusko_panels <- build_panels(kusko_data, KUSKO_YEARS,
                             KUSKO_LOG_LIM, KUSKO_DIST_LIM, Y_LIM)
save_fig(kusko_panels, length(KUSKO_YEARS), "Kusko")
cat("  Per-year figures...\n")
save_year_figs(kusko_data, KUSKO_YEARS, "Kusko",
               KUSKO_LOG_LIM, KUSKO_DIST_LIM, Y_LIM)

cat("\nDone. Figures saved to Figures/Heatmaps/\n")
