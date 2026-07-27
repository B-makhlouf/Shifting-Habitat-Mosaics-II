################################################################################
# CONTOUR FIGURES — threshold = 0.9, two methods
#
# Reads pre-computed assignment CSVs from Outputs/SensitivitySweep/t0.9/ and
# produces four two-column composite figures:
#
#   Figures/Contours/Yukon_ggplot_t0.9.png       }  ggplot geom_density_2d_filled
#   Figures/Contours/Kusko_ggplot_t0.9.png       }
#   Figures/Contours/Yukon_quantiles_t0.9.png    }  ks::kde production-weighted
#   Figures/Contours/Kusko_quantiles_t0.9.png    }    quantile contours
#
# Each figure: left column = Watershed Slope (log), right column = Distance
# Upstream, one row per year.
#
# USAGE (from project root):
#   source("Code/Analysis/01_DensityContours/Contours_t0.9.R")
#   Rscript Code/Analysis/01_DensityContours/Contours_t0.9.R
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(here)
library(ks)
library(scales)

# ==============================================================================
# Config
# ==============================================================================
YUKON_YEARS <- c(2015, 2016, 2021)
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
THRESHOLD   <- "0.9"
QUANTILES   <- c(0, 0.1,0.2, 0.4, 0.6, 0.8)   # contour probability levels (quantiles method)

csv_root     <- here("Outputs", "SensitivitySweep", paste0("t", THRESHOLD))
fig_dir      <- here("Figures", "Contours")
pres_fig_dir <- here("Figures", "Contours", "Presfigures", "Contours")
dir.create(fig_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(pres_fig_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Load shapefiles for habitat attributes
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
# Load CSVs and join attributes
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
# Fixed axis limits (derived from full spatial data, not filtered subset)
# ==============================================================================
Y_LIM <- c(0, 1)

YUKON_DIST_LIM <- range(yukon_attr$DistUpstre, na.rm = TRUE)
KUSKO_DIST_LIM <- range(kusko_attr$DistUpstre, na.rm = TRUE)

yukon_log_all    <- log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0])
kusko_log_all    <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
YUKON_LOG_LIM    <- quantile(yukon_log_all, c(0.01, 0.99), na.rm = TRUE)
KUSKO_LOG_LIM    <- quantile(kusko_log_all, c(0.01, 0.99), na.rm = TRUE)

# Evenly-spaced break *positions* in log10 space, labelled with original values.
# Using pretty() avoids the uneven gaps that log_breaks() produces when its
# "nice" original-scale values (1, 2, 5, 10 …) are back-converted to log space.
yukon_log_break_pos <- pretty(YUKON_LOG_LIM, n = 6)
kusko_log_break_pos <- pretty(KUSKO_LOG_LIM, n = 6)
yukon_log_break_lab <- signif(10^yukon_log_break_pos, 3)
kusko_log_break_lab <- signif(10^kusko_log_break_pos, 3)

# Distance Upstream x scale: computed per-basin from the actual data range so
# breaks land within the axis no matter how short the basin's reach network is.
make_dist_x_scale <- function(x_lim) {
  brks <- pretty(x_lim, n = 5)
  brks <- brks[brks >= x_lim[1] & brks <= x_lim[2]]
  scale_x_continuous(breaks = brks, labels = round(brks / 1e6, 2))
}

make_dist_y_scale <- function(x_lim) {
  brks <- pretty(x_lim, n = 5)
  brks <- brks[brks >= x_lim[1] & brks <= x_lim[2]]
  scale_y_continuous(breaks = brks, labels = round(brks / 1e6, 2))
}

# ==============================================================================
# Shared theme
# ==============================================================================
base_theme <- theme_grey() +
  theme(
    axis.text        = element_text(size = 22, color = "grey30"),
    axis.title       = element_text(size = 26, color = "grey20"),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey40", linewidth = 1.2),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 30, face = "bold", hjust = 0.5,
                                    margin = margin(b = 10)),
    legend.title     = element_text(size = 22),
    legend.text      = element_text(size = 20),
    legend.key.size  = unit(1.2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(16, 16, 16, 16)
  )

# White-background variant used for the per-year presentation figures
base_theme_white <- theme_bw() +
  theme(
    axis.text        = element_text(size = 22, color = "grey30"),
    axis.title       = element_text(size = 26, color = "grey20"),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80", linewidth = 1.2),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 30, face = "bold", hjust = 0.5,
                                    margin = margin(b = 10)),
    legend.title     = element_text(size = 22),
    legend.text      = element_text(size = 20),
    legend.key.size  = unit(1.2, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(16, 16, 16, 16)
  )

plain_ann <- function(title_text) {
  plot_annotation(
    title = title_text,
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title      = element_text(color = "grey10", size = 32, hjust = 0.5,
                                     margin = margin(b = 12))
    )
  )
}

# ==============================================================================
# Panel helpers — ggplot (geom_density_2d_filled)
# ------------------------------------------------------------------------------
# Fixed ndensity breaks (0–1 in 10 equal steps) force identical fill levels
# across every panel so guides = "collect" produces a single shared legend.
# ==============================================================================
# Start at 0.05 so near-zero-density cells get na.value = "white" and fade into
# the background rather than showing as a hard rectangular border.
GG_BREAKS <- c(0.05, seq(0.1, 1, by = 0.1))   # 10 values → 9 bands + transparent fringe

gg_log_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, y_lim, yr,
                         flipped = FALSE) {
  df <- df %>% filter(WtrshdSlp > 0) %>% mutate(log_slope = log10(WtrshdSlp))
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text",
               x = mean(if (flipped) y_lim     else x_lim_log),
               y = mean(if (flipped) x_lim_log else y_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      (if (flipped) scale_y_continuous(breaks = x_break_pos, labels = x_break_lab)
       else          scale_x_continuous(breaks = x_break_pos, labels = x_break_lab)) +
      (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim_log)
       else          coord_cartesian(xlim = x_lim_log, ylim = y_lim)) +
      (if (flipped) labs(x = "Assignment (normalized)", y = "Watershed Slope (log₁₀ scale)")
       else          labs(x = "Watershed Slope (log₁₀ scale)", y = "Assignment (normalized)")) +
      base_theme + ggtitle(yr))
  }
  base_aes <- if (flipped) {
    aes(x = assignment_norm, y = log_slope, weight = assignment_norm)
  } else {
    aes(x = log_slope, y = assignment_norm, weight = assignment_norm)
  }
  ggplot(df, base_aes) +
    geom_density_2d_filled(contour_var = "ndensity", breaks = GG_BREAKS) +
    scale_fill_viridis_d("Norm.\ndensity", direction = 1, na.value = "white") +
    (if (flipped) scale_y_continuous(breaks = x_break_pos, labels = x_break_lab)
     else          scale_x_continuous(breaks = x_break_pos, labels = x_break_lab)) +
    (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim_log)
     else          coord_cartesian(xlim = x_lim_log, ylim = y_lim)) +
    (if (flipped) labs(x = "Assignment (normalized)", y = "Watershed Slope (log₁₀ scale)")
     else          labs(x = "Watershed Slope (log₁₀ scale)", y = "Assignment (normalized)")) +
    base_theme + ggtitle(yr)
}

gg_dist_panel <- function(df, x_lim, y_lim, yr, flipped = FALSE) {
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text",
               x = mean(if (flipped) y_lim  else x_lim),
               y = mean(if (flipped) x_lim  else y_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      (if (flipped) make_dist_y_scale(x_lim) else make_dist_x_scale(x_lim)) +
      (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim)
       else          coord_cartesian(xlim = x_lim, ylim = y_lim)) +
      (if (flipped) labs(x = "Assignment (normalized)", y = "Distance Upstream (km × 1000)")
       else          labs(x = "Distance Upstream (km × 1000)", y = "Assignment (normalized)")) +
      base_theme + ggtitle(yr))
  }
  base_aes <- if (flipped) {
    aes(x = assignment_norm, y = DistUpstre, weight = assignment_norm)
  } else {
    aes(x = DistUpstre, y = assignment_norm, weight = assignment_norm)
  }
  ggplot(df, base_aes) +
    geom_density_2d_filled(contour_var = "ndensity", breaks = GG_BREAKS) +
    scale_fill_viridis_d("Norm.\ndensity", direction = 1, na.value = "white") +
    (if (flipped) make_dist_y_scale(x_lim) else make_dist_x_scale(x_lim)) +
    (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim)
     else          coord_cartesian(xlim = x_lim, ylim = y_lim)) +
    (if (flipped) labs(x = "Assignment (normalized)", y = "Distance Upstream (km × 1000)")
     else          labs(x = "Distance Upstream (km × 1000)", y = "Assignment (normalized)")) +
    base_theme + ggtitle(yr)
}

# ==============================================================================
# Panel helpers — quantiles (ks::kde, production-weighted)
# ==============================================================================
kde_breaks <- function(x, y, w) {
  w_norm  <- w / sum(w) * length(w)
  H       <- ks::Hpi(x = cbind(x, y))
  H       <- (H + t(H)) / 2
  fit     <- ks::kde(x = cbind(x, y), H = H, w = w_norm, gridsize = c(200, 200))
  pt_dens <- predict(fit, x = cbind(x, y))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w_norm / sum(w_norm))[ord])
  breaks  <- sort(unique(approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y))
  list(fit = fit, breaks = breaks)
}

qt_log_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, y_lim, yr,
                         flipped = FALSE) {
  df <- df %>% filter(WtrshdSlp > 0) %>% mutate(log_slope = log10(WtrshdSlp))
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text",
               x = mean(if (flipped) y_lim     else x_lim_log),
               y = mean(if (flipped) x_lim_log else y_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      (if (flipped) scale_y_continuous(breaks = x_break_pos, labels = x_break_lab)
       else          scale_x_continuous(breaks = x_break_pos, labels = x_break_lab)) +
      (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim_log)
       else          coord_cartesian(xlim = x_lim_log, ylim = y_lim)) +
      (if (flipped) labs(x = "Assignment (normalized)", y = "Watershed Slope (log₁₀ scale)")
       else          labs(x = "Watershed Slope (log₁₀ scale)", y = "Assignment (normalized)")) +
      base_theme + ggtitle(yr))
  }
  # When flipped, swap which variable is the first (x) dimension fed to kde_breaks
  if (flipped) {
    kb      <- kde_breaks(df$assignment_norm, df$log_slope, df$assignment_norm)
  } else {
    kb      <- kde_breaks(df$log_slope, df$assignment_norm, df$assignment_norm)
  }
  grid_df   <- expand.grid(x = kb$fit$eval.points[[1]], y = kb$fit$eval.points[[2]])
  grid_df$z <- as.vector(kb$fit$estimate)
  ggplot() +
    geom_contour_filled(data = grid_df,
                        aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = kb$breaks) +
    scale_fill_brewer("Quantiles",
                      labels = scales::percent(rev(QUANTILES[-1])),
                      palette = "YlOrRd", direction = 1) +
    (if (flipped) scale_y_continuous(breaks = x_break_pos, labels = x_break_lab)
     else          scale_x_continuous(breaks = x_break_pos, labels = x_break_lab)) +
    (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim_log)
     else          coord_cartesian(xlim = x_lim_log, ylim = y_lim)) +
    (if (flipped) labs(x = "Assignment (normalized)", y = "Watershed Slope (log₁₀ scale)")
     else          labs(x = "Watershed Slope (log₁₀ scale)", y = "Assignment (normalized)")) +
    base_theme + ggtitle(yr)
}

qt_dist_panel <- function(df, x_lim, y_lim, yr, flipped = FALSE) {
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text",
               x = mean(if (flipped) y_lim else x_lim),
               y = mean(if (flipped) x_lim else y_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      (if (flipped) make_dist_y_scale(x_lim) else make_dist_x_scale(x_lim)) +
      (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim)
       else          coord_cartesian(xlim = x_lim, ylim = y_lim)) +
      (if (flipped) labs(x = "Assignment (normalized)", y = "Distance Upstream (km × 1000)")
       else          labs(x = "Distance Upstream (km × 1000)", y = "Assignment (normalized)")) +
      base_theme + ggtitle(yr))
  }
  if (flipped) {
    kb      <- kde_breaks(df$assignment_norm, df$DistUpstre, df$assignment_norm)
  } else {
    kb      <- kde_breaks(df$DistUpstre, df$assignment_norm, df$assignment_norm)
  }
  grid_df   <- expand.grid(x = kb$fit$eval.points[[1]], y = kb$fit$eval.points[[2]])
  grid_df$z <- as.vector(kb$fit$estimate)
  ggplot() +
    geom_contour_filled(data = grid_df,
                        aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = kb$breaks) +
    scale_fill_brewer("Quantiles",
                      labels = scales::percent(rev(QUANTILES[-1])),
                      palette = "YlOrRd", direction = 1) +
    (if (flipped) make_dist_y_scale(x_lim) else make_dist_x_scale(x_lim)) +
    (if (flipped) coord_cartesian(xlim = y_lim, ylim = x_lim)
     else          coord_cartesian(xlim = x_lim, ylim = y_lim)) +
    (if (flipped) labs(x = "Assignment (normalized)", y = "Distance Upstream (km × 1000)")
     else          labs(x = "Distance Upstream (km × 1000)", y = "Assignment (normalized)")) +
    base_theme + ggtitle(yr)
}

# ==============================================================================
# Build interleaved panel list for patchwork ncol = 2
# [slp_yr1, dist_yr1, slp_yr2, dist_yr2, ...]  →  left=slope, right=dist per row
# ==============================================================================
build_panels <- function(yr_data, years, log_lim, log_break_pos, log_break_lab,
                         dist_lim, y_lim, method = c("ggplot", "quantiles")) {
  method  <- match.arg(method)
  log_fn  <- if (method == "ggplot") gg_log_panel  else qt_log_panel
  dist_fn <- if (method == "ggplot") gg_dist_panel else qt_dist_panel

  panels <- vector("list", length(years) * 2)
  for (i in seq_along(years)) {
    yr <- years[i]
    df <- yr_data[[as.character(yr)]]
    cat(sprintf("    %s — %d rows\n", yr, nrow(df)))
    panels[[2 * i - 1]] <- log_fn(df,  log_lim, log_break_pos, log_break_lab,
                                   y_lim, yr)
    panels[[2 * i]]     <- dist_fn(df, dist_lim, y_lim, yr)
  }
  panels
}

# ------------------------------------------------------------------------------
# Save one figure per year to Presfigures/Contours (white background)
# ------------------------------------------------------------------------------
save_year_figs <- function(yr_data, years, basin, method,
                           log_lim, log_break_pos, log_break_lab,
                           dist_lim, y_lim) {
  log_fn  <- if (method == "ggplot") gg_log_panel  else qt_log_panel
  dist_fn <- if (method == "ggplot") gg_dist_panel else qt_dist_panel

  for (yr in years) {
    df     <- yr_data[[as.character(yr)]]
    p_log  <- log_fn(df,  log_lim, log_break_pos, log_break_lab, y_lim, yr,
                     flipped = TRUE) + base_theme_white
    p_dist <- dist_fn(df, dist_lim, y_lim, yr,
                     flipped = TRUE) + base_theme_white

    fig <- (p_log | p_dist) +
      plot_annotation(
        title = sprintf("%s — %d", basin, yr),
        theme = theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title      = element_text(color = "grey10", size = 32, hjust = 0.5,
                                         margin = margin(b = 12))
        )
      )

    fname <- file.path(pres_fig_dir,
                       sprintf("%s_%d_%s_t%s.png", basin, yr, method, THRESHOLD))
    ggsave(fname, fig, width = 18, height = 7, dpi = 150)
    cat("  Saved:", fname, "\n")
  }
}

save_fig <- function(panels, n_years, basin, method, n_label) {
  fig <- wrap_plots(panels, ncol = 2, guides = "collect") +
    plain_ann(basin)
  fname <- file.path(fig_dir,
                     sprintf("%s_%s_t%s.png", basin, method, THRESHOLD))
  ggsave(fname, fig, width = 18, height = 7 * n_years, dpi = 150)
  cat("  Saved:", fname, "\n")
}

# ==============================================================================
# Produce all four figures
# ==============================================================================
for (method in c("ggplot", "quantiles")) {
  cat(sprintf("\n=== Method: %s ===\n", method))

  cat("  Building Yukon panels...\n")
  yukon_panels <- build_panels(yukon_data, YUKON_YEARS,
                               YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                               YUKON_DIST_LIM, Y_LIM, method)
  save_fig(yukon_panels, length(YUKON_YEARS), "Yukon", method,
           paste(sapply(yukon_data, nrow), collapse = " / "))

  cat("  Saving individual Yukon year figures...\n")
  save_year_figs(yukon_data, YUKON_YEARS, "Yukon", method,
                 YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                 YUKON_DIST_LIM, Y_LIM)

  cat("  Building Kusko panels...\n")
  kusko_panels <- build_panels(kusko_data, KUSKO_YEARS,
                               KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                               KUSKO_DIST_LIM, Y_LIM, method)
  save_fig(kusko_panels, length(KUSKO_YEARS), "Kusko", method,
           paste(sapply(kusko_data, nrow), collapse = " / "))

  cat("  Saving individual Kusko year figures...\n")
  save_year_figs(kusko_data, KUSKO_YEARS, "Kusko", method,
                 KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                 KUSKO_DIST_LIM, Y_LIM)
}

cat("\nDone. Four figures saved to Figures/Contours/\n")
