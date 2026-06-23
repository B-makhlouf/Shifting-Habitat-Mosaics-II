################################################################################
# CONTOUR FIGURES — assignment_norm > 0.8 filter, Watershed Slope vs Distance
#
# Reads pre-computed assignment CSVs from Outputs/SensitivitySweep/t0.9/,
# filters to assignment_norm > 0.8, then plots Watershed Slope (log₁₀) on x
# against Distance Upstream on y, with each point weighted by its actual
# assignment_norm value (so high-confidence reaches drive the density).
#
# Produces composite (all-years) and per-year figures for both methods:
#   ggplot    — geom_density_2d_filled
#   quantiles — ks::kde, production-weighted
#
# Output: Figures/Contours/Presfigures/Thresh/
#
# USAGE (from project root):
#   source("Code/Analysis/01_DensityContours/ContourThreshnew.R")
#   Rscript Code/Analysis/01_DensityContours/ContourThreshnew.R
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(here)
library(ks)
library(scales)
library(magick)

# ==============================================================================
# Config
# ==============================================================================
YUKON_YEARS  <- c(2015, 2016, 2021)
KUSKO_YEARS  <- c(2017, 2018, 2019, 2020, 2021, 2022)
CSV_THRESH   <- "0.9"          # folder under SensitivitySweep (data source)
FILT_THRESH  <- 0.7         # assignment_norm minimum to include
QUANTILES    <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9)

csv_root <- here("Outputs", "SensitivitySweep", paste0("t", CSV_THRESH))
fig_dir  <- here("Figures", "Contours", "Presfigures", "Thresh")
# Per-method subfolders created at run time inside the main loop

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
# Load CSVs, join attributes, filter to assignment_norm > FILT_THRESH
# ==============================================================================
cat(sprintf("Loading CSVs and filtering to assignment_norm > %.1f...\n", FILT_THRESH))

load_filtered <- function(basin_subdir, pattern, attr_df) {
  years <- if (basin_subdir == "Yukon") YUKON_YEARS else KUSKO_YEARS
  setNames(lapply(years, function(yr) {
    read_csv(
      file.path(csv_root, basin_subdir, sprintf(pattern, yr)),
      show_col_types = FALSE
    ) %>%
      dplyr::select(reachid, assignment_norm) %>%
      left_join(attr_df, by = "reachid") %>%
      filter(
        assignment_norm > FILT_THRESH,
        !is.na(WtrshdSlp), WtrshdSlp > 0,
        !is.na(DistUpstre)
      ) %>%
      mutate(log_slope = log10(WtrshdSlp))
  }), years)
}

yukon_data <- load_filtered("Yukon", "%d_Yukon_Full_Assignment_Results.csv", yukon_attr)
kusko_data <- load_filtered("Kusko", "%d_Kusko_Assignment_Results.csv",      kusko_attr)

# ==============================================================================
# Fixed axis limits (from full spatial data, not the filtered subset)
# ==============================================================================
YUKON_DIST_LIM <- range(yukon_attr$DistUpstre, na.rm = TRUE)
KUSKO_DIST_LIM <- range(kusko_attr$DistUpstre, na.rm = TRUE)

yukon_log_all       <- log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0])
kusko_log_all       <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
YUKON_LOG_LIM       <- quantile(yukon_log_all, c(0.01, 0.99), na.rm = TRUE)
KUSKO_LOG_LIM       <- quantile(kusko_log_all, c(0.01, 0.99), na.rm = TRUE)

yukon_log_break_pos <- pretty(YUKON_LOG_LIM, n = 6)
kusko_log_break_pos <- pretty(KUSKO_LOG_LIM, n = 6)
yukon_log_break_lab <- signif(10^yukon_log_break_pos, 3)
kusko_log_break_lab <- signif(10^kusko_log_break_pos, 3)

make_dist_y_scale <- function(dist_lim) {
  brks <- pretty(dist_lim, n = 5)
  brks <- brks[brks >= dist_lim[1] & brks <= dist_lim[2]]
  scale_y_continuous(breaks = brks, labels = round(brks / 1e6, 2))
}

# ==============================================================================
# Shared theme — white background throughout
# ==============================================================================
base_theme <- theme_bw() +
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
# Panel helper — ggplot (geom_density_2d_filled)
# x = log_slope, y = DistUpstre, weighted by assignment_norm
# ==============================================================================
# Start breaks at 0.05 so cells with density < 5 % of max get na.value = "white"
# and blend into the background instead of showing as a hard rectangular border.
GG_BREAKS <- c(0.05, seq(0.1, 1, by = 0.1))

gg_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, dist_lim, yr) {
  y_fmt <- make_dist_y_scale(dist_lim)
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text", x = mean(x_lim_log), y = mean(dist_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
      y_fmt +
      coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
      labs(x = "Watershed Slope (log₁₀ scale)",
           y = "Distance Upstream (km × 1000)") +
      base_theme + ggtitle(yr))
  }
  ggplot(df, aes(x = log_slope, y = DistUpstre, weight = assignment_norm)) +
    geom_density_2d_filled(contour_var = "ndensity", breaks = GG_BREAKS) +
    scale_fill_viridis_d("Norm.\ndensity", direction = 1, na.value = "white") +
    scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
    y_fmt +
    coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
    labs(x = "Watershed Slope (log₁₀ scale)",
         y = "Distance Upstream (km × 1000)") +
    base_theme + ggtitle(yr)
}

# ==============================================================================
# Panel helper — quantiles (ks::kde, weighted by assignment_norm)
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

qt_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, dist_lim, yr) {
  y_fmt <- make_dist_y_scale(dist_lim)
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text", x = mean(x_lim_log), y = mean(dist_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
      y_fmt +
      coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
      labs(x = "Watershed Slope (log₁₀ scale)",
           y = "Distance Upstream (km × 1000)") +
      base_theme + ggtitle(yr))
  }
  kb      <- kde_breaks(df$log_slope, df$DistUpstre, df$assignment_norm)
  grid_df <- expand.grid(x = kb$fit$eval.points[[1]], y = kb$fit$eval.points[[2]])
  grid_df$z <- as.vector(kb$fit$estimate)
  ggplot() +
    geom_contour_filled(data = grid_df,
                        aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = kb$breaks) +
    scale_fill_brewer("Quantiles",
                      labels = scales::percent(rev(QUANTILES[-1])),
                      palette = "YlOrRd", direction = -1) +
    scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
    y_fmt +
    coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
    labs(x = "Watershed Slope (log₁₀ scale)",
         y = "Distance Upstream (km × 1000)") +
    base_theme + ggtitle(yr)
}

# ==============================================================================
# Build panel list (one panel per year)
# ==============================================================================
build_panels <- function(yr_data, years, log_lim, log_break_pos, log_break_lab,
                         dist_lim, method = c("ggplot", "quantiles")) {
  method   <- match.arg(method)
  panel_fn <- if (method == "ggplot") gg_panel else qt_panel
  lapply(years, function(yr) {
    df <- yr_data[[as.character(yr)]]
    cat(sprintf("    %s — %d rows above %.1f\n", yr, nrow(df), FILT_THRESH))
    panel_fn(df, log_lim, log_break_pos, log_break_lab, dist_lim, yr)
  })
}

# ==============================================================================
# Save composite (all-years stacked) figure
# ==============================================================================
save_composite <- function(panels, n_years, basin, method, out_dir) {
  fig <- wrap_plots(panels, ncol = 1, guides = "collect") +
    plain_ann(sprintf("%s — assignment > %.1f", basin, FILT_THRESH))
  fname <- file.path(out_dir,
                     sprintf("%s_composite_thresh%.1f.png", basin, FILT_THRESH))
  ggsave(fname, fig, width = 10, height = 7 * n_years, dpi = 150)
  cat("  Saved:", fname, "\n")
}

# ==============================================================================
# Save one figure per year
# ==============================================================================
save_year_figs <- function(yr_data, years, basin, method, out_dir,
                           log_lim, log_break_pos, log_break_lab, dist_lim) {
  panel_fn <- if (method == "ggplot") gg_panel else qt_panel
  for (yr in years) {
    df    <- yr_data[[as.character(yr)]]
    fig   <- panel_fn(df, log_lim, log_break_pos, log_break_lab, dist_lim, yr)
    fname <- file.path(out_dir,
                       sprintf("%s_%d_thresh%.1f.png", basin, yr, FILT_THRESH))
    ggsave(fname, fig, width = 10, height = 7, dpi = 150)
    cat("  Saved:", fname, "\n")
  }
}

# ==============================================================================
# Produce all figures — one subfolder per method
# ==============================================================================
for (method in c("ggplot", "quantiles")) {
  cat(sprintf("\n=== Method: %s ===\n", method))

  out_dir <- file.path(fig_dir, method)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cat("  Building Yukon panels...\n")
  yukon_panels <- build_panels(yukon_data, YUKON_YEARS,
                               YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                               YUKON_DIST_LIM, method)
  save_composite(yukon_panels, length(YUKON_YEARS), "Yukon", method, out_dir)
  cat("  Saving individual Yukon year figures...\n")
  save_year_figs(yukon_data, YUKON_YEARS, "Yukon", method, out_dir,
                 YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                 YUKON_DIST_LIM)

  cat("  Building Kusko panels...\n")
  kusko_panels <- build_panels(kusko_data, KUSKO_YEARS,
                               KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                               KUSKO_DIST_LIM, method)
  save_composite(kusko_panels, length(KUSKO_YEARS), "Kusko", method, out_dir)
  cat("  Saving individual Kusko year figures...\n")
  save_year_figs(kusko_data, KUSKO_YEARS, "Kusko", method, out_dir,
                 KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                 KUSKO_DIST_LIM)
}

# ==============================================================================
# Animate per-year PNGs into a GIF (one per basin per method)
# ==============================================================================
make_gif <- function(years, basin, out_dir, fps = 1) {
  fnames <- file.path(out_dir,
                      sprintf("%s_%d_thresh%.1f.png", basin, years, FILT_THRESH))
  missing <- fnames[!file.exists(fnames)]
  if (length(missing) > 0) {
    cat("  Skipping GIF — missing frames:\n")
    for (f in missing) cat("   ", f, "\n")
    return(invisible(NULL))
  }
  frames  <- image_read(fnames)
  gif     <- image_animate(image_join(frames), fps = fps, optimize = TRUE)
  gif_path <- file.path(out_dir, sprintf("%s_animated.gif", basin))
  image_write(gif, gif_path)
  cat("  GIF saved:", gif_path, "\n")
}

cat("\nBuilding GIFs...\n")
for (method in c("ggplot", "quantiles")) {
  out_dir <- file.path(fig_dir, method)
  cat(sprintf("  %s/\n", method))
  make_gif(YUKON_YEARS, "Yukon", out_dir)
  make_gif(KUSKO_YEARS, "Kusko", out_dir)
}

cat(sprintf("\nDone. Figures saved to %s\n", fig_dir))

# ==============================================================================
# Option 2 — Smooth interpolated GIF
#
# Strategy: pre-compute a normalised KDE density surface per year on a shared
# 100×100 grid (MASS::kde2d, unweighted — reaches are already filtered to
# high-confidence via FILT_THRESH), tween the z values between consecutive
# years with tweenr::tween_states(), render each intermediate frame as a
# geom_contour_filled() plot, then stitch the PNGs into a GIF with magick.
#
# Output: *_animated_smooth.gif alongside the existing *_animated.gif files.
# Nothing above this line is modified.
#
# Install deps if missing:
#   install.packages(c("tweenr", "MASS"))
# ==============================================================================

library(tweenr)
library(MASS)     # kde2d  (ships with base R, usually already attached)

SMOOTH_TRANS_FRAMES <- 20   # interpolation frames between each pair of years
SMOOTH_HOLD_FRAMES  <-  8   # frames to hold still on each year
SMOOTH_FPS          <- 10   # playback speed (frames per second)

make_smooth_gif <- function(yr_data, years, basin,
                             log_lim, log_break_pos, log_break_lab,
                             dist_lim, method, out_dir) {
  cat(sprintf("  Smooth GIF: %s / %s (%d years)...\n", basin, method, length(years)))

  # ── 1. Pre-compute normalised KDE grid per year on a shared axis-locked grid ─
  nx <- 100; ny <- 100
  xgrid <- seq(log_lim[1], log_lim[2], length.out = nx)
  ygrid <- seq(dist_lim[1], dist_lim[2], length.out = ny)

  # expand.grid: x varies fastest → row k = (x[k %% nx + 1], y[k %/% nx + 1])
  # MASS::kde2d returns z[i,j] for x[i], y[j] → as.vector(z) matches this order
  grids <- lapply(years, function(yr) {
    df <- yr_data[[as.character(yr)]]
    if (nrow(df) < 10) {
      z_vals <- rep(0, nx * ny)
    } else {
      kfit   <- MASS::kde2d(df$log_slope, df$DistUpstre,
                             n = c(nx, ny), lims = c(log_lim, dist_lim))
      z_vals <- as.vector(kfit$z)
    }
    z_max <- max(z_vals, na.rm = TRUE)
    if (z_max > 0) z_vals <- z_vals / z_max   # normalise to [0, 1]

    data.frame(
      x    = rep(xgrid, times = ny),
      y    = rep(ygrid, each  = nx),
      z    = z_vals,
      year = as.numeric(yr)   # tweenr interpolates this too → used for title
    )
  })

  # ── 2. Tween between consecutive year grids ──────────────────────────────────
  n_total <- SMOOTH_TRANS_FRAMES * (length(years) - 1) +
             SMOOTH_HOLD_FRAMES  *  length(years)
  tweened <- tweenr::tween_states(
    grids,
    tweenlength = SMOOTH_TRANS_FRAMES,
    statelength = SMOOTH_HOLD_FRAMES,
    ease        = "cubic-in-out",
    nframes     = n_total
  )

  # ── 3. Choose fill scale to match the existing per-method aesthetic ──────────
  if (method == "ggplot") {
    fill_scale <- scale_fill_viridis_d("Norm.\ndensity", direction = 1,
                                        na.value = "white")
    brks <- GG_BREAKS
  } else {
    # quantiles method: YlOrRd palette with fixed normalised breaks
    # (per-frame quantile recalculation would fight the tween, so we use
    # fixed density thresholds matching the same visual progression)
    fill_scale <- scale_fill_brewer("Density", palette = "YlOrRd", direction = -1)
    brks <- c(0.05, 0.2, 0.4, 0.6, 0.8, 0.9)
  }
  y_fmt <- make_dist_y_scale(dist_lim)

  # ── 4. Render one PNG per frame into a temp directory ────────────────────────
  tmp_dir <- file.path(tempdir(),
                        sprintf("smooth_%s_%s_%d", basin, method, as.integer(Sys.time())))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  frame_ids <- sort(unique(tweened$.frame))
  png_paths <- character(length(frame_ids))

  for (i in seq_along(frame_ids)) {
    fdat     <- tweened[tweened$.frame == frame_ids[i], ]
    yr_label <- round(fdat$year[1])

    p <- ggplot(fdat, aes(x = x, y = y, z = z)) +
      geom_contour_filled(breaks = brks, na.rm = TRUE) +
      fill_scale +
      scale_x_continuous(breaks = log_break_pos, labels = log_break_lab) +
      y_fmt +
      coord_cartesian(xlim = log_lim, ylim = dist_lim) +
      labs(
        x     = "Watershed Slope (log₁₀ scale)",
        y     = "Distance Upstream (km × 1000)",
        title = sprintf("%s  ·  %d", basin, yr_label)
      ) +
      base_theme

    png_paths[i] <- file.path(tmp_dir, sprintf("frame_%04d.png", i))
    ggsave(png_paths[i], p, width = 10, height = 7, dpi = 100, bg = "white")
  }

  cat(sprintf("    %d frames rendered — combining into GIF...\n", length(frame_ids)))

  # ── 5. Stitch frames into a GIF with magick ───────────────────────────────────
  frames   <- image_read(png_paths)
  gif      <- image_animate(image_join(frames), fps = SMOOTH_FPS, optimize = TRUE)
  out_path <- file.path(out_dir, sprintf("%s_animated_smooth.gif", basin))
  image_write(gif, out_path)
  cat("  Saved:", out_path, "\n")
}

cat("\nBuilding smooth (interpolated) GIFs...\n")
for (method in c("ggplot", "quantiles")) {
  out_dir <- file.path(fig_dir, method)
  cat(sprintf("  %s/\n", method))
  make_smooth_gif(yukon_data, YUKON_YEARS, "Yukon",
                  YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                  YUKON_DIST_LIM, method, out_dir)
  make_smooth_gif(kusko_data, KUSKO_YEARS, "Kusko",
                  KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                  KUSKO_DIST_LIM, method, out_dir)
}

cat(sprintf("\nSmooth GIFs saved to %s\n", fig_dir))
