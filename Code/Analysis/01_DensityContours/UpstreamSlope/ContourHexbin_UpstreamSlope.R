################################################################################
# HEXBIN FIGURES — assignment_norm > 0.9 filter, Watershed Slope vs Distance
#
# Reads pre-computed assignment CSVs from Outputs/SensitivitySweep/t0.9/,
# filters to assignment_norm > 0.9, then plots Watershed Slope (log₁₀) on x
# against Distance Upstream on y using hexagonal binning.
#
# Hexbin cells are coloured by the sum of assignment_norm weights within each
# bin (i.e. weighted count), with a discrete colour scale spanning:
#   < 1 · < 2 · < 5 · < 10 · < 20 · < 50 · < 100 · ≥ 100
#
# Produces composite (all-years) and per-year figures plus animated GIFs.
#
# Output: Figures/Contours/Presfigures/Hexbin/UpstreamSlope/
#
# USAGE (from project root):
#   source("Code/Analysis/01_DensityContours/UpstreamSlope/ContourHexbin_UpstreamSlope.R")
#   Rscript Code/Analysis/01_DensityContours/UpstreamSlope/ContourHexbin_UpstreamSlope.R
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(here)
library(scales)
library(magick)
library(hexbin)

# ==============================================================================
# Config
# ==============================================================================
YUKON_YEARS  <- c(2015, 2016, 2021)
KUSKO_YEARS  <- c(2017, 2018, 2019, 2020, 2021, 2022)
CSV_THRESH   <- "0.9"          # folder under SensitivitySweep (data source)
FILT_THRESH  <- 0.0            # assignment_norm minimum to include
HEX_BINS     <- 30          # number of hexbin cells across each axis

csv_root <- here("Outputs", "SensitivitySweep", paste0("t", CSV_THRESH))
fig_dir  <- here("Figures", "Contours", "Presfigures", "Hexbin", "UpstreamSlope")

# Colour scale: continuous YlOrRd on a log10 axis, auto-scaled per panel

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
      dplyr::select(reachid, assignment_norm, assignment_individuals) %>%
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

# ==============================================================================
# Shared theme
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
# Panel helper — hexbin
# x = log_slope, y = DistUpstre
# fill = relative production per bin (bin sum / total sum), 0–1 within each year
# ==============================================================================
hex_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, dist_lim, yr) {
  y_fmt <- scale_y_continuous(labels = function(x) round(x / 1e6, 2))

  if (nrow(df) < 5) {
    return(
      ggplot() +
        annotate("text", x = mean(x_lim_log), y = mean(dist_lim),
                 label = sprintf("n = %d\n(too few)", nrow(df)),
                 color = "firebrick", size = 4, hjust = 0.5) +
        scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
        y_fmt +
        coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
        labs(x = "Watershed Slope (log₁₀ scale)",
             y = "Distance Upstream (km × 1000)") +
        base_theme +
        ggtitle(yr)
    )
  }

  # after_stat(value / max(value)) rescales bin sums so the peak bin = 1
  # and all others fall between 0–1, matching the assignment_norm convention.
  ggplot(df, aes(x = log_slope, y = DistUpstre)) +
    stat_summary_hex(
      aes(z    = assignment_individuals,
          fill = after_stat(value / max(value))),
      fun       = sum,
      bins      = HEX_BINS,
      color     = "white",
      linewidth = 0.15
    ) +
    scale_fill_distiller(
      name      = "Relative\nProduction",
      palette   = "YlOrRd",
      direction = 1,
      limits    = c(0, 1),
      labels    = scales::label_number(accuracy = 0.01)
    ) +
    scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
    y_fmt +
    coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
    labs(x = "Watershed Slope (log₁₀ scale)",
         y = "Distance Upstream (km × 1000)") +
    base_theme +
    ggtitle(yr)
}

# ==============================================================================
# Build panel list (one panel per year)
# ==============================================================================
build_panels <- function(yr_data, years, log_lim, log_break_pos, log_break_lab,
                         dist_lim) {
  lapply(years, function(yr) {
    df <- yr_data[[as.character(yr)]]
    cat(sprintf("    %s — %d rows above %.1f\n", yr, nrow(df), FILT_THRESH))
    hex_panel(df, log_lim, log_break_pos, log_break_lab, dist_lim, yr)
  })
}

# ==============================================================================
# Save composite (all-years stacked) figure
# ==============================================================================
save_composite <- function(panels, n_years, basin, out_dir) {
  fig <- wrap_plots(panels, ncol = 1, guides = "collect") +
    plain_ann(sprintf("%s — assignment > %.1f  (hexbin)", basin, FILT_THRESH))
  fname <- file.path(out_dir,
                     sprintf("%s_hexbin_composite_thresh%.1f.png", basin, FILT_THRESH))
  ggsave(fname, fig, width = 10, height = 9 * n_years, dpi = 150, limitsize = FALSE)
  cat("  Saved:", fname, "\n")
}

# ==============================================================================
# Save one figure per year
# ==============================================================================
save_year_figs <- function(yr_data, years, basin, out_dir,
                           log_lim, log_break_pos, log_break_lab, dist_lim) {
  for (yr in years) {
    df    <- yr_data[[as.character(yr)]]
    fig   <- hex_panel(df, log_lim, log_break_pos, log_break_lab, dist_lim, yr)
    fname <- file.path(out_dir,
                       sprintf("%s_%d_hexbin_thresh%.1f.png", basin, yr, FILT_THRESH))
    ggsave(fname, fig, width = 10, height = 9, dpi = 150)
    cat("  Saved:", fname, "\n")
  }
}

# ==============================================================================
# Produce all figures
# ==============================================================================
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("\nSaving figures to: %s\n", fig_dir))

cat("\n  Building Yukon panels...\n")
yukon_panels <- build_panels(yukon_data, YUKON_YEARS,
                             YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                             YUKON_DIST_LIM)
save_composite(yukon_panels, length(YUKON_YEARS), "Yukon", fig_dir)
cat("  Saving individual Yukon year figures...\n")
save_year_figs(yukon_data, YUKON_YEARS, "Yukon", fig_dir,
               YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
               YUKON_DIST_LIM)

cat("\n  Building Kusko panels...\n")
kusko_panels <- build_panels(kusko_data, KUSKO_YEARS,
                             KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                             KUSKO_DIST_LIM)
save_composite(kusko_panels, length(KUSKO_YEARS), "Kusko", fig_dir)
cat("  Saving individual Kusko year figures...\n")
save_year_figs(kusko_data, KUSKO_YEARS, "Kusko", fig_dir,
               KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
               KUSKO_DIST_LIM)

# ==============================================================================
# Animate per-year PNGs into a GIF (one per basin)
# ==============================================================================
make_gif <- function(years, basin, out_dir, fps = 1) {
  fnames <- file.path(out_dir,
                      sprintf("%s_%d_hexbin_thresh%.1f.png", basin, years, FILT_THRESH))
  missing <- fnames[!file.exists(fnames)]
  if (length(missing) > 0) {
    cat("  Skipping GIF — missing frames:\n")
    for (f in missing) cat("   ", f, "\n")
    return(invisible(NULL))
  }
  frames   <- image_read(fnames)
  gif      <- image_animate(image_join(frames), fps = fps, optimize = TRUE)
  gif_path <- file.path(out_dir, sprintf("%s_hexbin_animated.gif", basin))
  image_write(gif, gif_path)
  cat("  GIF saved:", gif_path, "\n")
}

cat("\nBuilding GIFs...\n")
make_gif(YUKON_YEARS, "Yukon", fig_dir)
make_gif(KUSKO_YEARS, "Kusko", fig_dir)

cat(sprintf("\nDone. Figures saved to %s\n", fig_dir))
