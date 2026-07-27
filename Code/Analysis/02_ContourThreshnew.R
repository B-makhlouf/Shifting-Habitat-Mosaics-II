################################################################################
# CONTOUR FIGURES — canonical contour script for all contour analysis
#
# Reads pre-computed assignment CSVs from Outputs/SensitivitySweep/t0.9/,
# filters to assignment_norm > FILT_THRESH, then plots Watershed Slope (log₁₀)
# on x against Distance Upstream on y, weighted by assignment_norm.
#
# Produces:
#   One contour figure per year per watershed (quantiles method):
#        Figures/02_Contours/{basin}_{year}_thresh{FILT}.png
#
# USAGE (from project root):
#   source("Code/Analysis/01_DensityContours/ContourThreshnew.R")
#   Rscript Code/Analysis/01_DensityContours/ContourThreshnew.R
################################################################################

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(ks)
library(scales)

# Shared parameters (single source of truth — edit values in params.R)
source(here("Code", "Analysis", "params.R"))

# ==============================================================================
# Config
# ==============================================================================
CSV_THRESH   <- "0.9"          # folder under SensitivitySweep (data source)
FILT_THRESH  <- as.numeric(Sys.getenv(
  "CONTOUR_FILTER_THRESHOLD",
  unset = as.character(CONTOUR_FILT_THRESH)
))
QUANTILES    <- c(0, .2, .4, .6, .8 )
REFERENCE_STYLE <- "cross"     # "cross" or "outline"
CONTOUR_PALETTE <- Sys.getenv("CONTOUR_PALETTE", "YlOrRd")

csv_root <- here("Outputs", "SensitivitySweep", paste0("t", CSV_THRESH))
fig_dir <- if (CONTOUR_PALETTE == "magma") {
  here("Figures", "02_Contours", "MagmaPalette_Preview")
} else {
  here("Figures", "02_Contours")
}
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

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
      dplyr::filter(
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
# Reference portfolio — weighted distribution pooled across all
# years for each basin. Each year is given equal total weight, and the average
# portfolio's 80% highest-density boundary is drawn as a fixed reference.
# ==============================================================================
average_portfolio_data <- function(yr_data) {
  valid_years <- yr_data[vapply(yr_data, nrow, integer(1)) >= 5]
  if (length(valid_years) == 0) return(NULL)

  dplyr::bind_rows(lapply(valid_years, function(df) {
    df %>%
      mutate(reference_weight = assignment_norm / sum(assignment_norm))
  }))
}

yukon_reference_data <- average_portfolio_data(yukon_data)
kusko_reference_data <- average_portfolio_data(kusko_data)

# ==============================================================================
# Fixed axis limits (from full spatial data, not the filtered subset)
# ==============================================================================
YUKON_DIST_LIM <- range(yukon_attr$DistUpstre, na.rm = TRUE)
# Cap the Kuskokwim panels at displayed value 8 (= 800 km).
KUSKO_DIST_LIM <- c(0, 8e5)

yukon_log_all       <- log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0])
kusko_log_all       <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
YUKON_LOG_LIM       <- quantile(yukon_log_all, c(0.01, 0.99), na.rm = TRUE)
KUSKO_LOG_LIM       <- quantile(kusko_log_all, c(0.01, 0.99), na.rm = TRUE)
# The annual Kuskokwim portfolios end near slope 50; align the panel edge with
# that intuitive labelled value rather than leaving empty space toward 100.
KUSKO_LOG_LIM[2]    <- log10(50)

SLOPE_BREAKS <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 50, 100)
KUSKO_SLOPE_BREAKS <- c(1, 2.5, 7, 20, 50)
make_slope_breaks <- function(log_lim) {
  values <- SLOPE_BREAKS[
    log10(SLOPE_BREAKS) >= log_lim[1] &
      log10(SLOPE_BREAKS) <= log_lim[2]
  ]
  list(position = log10(values), label = format(values, trim = TRUE))
}

make_fixed_slope_breaks <- function(values, log_lim) {
  values <- values[
    log10(values) >= log_lim[1] & log10(values) <= log_lim[2]
  ]
  list(
    position = log10(values),
    label = format(values, trim = TRUE, scientific = FALSE,
                   drop0trailing = TRUE)
  )
}

X_AXIS_LABEL <- "Watershed Slope (log10 scale)"

yukon_slope_breaks <- make_fixed_slope_breaks(
  KUSKO_SLOPE_BREAKS, YUKON_LOG_LIM
)
kusko_slope_breaks <- make_fixed_slope_breaks(
  KUSKO_SLOPE_BREAKS, KUSKO_LOG_LIM
)
yukon_log_break_pos <- yukon_slope_breaks$position
kusko_log_break_pos <- kusko_slope_breaks$position
yukon_log_break_lab <- yukon_slope_breaks$label
kusko_log_break_lab <- kusko_slope_breaks$label

make_dist_y_scale <- function(dist_lim) {
  brks <- pretty(dist_lim, n = 5)
  brks <- brks[brks >= dist_lim[1] & brks <= dist_lim[2]]
  scale_y_continuous(breaks = brks, labels = round(brks / 1e5, 1))
}

# ==============================================================================
# Shared theme — white background throughout
# ==============================================================================
base_theme <- theme_bw() +
  theme(
    axis.text        = element_text(size = 44, face = "bold",
                                    color = "grey20"),
    axis.title       = element_text(size = 52, face = "bold",
                                    color = "grey15"),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 44, face = "bold", hjust = 0.5,
                                    margin = margin(b = 10)),
    legend.title     = element_text(size = 42, face = "bold"),
    legend.text      = element_text(size = 38, face = "bold"),
    legend.key.size  = unit(1.2, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(16, 16, 16, 16)
  )

# ==============================================================================
# Panel helper — ggplot (geom_density_2d_filled)
# x = log_slope, y = DistUpstre, weighted by assignment_norm
# ==============================================================================
# Start breaks at 0.05 so cells with density < 5 % of max get na.value = "white"
# and blend into the background instead of showing as a hard rectangular border.
GG_BREAKS <- c(0.05, seq(0.1, 1, by = 0.1))

# The fixed reference outline is added after the annual filled contours so it
# remains visible while staying visually subordinate to the annual portfolio.
gg_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, dist_lim, yr,
                     reference_contour = NULL, reference_point = NULL) {
  y_fmt <- make_dist_y_scale(dist_lim)
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text", x = mean(x_lim_log), y = mean(dist_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
      y_fmt +
      coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
      labs(x = X_AXIS_LABEL,
           y = "Distance upstream (100 km)") +
      base_theme + ggtitle(yr))
  }
  ggplot(df, aes(x = log_slope, y = DistUpstre, weight = assignment_norm)) +
    geom_density_2d_filled(contour_var = "ndensity", breaks = GG_BREAKS) +
    scale_fill_viridis_d("Norm.\ndensity", direction = 1, na.value = "white") +
    reference_layer(reference_contour, reference_point) +
    scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
    y_fmt +
    coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
    labs(x = X_AXIS_LABEL,
         y = "Distance upstream (100 km)") +
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

quantile_fill_scale <- function() {
  labels <- scales::percent(rev(QUANTILES[-1]))

  if (CONTOUR_PALETTE == "magma") {
    return(scale_fill_viridis_d(
      "Quantiles", labels = labels, option = "magma", direction = 1
    ))
  }

  scale_fill_brewer(
    "Quantiles", labels = labels, palette = "YlOrRd", direction = -1
  )
}

reference_80_contour <- function(reference_df) {
  if (is.null(reference_df) || nrow(reference_df) < 5) return(NULL)

  x <- reference_df$log_slope
  y <- reference_df$DistUpstre
  w <- reference_df$reference_weight
  w_norm <- w / sum(w) * length(w)
  H <- ks::Hpi(x = cbind(x, y))
  H <- (H + t(H)) / 2
  fit <- ks::kde(
    x = cbind(x, y), H = H, w = w_norm, gridsize = c(200, 200)
  )

  point_density <- predict(fit, x = cbind(x, y))
  ord <- order(-point_density)
  cumulative_weight <- cumsum((w_norm / sum(w_norm))[ord])
  level_80 <- approx(
    cumulative_weight, point_density[ord], xout = 0.8, rule = 2
  )$y

  grid_df <- expand.grid(x = fit$eval.points[[1]], y = fit$eval.points[[2]])
  grid_df$z <- as.vector(fit$estimate)
  list(grid = grid_df, level = level_80)
}

reference_center <- function(reference_df) {
  if (is.null(reference_df) || nrow(reference_df) == 0) return(NULL)

  c(
    x = weighted.mean(
      reference_df$log_slope, reference_df$reference_weight, na.rm = TRUE
    ),
    y = weighted.mean(
      reference_df$DistUpstre, reference_df$reference_weight, na.rm = TRUE
    )
  )
}

reference_layer <- function(reference_contour, reference_point) {
  if (REFERENCE_STYLE == "cross") {
    if (is.null(reference_point) || anyNA(reference_point)) return(NULL)
    return(list(
      geom_vline(
        xintercept = unname(reference_point["x"]),
        colour = "grey15",
        linewidth = 1.4,
        alpha = 0.3
      ),
      geom_hline(
        yintercept = unname(reference_point["y"]),
        colour = "grey15",
        linewidth = 1.4,
        alpha = 0.3
      )
    ))
  }

  if (REFERENCE_STYLE == "outline") {
    if (is.null(reference_contour)) return(NULL)
    return(geom_contour(
    data = reference_contour$grid,
    aes(x = x, y = y, z = z),
    breaks = reference_contour$level,
    colour = "grey15",
    linewidth = 1.4,
    alpha = 0.3,
    inherit.aes = FALSE
    ))
  }

  stop("REFERENCE_STYLE must be either 'cross' or 'outline'.", call. = FALSE)
}

qt_panel <- function(df, x_lim_log, x_break_pos, x_break_lab, dist_lim, yr,
                     reference_contour = NULL, reference_point = NULL) {
  y_fmt <- make_dist_y_scale(dist_lim)
  if (nrow(df) < 5) {
    return(ggplot() +
      annotate("text", x = mean(x_lim_log), y = mean(dist_lim),
               label = sprintf("n = %d\n(too few)", nrow(df)),
               color = "firebrick", size = 4, hjust = 0.5) +
      scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
      y_fmt +
      coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
      labs(x = X_AXIS_LABEL,
           y = "Distance upstream (100 km)") +
      base_theme + ggtitle(yr))
  }
  kb      <- kde_breaks(df$log_slope, df$DistUpstre, df$assignment_norm)
  grid_df <- expand.grid(x = kb$fit$eval.points[[1]], y = kb$fit$eval.points[[2]])
  grid_df$z <- as.vector(kb$fit$estimate)
  ggplot() +
    geom_contour_filled(data = grid_df,
                        aes(x = x, y = y, z = z, fill = after_stat(level)),
                        breaks = kb$breaks) +
    quantile_fill_scale() +
    reference_layer(reference_contour, reference_point) +
    scale_x_continuous(breaks = x_break_pos, labels = x_break_lab) +
    y_fmt +
    coord_cartesian(xlim = x_lim_log, ylim = dist_lim) +
    labs(x = X_AXIS_LABEL,
         y = "Distance upstream (100 km)") +
    base_theme + ggtitle(yr)
}

# ==============================================================================
# Save one figure per year
# ==============================================================================
save_year_figs <- function(yr_data, years, basin, method, out_dir,
                           log_lim, log_break_pos, log_break_lab, dist_lim,
                           reference_contour = NULL, reference_point = NULL) {
  panel_fn <- if (method == "ggplot") gg_panel else qt_panel
  for (yr in years) {
    df    <- yr_data[[as.character(yr)]]
    fig   <- panel_fn(df, log_lim, log_break_pos, log_break_lab, dist_lim, yr,
                      reference_contour, reference_point)
    fname <- file.path(out_dir,
                       sprintf("%s_%d_thresh%.1f.png", basin, yr, FILT_THRESH))
    ggsave(fname, fig, width = 10, height = 7, dpi = 150)
    cat("  Saved:", fname, "\n")
  }
}

# ==============================================================================
# Produce all figures — written directly to fig_dir
# ==============================================================================
yukon_reference_contour <- reference_80_contour(yukon_reference_data)
kusko_reference_contour <- reference_80_contour(kusko_reference_data)
yukon_reference_point <- reference_center(yukon_reference_data)
kusko_reference_point <- reference_center(kusko_reference_data)

for (method in c("quantiles")) {
  cat(sprintf("\n=== Method: %s ===\n", method))

  cat("  Saving individual Yukon year figures...\n")
  save_year_figs(yukon_data, YUKON_YEARS, "Yukon", method, fig_dir,
                 YUKON_LOG_LIM, yukon_log_break_pos, yukon_log_break_lab,
                 YUKON_DIST_LIM, yukon_reference_contour,
                 yukon_reference_point)

  cat("  Saving individual Kusko year figures...\n")
  save_year_figs(kusko_data, KUSKO_YEARS, "Kusko", method, fig_dir,
                 KUSKO_LOG_LIM, kusko_log_break_pos, kusko_log_break_lab,
                 KUSKO_DIST_LIM, kusko_reference_contour,
                 kusko_reference_point)
}

cat(sprintf("\nDone. Figures saved to %s\n", fig_dir))
