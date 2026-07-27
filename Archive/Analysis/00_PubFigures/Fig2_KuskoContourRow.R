################################################################################
# Fig2_KuskoContourRow.R
#
# Publication Figure 2 — Kuskokwim contour panels, single row
#
# One panel per year (2017-2022): production-weighted KDE quantile contours
# mapping assignment production to watershed slope (log10) vs. distance
# upstream. Style matches Contours_t0.9.R (YlOrRd brewer palette, quantile
# bands, shared axes across all panels).
#
# Reads:  Outputs/SensitivitySweep/t0.9/Kusko/   (pre-computed CSVs)
#         Data/Spatial Data/AnalysisShapefiles/Kusko_GEO.shp  (for attributes)
# Writes: Figures/00_PubFigures/Fig2_KuskoContourRow.png
#         Figures/00_PubFigures/Fig2_KuskoContourRow.pdf
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(ks)
  library(scales)
  library(here)
})

# ==============================================================================
# Config — edit here to change years, threshold, or quantile levels
# ==============================================================================
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
THRESHOLD   <- "0.9"

# Quantile contour levels: each value p means "the contour enclosing the
# top-p fraction of production-weighted density."
QUANTILES <- c(0, 0.10, 0.20, 0.40, 0.60, 0.80)

OUT_DIR <- here("Figures", "00_PubFigures")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

csv_root <- here("Outputs", "SensitivitySweep", paste0("t", THRESHOLD))

# ==============================================================================
# Load habitat attributes from shapefile
# ==============================================================================
cat("Loading shapefile attributes...\n")
kusko_attr <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

# ==============================================================================
# Load CSVs and join attributes
# ==============================================================================
cat("Loading CSVs...\n")
kusko_data <- setNames(
  lapply(KUSKO_YEARS, function(yr) {
    read_csv(
      file.path(csv_root, "Kusko",
                sprintf("%d_Kusko_Assignment_Results.csv", yr)),
      show_col_types = FALSE
    ) %>%
      dplyr::select(reachid, assignment_norm) %>%
      left_join(kusko_attr, by = "reachid") %>%
      filter(assignment_norm > 0, !is.na(WtrshdSlp), !is.na(DistUpstre),
             WtrshdSlp > 0)
  }),
  KUSKO_YEARS
)

# ==============================================================================
# Fixed axis limits derived from the full attribute table
# ==============================================================================
kusko_log_all  <- log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0])
KUSKO_LOG_LIM  <- quantile(kusko_log_all, c(0.01, 0.99), na.rm = TRUE)
KUSKO_DIST_LIM <- range(kusko_attr$DistUpstre, na.rm = TRUE)

log_break_pos <- pretty(KUSKO_LOG_LIM, n = 5)
log_break_lab <- signif(10^log_break_pos, 3)

dist_breaks <- pretty(KUSKO_DIST_LIM, n = 5)
dist_breaks <- dist_breaks[dist_breaks >= KUSKO_DIST_LIM[1] &
                             dist_breaks <= KUSKO_DIST_LIM[2]]

# ==============================================================================
# KDE quantile contour helper
# ==============================================================================
kde_breaks <- function(x, y, w) {
  w_norm  <- w / sum(w) * length(w)
  H       <- ks::Hpi(x = cbind(x, y))
  H       <- (H + t(H)) / 2
  fit     <- ks::kde(x = cbind(x, y), H = H, w = w_norm, gridsize = c(200, 200))
  pt_dens <- predict(fit, x = cbind(x, y))
  ord     <- order(-pt_dens)
  cum_w   <- cumsum((w_norm / sum(w_norm))[ord])
  breaks  <- sort(unique(
    approx(cum_w, pt_dens[ord], xout = QUANTILES, rule = 2)$y
  ))
  list(fit = fit, breaks = breaks)
}

# ==============================================================================
# Single-panel function
# ==============================================================================
panel_theme <- theme_bw(base_size = 11) +
  theme(
    axis.text        = element_text(size = 9,  color = "grey30"),
    axis.title       = element_text(size = 10, color = "grey20"),
    axis.title.x     = element_text(margin = margin(t = 6)),
    axis.title.y     = element_text(margin = margin(r = 6)),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5,
                                    margin = margin(b = 4)),
    legend.position  = "none",
    plot.margin      = margin(6, 6, 6, 6)
  )

make_contour_panel <- function(df, yr, show_y_axis = FALSE,
                                show_legend = FALSE) {
  if (nrow(df) < 10) {
    return(
      ggplot() +
        annotate("text", x = mean(KUSKO_LOG_LIM), y = mean(KUSKO_DIST_LIM),
                 label = sprintf("%d\n(n = %d; too few)", yr, nrow(df)),
                 color = "firebrick", size = 4, hjust = 0.5) +
        coord_cartesian(xlim = KUSKO_LOG_LIM, ylim = KUSKO_DIST_LIM) +
        labs(title = yr) + panel_theme
    )
  }

  kb      <- kde_breaks(log10(df$WtrshdSlp), df$DistUpstre, df$assignment_norm)
  grid_df <- expand.grid(
    x = kb$fit$eval.points[[1]],
    y = kb$fit$eval.points[[2]]
  )
  grid_df$z <- as.vector(kb$fit$estimate)

  quant_labels <- scales::percent(rev(QUANTILES[-1]))

  p <- ggplot() +
    geom_contour_filled(
      data = grid_df,
      aes(x = x, y = y, z = z, fill = after_stat(level)),
      breaks = kb$breaks
    ) +
    scale_fill_brewer(
      "Quantiles",
      labels  = quant_labels,
      palette = "YlOrRd",
      direction = 1
    ) +
    scale_x_continuous(
      breaks = log_break_pos,
      labels = log_break_lab,
      limits = KUSKO_LOG_LIM
    ) +
    scale_y_continuous(
      breaks = dist_breaks,
      labels = round(dist_breaks / 1e3, 0),  # km × 1000
      limits = KUSKO_DIST_LIM
    ) +
    coord_cartesian(xlim = KUSKO_LOG_LIM, ylim = KUSKO_DIST_LIM) +
    labs(
      title = as.character(yr),
      x     = expression("Watershed Slope (log"[10]*" scale)"),
      y     = if (show_y_axis) "Distance Upstream\n(km × 1000)" else NULL
    ) +
    panel_theme

  if (show_legend) {
    p <- p + theme(
      legend.position  = "right",
      legend.title     = element_text(face = "bold", size = 9),
      legend.text      = element_text(size = 8),
      legend.key.size  = unit(0.45, "cm")
    )
  }

  if (!show_y_axis) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  p
}

# ==============================================================================
# Build panels (show y-axis on leftmost, legend on rightmost)
# ==============================================================================
cat("Computing KDE contours...\n")
n <- length(KUSKO_YEARS)
panels <- lapply(seq_along(KUSKO_YEARS), function(i) {
  yr <- KUSKO_YEARS[i]
  cat(sprintf("  %d...\n", yr))
  make_contour_panel(
    df          = kusko_data[[as.character(yr)]],
    yr          = yr,
    show_y_axis = (i == 1),
    show_legend = (i == n)
  )
})

# ==============================================================================
# Assemble single row
# ==============================================================================
fig <- wrap_plots(panels, nrow = 1) +
  plot_annotation(
    title = "Kuskokwim: Salmon Production vs. Landscape Features",
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title      = element_text(face = "bold", size = 14, hjust = 0.5,
                                     margin = margin(b = 8))
    )
  )

# ==============================================================================
# Save
# ==============================================================================
fig_width <- 3.2 * n + 1.2   # ~20.4 in for 6 panels
ggsave(file.path(OUT_DIR, "Fig2_KuskoContourRow.png"),
       fig, width = fig_width, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "Fig2_KuskoContourRow.pdf"),
       fig, width = fig_width, height = 5.5, bg = "white")

cat(sprintf("Fig2 saved -> %s\n", OUT_DIR))
