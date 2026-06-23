################################################################################
# TOP-50% PRODUCTION MAPS — KUSKOKWIM
#
# Produces one base-R map per year in the exact same style as
# 00_FullBasinRelativeProdMaps.R:  same basin fill, same YlOrRd ramp, same
# line-width scale, same draw_colorbar() legend.
#
# The only difference from the full relative-production maps: only segments
# whose normalised assignment (assignment_norm) is >= TOP50K_THRESHOLD
# receive a YlOrRd colour. All other eligible segments are drawn in grey70;
# segments below MIN_STREAM_ORDER_K are omitted (lwd = 0 / col = NA).
#
# STANDALONE USE:
#   source("kusko_top50prod.R")  — saves one PNG per year to
#                                  Figures/Maps/Top50Prod/Kusko/
#
# SOURCED BY Contours.R for the twelve-panel figure:
#   Exposes draw_kusko_top50_map() so Contours.R can render into a temp device
#   and wrap the raster as a patchwork-compatible grob via wrap_elements().
#
# REQUIRES in environment (or will be loaded from shapefiles):
#   KUSKO_EDGES  — sf object for the Kuskokwim river network
#   KUSKO_BASIN  — sf object for the Kuskokwim basin outline
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(RColorBrewer)
  library(here)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------
if (!exists("KUSKO_YEARS")) KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
TOP50K_THRESHOLD   <- if (exists("CONTOUR_THRESHOLD")) CONTOUR_THRESHOLD else 0.5  # inherits from Contours.R if sourced there
MIN_STREAM_ORDER_K <- 2     # matches 00_FullBasinRelativeProdMaps.R

prod_dir_k50 <- here("Outputs", "ProductionData", "Kusko")
if (!exists("map_dir_k50")) map_dir_k50 <- here("Figures", "Maps", "Top50Prod", "Kusko")
dir.create(map_dir_k50, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Color palette  (identical to 00_FullBasinRelativeProdMaps.R)
# ------------------------------------------------------------------------------
N_PAL_K    <- 500
PAL_CONT_K <- colorRampPalette(brewer.pal(9, "YlOrRd"))(N_PAL_K)

# ------------------------------------------------------------------------------
# Helper: stream order -> line width  (Kuskokwim scale, from 00_FullBasinRelativeProdMaps.R)
# ------------------------------------------------------------------------------
so_to_lw_k50 <- function(so) {
  ifelse(so >= 9, 5.0,
  ifelse(so >= 8, 6.0,
  ifelse(so >= 7, 5.0,
  ifelse(so >= 6, 3.0,
  ifelse(so >= 5, 2.7,
  ifelse(so >= 4, 2.7,
  ifelse(so >= 3, 2.5,
  ifelse(so >= 2, 1.5, 0))))))))
}

# ------------------------------------------------------------------------------
# Colorbar  (identical structure to top50prod.R)
# ------------------------------------------------------------------------------
draw_colorbar_k50 <- function(max_rescale, n_steps = 200,
                               title = "% of run per segment") {
  usr <- par("usr")
  pw  <- usr[2] - usr[1]
  ph  <- usr[4] - usr[3]

  bx0 <- usr[1] + 0.030 * pw
  bx1 <- bx0    + 0.022 * pw
  by0 <- usr[3] + 0.55  * ph
  by1 <- usr[3] + 0.88  * ph

  pal  <- colorRampPalette(brewer.pal(9, "YlOrRd"))(n_steps)
  step <- (by1 - by0) / n_steps
  for (k in seq_len(n_steps)) {
    rect(bx0, by0 + (k - 1) * step, bx1, by0 + k * step,
         col = pal[k], border = NA)
  }
  rect(bx0, by0, bx1, by1, border = "black", lwd = 0.5)

  tick_fracs <- c(0, 0.25, 0.5, 0.75, 1.0)
  tick_pct   <- round(tick_fracs * max_rescale * 100, 3)
  tick_y     <- by0 + tick_fracs * (by1 - by0)
  text(bx1 + 0.008 * pw, tick_y,
       paste0(tick_pct, "%"), adj = 0, cex = 0.62)
  text((bx0 + bx1) / 2, by1 + 0.030 * ph,
       title, adj = 0.5, cex = 0.70, font = 2)
}

# ------------------------------------------------------------------------------
# Main drawing function
# Draws into the currently open graphics device.
# Wrap in  png(...) / dev.off()  when saving to file.
# ------------------------------------------------------------------------------
draw_kusko_top50_map <- function(yr,
                                  edges,
                                  basin,
                                  prod_dir  = prod_dir_k50,
                                  threshold = TOP50K_THRESHOLD,
                                  min_so    = MIN_STREAM_ORDER_K) {

  # Ensure CRS consistency
  if (!identical(sf::st_crs(edges), sf::st_crs(basin))) {
    edges <- sf::st_transform(edges, sf::st_crs(basin))
  }

  # Load production values for this year
  prod <- read_csv(
    file.path(prod_dir,
              sprintf("%d_Kusko_Assignment_Results.csv", yr)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(reachid, assignment_rescale, assignment_norm)

  # Merge into edges (preserves geometry order)
  edf             <- edges %>% left_join(prod, by = "reachid")
  rescale         <- edf$assignment_rescale
  assignment_norm <- edf$assignment_norm
  so              <- ifelse(is.na(edf$Str_Order), 1L, as.integer(edf$Str_Order))
  lw              <- so_to_lw_k50(so)

  # Masks
  top50_mask <- !is.na(assignment_norm) & assignment_norm >= threshold
  below_min  <- so < min_so

  # Build colour codes: grey70 background, YlOrRd only for above-threshold segments
  colcode <- rep("grey70", nrow(edf))
  max_val <- max(rescale[top50_mask], na.rm = TRUE)
  if (!is.na(max_val) && max_val > 0) {
    idx                 <- pmax(1L, ceiling(rescale[top50_mask] / max_val * N_PAL_K))
    colcode[top50_mask] <- PAL_CONT_K[idx]
  }
  colcode[below_min] <- NA
  lw[below_min]      <- 0

  # Plot — same style as run_kusko() in 00_FullBasinRelativeProdMaps.R
  plot(sf::st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Top-%d%% Production - Full Kuskokwim Basin\nYear: %d",
                      round(threshold * 100), yr),
       bg = "white")
  plot(sf::st_geometry(edf), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar_k50(max_val, title = "% of run per segment")
}

# ------------------------------------------------------------------------------
# Load spatial data if not already in environment
# ------------------------------------------------------------------------------
if (!exists("KUSKO_EDGES")) {
  message("kusko_top50prod: loading KUSKO_EDGES...")
  KUSKO_EDGES <- sf::st_read(
    here("Data", "Spatial Data", "AnalysisShapefiles",
         "Kusko_edges_geomorphAdded.shp"),
    quiet = TRUE
  )
}
if (!exists("KUSKO_BASIN")) {
  message("kusko_top50prod: loading KUSKO_BASIN...")
  KUSKO_BASIN <- sf::st_read(
    here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
    quiet = TRUE
  )
  KUSKO_EDGES <- sf::st_transform(KUSKO_EDGES, sf::st_crs(KUSKO_BASIN))
}

# ------------------------------------------------------------------------------
# Standalone driver: save one PNG per year
# (Also runs when sourced by Contours.R — individual maps saved as a side effect
#  before the twelve-panel figure is assembled.)
# ------------------------------------------------------------------------------
cat("\n### TOP-50% PRODUCTION MAPS — KUSKOKWIM ###\n")

for (yr in KUSKO_YEARS) {
  cat(sprintf("  Building: %d\n", yr))
  png(file.path(map_dir_k50, sprintf("Kusko_%d_top50prod.png", yr)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  draw_kusko_top50_map(yr, KUSKO_EDGES, KUSKO_BASIN)
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
}

cat("  Saved to:", map_dir_k50, "\n")
