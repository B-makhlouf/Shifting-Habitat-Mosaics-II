################################################################################
# CONTOUR COMPANION MAPS — norm > 0.7 only
#
# Reads pre-computed assignment CSVs from Outputs/ProductionData/ and renders
# one map per basin per year using the EXACT same aesthetics as
# 00_FullBasinRelativeProdMaps.R:
#   - Same color_continuous() / YlOrRd ramp
#   - Same per-stream-order line widths (+ 0.8 boost for norm > 0.7)
#   - Same draw_colorbar()
#   - Same PNG dimensions, par(mar), basin fill
#
# The only difference: segments with norm <= 0.7 are drawn in light grey (gray85)
# so the full river network is visible as context; high-confidence segments
# retain their YlOrRd production colours.
#
# After all PNGs are saved, magick stitches them into one GIF per basin.
#
# Output: Figures/Maps/ContourCompanion/
#
# USAGE (from project root):
#   source("Code/Analysis/00_ProvenanceEstimates/ContourCompanionMaps.R")
#   Rscript Code/Analysis/00_ProvenanceEstimates/ContourCompanionMaps.R
################################################################################

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr)
  library(RColorBrewer); library(here); library(magick)
})

# ==============================================================================
# Config
# ==============================================================================
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2021)
NORM_THRESH <- 0.7

PATHS <- list(
  kusko_edges = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Kusko_edges_geomorphAdded.shp"),
  kusko_basin = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Kusko_basin.shp"),
  yukon_edges = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Yukon_edges_geomorphAdded.shp"),
  yukon_basin = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Yukon_basin.shp"),
  csv_kusko   = here("Outputs", "ProductionData", "Kusko"),
  csv_yukon   = here("Outputs", "ProductionData", "Yukon_full"),
  out_dir     = here("Figures", "Maps", "ContourCompanion")
)
dir.create(PATHS$out_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Exact color helpers from 00_FullBasinRelativeProdMaps.R
# ==============================================================================
N_PAL    <- 500
PAL_CONT <- colorRampPalette(brewer.pal(9, "YlOrRd"))(N_PAL)

color_continuous <- function(rescale_vals) {
  max_val <- max(rescale_vals, na.rm = TRUE)
  cols    <- rep("grey85", length(rescale_vals))
  if (max_val > 0) {
    has_prod       <- rescale_vals > 0
    idx            <- pmax(1L, ceiling(rescale_vals[has_prod] / max_val * N_PAL))
    cols[has_prod] <- PAL_CONT[idx]
  }
  cols
}

draw_colorbar <- function(n_steps = 200, title = "Relative production") {
  usr <- par("usr")
  pw  <- usr[2] - usr[1]
  ph  <- usr[4] - usr[3]
  bx0 <- usr[1] + 0.030 * pw
  bx1 <- bx0    + 0.022 * pw
  by0 <- usr[3] + 0.55  * ph
  by1 <- usr[3] + 0.88  * ph
  pal  <- colorRampPalette(brewer.pal(9, "YlOrRd"))(n_steps)
  step <- (by1 - by0) / n_steps
  for (k in seq_len(n_steps))
    rect(bx0, by0 + (k-1)*step, bx1, by0 + k*step, col = pal[k], border = NA)
  rect(bx0, by0, bx1, by1, border = "black", lwd = 0.5)
  tick_fracs <- c(0, 0.25, 0.5, 0.75, 1.0)
  tick_y     <- by0 + tick_fracs * (by1 - by0)
  text(bx1 + 0.008*pw, tick_y, tick_fracs, adj = 0, cex = 0.62)
  text((bx0+bx1)/2, by1 + 0.030*ph, title, adj = 0.5, cex = 0.70, font = 2)
}

# ==============================================================================
# Load shapefiles once
# ==============================================================================
cat("Loading shapefiles...\n")
KUSKO_EDGES <- st_read(PATHS$kusko_edges, quiet = TRUE)
KUSKO_BASIN <- st_read(PATHS$kusko_basin, quiet = TRUE)
KUSKO_EDGES <- st_transform(KUSKO_EDGES, st_crs(KUSKO_BASIN))

YUKON_EDGES <- st_read(PATHS$yukon_edges, quiet = TRUE)
YUKON_BASIN <- st_read(PATHS$yukon_basin, quiet = TRUE)
YUKON_EDGES <- st_transform(YUKON_EDGES, st_crs(YUKON_BASIN))

# ==============================================================================
# KUSKOKWIM
# ==============================================================================
map_kusko <- function(year) {
  cat(sprintf("  Kusko %d\n", year))
  MIN_STREAM_ORDER <- 3
  edges <- KUSKO_EDGES
  basin <- KUSKO_BASIN

  csv <- read_csv(
    file.path(PATHS$csv_kusko,
              sprintf("%d_Kusko_Assignment_Results.csv", year)),
    show_col_types = FALSE
  )

  edf     <- st_drop_geometry(edges) %>%
    left_join(csv %>% select(reachid, assignment_rescale, assignment_norm),
              by = "reachid")
  rescale <- replace(edf$assignment_rescale, is.na(edf$assignment_rescale), 0)
  norm    <- replace(edf$assignment_norm,    is.na(edf$assignment_norm),    0)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)

  # Exact line widths from original
  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 5.0,
        ifelse(so >= 8, 6.0,
        ifelse(so >= 7, 5.0,
        ifelse(so >= 6, 3.0,
        ifelse(so >= 5, 2.7,
        ifelse(so >= 4, 2.7,
        ifelse(so >= 3, 2.5,
        ifelse(so >= 2, 1.5, 0))))))))
  lw[so < MIN_STREAM_ORDER]      <- 0
  lw[norm > 0.7 & lw > 0]       <- lw[norm > 0.7 & lw > 0] + 0.8

  # Segments above threshold get production colour; below threshold shown in grey
  colcode <- color_continuous(rescale)
  colcode[norm <= NORM_THRESH]     <- "gray85"
  colcode[stream_order_prior == 0] <- NA

  fname <- file.path(PATHS$out_dir, sprintf("Kusko_%d_companion.png", year))
  png(fname, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Production (norm > %.1f) — Kuskokwim  |  Year: %d",
                      NORM_THRESH, year),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat("    ->", fname, "\n")
}

# ==============================================================================
# YUKON
# ==============================================================================
map_yukon <- function(year) {
  cat(sprintf("  Yukon %d\n", year))
  MIN_STREAM_ORDER <- 4
  edges <- YUKON_EDGES
  basin <- YUKON_BASIN

  csv <- read_csv(
    file.path(PATHS$csv_yukon,
              sprintf("%d_Yukon_Full_Assignment_Results.csv", year)),
    show_col_types = FALSE
  )

  edf     <- st_drop_geometry(edges) %>%
    left_join(csv %>% select(reachid, assignment_rescale, assignment_norm),
              by = "reachid")
  rescale <- replace(edf$assignment_rescale, is.na(edf$assignment_rescale), 0)
  norm    <- replace(edf$assignment_norm,    is.na(edf$assignment_norm),    0)

  below_min <- !is.na(edges$Str_Order) & edges$Str_Order < MIN_STREAM_ORDER

  # Exact line widths from original
  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 3.7,
        ifelse(so >= 8, 5.0,
        ifelse(so >= 7, 3.0,
        ifelse(so >= 6, 2.0,
        ifelse(so >= 5, 1.5,
        ifelse(so >= 4, 1.5,
        ifelse(so >= 3, 1.2,
        ifelse(so >= 2, 0.8, 0))))))))
  lw[so < MIN_STREAM_ORDER]  <- 0
  lw[norm > 0.7 & lw > 0]   <- lw[norm > 0.7 & lw > 0] + 0.8

  # Segments above threshold get production colour; below threshold shown in grey
  colcode <- color_continuous(rescale)
  colcode[norm <= NORM_THRESH] <- "gray85"
  colcode[below_min]           <- NA

  fname <- file.path(PATHS$out_dir, sprintf("Yukon_%d_companion.png", year))
  png(fname, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Production (norm > %.1f) — Yukon Basin  |  Year: %d",
                      NORM_THRESH, year),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat("    ->", fname, "\n")
}

# ==============================================================================
# Produce PNGs
# ==============================================================================
cat("\nKuskokwim maps...\n")
for (yr in KUSKO_YEARS)
  tryCatch(map_kusko(yr),
           error = function(e) cat("  ERROR Kusko", yr, ":", e$message, "\n"))

cat("\nYukon maps...\n")
for (yr in YUKON_YEARS)
  tryCatch(map_yukon(yr),
           error = function(e) cat("  ERROR Yukon", yr, ":", e$message, "\n"))

# ==============================================================================
# Stitch into GIFs (one per basin)
# ==============================================================================
make_gif <- function(years, basin, fps = 1) {
  fnames  <- file.path(PATHS$out_dir,
                       sprintf("%s_%d_companion.png", basin, years))
  missing <- fnames[!file.exists(fnames)]
  if (length(missing) > 0) {
    cat(sprintf("  Skipping %s GIF — missing frames:\n", basin))
    for (f in missing) cat("   ", f, "\n")
    return(invisible(NULL))
  }
  gif      <- image_animate(image_join(image_read(fnames)),
                             fps = fps, optimize = TRUE)
  gif_path <- file.path(PATHS$out_dir, sprintf("%s_companion.gif", basin))
  image_write(gif, gif_path)
  cat("  GIF saved:", gif_path, "\n")
}

cat("\nBuilding GIFs...\n")
make_gif(KUSKO_YEARS, "Kusko")
make_gif(YUKON_YEARS, "Yukon")

cat(sprintf("\nDone. All output in: %s\n", PATHS$out_dir))
