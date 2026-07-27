################################################################################
# 06_CombinedMapContour.R  —  ONE combined multi-panel figure per basin, in R.
#
# Stitches the already-rendered panel PNGs into a clean grid (one row per year):
#
#        [ production map ] [ density contour ]
#
# with a single HORIZONTAL "Relative production" legend across the top (the maps
# themselves are drawn legend-less by 01_FullBasinRelativeProdMaps.R). It does
# NOT re-plot data; it trims, scales, and tiles the PNGs.
#
# RUN THESE FIRST so the panels on disk are current:
#   1) Code/Analysis/01_FullBasinRelativeProdMaps.R   (legend-less, title-less maps)
#   2) Code/Analysis/02_ContourThreshnew.R            (contour panels)
#
# USAGE (from project root):
#   source("Code/Analysis/06_CombinedMapContour.R")
#   Rscript Code/Analysis/06_CombinedMapContour.R
# Output: Figures/00_PubFigures/{Kuskokwim,Yukon}_Combined.{png,pdf}
#
# Requires the 'magick' package:  install.packages("magick")
################################################################################

suppressPackageStartupMessages({ library(magick); library(here); library(RColorBrewer) })
source(here("Code", "Analysis", "params.R"))

fig_dir <- here("Figures", "00_PubFigures"); dir.create(fig_dir, TRUE, FALSE)
map_dir <- here("Figures", "01_ProdMaps")
con_dir <- here("Figures", "02_Contours")

# ---- Layout knobs ------------------------------------------------------------
ROW_H    <- 1000                  # common height of every panel (px)
COL_GUT  <- 40                    # gap between map and contour columns (px)
GUTTER   <- 46                    # vertical gap between rows (px)
MARGIN   <- 60                    # outer white margin (px)
LEG_H    <- 250                   # legend band height (px)
SEP_COL  <- "#DFDFDF"; BG <- "white"

# Production bins (must match GREY_LOW + PAL_BINS in 01_FullBasinRelativeProdMaps.R)
PAL_BINS <- c("grey59", brewer.pal(9, "YlOrRd")[c(4, 7, 9)])
BRK_LAB  <- c("0", "0.5", "0.7", "0.9", "1.0")

BASINS <- list(
  Kuskokwim = list(years = KUSKO_YEARS,
    map = function(y) file.path(map_dir, "Kusko",  sprintf("Kusko_%d_relprod.png", y)),
    con = function(y) file.path(con_dir,           sprintf("Kusko_%d_thresh0.7.png", y))),
  Yukon = list(years = YUKON_YEARS,
    map = function(y) file.path(map_dir, "Yukon",  sprintf("Yukon_Full_%d_relprod.png", y)),
    con = function(y) file.path(con_dir,           sprintf("Yukon_%d_thresh0.7.png", y)))
)

prep   <- function(path, h) image_resize(image_trim(image_read(path), fuzz = 2), paste0("x", h))
pad_to <- function(img, w, h) image_extent(img, paste0(w, "x", h), gravity = "center", color = BG)

# Horizontal production legend: title, a row of 4 colour swatches, break labels.
prod_legend <- function(band_w) {
  cv <- image_blank(band_w, LEG_H, BG)
  cv <- image_annotate(cv, "Relative production  (share of annual max)",
                       size = 52, weight = 700, location = "+8+8", color = "#222222")
  sw <- 175; sh <- 70; x0 <- 12; y0 <- 96          # swatch geometry
  for (i in seq_along(PAL_BINS)) {
    blk <- image_blank(sw, sh, PAL_BINS[i])
    cv  <- image_composite(cv, blk, offset = sprintf("+%d+%d", x0 + (i - 1) * sw, y0))
  }
  for (i in seq_along(BRK_LAB))                     # labels at each swatch boundary
    cv <- image_annotate(cv, BRK_LAB[i], size = 42, color = "#333333",
                         location = sprintf("+%d+%d", x0 + (i - 1) * sw - 12, y0 + sh + 10))
  cv
}

build <- function(name, cfg) {
  cat("=== ", name, " ===\n", sep = "")
  maps <- lapply(cfg$years, function(y) prep(cfg$map(y), ROW_H))
  cons <- lapply(cfg$years, function(y) prep(cfg$con(y), ROW_H))
  col1 <- max(vapply(maps, function(i) image_info(i)$width, 0))
  col2 <- max(vapply(cons, function(i) image_info(i)$width, 0))
  gut  <- image_blank(COL_GUT, ROW_H, BG)
  rowW <- col1 + COL_GUT + col2

  parts <- list(prod_legend(rowW))                 # legend band spans the top
  for (i in seq_along(cfg$years)) {
    parts[[length(parts) + 1]] <-
      image_append(c(pad_to(maps[[i]], col1, ROW_H), gut, pad_to(cons[[i]], col2, ROW_H)))
    if (i < length(cfg$years)) {                    # gutter + hairline separator
      parts[[length(parts) + 1]] <- image_blank(rowW, (GUTTER - 2) %/% 2, BG)
      parts[[length(parts) + 1]] <- image_blank(rowW, 2, SEP_COL)
      parts[[length(parts) + 1]] <- image_blank(rowW, (GUTTER - 2) %/% 2, BG)
    }
  }
  fig <- image_append(image_join(parts), stack = TRUE)
  fig <- image_border(image_background(fig, BG), BG, paste0(MARGIN, "x", MARGIN))

  png <- file.path(fig_dir, sprintf("%s_Combined.png", name))
  image_write(fig, png, format = "png", density = 200)
  tryCatch(image_write(fig, file.path(fig_dir, sprintf("%s_Combined.pdf", name)),
                       format = "pdf", density = 200),
           error = function(e) cat("  (PDF skipped: ", conditionMessage(e), ")\n", sep = ""))
  cat("  saved ", png, "\n", sep = "")
}

for (nm in names(BASINS)) build(nm, BASINS[[nm]])
cat("\nDone. Figures in Figures/00_PubFigures/\n")
