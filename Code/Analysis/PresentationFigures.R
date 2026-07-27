################################################################################
# MANUSCRIPT PRESENTATION FIGURES
#
# Pairs each annual production map (left) with its corresponding landscape
# contour (right). Source figures are read only; nothing is redrawn or replaced.
#
# Outputs:
#   Figures/00_PubFigures/Figure1_KuskoMultiPanel.png
#   Figures/00_PubFigures/Figure2_YukonMultiPanel.png
#
# Run from the project root:
#   Rscript Code/Analysis/PresentationFigures.R
################################################################################

project_lib <- file.path(getwd(), ".r-library")
if (dir.exists(project_lib)) .libPaths(c(project_lib, .libPaths()))

suppressPackageStartupMessages({
  library(here)
  library(magick)
  library(sf)
  library(readr)
  library(RColorBrewer)
})

source(here("Code", "Analysis", "params.R"))

# ---- Layout ------------------------------------------------------------------
PANEL_WIDTH  <- 1600L
PANEL_HEIGHT <- 1300L
GUTTER       <- 18L
COLUMN_GUTTER <- 0L
YEAR_WIDTH   <- 150L
HEADER_HEIGHT <- 500L
FOOTER_HEIGHT <- 220L
BACKGROUND   <- "white"
POINT_SIZE   <- 100

map_dir     <- here("Figures", "01_ProdMaps")
contour_dir <- here("Figures", "02_Contours")
output_dir  <- here("Figures", "00_PubFigures")
panel_map_dir <- file.path(
  tempdir(), "ShiftingHabitatMosaics_publication_panel_maps"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(panel_map_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Presentation-specific production maps ----------------------------------
# Use the same annual ProductionData CSVs and relative-production transformation
# as 01_FullBasinRelativeProdMaps.R. The presentation maps differ only in their
# four-bin palette and retained gray basin styling.
YLORRD_9 <- brewer.pal(9, "YlOrRd")
PROD_RAMP_START <- 0.5
N_PROD_COLORS <- 256L
N_BELOW_RAMP <- floor(PROD_RAMP_START * (N_PROD_COLORS - 1L)) + 1L
PROD_COLORS <- c(
  rep("#2F2F2F", N_BELOW_RAMP),
  colorRampPalette(YLORRD_9[3:9])(N_PROD_COLORS - N_BELOW_RAMP)
)

make_threshold_map <- function(edges, basin, csv_path, output_path, basin_name) {
  assignment <- read_csv(csv_path, show_col_types = FALSE)
  rescale <- assignment$assignment_rescale[
    match(edges$reachid, assignment$reachid)
  ]
  max_rescale <- max(rescale, na.rm = TRUE)
  z <- if (is.finite(max_rescale) && max_rescale > 0) {
    rescale / max_rescale
  } else {
    rep(0, length(rescale))
  }
  color_index <- pmax(
    1L, pmin(length(PROD_COLORS), floor(z * (length(PROD_COLORS) - 1L)) + 1L)
  )

  png(output_path, width = 9, height = 8, units = "in", res = 300,
      bg = "white")
  par(mar = c(0.35, 0.35, 0.35, 0.35), bg = "white")
  plot(st_geometry(basin), col = "gray42", border = "black",
       lwd = 2, bg = "white")

  stream_order <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  min_stream_order <- if (basin_name == "Kuskokwim") {
    KUSKO_PARAMS$min_stream_order
  } else if (basin_name == "Yukon") {
    YUKON_PARAMS$min_stream_order
  } else {
    stop("Unknown basin_name: ", basin_name)
  }
  base_keep <- stream_order >= min_stream_order
  # Match the original annual Kuskokwim map widths. Every eligible reach is
  # colored, including zero-production reaches in the lightest bin.
  if (basin_name == "Kuskokwim") {
    production_lwd <- ifelse(stream_order >= 9, 5.0,
                      ifelse(stream_order >= 8, 6.0,
                      ifelse(stream_order >= 7, 5.0,
                      ifelse(stream_order >= 6, 3.5,
                      ifelse(stream_order >= 5, 3.0,
                      ifelse(stream_order >= 4, 2.2,
                      ifelse(stream_order >= 3, 2.5,
                      ifelse(stream_order >= 2, 1.5, 0))))))))
  } else {
    production_lwd <- pmax(2.2, pmin(6.0, 0.72 * stream_order))
  }
  keep <- base_keep & is.finite(z)
  if (any(keep)) {
    high_production <- z[keep] > 0.7
    width_factor <- rep(0.75, sum(keep))
    width_factor[high_production] <- 1.5 + 1.5 *
      ((z[keep][high_production] - 0.7) / 0.3)
    scaled_lwd <- production_lwd[keep] * width_factor
    plot(st_geometry(edges[keep, ]), add = TRUE,
         col = PROD_COLORS[color_index[keep]], lwd = scaled_lwd,
         lend = "round", ljoin = "round")
  }
  dev.off()
  invisible(output_path)
}

build_threshold_maps <- function(basin_name, years, edges_path, basin_path,
                                 csv_paths, file_prefix) {
  cat(sprintf("Building %s full-distribution production maps...\n", basin_name))
  edges <- st_read(edges_path, quiet = TRUE)
  basin <- st_read(basin_path, quiet = TRUE)
  paths <- file.path(panel_map_dir,
                     sprintf("%s_%d_full_distribution.png", file_prefix, years))
  for (i in seq_along(years)) {
    make_threshold_map(edges, basin, csv_paths[i], paths[i], basin_name)
  }
  paths
}

read_contour_panel <- function(path, show_x_ticks = FALSE) {
  if (!file.exists(path)) stop("Missing source figure: ", path, call. = FALSE)
  # Remove the repeated year title, legend, and x-axis title from the canonical
  # raster while retaining the plotting region, ticks, and y-axis information.
  crop_geometry <- if (show_x_ticks) {
    "905x790+125+115"
  } else {
    "905x710+125+115"
  }
  crop_height <- if (show_x_ticks) 790L else 710L
  panel <- image_read(path) |>
    # Exclude the source title, legend, x title, and partially clipped y title.
    image_crop(crop_geometry)
  # Remove the last few pixels of the source's vertical y-axis title. The
  # numeric y tick labels begin farther right and remain fully visible.
  y_title_remnant_mask <- image_blank(18, crop_height, color = BACKGROUND)
  panel <- image_composite(panel, y_title_remnant_mask, gravity = "west")
  if (!show_x_ticks) {
    # Cover only the repeated x tick text below the plotting frame. Keeping the
    # left edge clear preserves the complete y = 0 label.
    x_tick_mask <- image_blank(785, 18, color = BACKGROUND)
    panel <- image_composite(panel, x_tick_mask, gravity = "southeast")
  }
  panel |>
    image_extent(sprintf("1050x%d", crop_height),
                 gravity = "east", color = BACKGROUND) |>
    image_resize(sprintf("%dx%d", PANEL_WIDTH, PANEL_HEIGHT)) |>
    image_extent(sprintf("%dx%d", PANEL_WIDTH, PANEL_HEIGHT),
                 gravity = "center", color = BACKGROUND)
}

read_map_panel <- function(path, enlarge = FALSE) {
  if (!file.exists(path)) stop("Missing source figure: ", path, call. = FALSE)
  img <- image_read(path)
  if (enlarge) img <- image_trim(img, fuzz = 3)
  img |>
    image_resize(sprintf("%dx%d", PANEL_WIDTH, PANEL_HEIGHT)) |>
    image_extent(sprintf("%dx%d", PANEL_WIDTH, PANEL_HEIGHT),
                 gravity = "center", color = BACKGROUND)
}

tag_panel <- function(img, tag, contour = FALSE) {
  tag_x <- if (contour) 190L else 28L
  image_annotate(
    img, sprintf("(%s)", tag),
    gravity = "southwest", location = sprintf("+%d+20", tag_x),
    size = POINT_SIZE, weight = 700, color = "black",
    boxcolor = "white"
  )
}

make_header <- function() {
  cell <- function(label) {
    image_blank(PANEL_WIDTH, HEADER_HEIGHT, color = BACKGROUND) |>
      image_annotate(label, gravity = "center", size = 42,
                     weight = 700, color = "#222222")
  }
  active_prod_colors <- PROD_COLORS[
    (N_BELOW_RAMP + 1L):length(PROD_COLORS)
  ]
  color_blocks <- lapply(active_prod_colors, function(col) {
    image_blank(4, 100, color = col)
  })
  color_bar <- image_append(do.call(c, color_blocks), stack = FALSE) |>
    image_resize("1100x100!") |>
    image_border(color = "#555555", geometry = "1x1")
  tick_line <- image_blank(1100, 125, color = BACKGROUND) |>
    image_annotate("0.5", gravity = "west", location = "+4+0",
                   size = 96, weight = 700, color = "#222222") |>
    image_annotate("0.6", gravity = "west", location = "+210+0",
                   size = 96, weight = 700, color = "#222222") |>
    image_annotate("0.7", gravity = "west", location = "+430+0",
                   size = 96, weight = 700, color = "#222222") |>
    image_annotate("0.8", gravity = "west", location = "+650+0",
                   size = 96, weight = 700, color = "#222222") |>
    image_annotate("0.9", gravity = "west", location = "+870+0",
                   size = 96, weight = 700, color = "#222222") |>
    image_annotate("1", gravity = "east", location = "+4+0",
                   size = 96, weight = 700, color = "#222222")
  legend <- image_append(c(color_bar, tick_line), stack = TRUE)
  production_cell <- image_blank(PANEL_WIDTH, HEADER_HEIGHT,
                                 color = BACKGROUND) |>
    image_annotate("Relative production",
                   gravity = "north", location = "+0+5", size = 108,
                   weight = 700, color = "#222222") |>
    image_composite(legend, gravity = "south", offset = "+0+5")

  # Must match QUANTILES in 02_ContourThreshnew.R:
  # c(0, .25, .5, .75, .9), yielding four filled contour intervals.
  contour_colors <- brewer.pal(4, "YlOrRd")
  contour_labels <- c("25", "50", "75", "90")
  contour_blocks <- lapply(contour_colors, function(col) {
    image_blank(220, 100, color = col)
  })
  contour_bar <- image_append(do.call(c, contour_blocks), stack = FALSE)
  contour_ticks <- image_blank(880, 125, color = BACKGROUND)
  for (i in seq_along(contour_labels)) {
    contour_ticks <- image_annotate(
      contour_ticks, contour_labels[i], gravity = "west",
      location = sprintf("+%d+0", (i - 1L) * 220L + 76L),
      size = 96, weight = 700, color = "#222222"
    )
  }
  contour_legend <- image_append(c(contour_bar, contour_ticks), stack = TRUE)
  contour_cell <- function(title) {
    title_size <- if (identical(title, "Highest density reaches")) 100 else 118
    image_blank(PANEL_WIDTH, HEADER_HEIGHT, color = BACKGROUND) |>
    image_annotate(title,
                   gravity = "north", location = "+110+5", size = title_size,
                   weight = 700, color = "#222222") |>
    image_annotate("Contour quantile (%)", gravity = "center",
                   location = "+110-48", size = 96, weight = 700,
                   color = "#222222") |>
    image_composite(contour_legend, gravity = "south", offset = "+110+5")
  }

  year_blank <- image_blank(YEAR_WIDTH, HEADER_HEIGHT, color = BACKGROUND)
  spacer <- image_blank(GUTTER, HEADER_HEIGHT, color = BACKGROUND)
  column_spacer <- image_blank(COLUMN_GUTTER, HEADER_HEIGHT,
                               color = BACKGROUND)
  image_append(c(year_blank, spacer, production_cell, column_spacer,
                 contour_cell("All individuals"), column_spacer,
                 contour_cell("Highest density reaches")), stack = FALSE)
}

make_three_panel_row <- function(map_path, all_contour_path,
                                 high_contour_path, tags, year,
                                 enlarge_map = FALSE,
                                 show_x_ticks = FALSE) {
  left   <- tag_panel(read_map_panel(map_path, enlarge_map), tags[1])
  middle <- tag_panel(
    read_contour_panel(all_contour_path, show_x_ticks), tags[2],
    contour = TRUE
  )
  right <- tag_panel(
    read_contour_panel(high_contour_path, show_x_ticks), tags[3],
    contour = TRUE
  )
  spacer <- image_blank(GUTTER, PANEL_HEIGHT, color = BACKGROUND)
  column_spacer <- image_blank(COLUMN_GUTTER, PANEL_HEIGHT,
                               color = BACKGROUND)
  year_strip <- image_blank(YEAR_WIDTH, PANEL_HEIGHT, color = BACKGROUND) |>
    image_annotate(as.character(year), gravity = "center", degrees = 90,
                   size = 140, weight = 700, color = "#222222")
  image_append(c(year_strip, spacer, left, column_spacer, middle,
                 column_spacer, right), stack = FALSE)
}

make_footer <- function() {
  left_blank <- image_blank(YEAR_WIDTH + GUTTER + PANEL_WIDTH + COLUMN_GUTTER,
                            FOOTER_HEIGHT, color = BACKGROUND)
  x_label <- image_blank(2L * PANEL_WIDTH + COLUMN_GUTTER, FOOTER_HEIGHT,
                         color = BACKGROUND) |>
    image_annotate("Watershed Slope (log10 scale)", gravity = "north",
                   location = "+0+38", size = 120, weight = 700,
                   color = "#222222")
  image_append(c(left_blank, x_label), stack = FALSE)
}

save_manuscript_panel <- function(basin, years, map_paths, all_contour_paths,
                                  high_contour_paths, output_stem,
                                  enlarge_maps = FALSE) {
  if (length(years) != length(map_paths) ||
      length(years) != length(all_contour_paths) ||
      length(years) != length(high_contour_paths)) {
    stop("Years, maps, and both contour sets must have equal lengths.",
         call. = FALSE)
  }

  tags <- letters[seq_len(3L * length(years))]
  rows <- lapply(seq_along(years), function(i) {
    make_three_panel_row(
      map_paths[i], all_contour_paths[i], high_contour_paths[i],
      tags[(3L * i - 2L):(3L * i)], years[i], enlarge_maps,
      show_x_ticks = i == length(years)
    )
  })
  total_width <- YEAR_WIDTH + GUTTER + 2L * COLUMN_GUTTER + 3L * PANEL_WIDTH
  row_gap <- image_blank(total_width, GUTTER,
                         color = BACKGROUND)
  pieces <- list(make_header())
  for (i in seq_along(rows)) {
    pieces <- c(pieces, list(rows[[i]]))
    if (i < length(rows)) pieces <- c(pieces, list(row_gap))
  }
  pieces <- c(pieces, list(make_footer()))
  figure <- image_append(do.call(c, pieces), stack = TRUE)

  # One shared Y-axis title makes the common contour scale explicit while
  # avoiding six repeated labels.
  shared_y_title <- image_blank(1900, 360, color = BACKGROUND) |>
    image_annotate("Distance upstream (100 km)",
                   gravity = "center", size = 120, weight = 700,
                   color = "#222222") |>
    image_rotate(90)
  figure_info <- image_info(figure)
  figure <- image_extent(
    figure,
    sprintf("%dx%d", figure_info$width + 360L, figure_info$height),
    gravity = "west", color = BACKGROUND
  )
  figure <- image_composite(
    figure, shared_y_title, gravity = "east", offset = "+0+0"
  )

  png_path <- file.path(output_dir, paste0(output_stem, ".png"))
  image_write(figure, png_path, format = "png", density = "300x300")
  cat(sprintf("Saved %s (%s; %d panels)\n", output_stem, basin,
              3L * length(years)))
  invisible(png_path)
}

# ---- Kuskokwim: 18 panels ----------------------------------------------------
kusko_years <- KUSKO_YEARS
kusko_maps <- build_threshold_maps(
  "Kuskokwim", kusko_years,
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges_geomorphAdded.shp"),
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  here("Outputs", "ProductionData", "Kusko",
       sprintf("%d_Kusko_Assignment_Results.csv", kusko_years)),
  "Kusko"
)
kusko_all_contours <- file.path(
  contour_dir,
  sprintf("Kusko_%d_thresh%.1f.png", kusko_years, CONTOUR_FILT_THRESH)
)

# Always refresh the threshold-0.5 contour series so Figure 1 remains
# reproducible after the pipeline clears Figures/ or the source data change.
old_contour_override <- Sys.getenv(
  "CONTOUR_FILTER_THRESHOLD", unset = NA_character_
)
Sys.setenv(CONTOUR_FILTER_THRESHOLD = "0.5")
sys.source(
  here("Code", "Analysis", "02_ContourThreshnew.R"),
  envir = new.env(parent = globalenv())
)
if (is.na(old_contour_override)) {
  Sys.unsetenv("CONTOUR_FILTER_THRESHOLD")
} else {
  Sys.setenv(CONTOUR_FILTER_THRESHOLD = old_contour_override)
}

kusko_high_contours <- file.path(
  contour_dir,
  sprintf("Kusko_%d_thresh0.5.png", kusko_years)
)

save_manuscript_panel(
  basin = "Kuskokwim",
  years = kusko_years,
  map_paths = kusko_maps,
  all_contour_paths = kusko_all_contours,
  high_contour_paths = kusko_high_contours,
  output_stem = "Figure1_KuskoMultiPanel",
  enlarge_maps = TRUE
)

cat("Kuskokwim Figure 1 complete.\n")

# ---- Yukon: 12 panels --------------------------------------------------------
yukon_years <- YUKON_YEARS
yukon_maps <- build_threshold_maps(
  "Yukon", yukon_years,
  here("Data", "Spatial Data", "AnalysisShapefiles",
       "Yukon_edges_geomorphAdded.shp"),
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  here("Outputs", "ProductionData", "Yukon_full",
       sprintf("%d_Yukon_Full_Assignment_Results.csv", yukon_years)),
  "Yukon"
)
yukon_all_contours <- file.path(
  contour_dir,
  sprintf("Yukon_%d_thresh%.1f.png", yukon_years, CONTOUR_FILT_THRESH)
)
yukon_high_contours <- file.path(
  contour_dir,
  sprintf("Yukon_%d_thresh0.5.png", yukon_years)
)

save_manuscript_panel(
  basin = "Yukon",
  years = yukon_years,
  map_paths = yukon_maps,
  all_contour_paths = yukon_all_contours,
  high_contour_paths = yukon_high_contours,
  output_stem = "Figure2_YukonMultiPanel",
  enlarge_maps = TRUE
)

cat("Yukon Figure 2 complete.\n")
