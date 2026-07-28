################################################################################
# EMPIRICAL CHANGE FROM THE AMONG-YEAR AVERAGE
#
# Reads the current production estimates created by
# 01_FullBasinRelativeProdMaps.R and produces one figure for each basin-year:
# annual percentage-point departure from the basin's equal-year average.
# The same assignment_norm > CONTOUR_FILT_THRESH rule used by the contour
# figures is applied before annual population shares are calculated.
#
# Fixed classes make every panel directly comparable and avoid KDE/bandwidth
# choices. Cell-level plotting data are also written to Outputs for auditing.
################################################################################

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(here)
library(scales)

source(here("Code", "Analysis", "params.R"))

production_root <- here("Outputs", "ProductionData")
figure_dir <- here("Figures", "02_Contours", "02_change_from_average")
output_dir <- here("Outputs", "EmpiricalChangeFromAverage")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# These fixed breaks are shared by both basins and all years.
SLOPE_BREAKS <- c(-Inf, 1, 2.5, 7, 20, Inf)
SLOPE_LABELS <- c("<1", "1-2.5", "2.5-7", "7-20", "20+")

# DistUpstre is stored in metres.
DISTANCE_BREAKS <- c(-Inf, 2e5, 4e5, 6e5, 8e5, Inf)
DISTANCE_LABELS <- c("<200", "200-400", "400-600", "600-800", "800+")

cat("Loading reach morphology...\n")
yukon_attr <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  quiet = TRUE
) |>
  st_drop_geometry() |>
  select(reachid, WtrshdSlp, DistUpstre)

kusko_attr <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
) |>
  st_drop_geometry() |>
  select(reachid, WtrshdSlp, DistUpstre)

load_basin <- function(basin, subdir, file_pattern, years, attr_df) {
  bind_rows(lapply(years, function(year) {
    read_csv(
      file.path(production_root, subdir, sprintf(file_pattern, year)),
      show_col_types = FALSE
    ) |>
      select(reachid, assignment_norm, assignment_individuals) |>
      left_join(attr_df, by = "reachid") |>
      filter(
        assignment_norm > CONTOUR_FILT_THRESH,
        is.finite(assignment_individuals), assignment_individuals > 0,
        is.finite(WtrshdSlp), WtrshdSlp > 0,
        is.finite(DistUpstre), DistUpstre >= 0
      ) |>
      transmute(
        basin = basin,
        year = factor(year, levels = years),
        fish = assignment_individuals,
        slope_class = cut(
          WtrshdSlp, breaks = SLOPE_BREAKS, labels = SLOPE_LABELS,
          right = FALSE
        ),
        distance_class = cut(
          DistUpstre, breaks = DISTANCE_BREAKS, labels = DISTANCE_LABELS,
          right = FALSE
        )
      )
  }))
}

annual_cell_percentages <- function(reach_data, years) {
  summarized <- reach_data |>
    group_by(basin, year, slope_class, distance_class) |>
    summarise(fish = sum(fish), .groups = "drop")

  full_grid <- expand_grid(
    basin = unique(reach_data$basin),
    year = factor(years, levels = years),
    slope_class = factor(SLOPE_LABELS, levels = SLOPE_LABELS),
    distance_class = factor(DISTANCE_LABELS, levels = DISTANCE_LABELS)
  )

  full_grid |>
    left_join(
      summarized,
      by = c("basin", "year", "slope_class", "distance_class")
    ) |>
    mutate(fish = coalesce(fish, 0)) |>
    group_by(basin, year) |>
    mutate(
      annual_fish = sum(fish),
      percent_fish = 100 * fish / annual_fish
    ) |>
    ungroup() |>
    group_by(basin, slope_class, distance_class) |>
    mutate(
      average_percent = mean(percent_fish),
      difference_pp = percent_fish - average_percent
    ) |>
    ungroup()
}

yukon_reaches <- load_basin(
  "Yukon", "Yukon_full", "%d_Yukon_Full_Assignment_Results.csv",
  YUKON_YEARS, yukon_attr
)
kusko_reaches <- load_basin(
  "Kuskokwim", "Kusko", "%d_Kusko_Assignment_Results.csv",
  KUSKO_YEARS, kusko_attr
)

yukon_cells <- annual_cell_percentages(yukon_reaches, YUKON_YEARS)
kusko_cells <- annual_cell_percentages(kusko_reaches, KUSKO_YEARS)
all_cells <- bind_rows(yukon_cells, kusko_cells)

# Calculation checks: every annual empirical distribution sums to 100%, every
# cell's equal-year departures average to zero, and every annual departure grid
# sums to zero (minor floating-point error allowed).
annual_totals <- all_cells |>
  group_by(basin, year) |>
  summarise(total = sum(percent_fish), .groups = "drop")
cell_departure_means <- all_cells |>
  group_by(basin, slope_class, distance_class) |>
  summarise(total = sum(difference_pp), .groups = "drop")
annual_departure_totals <- all_cells |>
  group_by(basin, year) |>
  summarise(total = sum(difference_pp), .groups = "drop")
if (
  any(abs(annual_totals$total - 100) > 1e-8) ||
  any(abs(cell_departure_means$total) > 1e-8) ||
  any(abs(annual_departure_totals$total) > 1e-8)
) {
  stop("Empirical change-from-average calculation failed its sum checks.")
}

write_csv(
  all_cells |>
    mutate(
      year = as.integer(as.character(year)),
      slope_class = as.character(slope_class),
      distance_class = as.character(distance_class)
    ),
  file.path(output_dir, "annual_morphology_population_percentages.csv")
)

heatmap_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11, color = "grey25"),
    axis.text.y = element_text(size = 11, color = "grey25"),
    axis.title = element_text(size = 14, face = "bold", color = "grey15"),
    strip.text = element_text(size = 15, face = "bold", color = "grey15"),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "right",
    legend.key.height = grid::unit(2.2, "cm"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "grey35", hjust = 0),
    plot.caption = element_text(size = 10, color = "grey45", hjust = 0),
    plot.margin = margin(14, 18, 12, 14)
  )

cell_text_color <- function(fill_value, threshold) {
  ifelse(abs(fill_value) >= threshold, "white", "grey15")
}

plot_difference_heatmap <- function(cells, basin, selected_year, limit) {
  cells <- cells |> filter(as.integer(as.character(year)) == selected_year)

  ggplot(
    cells,
    aes(x = slope_class, y = distance_class, fill = difference_pp)
  ) +
    geom_tile(color = "white", linewidth = 0.7) +
    scale_fill_gradientn(
      "Difference from average",
      colours = c(
        "#3B4FA3", "#4D7FBA", "#6DA5C7", "#92C4D3", "#B9D9DF",
        "#ECEBD0",
        "#FED98E", "#FDB56A", "#F8794C", "#E83B2F", "#B4042D"
      ),
      values = seq(0, 1, length.out = 11),
      limits = c(-limit, limit),
      labels = label_number(suffix = " pp", accuracy = 0.1)
    ) +
    # Match the contour orientation: distance increases from bottom to top.
    scale_y_discrete(limits = DISTANCE_LABELS) +
    labs(
      x = "Watershed slope",
      y = "Distance upstream (km)",
      title = paste0(basin, " ", selected_year, ": change from average"),
      subtitle = paste0(
        "Annual retained-population share minus the equal-year basin average"
      ),
      caption = paste0(
        "Reaches require assignment_norm > ", CONTOUR_FILT_THRESH,
        "; cell colours are percentage-point differences and each panel sums to 0."
      )
    ) +
    coord_equal() +
    heatmap_theme
}

save_basin_figures <- function(cells, basin, file_prefix, years) {
  limit <- max(abs(cells$difference_pp), na.rm = TRUE)
  for (selected_year in years) {
    figure <- plot_difference_heatmap(cells, basin, selected_year, limit)
    path <- file.path(
      figure_dir,
      sprintf("%s_%d_change_from_average.png", file_prefix, selected_year)
    )
    ggsave(path, figure, width = 10, height = 7, dpi = 180)
    cat("Saved:", path, "\n")
  }
}

cat("Creating morphology population heatmaps...\n")
save_basin_figures(yukon_cells, "Yukon", "Yukon", YUKON_YEARS)
save_basin_figures(
  kusko_cells, "Kuskokwim", "Kusko", KUSKO_YEARS
)
cat("Done. Heatmaps saved to:", figure_dir, "\n")
