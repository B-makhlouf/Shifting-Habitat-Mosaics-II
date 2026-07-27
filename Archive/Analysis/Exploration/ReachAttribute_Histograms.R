################################################################################
# REACH ATTRIBUTE HISTOGRAMS
#
# Proportional histograms (proportion of assigned fish per bin) for four
# reach-level variables, run for both the full-basin and US-only assignments:
#
#   Variables:
#     mean_temp  — extracted from SNAP raster via CalculateTemp.R
#     slope      — from edge shapefile
#     mean_elev  — from edge shapefile
#     log_slope  — log1p(slope)
#
#   Assignment sets:
#     FullBasin  — Outputs/ProductionData/Kusko|Yukon_full/
#     USonly     — Outputs/ProductionData/USonly/Kusko|Yuk_US/
#
# Two figure types per variable × watershed × assignment set:
#   1. Multi-year stacked panel (one panel per year, shared axes)
#   2. Individual-year figures  (fixed axes shared within watershed)
#
# Years:  Kusko 2017–2022   Yukon 2015, 2016, 2018, 2021
#
# Outputs: Figures/Histograms/FullBasin/<variable>/
#          Figures/Histograms/USonly/<variable>/
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(sf)
  library(here)
})

source(here("Code", "Analysis", "CalculateTemp.R"))

# ---- Years -------------------------------------------------------------------
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

# ---- Paths -------------------------------------------------------------------
KUSKO_EDGES_PATH <- here("Data", "Spatial Data", "AnalysisShapefiles",
                          "Kusko_edges_geomorphAdded.shp")
YUKON_EDGES_PATH <- here("Data", "Spatial Data", "AnalysisShapefiles",
                          "Yukon_edges_geomorphAdded.shp")
OUT_ROOT         <- here("Figures", "Histograms")


# ---- Build reach attribute tables (once per watershed) -----------------------
message("Reading Kusko edges and extracting temperature...")
kusko_edges_raw  <- st_read(KUSKO_EDGES_PATH, quiet = TRUE)
kusko_edges_tmp  <- add_mean_temp(kusko_edges_raw, temp_raster_path_for_year(2022))
kusko_reach_attrs <- st_drop_geometry(kusko_edges_tmp) %>%
  select(reachid, mean_temp, slope, mean_elev) %>%
  filter(mean_temp > -999) %>%
  mutate(log_slope = log1p(slope))

message("Reading Yukon edges and extracting temperature...")
yukon_edges_raw  <- st_read(YUKON_EDGES_PATH, quiet = TRUE)
yukon_edges_tmp  <- add_mean_temp(yukon_edges_raw, temp_raster_path_for_year(2022))
yukon_reach_attrs <- st_drop_geometry(yukon_edges_tmp) %>%
  select(reachid, mean_temp, slope, mean_elev) %>%
  filter(mean_temp > -999) %>%
  mutate(log_slope = log1p(slope))


# ---- Load assignment results and join reach attrs ----------------------------
read_assignments <- function(years, results_dir, result_suffix, reach_attrs) {
  purrr::map_dfr(years, function(yr) {
    fname <- file.path(results_dir,
                       sprintf("%d_%s_Assignment_Results.csv", yr, result_suffix))
    if (!file.exists(fname)) {
      warning(sprintf("File not found, skipping: %s", fname))
      return(NULL)
    }
    read_csv(fname, show_col_types = FALSE) %>%
      inner_join(reach_attrs, by = "reachid") %>%
      filter(assignment_individuals > 0) %>%
      mutate(year = factor(yr))
  })
}


# ---- Shared theme ------------------------------------------------------------
hist_theme <- theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11, hjust = 0),
    panel.spacing    = unit(0.6, "lines"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 9),
    plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 9, color = "grey40", margin = margin(b = 10))
  )


# ---- Annotation formatter ----------------------------------------------------
format_val <- function(x, var) {
  switch(var,
    mean_temp = sprintf("%.1f °C", x),
    slope     = sprintf("%.2f %%", x),
    mean_elev = sprintf("%.0f m",  x),
    log_slope = sprintf("%.2f",    x),
    sprintf("%.2f", x)
  )
}


# ==============================================================================
# CORE HELPERS
# ==============================================================================

# Compute breaks from the fish-weighted use distribution (clips 0.5–99.5%)
get_breaks <- function(dat, var, n_bins) {
  all_vals <- rep(dat[[var]], times = pmax(1L, round(dat$assignment_individuals)))
  clip_lo  <- quantile(all_vals, 0.005, na.rm = TRUE)
  clip_hi  <- quantile(all_vals, 0.995, na.rm = TRUE)
  seq(clip_lo, clip_hi, length.out = n_bins + 1)
}

# Bin use data by breaks; returns bin midpoints + per-year proportions
bin_use <- function(dat, var, breaks) {
  mids <- (breaks[-length(breaks)] + breaks[-1]) / 2
  dat %>%
    filter(is.finite(.data[[var]]),
           .data[[var]] >= breaks[1], .data[[var]] <= breaks[length(breaks)]) %>%
    mutate(bin_mid = mids[as.integer(cut(.data[[var]], breaks = breaks,
                                         include.lowest = TRUE))]) %>%
    group_by(year, bin_mid) %>%
    summarise(fish = sum(assignment_individuals), .groups = "drop") %>%
    group_by(year) %>%
    mutate(prop_use   = fish / sum(fish),
           total_fish = sum(fish)) %>%
    ungroup()
}


# ==============================================================================
# PLOT FUNCTIONS
# ==============================================================================

# ---- Multi-year stacked histogram --------------------------------------------
make_histogram <- function(dat, var, watershed_label, fill_color, out_file,
                            title_str, x_label, x_fmt = NULL, n_bins = 20) {

  breaks <- get_breaks(dat, var, n_bins)
  use_df <- bin_use(dat, var, breaks)

  bin_w   <- diff(breaks)[1] * 0.9
  xpad    <- diff(range(breaks)) * 0.04
  xlims   <- c(breaks[1] - xpad, breaks[length(breaks)] + xpad)
  n_years <- length(unique(dat$year))

  anno <- use_df %>%
    group_by(year) %>%
    summarise(total_fish = first(total_fish),
              wtd_mean   = sum(bin_mid * prop_use),
              .groups    = "drop") %>%
    mutate(label = sprintf("n = %s  |  wtd. mean = %s",
                           formatC(round(total_fish), format = "d", big.mark = ","),
                           format_val(wtd_mean, var)))

  p <- ggplot(use_df, aes(x = bin_mid, y = prop_use)) +
    geom_col(fill = fill_color, color = "white", linewidth = 0.25, width = bin_w) +
    geom_vline(data = anno, aes(xintercept = wtd_mean),
               inherit.aes = FALSE,
               color = "grey20", linetype = "dashed", linewidth = 0.6) +
    geom_text(data = anno, aes(x = Inf, y = Inf, label = label),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.6, size = 3, color = "grey30") +
    facet_wrap(~ year, ncol = 1, scales = "fixed") +
    coord_cartesian(xlim = xlims) +
    { if (!is.null(x_fmt)) scale_x_continuous(labels = x_fmt) else scale_x_continuous() } +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title    = sprintf("%s — %s", watershed_label, title_str),
         subtitle = "Proportion of assigned fish per bin. Dashed line = weighted mean. Y-axis shared across years.",
         x = x_label, y = "Proportion of Fish Assigned") +
    hist_theme

  ggsave(out_file, plot = p, width = 8, height = 2.5 * n_years,
         dpi = 300, bg = "white")
  message(sprintf("Saved: %s", out_file))
  invisible(p)
}


# ---- Individual-year figures -------------------------------------------------
make_individual_years <- function(dat, var, watershed_label, fill_color, out_dir,
                                   title_str, x_label, file_tag,
                                   x_fmt = NULL, n_bins = 20) {

  breaks <- get_breaks(dat, var, n_bins)
  use_df <- bin_use(dat, var, breaks)

  bin_w  <- diff(breaks)[1] * 0.9
  xpad   <- diff(range(breaks)) * 0.04
  xlims  <- c(breaks[1] - xpad, breaks[length(breaks)] + xpad)
  y_max  <- use_df %>% group_by(year) %>%
    summarise(mx = max(prop_use), .groups = "drop") %>%
    pull(mx) %>% max() * 1.15

  anno_all <- use_df %>%
    group_by(year) %>%
    summarise(total_fish = first(total_fish),
              wtd_mean   = sum(bin_mid * prop_use),
              .groups    = "drop") %>%
    mutate(label = sprintf("n = %s  |  wtd. mean = %s",
                           formatC(round(total_fish), format = "d", big.mark = ","),
                           format_val(wtd_mean, var)))

  for (yr in levels(use_df$year)) {
    yr_df   <- use_df   %>% filter(year == yr)
    yr_anno <- anno_all %>% filter(year == yr)

    p <- ggplot(yr_df, aes(x = bin_mid, y = prop_use)) +
      geom_col(fill = fill_color, color = "white", linewidth = 0.25, width = bin_w) +
      geom_vline(xintercept = yr_anno$wtd_mean,
                 color = "grey20", linetype = "dashed", linewidth = 0.6) +
      annotate("text", x = Inf, y = Inf, label = yr_anno$label,
               hjust = 1.05, vjust = 1.6, size = 3.5, color = "grey30") +
      coord_cartesian(xlim = xlims, ylim = c(0, y_max)) +
      { if (!is.null(x_fmt)) scale_x_continuous(labels = x_fmt) else scale_x_continuous() } +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title    = sprintf("%s — %s — %s", watershed_label, title_str, yr),
           subtitle = "Proportion of assigned fish per bin. Axes fixed across all years.",
           x = x_label, y = "Proportion of Fish Assigned") +
      hist_theme

    out_file <- file.path(out_dir,
                          sprintf("%s_%s_%s.png", watershed_label, file_tag, yr))
    ggsave(out_file, plot = p, width = 8, height = 4, dpi = 300, bg = "white")
    message(sprintf("Saved: %s", out_file))
  }
}


# ==============================================================================
# VARIABLE SPECS
# ==============================================================================

var_specs <- list(
  list(
    var       = "mean_temp",
    title_str = "Mean Air Temperature of Assigned Reaches",
    x_label   = "Mean Air Temperature (°C)",
    x_fmt     = function(x) paste0(x, " °C"),
    file_tag  = "AirTemp",
    subdir    = "AirTemp"
  ),
  list(
    var       = "slope",
    title_str = "Channel Slope of Assigned Reaches",
    x_label   = "Channel Slope (%)",
    x_fmt     = function(x) paste0(x, "%"),
    file_tag  = "Slope",
    subdir    = "Slope"
  ),
  list(
    var       = "mean_elev",
    title_str = "Mean Elevation of Assigned Reaches",
    x_label   = "Mean Elevation (m)",
    x_fmt     = function(x) paste0(formatC(x, format = "f", digits = 0), " m"),
    file_tag  = "Elevation",
    subdir    = "Elevation"
  ),
  list(
    var       = "log_slope",
    title_str = "Log(Slope + 1) of Assigned Reaches",
    x_label   = "log(Channel Slope + 1)",
    x_fmt     = NULL,
    file_tag  = "LogSlope",
    subdir    = "LogSlope"
  )
)


# ==============================================================================
# WATERSHED × ASSIGNMENT-SET SPECS
# ==============================================================================

runs <- list(
  # ---- Full basin ------------------------------------------------------------
  list(
    run_label   = "FullBasin",
    label       = "Kuskokwim",
    years       = KUSKO_YEARS,
    results_dir = here("Outputs", "ProductionData", "Kusko"),
    suffix      = "Kusko",
    reach_attrs = kusko_reach_attrs,
    color       = "#4292c6"
  ),
  list(
    run_label   = "FullBasin",
    label       = "Yukon",
    years       = YUKON_YEARS,
    results_dir = here("Outputs", "ProductionData", "Yukon_full"),
    suffix      = "Yukon_Full",
    reach_attrs = yukon_reach_attrs,
    color       = "#41ab5d"
  ),
  # ---- US only ---------------------------------------------------------------
  list(
    run_label   = "USonly",
    label       = "Kuskokwim",
    years       = KUSKO_YEARS,
    results_dir = here("Outputs", "ProductionData", "USonly", "Kusko"),
    suffix      = "Kusko",
    reach_attrs = kusko_reach_attrs,
    color       = "#4292c6"
  ),
  list(
    run_label   = "USonly",
    label       = "Yukon",
    years       = YUKON_YEARS,
    results_dir = here("Outputs", "ProductionData", "USonly", "Yuk_US"),
    suffix      = "Yuk_US",
    reach_attrs = yukon_reach_attrs,
    color       = "#41ab5d"
  )
)


# ==============================================================================
# GENERATE ALL FIGURES
# ==============================================================================

for (run in runs) {

  dat <- read_assignments(run$years, run$results_dir, run$suffix, run$reach_attrs)
  if (is.null(dat) || nrow(dat) == 0) {
    warning(sprintf("No data for %s %s — skipping.", run$run_label, run$label))
    next
  }

  for (spec in var_specs) {

    var_dir <- file.path(OUT_ROOT, run$run_label, spec$subdir)
    ind_dir <- file.path(var_dir, "Individual_Years", run$label)
    dir.create(ind_dir, recursive = TRUE, showWarnings = FALSE)

    # Stacked multi-year
    make_histogram(
      dat             = dat,
      var             = spec$var,
      watershed_label = run$label,
      fill_color      = run$color,
      out_file        = file.path(var_dir,
                                  sprintf("%s_%s_Histograms.png",
                                          run$label, spec$file_tag)),
      title_str       = spec$title_str,
      x_label         = spec$x_label,
      x_fmt           = spec$x_fmt
    )

    # Individual years
    make_individual_years(
      dat             = dat,
      var             = spec$var,
      watershed_label = run$label,
      fill_color      = run$color,
      out_dir         = ind_dir,
      title_str       = spec$title_str,
      x_label         = spec$x_label,
      file_tag        = spec$file_tag,
      x_fmt           = spec$x_fmt
    )
  }
}

message("\nDone. All figures written to Figures/Histograms/")
