################################################################################
# AIR TEMPERATURE HISTOGRAMS
#
# Produces two multi-panel figures (one per watershed), each stacked in a
# single column with one panel per year. Years match those used in the full
# basin production script (01_FullBasinProductionEstimates.R).
#
# For each stream reach, mean air temperature is extracted from the SNAP
# raster via CalculateTemp.R. Assignment results (assignment_individuals per
# reach) are joined to temperature values, then reaches are binned by
# temperature and total fish per bin are summed — giving a temperature
# histogram weighted by assigned fish count.
#
# Outputs:
#   Figures/AirTemp/Kusko_AirTemp_Histograms.png
#   Figures/AirTemp/Yukon_AirTemp_Histograms.png
#   Figures/AirTemp/Individual_Years/Kusko/Kuskokwim_AirTemp_<year>.png
#   Figures/AirTemp/Individual_Years/Yukon/Yukon_AirTemp_<year>.png
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(sf)
  library(here)
})

source(here("Code", "Analysis", "CalculateTemp.R"))

# ---- Years (matching 01_FullBasinProductionEstimates.R) ----------------------
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

OUT_DIR <- here("Figures", "AirTemp")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Paths -------------------------------------------------------------------
KUSKO_EDGES_PATH <- here("Data", "Spatial Data", "AnalysisShapefiles",
                          "Kusko_edges_geomorphAdded.shp")
YUKON_EDGES_PATH <- here("Data", "Spatial Data", "AnalysisShapefiles",
                          "Yukon_edges_geomorphAdded.shp")
KUSKO_RESULTS_DIR <- here("Outputs", "ProductionData", "Kusko")
YUKON_RESULTS_DIR <- here("Outputs", "ProductionData", "Yukon_full")

# ---- Extract mean temperature for each edge (done once per watershed) --------
message("Extracting temperature for Kusko edges...")
kusko_edges_raw <- st_read(KUSKO_EDGES_PATH, quiet = TRUE)
kusko_temp_path <- temp_raster_path_for_year(2022)   # placeholder — same raster for all years
kusko_edges     <- add_mean_temp(kusko_edges_raw, kusko_temp_path)
kusko_temp_df   <- st_drop_geometry(kusko_edges) %>%
  select(reachid, mean_temp) %>%
  filter(!is.na(mean_temp), mean_temp > -999)   # drop nodata sentinels

message("Extracting temperature for Yukon edges...")
yukon_edges_raw <- st_read(YUKON_EDGES_PATH, quiet = TRUE)
yukon_temp_path <- temp_raster_path_for_year(2022)
yukon_edges     <- add_mean_temp(yukon_edges_raw, yukon_temp_path)
yukon_temp_df   <- st_drop_geometry(yukon_edges) %>%
  select(reachid, mean_temp) %>%
  filter(!is.na(mean_temp), mean_temp > -999)


# ---- Helper: read assignment results and join temperature --------------------
read_temp_assignments <- function(years, results_dir, result_suffix, temp_df) {
  purrr::map_dfr(years, function(yr) {
    fname <- file.path(results_dir,
                       sprintf("%d_%s_Assignment_Results.csv", yr, result_suffix))
    if (!file.exists(fname)) {
      warning(sprintf("File not found, skipping: %s", fname))
      return(NULL)
    }
    read_csv(fname, show_col_types = FALSE) %>%
      inner_join(temp_df, by = "reachid") %>%
      filter(assignment_individuals > 0) %>%
      transmute(year = factor(yr), mean_temp, assignment_individuals)
  })
}

kusko_dat <- read_temp_assignments(KUSKO_YEARS, KUSKO_RESULTS_DIR, "Kusko",      kusko_temp_df)
yukon_dat <- read_temp_assignments(YUKON_YEARS, YUKON_RESULTS_DIR, "Yukon_Full", yukon_temp_df)


# ---- Shared theme (mirrors NatalIso_Histograms.R) ----------------------------
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


# ---- Plot function -----------------------------------------------------------
# dat: data frame with columns year (factor), mean_temp, assignment_individuals
# Each observation is a *reach*; assignment_individuals is the fish count weight.
make_temp_histogram <- function(dat, watershed_label, fill_color, out_file,
                                 n_bins = 20) {

  n_years <- length(unique(dat$year))

  # Per-year annotation: total fish and weighted median temperature
  anno <- dat %>%
    group_by(year) %>%
    summarise(
      total_fish = sum(assignment_individuals),
      med_temp   = weighted.mean(mean_temp, assignment_individuals),
      .groups    = "drop"
    ) %>%
    mutate(label = sprintf("n = %s  |  wtd. median = %.1f °C",
                           formatC(round(total_fish), format = "d", big.mark = ","),
                           med_temp))

  # Clip x-axis to 0.5th–99.5th percentile of temp (weighted)
  all_temps <- rep(dat$mean_temp, times = pmax(1, round(dat$assignment_individuals)))
  clip_lo   <- quantile(all_temps, 0.005, na.rm = TRUE)
  clip_hi   <- quantile(all_temps, 0.995, na.rm = TRUE)
  xpad      <- (clip_hi - clip_lo) * 0.04
  xlims     <- c(clip_lo - xpad, clip_hi + xpad)

  dat_clipped <- dat %>% filter(mean_temp >= clip_lo, mean_temp <= clip_hi)

  # Expand reaches into pseudo-individual rows for ggplot histogram
  # (ggplot2 doesn't natively support weighted histograms; we use stat_bin with
  # weight aesthetic, which sums weights per bin)
  p <- ggplot(dat_clipped, aes(x = mean_temp)) +
    stat_bin(
      aes(weight = assignment_individuals),
      bins      = n_bins,
      fill      = fill_color,
      color     = "white",
      linewidth = 0.25
    ) +
    geom_vline(
      data        = anno,
      aes(xintercept = med_temp),
      inherit.aes = FALSE,
      color       = "grey20",
      linetype    = "dashed",
      linewidth   = 0.6
    ) +
    geom_text(
      data        = anno,
      aes(x = Inf, y = Inf, label = label),
      inherit.aes = FALSE,
      hjust = 1.05, vjust = 1.6,
      size  = 3, color = "grey30"
    ) +
    facet_wrap(~ year, ncol = 1, scales = "fixed") +
    coord_cartesian(xlim = xlims) +
    scale_x_continuous(labels = function(x) paste0(x, " °C")) +
    labs(
      title    = sprintf("%s — Mean Air Temperature of Assigned Reaches", watershed_label),
      subtitle = "Weighted by assigned fish count. Dashed line = weighted mean temperature.",
      x        = "Mean Air Temperature (°C)",
      y        = "Fish Assigned"
    ) +
    hist_theme

  ggsave(
    out_file,
    plot   = p,
    width  = 8,
    height = 2.5 * n_years,
    dpi    = 300,
    bg     = "white"
  )
  message(sprintf("Saved: %s", out_file))
  invisible(p)
}


# ---- Helper: compute shared axis limits for a dataset -----------------------
get_shared_limits_temp <- function(dat, n_bins = 20) {
  all_temps <- rep(dat$mean_temp, times = pmax(1, round(dat$assignment_individuals)))
  clip_lo   <- quantile(all_temps, 0.005, na.rm = TRUE)
  clip_hi   <- quantile(all_temps, 0.995, na.rm = TRUE)
  xpad      <- (clip_hi - clip_lo) * 0.04
  dat_clipped <- dat %>% filter(mean_temp >= clip_lo, mean_temp <= clip_hi)

  # Find the tallest bin across all years for a shared y-axis
  max_count <- dat_clipped %>%
    group_by(year) %>%
    summarise(
      max_bin = {
        breaks <- seq(clip_lo, clip_hi, length.out = n_bins + 1)
        bdf    <- cut(mean_temp, breaks = breaks, include.lowest = TRUE)
        tapply(assignment_individuals, bdf, sum, na.rm = TRUE) %>%
          max(na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    pull(max_bin) %>%
    max(na.rm = TRUE)

  list(
    xlims       = c(clip_lo - xpad, clip_hi + xpad),
    ylim        = c(0, max_count * 1.15),
    dat_clipped = dat_clipped
  )
}


# ---- Individual-year figures (shared axes within each watershed) -------------
make_individual_years_temp <- function(dat, watershed_label, fill_color,
                                        out_dir, n_bins = 20) {

  lims <- get_shared_limits_temp(dat, n_bins = n_bins)

  anno_all <- dat %>%
    group_by(year) %>%
    summarise(
      total_fish = sum(assignment_individuals),
      med_temp   = weighted.mean(mean_temp, assignment_individuals),
      .groups    = "drop"
    ) %>%
    mutate(label = sprintf("n = %s  |  wtd. mean = %.1f °C",
                           formatC(round(total_fish), format = "d", big.mark = ","),
                           med_temp))

  for (yr in levels(dat$year)) {
    yr_dat  <- lims$dat_clipped %>% filter(year == yr)
    yr_anno <- anno_all %>% filter(year == yr)

    p <- ggplot(yr_dat, aes(x = mean_temp)) +
      stat_bin(
        aes(weight = assignment_individuals),
        bins      = n_bins,
        fill      = fill_color,
        color     = "white",
        linewidth = 0.25
      ) +
      geom_vline(
        xintercept = yr_anno$med_temp,
        color      = "grey20",
        linetype   = "dashed",
        linewidth  = 0.6
      ) +
      annotate(
        "text",
        x     = Inf, y = Inf,
        label = yr_anno$label,
        hjust = 1.05, vjust = 1.6,
        size  = 3.5, color = "grey30"
      ) +
      coord_cartesian(xlim = lims$xlims, ylim = lims$ylim) +
      scale_x_continuous(labels = function(x) paste0(x, " °C")) +
      labs(
        title    = sprintf("%s — Mean Air Temperature — %s", watershed_label, yr),
        subtitle = "Weighted by assigned fish count. Axes fixed across all years.",
        x        = "Mean Air Temperature (°C)",
        y        = "Fish Assigned"
      ) +
      hist_theme

    out_file <- file.path(out_dir, sprintf("%s_AirTemp_%s.png", watershed_label, yr))
    ggsave(out_file, plot = p, width = 8, height = 4, dpi = 300, bg = "white")
    message(sprintf("Saved: %s", out_file))
  }
}


# ---- Generate multi-year stacked figures ------------------------------------
make_temp_histogram(
  dat              = kusko_dat,
  watershed_label  = "Kuskokwim",
  fill_color       = "#4292c6",
  out_file         = file.path(OUT_DIR, "Kusko_AirTemp_Histograms.png")
)

make_temp_histogram(
  dat              = yukon_dat,
  watershed_label  = "Yukon",
  fill_color       = "#41ab5d",
  out_file         = file.path(OUT_DIR, "Yukon_AirTemp_Histograms.png")
)


# ---- Individual-year figures -------------------------------------------------
ind_dir_kusko <- file.path(OUT_DIR, "Individual_Years", "Kusko")
ind_dir_yukon <- file.path(OUT_DIR, "Individual_Years", "Yukon")
dir.create(ind_dir_kusko, recursive = TRUE, showWarnings = FALSE)
dir.create(ind_dir_yukon, recursive = TRUE, showWarnings = FALSE)

make_individual_years_temp(kusko_dat, "Kuskokwim", "#4292c6", ind_dir_kusko)
make_individual_years_temp(yukon_dat, "Yukon",      "#41ab5d", ind_dir_yukon)

message("\nDone.")
