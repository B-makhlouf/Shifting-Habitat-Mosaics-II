################################################################################
# NATAL ISOTOPE VALUE HISTOGRAMS
#
# Produces two multi-panel figures (one per watershed), each stacked in a
# single column with one panel per year. Years match those used in the full
# basin production script (01_FullBasinProductionEstimates.R).
#
# Isotope: 87Sr/86Sr (strontium isotope ratio)
#
# Outputs:
#   Figures/NatalIso/Kusko_NatalIso_Histograms.png
#   Figures/NatalIso/Yukon_NatalIso_Histograms.png
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(here)
})

# ---- Years (matching 01_FullBasinProductionEstimates.R) ----------------------
KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

NATAL_DIR <- here("Data", "Natal Origins")
OUT_DIR   <- here("Figures", "NatalIso")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# ---- Helper: read and stack natal iso data for a set of years ----------------
read_natal <- function(years, watershed) {
  purrr::map_dfr(years, function(yr) {
    fname <- file.path(NATAL_DIR,
                       sprintf("%d_%s_Natal_Origins_Genetics_CPUE.csv", yr, watershed))
    if (!file.exists(fname)) {
      warning(sprintf("File not found, skipping: %s", fname))
      return(NULL)
    }
    read_csv(fname, show_col_types = FALSE) %>%
      filter(!is.na(natal_iso)) %>%
      transmute(year = factor(yr), natal_iso)
  })
}


# ---- Load data ---------------------------------------------------------------
kusko_dat <- read_natal(KUSKO_YEARS, "Kusko")
yukon_dat <- read_natal(YUKON_YEARS, "Yukon")


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


# ---- Plot function -----------------------------------------------------------
make_histogram <- function(dat, watershed_label, fill_color, out_file) {

  n_years <- length(unique(dat$year))

  # Compute per-year summary stats for annotation
  anno <- dat %>%
    group_by(year) %>%
    summarise(
      n    = n(),
      med  = median(natal_iso),
      .groups = "drop"
    ) %>%
    mutate(label = sprintf("n = %d  |  median = %.4f", n, med))

  # x-axis limits: clip to 0.5th–99.5th percentile to exclude outliers,
  # then add a small pad. Use coord_cartesian so bins are computed over the
  # clipped range (data outside are simply excluded from view).
  clip_lo <- quantile(dat$natal_iso, 0.005, na.rm = TRUE)
  clip_hi <- quantile(dat$natal_iso, 0.995, na.rm = TRUE)
  xpad    <- (clip_hi - clip_lo) * 0.04
  xlims   <- c(clip_lo - xpad, clip_hi + xpad)

  # Filter data to clipped range so bin widths are sensible
  dat_clipped <- dat %>% filter(natal_iso >= clip_lo, natal_iso <= clip_hi)

  p <- ggplot(dat_clipped, aes(x = natal_iso)) +
    geom_histogram(
      bins      = 20,
      fill      = fill_color,
      color     = "white",
      linewidth = 0.25
    ) +
    geom_vline(
      data        = anno,
      aes(xintercept = med),
      color       = "grey20",
      linetype    = "dashed",
      linewidth   = 0.6
    ) +
    geom_text(
      data  = anno,
      aes(x = Inf, y = Inf, label = label),
      hjust = 1.05, vjust = 1.6,
      size  = 3, color = "grey30"
    ) +
    facet_wrap(~ year, ncol = 1, scales = "fixed") +
    coord_cartesian(xlim = xlims) +
    scale_x_continuous() +
    labs(
      title    = sprintf("%s — Natal Otolith ⁸⁷Sr/⁸⁶Sr", watershed_label),
      subtitle = "Dashed line = median. Y-axis shared across years.",
      x        = expression(""^87*Sr/""^86*Sr[natal]),
      y        = "Count"
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
get_shared_limits <- function(dat) {
  clip_lo <- quantile(dat$natal_iso, 0.005, na.rm = TRUE)
  clip_hi <- quantile(dat$natal_iso, 0.995, na.rm = TRUE)
  xpad    <- (clip_hi - clip_lo) * 0.04
  dat_clipped <- dat %>% filter(natal_iso >= clip_lo, natal_iso <= clip_hi)

  # Build histograms to find the tallest bin across all years
  max_count <- dat_clipped %>%
    group_by(year) %>%
    summarise(
      max_bin = max(hist(natal_iso, breaks = 60, plot = FALSE)$counts),
      .groups = "drop"
    ) %>%
    pull(max_bin) %>%
    max()

  list(
    xlims       = c(clip_lo - xpad, clip_hi + xpad),
    ylim        = c(0, max_count * 1.1),
    dat_clipped = dat_clipped
  )
}


# ---- Generate figures --------------------------------------------------------
make_histogram(
  dat              = kusko_dat,
  watershed_label  = "Kuskokwim",
  fill_color       = "#4292c6",
  out_file         = file.path(OUT_DIR, "Kusko_NatalIso_Histograms.png")
)

make_histogram(
  dat              = yukon_dat,
  watershed_label  = "Yukon",
  fill_color       = "#41ab5d",
  out_file         = file.path(OUT_DIR, "Yukon_NatalIso_Histograms.png")
)


# ---- Individual year figures (shared axes within each watershed) -------------
make_individual_years <- function(dat, watershed_label, fill_color, out_dir) {

  lims <- get_shared_limits(dat)

  anno_all <- dat %>%
    group_by(year) %>%
    summarise(n = n(), med = median(natal_iso), .groups = "drop") %>%
    mutate(label = sprintf("n = %d  |  median = %.4f", n, med))

  for (yr in levels(dat$year)) {
    yr_dat  <- lims$dat_clipped %>% filter(year == yr)
    yr_anno <- anno_all %>% filter(year == yr)

    p <- ggplot(yr_dat, aes(x = natal_iso)) +
      geom_histogram(
        bins      = 60,
        fill      = fill_color,
        color     = "white",
        linewidth = 0.25
      ) +
      geom_vline(
        xintercept = yr_anno$med,
        color      = "grey20",
        linetype   = "dashed",
        linewidth  = 0.6
      ) +
      annotate(
        "text",
        x = Inf, y = Inf,
        label = yr_anno$label,
        hjust = 1.05, vjust = 1.6,
        size  = 3.5, color = "grey30"
      ) +
      coord_cartesian(xlim = lims$xlims, ylim = lims$ylim) +
      scale_x_continuous() +
      labs(
        title = sprintf("%s — Natal Otolith ⁸⁷Sr/⁸⁶Sr — %s", watershed_label, yr),
        subtitle = "Dashed line = median. Axes fixed across all years.",
        x = expression(""^87*Sr/""^86*Sr[natal]),
        y = "Count"
      ) +
      hist_theme

    out_file <- file.path(out_dir, sprintf("%s_NatalIso_%s.png", watershed_label, yr))
    ggsave(out_file, plot = p, width = 8, height = 4, dpi = 300, bg = "white")
    message(sprintf("Saved: %s", out_file))
  }
}

ind_dir_kusko <- file.path(OUT_DIR, "Individual_Years", "Kusko")
ind_dir_yukon <- file.path(OUT_DIR, "Individual_Years", "Yukon")
dir.create(ind_dir_kusko, recursive = TRUE, showWarnings = FALSE)
dir.create(ind_dir_yukon, recursive = TRUE, showWarnings = FALSE)

make_individual_years(kusko_dat, "Kuskokwim", "#4292c6", ind_dir_kusko)
make_individual_years(yukon_dat, "Yukon",      "#41ab5d", ind_dir_yukon)

message("\nDone.")
