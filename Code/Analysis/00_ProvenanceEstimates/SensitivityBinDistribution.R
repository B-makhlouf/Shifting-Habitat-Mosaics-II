################################################################################
# SENSITIVITY ANALYSIS — Relative Production Bin Distribution by Threshold
# ALL YEARS · BOTH BASINS
#
# Mirrors the actual production pipeline: for each threshold τ ∈ {0.2, …, 0.9}
#
#   Step 1  Compute rescaled likelihood per reach per individual (max = 1.0).
#   Step 2  Apply threshold: zero out any reach-individual value below τ.
#   Step 3  Aggregate across all individuals (row means) → per-reach production.
#   Step 4  Re-normalise aggregate to [0, 1] (divide by max) → rel_production.
#   Step 5  Bin retained reaches (rel_production > 0) into 0.1-wide bins.
#   Step 6  proportion in bin = km_in_bin / total_retained_km  → sums to 100%.
#
# Because the aggregate is re-normalised AFTER zeroing, production values always
# span the full [0, 1] range — giving a distribution across all bins at every
# threshold, with the composition shifting as τ rises.
#
# Pooling: Steps 1–6 are run per year; proportions are then averaged across
# years (equal weight per year), shown with a per-year breakdown in Figure 2.
#
# Output figures saved to Figures/SensitivityAnalysis/:
#   sensitivity_bin_distribution_pooled.png   — pooled stacked bar, 2 basins
#   sensitivity_bin_distribution_peryear.png  — per-year stacked bars (grid)
################################################################################

suppressPackageStartupMessages({
  library(sf);       library(dplyr);    library(readr)
  library(tibble);   library(tidyr);    library(purrr)
  library(ggplot2);  library(here)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

# Threshold sweep
THRESHOLDS <- seq(0.2, 0.9, by = 0.1)

# Relative production bin breaks (left-closed, right-open; last bin closes at 1)
BIN_BREAKS <- seq(0.1, 1.0, by = 0.1)

# Stream-order floor (matches other scripts in this directory)
MIN_STREAM_ORDER <- 3

OUT_DIR <- here("Figures", "SensitivityAnalysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# PATHS & SHARED DATA
# ==============================================================================

PATHS <- list(
  kusko_edges    = here("Data", "Spatial Data", "AnalysisShapefiles",
                        "Kusko_edges_geomorphAdded.shp"),
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles",
                        "Yukon_GEO2.shp"),
  natal_dir      = here("Data", "Natal Origins"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv")
)

cat("Loading spatial layers...\n")
KUSKO_EDGES <- st_read(PATHS$kusko_edges, quiet = TRUE)
YUKON_EDGES <- st_read(PATHS$yukon_edges, quiet = TRUE)

daily_gen_wide <- read_csv(PATHS$daily_genetics, show_col_types = FALSE) |>
  select(sampleYear, DOY, genetic_assignment, proportion) |>
  pivot_wider(names_from = genetic_assignment, values_from = proportion,
              values_fill = 0) |>
  rename(year = sampleYear,
         avg_Lower = Lower, avg_Middle = Middle, avg_Upper = Upper)


# ==============================================================================
# HELPER — bin labels (ordered factor levels, used throughout)
# ==============================================================================

make_bin_labels <- function(breaks) {
  paste0(
    formatC(breaks[-length(breaks)], format = "f", digits = 1), "–",
    formatC(breaks[-1],              format = "f", digits = 1)
  )
}

BIN_LABELS <- make_bin_labels(BIN_BREAKS)   # "0.1–0.2", "0.2–0.3", ..., "0.9–1.0"
N_BINS     <- length(BIN_LABELS)


# ==============================================================================
# HELPER — aggregate production bin distribution sweep
#
# resc_matrix : reaches × individuals  (rescaled likelihood, values in [0, 1])
# reach_km    : numeric vector, length = nrow(resc_matrix)
#
# Pipeline per threshold τ (mirrors the production map scripts):
#   1. Zero out values below τ  →  resc_z
#   2. Row-means across all individuals  →  agg_prod  (per-reach aggregate)
#   3. Normalise: rel_prod = agg_prod / max(agg_prod)  → spans [0, 1]
#   4. Retained reaches = rel_prod > 0
#   5. Bin retained reaches by rel_prod into 0.1-wide bins
#   6. prop = km_in_bin / total_retained_km  → bins sum to 100%
#
# Returns one row per (threshold × bin): threshold | bin_lo | bin_hi |
#   bin_label | prop | total_retained_km
# ==============================================================================

bin_distribution_sweep <- function(resc_matrix, reach_km) {

  out <- vector("list", length(THRESHOLDS) * N_BINS)
  idx <- 1L

  for (t in THRESHOLDS) {

    # Step 1: zero out below-threshold values
    resc_z       <- resc_matrix
    resc_z[resc_z < t] <- 0

    # Step 2: aggregate across individuals (equal weight per fish)
    agg_prod <- rowMeans(resc_z, na.rm = TRUE)

    # Step 3: re-normalise to [0, 1]
    mx <- max(agg_prod, na.rm = TRUE)
    if (is.na(mx) || mx == 0) {
      # No signal survives this threshold — record zeros and move on
      for (bi in seq_len(N_BINS)) {
        out[[idx]] <- data.frame(
          threshold = t, bin_lo = BIN_BREAKS[bi],
          bin_hi = BIN_BREAKS[bi + 1L], bin_label = BIN_LABELS[bi],
          prop = 0, total_retained_km = 0
        )
        idx <- idx + 1L
      }
      next
    }
    rel_prod <- agg_prod / mx   # max reach = 1.0

    # Step 4: retained reaches
    retained         <- rel_prod > 0
    total_retained_km <- sum(reach_km[retained], na.rm = TRUE)

    # Steps 5–6: bin and proportion
    for (bi in seq_len(N_BINS)) {
      lo <- BIN_BREAKS[bi]
      hi <- BIN_BREAKS[bi + 1L]

      # Last bin is closed at 1 (max normalised value hits exactly 1.0)
      if (bi == N_BINS) {
        in_bin <- retained & rel_prod >= lo & rel_prod <= hi
      } else {
        in_bin <- retained & rel_prod >= lo & rel_prod <  hi
      }

      km_bin <- sum(reach_km[in_bin], na.rm = TRUE)
      prop   <- if (total_retained_km > 0) km_bin / total_retained_km else 0

      out[[idx]] <- data.frame(
        threshold         = t,
        bin_lo            = lo,
        bin_hi            = hi,
        bin_label         = BIN_LABELS[bi],
        prop              = prop,
        total_retained_km = total_retained_km
      )
      idx <- idx + 1L
    }
  }

  bind_rows(out)
}


# ==============================================================================
# KUSKOKWIM — rescaled likelihood matrix for one year
#   (identical pipeline to SensitivityThresholdAnalysis.R)
# ==============================================================================

compute_kusko_resc <- function(year) {
  cat(sprintf("  [Kusko %d] loading natal data...\n", year))
  edges     <- KUSKO_EDGES

  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )
  natal <- natal_raw |> filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  if (nrow(natal) == 0) stop("No natal data for Kusko ", year)
  cat(sprintf("    %d fish\n", nrow(natal)))

  pid_isose_mod <- mean(edges$isose_pred, na.rm = TRUE)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  presence_prior     <- ifelse(edges$Str_Order %in% c(7, 8) &
                                 edges$SPAWNING_C == 0, 0, 1)
  fixed_prior        <- stream_order_prior * edges$UniPh2oNoE * presence_prior
  pid_iso            <- edges$iso_pred

  RESC <- matrix(0, nrow = nrow(edges), ncol = nrow(natal))
  for (i in seq_len(nrow(natal))) {
    lik <- (1 / sqrt(2 * pi * error^2)) *
             exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a   <- lik * fixed_prior
    s   <- sum(a, na.rm = TRUE);  if (s  == 0) next
    an  <- a / s
    mx  <- max(an, na.rm = TRUE); if (mx == 0) next
    RESC[, i] <- an / mx
  }

  reach_km       <- edges$length_m / 1000
  total_basin_km <- sum(reach_km[stream_order_prior == 1], na.rm = TRUE)
  cat(sprintf("    Total eligible network: %.0f km\n", total_basin_km))

  list(resc = RESC, reach_km = reach_km,
       total_basin_km = total_basin_km, n_fish = nrow(natal))
}


# ==============================================================================
# YUKON — rescaled likelihood matrix for one year
# ==============================================================================

compute_yukon_resc <- function(year) {
  gen_cols <- c("Lower", "Middle", "Upper")
  cat(sprintf("  [Yukon %d] loading natal data...\n", year))
  edges <- YUKON_EDGES

  LY <- which(tolower(edges$GenLMU) == "lower")
  MY <- which(tolower(edges$GenLMU) == "middle")
  UY <- which(tolower(edges$GenLMU) == "upper")

  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Yukon_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )
  avg_cols  <- paste0("avg_", gen_cols)
  dgen_year <- daily_gen_wide |> filter(year == !!year) |>
    select(DOY, all_of(avg_cols))
  natal_raw <- natal_raw |> left_join(dgen_year, by = "DOY")
  for (col in gen_cols) {
    ac <- paste0("avg_", col)
    natal_raw[[col]] <- ifelse(is.na(natal_raw[[col]]),
                               natal_raw[[ac]], natal_raw[[col]])
  }
  natal_raw <- natal_raw |> select(-all_of(avg_cols))
  natal <- natal_raw |>
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop),
           if_all(all_of(gen_cols), ~ !is.na(.x)))
  if (nrow(natal) == 0) stop("No natal data for Yukon ", year)
  cat(sprintf("    %d fish\n", nrow(natal)))

  pid_isose_mod <- mean(edges$isose_pred, na.rm = TRUE)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  pid_iso            <- edges$iso_pred

  RESC <- matrix(0, nrow = nrow(edges), ncol = nrow(natal))
  for (i in seq_len(nrow(natal))) {
    gen_prior     <- rep(0, nrow(edges))
    gen_prior[LY] <- as.numeric(natal$Lower[i])
    gen_prior[MY] <- as.numeric(natal$Middle[i])
    gen_prior[UY] <- as.numeric(natal$Upper[i])
    lik <- (1 / sqrt(2 * pi * error^2)) *
             exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a   <- lik * stream_order_prior * gen_prior
    s   <- sum(a, na.rm = TRUE);  if (s  == 0) next
    an  <- a / s
    mx  <- max(an, na.rm = TRUE); if (mx == 0) next
    RESC[, i] <- an / mx
  }

  reach_km       <- edges$Shape_Leng / 1000
  total_basin_km <- sum(reach_km[stream_order_prior == 1], na.rm = TRUE)
  cat(sprintf("    Total eligible network: %.0f km\n", total_basin_km))

  list(resc = RESC, reach_km = reach_km,
       total_basin_km = total_basin_km, n_fish = nrow(natal))
}


# ==============================================================================
# RUN — accumulate bin distributions across all years per basin
# ==============================================================================

run_basin_bins <- function(years, compute_fn, basin_label) {
  all_rows <- list()
  for (yr in years) {
    dat <- tryCatch(
      compute_fn(yr),
      error = function(e) {
        cat(sprintf("    SKIPPED (error): %s\n", e$message)); NULL
      }
    )
    if (is.null(dat)) next
    cat(sprintf("    Computing bin distributions...\n"))
    df <- bin_distribution_sweep(dat$resc, dat$reach_km) |>
      mutate(year = yr, basin = basin_label,
             total_basin_km = dat$total_basin_km)
    all_rows[[as.character(yr)]] <- df
  }
  bind_rows(all_rows)
}

cat("\n=== Kuskokwim ===\n")
kusko_bins <- run_basin_bins(KUSKO_YEARS, compute_kusko_resc, "Kuskokwim")

cat("\n=== Yukon ===\n")
yukon_bins <- run_basin_bins(YUKON_YEARS, compute_yukon_resc, "Yukon")

all_bins <- bind_rows(kusko_bins, yukon_bins) |>
  mutate(
    bin_label = factor(bin_label, levels = BIN_LABELS),
    threshold_label = sprintf("τ = %.1f", threshold)   # "τ = 0.2" etc.
  )


# ==============================================================================
# SUMMARISE
# ==============================================================================

# -- Pooled: mean proportion across years per (basin × τ × bin) ---------------
pooled_bins <- all_bins |>
  group_by(basin, threshold, threshold_label, bin_lo, bin_hi, bin_label) |>
  summarise(
    pool_mean = mean(prop, na.rm = TRUE),
    pool_sd   = sd(prop,   na.rm = TRUE),
    n_years   = n(),
    .groups   = "drop"
  )

# -- Per-year rows (one row per year × τ × bin, already computed) -------------
year_bins <- all_bins |>
  mutate(year = factor(year)) |>
  select(basin, year, threshold, threshold_label, bin_lo, bin_hi,
         bin_label, prop)


# ==============================================================================
# COLOUR PALETTE  (low production = light teal, high = dark purple)
# ==============================================================================

bin_pal <- colorRampPalette(
  c("#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8",
    "#253494", "#54278F", "#6A0136")
)(N_BINS)
names(bin_pal) <- BIN_LABELS


# ==============================================================================
# FIGURE 1 — Pooled stacked bar chart, faceted by basin
#
#   x    = threshold (τ)
#   y    = mean proportion of eligible basin in bin  (stacked → total retained)
#   fill = relative production bin
# ==============================================================================

p1 <- ggplot(
    pooled_bins |> filter(pool_mean > 0),
    aes(x = factor(threshold), y = pool_mean, fill = bin_label)   # pool_mean = mean across years
  ) +
  geom_col(position = "stack", width = 0.72,
           color = "white", linewidth = 0.25) +
  facet_wrap(~ basin, ncol = 2) +
  scale_fill_manual(
    values = bin_pal,
    name   = "Relative\nproduction\nbin",
    drop   = FALSE,
    guide  = guide_legend(reverse = TRUE)   # high bins at top of legend
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.04))
  ) +
  scale_x_discrete(labels = function(x) sprintf("τ = %s", x)) +
  labs(
    title    = "Relative Production Bin Distribution — Sensitivity to Threshold",
    subtitle = paste0(
      "Each bar sums to 100%. Proportion = km in bin ÷ km retained at that threshold per individual,\n",
      "averaged across all fish pooled across all years. Shows how the quality distribution of\n",
      "retained habitat shifts as the threshold rises."
    ),
    x = "Sensitivity threshold (τ)",
    y = "Proportion of retained basin habitat"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "right",
    legend.key.size    = unit(0.42, "cm"),
    legend.text        = element_text(size = 8.5),
    legend.title       = element_text(size = 9),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(size = 8.5, color = "grey30"),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey92", color = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 9)
  )

out1 <- file.path(OUT_DIR, "sensitivity_bin_distribution_pooled.png")
ggsave(out1, plot = p1, width = 11, height = 5.5, dpi = 300, bg = "white")
cat(sprintf("\nPooled bin figure saved -> %s\n", out1))


# ==============================================================================
# FIGURE 2 — Per-year grid: rows = year, columns = basin
# ==============================================================================

# Build a complete year × basin grid (years appear in both basins where available)
kusko_years_chr <- as.character(sort(KUSKO_YEARS))
yukon_years_chr <- as.character(sort(YUKON_YEARS))

# Order rows: all Kusko years first (descending), then Yukon-only years
all_years_ordered <- rev(sort(unique(as.integer(
  c(kusko_years_chr, yukon_years_chr)
))))
year_levels <- as.character(all_years_ordered)

p2 <- ggplot(
    year_bins |>
      filter(prop > 0) |>
      mutate(year = factor(year, levels = year_levels)),
    aes(x = factor(threshold), y = prop, fill = bin_label)
  ) +
  geom_col(position = "stack", width = 0.72,
           color = "white", linewidth = 0.20) +
  facet_grid(year ~ basin, scales = "free_y") +
  scale_fill_manual(
    values = bin_pal,
    name   = "Relative\nproduction\nbin",
    drop   = FALSE,
    guide  = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_discrete(labels = function(x) sprintf("%.1f", as.numeric(x))) +
  labs(
    title    = "Relative Production Bin Distribution — Per Year",
    subtitle = paste0(
      "Each bar sums to 100%: proportion of retained km falling in each production bin.\n",
      "Missing panels indicate years with no data for that basin."
    ),
    x = "Threshold (τ)",
    y = "Proportion of retained basin habitat"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position    = "right",
    legend.key.size    = unit(0.40, "cm"),
    legend.text        = element_text(size = 8),
    legend.title       = element_text(size = 8.5),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(size = 8, color = "grey30"),
    strip.text         = element_text(face = "bold", size = 9),
    strip.text.y       = element_text(angle = 0),
    strip.background   = element_rect(fill = "grey92", color = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 7.5)
  )

out2 <- file.path(OUT_DIR, "sensitivity_bin_distribution_peryear.png")
ggsave(out2, plot = p2, width = 10, height = 11, dpi = 300, bg = "white")
cat(sprintf("Per-year bin figure saved  -> %s\n", out2))


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n--- Pooled summary: mean % of retained basin per bin, by threshold ---\n")
cat("(each row of bins should sum to ~100%)\n")
pooled_bins |>
  mutate(pct = round(pool_mean * 100, 1)) |>
  select(basin, threshold, bin_label, pct) |>
  pivot_wider(names_from = bin_label, values_from = pct, values_fill = 0) |>
  arrange(basin, threshold) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\nDone.\n")
