################################################################################
# SENSITIVITY THRESHOLD ANALYSIS — Per-Individual Kept Habitat (River km)
# ALL YEARS · BOTH BASINS
#
# For every available year in each basin, sweeps across a range of sensitivity
# threshold values and computes how many river km are retained in the isotopic
# assignment for every individual fish.
#
# The metric:
#   For each individual i and threshold τ:
#     kept_km[i, τ] = Σ reach_length_km  for all reaches where the
#                     rescaled likelihood ≥ τ
#
#   Rescaled likelihood pipeline (identical to production maps):
#     1. Normal likelihood per reach from isotope data + priors
#     2. Normalize to a probability distribution
#     3. Rescale so the top reach = 1.0  (threshold applied AFTER this step)
#
# Output figure — one combined PNG:
#   Two facets (Kuskokwim | Yukon), each showing:
#     • Light ribbon  — 10th–90th pct across ALL individuals pooled (all years)
#     • Dark ribbon   — IQR (25th–75th pct) pooled across all years
#     • Colored lines — per-year mean kept km, one line per year
#     • Dashed line   — current default threshold (τ = 0.7)
################################################################################

suppressPackageStartupMessages({
  library(sf);       library(dplyr);    library(readr)
  library(readxl);   library(tibble);   library(tidyr)
  library(ggplot2);  library(here)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

# Sensitivity threshold sweep
THRESHOLDS <- seq(0.05, 0.99, by = 0.02)

# Current default (reference line)
DEFAULT_THRESHOLD <- 0.7

# Stream-order floor (matches 00_FullBasinIndividualsMaps.R override)
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

daily_gen_wide <- read_csv(PATHS$daily_genetics, show_col_types = FALSE) %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(names_from = genetic_assignment, values_from = proportion,
              values_fill = 0) %>%
  rename(year = sampleYear,
         avg_Lower = Lower, avg_Middle = Middle, avg_Upper = Upper)


# ==============================================================================
# HELPER — sweep thresholds for a pre-built rescaled likelihood matrix
# ==============================================================================
# Returns a data.frame: fish_id | threshold | kept_km

kept_km_sweep <- function(resc_matrix, reach_km) {
  n_fish    <- ncol(resc_matrix)
  n_thresh  <- length(THRESHOLDS)
  n_rows    <- n_fish * n_thresh

  fish_id_vec   <- integer(n_rows)
  threshold_vec <- numeric(n_rows)
  kept_km_vec   <- numeric(n_rows)

  row_idx <- 1L
  for (i in seq_len(n_fish)) {
    col <- resc_matrix[, i]
    for (t in THRESHOLDS) {
      kept_km_vec[row_idx]   <- sum(reach_km[col >= t], na.rm = TRUE)
      fish_id_vec[row_idx]   <- i
      threshold_vec[row_idx] <- t
      row_idx <- row_idx + 1L
    }
  }
  data.frame(fish_id = fish_id_vec, threshold = threshold_vec,
             kept_km = kept_km_vec)
}


# ==============================================================================
# KUSKOKWIM — rescaled likelihood matrix for one year
# ==============================================================================

compute_kusko_resc <- function(year) {
  cat(sprintf("  [Kusko %d] loading natal data...\n", year))
  edges <- KUSKO_EDGES

  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )
  natal <- natal_raw %>% filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
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
    lik  <- (1 / sqrt(2 * pi * error^2)) *
              exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a    <- lik * fixed_prior
    s    <- sum(a, na.rm = TRUE); if (s == 0) next
    an   <- a / s
    mx   <- max(an, na.rm = TRUE); if (mx == 0) next
    RESC[, i] <- an / mx
  }

  # Reach lengths in km; total eligible network (stream-order filter applied)
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
  dgen_year <- daily_gen_wide %>% filter(year == !!year) %>%
    select(DOY, all_of(avg_cols))
  natal_raw <- natal_raw %>% left_join(dgen_year, by = "DOY")
  for (col in gen_cols) {
    ac <- paste0("avg_", col)
    natal_raw[[col]] <- ifelse(is.na(natal_raw[[col]]),
                               natal_raw[[ac]], natal_raw[[col]])
  }
  natal_raw <- natal_raw %>% select(-all_of(avg_cols))
  natal <- natal_raw %>%
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
    lik  <- (1 / sqrt(2 * pi * error^2)) *
              exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a    <- lik * stream_order_prior * gen_prior
    s    <- sum(a, na.rm = TRUE); if (s == 0) next
    an   <- a / s
    mx   <- max(an, na.rm = TRUE); if (mx == 0) next
    RESC[, i] <- an / mx
  }

  # Reach lengths in km; total eligible network (stream-order filter applied)
  reach_km       <- edges$Shape_Leng / 1000
  total_basin_km <- sum(reach_km[stream_order_prior == 1], na.rm = TRUE)
  cat(sprintf("    Total eligible network: %.0f km\n", total_basin_km))

  list(resc = RESC, reach_km = reach_km,
       total_basin_km = total_basin_km, n_fish = nrow(natal))
}


# ==============================================================================
# RUN — loop over all years, accumulate per-individual records
# ==============================================================================

run_basin <- function(years, compute_fn, basin_label) {
  all_rows <- list()
  for (yr in years) {
    dat <- tryCatch(
      compute_fn(yr),
      error = function(e) {
        cat(sprintf("    SKIPPED (error): %s\n", e$message)); NULL
      }
    )
    if (is.null(dat)) next
    cat(sprintf("    sweeping thresholds...\n"))
    df <- kept_km_sweep(dat$resc, dat$reach_km) %>%
      mutate(year = yr, basin = basin_label,
             total_basin_km = dat$total_basin_km)
    all_rows[[as.character(yr)]] <- df
  }
  bind_rows(all_rows)
}

cat("\n=== Kuskokwim ===\n")
kusko_df <- run_basin(KUSKO_YEARS, compute_kusko_resc, "Kuskokwim")

cat("\n=== Yukon ===\n")
yukon_df <- run_basin(YUKON_YEARS, compute_yukon_resc, "Yukon")

combined <- bind_rows(kusko_df, yukon_df) %>%
  mutate(prop_kept = kept_km / total_basin_km)   # proportion of eligible network


# ==============================================================================
# SUMMARISE — absolute km
# ==============================================================================

# Per-year mean line (km)
year_summary <- combined %>%
  group_by(basin, year, threshold) %>%
  summarise(mean_km = mean(kept_km, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = factor(year))

# Pooled ribbon + mean across ALL individuals from ALL years (km)
pooled_ribbon <- combined %>%
  group_by(basin, threshold) %>%
  summarise(
    mean_km = mean(kept_km, na.rm = TRUE),
    p10 = quantile(kept_km, 0.10, na.rm = TRUE),
    p25 = quantile(kept_km, 0.25, na.rm = TRUE),
    p75 = quantile(kept_km, 0.75, na.rm = TRUE),
    p90 = quantile(kept_km, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# SUMMARISE — proportion of eligible basin network
# ==============================================================================

# Per-year mean line (proportion)
year_summary_prop <- combined %>%
  group_by(basin, year, threshold) %>%
  summarise(mean_prop = mean(prop_kept, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = factor(year))

# Pooled ribbon + mean (proportion)
pooled_ribbon_prop <- combined %>%
  group_by(basin, threshold) %>%
  summarise(
    mean_prop = mean(prop_kept, na.rm = TRUE),
    p10 = quantile(prop_kept, 0.10, na.rm = TRUE),
    p25 = quantile(prop_kept, 0.25, na.rm = TRUE),
    p75 = quantile(prop_kept, 0.75, na.rm = TRUE),
    p90 = quantile(prop_kept, 0.90, na.rm = TRUE),
    .groups = "drop"
  )


# ==============================================================================
# FIGURE
# ==============================================================================

# Color palettes
kusko_years_chr <- as.character(sort(KUSKO_YEARS))
yukon_years_chr <- as.character(sort(YUKON_YEARS))

# Kusko: blues; Yukon: reds/oranges
kusko_pal <- colorRampPalette(c("#C6DBEF", "#084594"))(length(kusko_years_chr))
yukon_pal <- colorRampPalette(c("#FCBBA1", "#99000D"))(length(yukon_years_chr))

year_colors <- setNames(
  c(kusko_pal, yukon_pal),
  c(kusko_years_chr, yukon_years_chr)
)

# Ribbon fill by basin (basin label used as the fill aes in pooled layer)
ribbon_fill  <- c(Kuskokwim = "#6BAED6", Yukon = "#F4A582")
ribbon_color <- c(Kuskokwim = "#2166AC", Yukon = "#D6604D")

p <- ggplot() +

  # ── Pooled 10–90th pct ribbon (lightest) ──────────────────────────────────
  geom_ribbon(
    data = pooled_ribbon,
    aes(x = threshold, ymin = p10, ymax = p90, fill = basin),
    alpha = 0.18, color = NA
  ) +

  # ── Pooled IQR ribbon (darker) ────────────────────────────────────────────
  geom_ribbon(
    data = pooled_ribbon,
    aes(x = threshold, ymin = p25, ymax = p75, fill = basin),
    alpha = 0.30, color = NA
  ) +

  # ── Per-year mean lines ───────────────────────────────────────────────────
  geom_line(
    data = year_summary,
    aes(x = threshold, y = mean_km, color = year, group = year),
    linewidth = 0.75, alpha = 0.95
  ) +

  # ── Default threshold reference ───────────────────────────────────────────
  geom_vline(xintercept = DEFAULT_THRESHOLD,
             linetype = "dashed", linewidth = 0.55, color = "grey25") +
  annotate("text",
           x = DEFAULT_THRESHOLD + 0.013, y = Inf,
           label = sprintf("τ = %.1f", DEFAULT_THRESHOLD),
           vjust = 1.5, hjust = 0, size = 2.8, color = "grey25") +

  # ── Facet by basin ────────────────────────────────────────────────────────
  facet_wrap(~ basin, ncol = 2, scales = "free_y") +

  # ── Scales ────────────────────────────────────────────────────────────────
  scale_fill_manual(values = ribbon_fill,  guide = "none") +
  scale_color_manual(
    values = year_colors,
    name   = "Year",
    breaks = c(kusko_years_chr, yukon_years_chr)
  ) +
  scale_x_continuous(breaks = seq(0.0, 1.0, by = 0.1), expand = c(0.01, 0)) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.06))) +

  # ── Labels ────────────────────────────────────────────────────────────────
  labs(
    title    = "Sensitivity Threshold vs. Kept Habitat per Individual — All Years",
    subtitle = paste0(
      "Colored lines = per-year mean across all sampled individuals\n",
      "Ribbons = IQR (dark) and 10–90th pct (light) pooled across all years"
    ),
    x = "Sensitivity threshold (τ)",
    y = "Kept habitat (river km per individual)"
  ) +

  # ── Theme ─────────────────────────────────────────────────────────────────
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "right",
    legend.key.size    = unit(0.40, "cm"),
    legend.text        = element_text(size = 8.5),
    plot.title         = element_text(face = "bold"),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey92", color = NA),
    panel.grid.minor   = element_blank()
  )

out_file <- file.path(OUT_DIR, "sensitivity_threshold_kept_km_all_years.png")
ggsave(out_file, plot = p, width = 11, height = 5.5, dpi = 300, bg = "white")
cat(sprintf("\nFigure saved -> %s\n", out_file))


# ==============================================================================
# Console summary at the default threshold
# ==============================================================================

# ==============================================================================
# FIGURE 2 — Single panel: pooled mean + ribbon for each basin overlaid
# ==============================================================================
# Basin colors: Kusko = blue, Yukon = red
basin_line_col <- c(Kuskokwim = "#2166AC", Yukon     = "#C0392B")
basin_fill_col <- c(Kuskokwim = "#6BAED6", Yukon     = "#F4A582")

p2 <- ggplot(pooled_ribbon,
             aes(x = threshold, color = basin, fill = basin)) +

  # 10–90th pct ribbon
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.18, color = NA) +

  # IQR ribbon
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.32, color = NA) +

  # Pooled mean line (bold)
  geom_line(aes(y = mean_km), linewidth = 1.1) +

  # Default threshold reference
  geom_vline(xintercept = DEFAULT_THRESHOLD,
             linetype = "dashed", linewidth = 0.55, color = "grey25") +
  annotate("text",
           x = DEFAULT_THRESHOLD + 0.013, y = Inf,
           label = sprintf("τ = %.1f", DEFAULT_THRESHOLD),
           vjust = 1.5, hjust = 0, size = 3.0, color = "grey25") +

  scale_color_manual(values = basin_line_col, name = "Basin") +
  scale_fill_manual( values = basin_fill_col, name = "Basin") +
  scale_x_continuous(breaks = seq(0.0, 1.0, by = 0.1), expand = c(0.01, 0)) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.06))) +

  labs(
    title    = "Sensitivity Threshold vs. Kept Habitat per Individual — Both Basins",
    subtitle = paste0(
      "Bold line = mean across all individuals pooled across all years\n",
      "Ribbons = IQR (dark) and 10–90th pct (light) pooled across all years"
    ),
    x = "Sensitivity threshold (τ)",
    y = "Kept habitat (river km per individual)"
  ) +

  theme_bw(base_size = 11) +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.88, 0.85),
    legend.background      = element_rect(fill = "white", color = "grey80"),
    legend.key.size        = unit(0.45, "cm"),
    plot.title             = element_text(face = "bold"),
    panel.grid.minor       = element_blank()
  )

out_file2 <- file.path(OUT_DIR, "sensitivity_threshold_kept_km_comparison.png")
ggsave(out_file2, plot = p2, width = 8, height = 5.5, dpi = 300, bg = "white")
cat(sprintf("\nComparison figure saved -> %s\n", out_file2))


# ==============================================================================
# Console summary at the default threshold
# ==============================================================================

# ==============================================================================
# FIGURE 3 — Proportion: faceted per-year lines + pooled ribbon
# ==============================================================================

p3 <- ggplot() +

  geom_ribbon(
    data = pooled_ribbon_prop,
    aes(x = threshold, ymin = p10, ymax = p90, fill = basin),
    alpha = 0.18, color = NA
  ) +
  geom_ribbon(
    data = pooled_ribbon_prop,
    aes(x = threshold, ymin = p25, ymax = p75, fill = basin),
    alpha = 0.30, color = NA
  ) +
  geom_line(
    data = year_summary_prop,
    aes(x = threshold, y = mean_prop, color = year, group = year),
    linewidth = 0.75, alpha = 0.95
  ) +
  geom_vline(xintercept = DEFAULT_THRESHOLD,
             linetype = "dashed", linewidth = 0.55, color = "grey25") +
  annotate("text",
           x = DEFAULT_THRESHOLD + 0.013, y = Inf,
           label = sprintf("τ = %.1f", DEFAULT_THRESHOLD),
           vjust = 1.5, hjust = 0, size = 2.8, color = "grey25") +
  facet_wrap(~ basin, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = ribbon_fill,  guide = "none") +
  scale_color_manual(
    values = year_colors,
    name   = "Year",
    breaks = c(kusko_years_chr, yukon_years_chr)
  ) +
  scale_x_continuous(breaks = seq(0.0, 1.0, by = 0.1), expand = c(0.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.06))) +
  labs(
    title    = "Sensitivity Threshold vs. Proportion of Eligible Basin Kept — All Years",
    subtitle = paste0(
      "Colored lines = per-year mean across all individuals\n",
      "Ribbons = IQR (dark) and 10–90th pct (light) pooled across all years\n",
      "Denominator = total km of reaches passing stream-order filter"
    ),
    x = "Sensitivity threshold (τ)",
    y = "Proportion of eligible basin kept per individual"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "right",
    legend.key.size    = unit(0.40, "cm"),
    legend.text        = element_text(size = 8.5),
    plot.title         = element_text(face = "bold"),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey92", color = NA),
    panel.grid.minor   = element_blank()
  )

out_file3 <- file.path(OUT_DIR, "sensitivity_threshold_prop_kept_all_years.png")
ggsave(out_file3, plot = p3, width = 11, height = 5.5, dpi = 300, bg = "white")
cat(sprintf("\nProportion faceted figure saved -> %s\n", out_file3))


# ==============================================================================
# FIGURE 4 — Proportion: single panel, both basins overlaid
# ==============================================================================

p4 <- ggplot(pooled_ribbon_prop,
             aes(x = threshold, color = basin, fill = basin)) +

  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.18, color = NA) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.32, color = NA) +
  geom_line(aes(y = mean_prop), linewidth = 1.1) +
  geom_vline(xintercept = DEFAULT_THRESHOLD,
             linetype = "dashed", linewidth = 0.55, color = "grey25") +
  annotate("text",
           x = DEFAULT_THRESHOLD + 0.013, y = Inf,
           label = sprintf("τ = %.1f", DEFAULT_THRESHOLD),
           vjust = 1.5, hjust = 0, size = 3.0, color = "grey25") +
  scale_color_manual(values = basin_line_col, name = "Basin") +
  scale_fill_manual( values = basin_fill_col, name = "Basin") +
  scale_x_continuous(breaks = seq(0.0, 1.0, by = 0.1), expand = c(0.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.06))) +
  labs(
    title    = "Sensitivity Threshold vs. Proportion of Eligible Basin Kept — Both Basins",
    subtitle = paste0(
      "Bold line = mean across all individuals pooled across all years\n",
      "Ribbons = IQR (dark) and 10–90th pct (light) pooled across all years\n",
      "Denominator = total km of reaches passing stream-order filter"
    ),
    x = "Sensitivity threshold (τ)",
    y = "Proportion of eligible basin kept per individual"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.88, 0.85),
    legend.background      = element_rect(fill = "white", color = "grey80"),
    legend.key.size        = unit(0.45, "cm"),
    plot.title             = element_text(face = "bold"),
    panel.grid.minor       = element_blank()
  )

out_file4 <- file.path(OUT_DIR, "sensitivity_threshold_prop_kept_comparison.png")
ggsave(out_file4, plot = p4, width = 8, height = 5.5, dpi = 300, bg = "white")
cat(sprintf("\nProportion comparison figure saved -> %s\n", out_file4))


# ==============================================================================
# Console summary at the default threshold
# ==============================================================================

cat(sprintf("\n--- Per-year summary at τ = %.1f ---\n", DEFAULT_THRESHOLD))
year_summary %>%
  filter(abs(threshold - DEFAULT_THRESHOLD) < 0.011) %>%
  arrange(basin, year) %>%
  mutate(mean_km = round(mean_km, 1)) %>%
  as.data.frame() %>%
  print()

cat(sprintf("\n--- Pooled basin summary at τ = %.1f (km) ---\n", DEFAULT_THRESHOLD))
pooled_ribbon %>%
  filter(abs(threshold - DEFAULT_THRESHOLD) < 0.011) %>%
  select(basin, mean_km, p25, p75, p10, p90) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  as.data.frame() %>%
  print()

cat(sprintf("\n--- Pooled basin summary at τ = %.1f (proportion of eligible network) ---\n",
            DEFAULT_THRESHOLD))
pooled_ribbon_prop %>%
  filter(abs(threshold - DEFAULT_THRESHOLD) < 0.011) %>%
  select(basin, mean_prop, p25, p75, p10, p90) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  as.data.frame() %>%
  print()

cat("\nDone.\n")
