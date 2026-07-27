################################################################################
# Yukon and Kuskokwim threshold sensitivity
#
# For each sampled fish, calculate the percentage of the stream-order-eligible
# basin network retained after applying a cutoff to its peak-rescaled assignment
# likelihood. Both basins are evaluated from 0.0 to 0.9.
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(sf)
  library(tidyr)
})

source(here("Code", "Analysis", "params.R"))

THRESHOLDS <- seq(0, 0.9, by = 0.1)
OUT_DIR <- here("Outputs", "SensitivityAnalysis")
FIG_DIR <- here("Figures", "SensitivityAnalysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

PATHS <- list(
  kusko_edges = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Kusko_edges_geomorphAdded.shp"),
  yukon_edges = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Yukon_edges_geomorphAdded.shp"),
  natal_dir = here("Data", "Natal Origins")
)

positive_at_threshold <- function(x, threshold) {
  # At zero, "no threshold" means positive assignment support. Using x >= 0
  # would incorrectly retain reaches whose prior or likelihood is exactly zero.
  if (threshold == 0) x > 0 else x > threshold
}

reach_length_km <- function(edges) {
  if ("length_m" %in% names(edges)) return(as.numeric(edges$length_m) / 1000)
  if ("Shape_Leng" %in% names(edges)) return(as.numeric(edges$Shape_Leng) / 1000)
  as.numeric(sf::st_length(edges)) / 1000
}

summarize_fish <- function(rescaled, reach_km, denominator_km, year, basin,
                           thresholds) {
  fish_ids <- sprintf("%s_%d_%04d", basin, year, seq_len(ncol(rescaled)))
  bind_rows(lapply(thresholds, function(threshold) {
    kept <- apply(rescaled, 2, function(x) {
      sum(reach_km[positive_at_threshold(x, threshold)], na.rm = TRUE)
    })
    tibble(
      basin = basin,
      year = year,
      fish_id = fish_ids,
      threshold = threshold,
      retained_km = kept,
      basin_km = denominator_km,
      retained_pct = 100 * kept / denominator_km
    )
  }))
}

compute_kusko <- function(year, edges) {
  natal <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))

  pid_iso <- edges$iso_pred
  pid_isose_mod <- pmax(edges$isose_pred, KUSKO_PARAMS$min_error)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)
  stream_prior <- as.numeric(!is.na(edges$Str_Order) &
                               edges$Str_Order >= KUSKO_PARAMS$min_stream_order)
  presence_prior <- ifelse(edges$Str_Order %in% c(7, 8) &
                             edges$SPAWNING_C == 0, 0, 1)
  fixed_prior <- stream_prior * edges$UniPh2oNoE * presence_prior

  resc <- vapply(natal$natal_iso, function(natal_iso) {
    likelihood <- dnorm(natal_iso, mean = pid_iso, sd = error)
    assignment <- likelihood * fixed_prior
    assignment_norm <- assignment / sum(assignment, na.rm = TRUE)
    assignment_norm / max(assignment_norm, na.rm = TRUE)
  }, numeric(nrow(edges)))

  lengths <- reach_length_km(edges)
  denominator <- sum(lengths[stream_prior == 1], na.rm = TRUE)
  summarize_fish(resc, lengths, denominator, year, "Kuskokwim", THRESHOLDS)
}

compute_yukon <- function(year, edges) {
  genetic_cols <- c("Lower", "Middle", "Upper")
  natal <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Yukon_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )
  missing_genetics <- !complete.cases(natal[, genetic_cols])
  natal[missing_genetics, genetic_cols] <- 1 / length(genetic_cols)
  natal <- natal %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop),
           if_all(all_of(genetic_cols), ~ !is.na(.x)))

  lower <- which(tolower(edges$GenLMU) == "lower")
  middle <- which(tolower(edges$GenLMU) == "middle")
  upper <- which(tolower(edges$GenLMU) == "upper")
  pid_iso <- edges$iso_pred
  mean_isoscape_error <- mean(edges$isose_pred, na.rm = TRUE)
  error <- sqrt(mean_isoscape_error^2 +
                  (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)
  stream_prior <- as.numeric(!is.na(edges$Str_Order) &
                               edges$Str_Order >= YUKON_PARAMS$min_stream_order)
  presence_prior <- ifelse(edges$Str_Order %in% c(8, 9) &
                             edges$SPAWNING_C == 0, 0, 1)
  presence_prior[upper] <- 1
  habitat_prior <- as.numeric(!is.na(edges$slope) &
                                edges$slope <= YUKON_PARAMS$channel_slope_cutoff)
  base_prior <- stream_prior * habitat_prior * presence_prior

  resc <- vapply(seq_len(nrow(natal)), function(i) {
    genetic_prior <- numeric(nrow(edges))
    genetic_prior[lower] <- natal$Lower[i]
    genetic_prior[middle] <- natal$Middle[i]
    genetic_prior[upper] <- natal$Upper[i]
    assignment <- dnorm(natal$natal_iso[i], mean = pid_iso, sd = error) *
      base_prior * genetic_prior
    assignment_norm <- assignment / sum(assignment, na.rm = TRUE)
    assignment_norm / max(assignment_norm, na.rm = TRUE)
  }, numeric(nrow(edges)))

  lengths <- reach_length_km(edges)
  denominator <- sum(lengths[stream_prior == 1], na.rm = TRUE)
  summarize_fish(resc, lengths, denominator, year, "Yukon", THRESHOLDS)
}

message("Reading basin networks...")
kusko_edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
yukon_edges <- st_read(PATHS$yukon_edges, quiet = TRUE)

message("Calculating Kuskokwim threshold sweep...")
kusko_fish <- bind_rows(lapply(KUSKO_YEARS, compute_kusko, edges = kusko_edges))
message("Calculating Yukon threshold sweep...")
yukon_fish <- bind_rows(lapply(YUKON_YEARS, compute_yukon, edges = yukon_edges))

fish_results <- bind_rows(kusko_fish, yukon_fish)
write_csv(fish_results,
          file.path(OUT_DIR, "individual_threshold_retention_both_basins.csv"))

summary_output <- fish_results %>%
  group_by(basin, threshold) %>%
  summarise(
    n_fish = n(),
    mean_pct = mean(retained_pct),
    median_pct = median(retained_pct),
    p10_pct = quantile(retained_pct, 0.10),
    p25_pct = quantile(retained_pct, 0.25),
    p75_pct = quantile(retained_pct, 0.75),
    p90_pct = quantile(retained_pct, 0.90),
    .groups = "drop"
  )
write_csv(summary_output,
          file.path(OUT_DIR, "threshold_retention_summary_both_basins.csv"))

plot_data <- bind_rows(
  yukon_fish %>% transmute(series = "Yukon", threshold, fish_id, retained_pct),
  kusko_fish %>% transmute(series = "Kuskokwim", threshold, fish_id, retained_pct)
)

p <- ggplot(plot_data, aes(threshold, retained_pct, color = series,
                           fill = series)) +
  stat_summary(geom = "ribbon", fun.min = function(x) quantile(x, 0.25),
               fun.max = function(x) quantile(x, 0.75),
               alpha = 0.18, color = NA) +
  stat_summary(geom = "line", fun = median, linewidth = 1.1) +
  scale_color_manual(values = c("Yukon" = "#C0392B",
                                "Kuskokwim" = "#2166AC")) +
  scale_fill_manual(values = c("Yukon" = "#F4A582",
                               "Kuskokwim" = "#6BAED6")) +
  scale_x_continuous(breaks = THRESHOLDS) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Habitat retained per individual assignment",
    subtitle = paste0(
      "Lines show medians; ribbons show interquartile ranges across fish and years\n",
      "The same threshold is applied to both basins; denominator is stream-order-eligible river km"
    ),
    x = "Assignment threshold",
    y = "Percent of total eligible basin retained",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(FIG_DIR, "threshold_retention_both_basins.png"),
       p, width = 8.5, height = 5.5, dpi = 300, bg = "white")

print(summary_output, n = Inf)
message("Done.")
