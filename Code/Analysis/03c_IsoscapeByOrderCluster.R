################################################################################
# 03c_IsoscapeByOrderCluster.R
#
# SENSITIVITY ANALYSIS — isoscape (iso_pred) distribution by stream-order
# cluster, computed WITHIN genetic regions, for the Yukon and Kuskokwim.
#
# QUESTION
# --------
# The portfolio / CV analysis (03e_NestedCatchmentCV.R) bins the river network by
# Strahler order and asks how interannual production CV changes with spatial
# scale. This script is the analogous SENSITIVITY check on the underlying
# ISOSCAPE: as we raise the stream-order threshold that defines a "cluster"
# (5th order and higher, 6th and higher, ...), how does the distribution of the
# predicted isotope value (iso_pred) shift, and is that shift consistent across
# genetic reporting regions?
#
# CLUSTER (UNIT OF ANALYSIS)
# --------------------------
# A "cluster" at threshold K is the set of ALL reaches whose Strahler order is
# >= K  (cumulative, not a single order). Thresholds analysed: K = 5,6,7,8,9.
# This mirrors the "5th and higher, 6th and higher, ..." framing used for the
# CV sensitivity analysis. Reaches are NOT production-weighted: each reach
# contributes its raw iso_pred once, so the result describes the isoscape of the
# habitat available in each cluster, independent of any year's run.
#
# GENETIC REGION
# --------------
# Yukon edges carry a genetic reporting group in the `GenLMU` field
# (lower / middle / upper). The Yukon analysis is faceted by these three
# regions. Kuskokwim edges have no genetic subdivision (single reporting group),
# so the Kuskokwim is analysed separately as one whole-basin region.
#
# Isoscape metric : iso_pred (predicted tributary isotope value per reach)
# Source layers   : the same geomorph-augmented edge shapefiles used by the
#                   production maps (01_FullBasinRelativeProdMaps.R).
#
# Outputs:
#   Outputs/IsoscapeSensitivity/<Basin>_iso_by_ordercluster.csv  (region x K summary)
#   Outputs/IsoscapeSensitivity/<Basin>_iso_cluster_reaches.csv  (reach-level long)
#   Figures/03_IsoscapeSensitivity/iso_by_ordercluster_<Basin>.(png|pdf)
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

out_dir <- here("Outputs", "IsoscapeSensitivity")
fig_dir <- here("Figures", "03_IsoscapeSensitivity")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- analysis parameters ----------------------------------------------------
# Cumulative stream-order thresholds defining each cluster (">= K").
CLUSTER_THRESHOLDS <- 5:9

BASINS <- list(
  Yukon = list(
    edges     = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Yukon_edges_geomorphAdded.shp"),
    gen_field = "GenLMU",                       # lower / middle / upper
    region_levels = c("lower", "middle", "upper")
  ),
  Kusko = list(
    edges     = here("Data", "Spatial Data", "AnalysisShapefiles",
                     "Kusko_edges_geomorphAdded.shp"),
    gen_field = NA_character_,                  # no genetic subdivision
    region_levels = "Kuskokwim"
  )
)

# ---- helpers ----------------------------------------------------------------

## Reach attribute table (geometry dropped) with the three columns we need:
## genetic region, Strahler order, predicted isotope value.
load_reaches <- function(cfg) {
  e  <- sf::st_read(cfg$edges, quiet = TRUE)
  df <- sf::st_drop_geometry(e)

  region <- if (is.na(cfg$gen_field)) {
    rep(cfg$region_levels[1], nrow(df))
  } else {
    tolower(as.character(df[[cfg$gen_field]]))
  }

  tibble(
    region       = region,
    stream_order = as.integer(round(df$Str_Order)),
    iso_pred     = as.numeric(df$iso_pred)
  ) %>%
    dplyr::filter(!is.na(stream_order), !is.na(iso_pred))
}

## Expand reaches into one row per (cluster threshold) they belong to:
## a reach of order o is a member of every cluster K with K <= o (and K in
## CLUSTER_THRESHOLDS). Returns long reach-level data for boxplots + summaries.
expand_clusters <- function(reaches, thresholds) {
  bind_rows(lapply(thresholds, function(K) {
    reaches %>%
      dplyr::filter(stream_order >= K) %>%
      mutate(cluster_min_order = K,
             cluster = factor(paste0("≥ ", K),
                              levels = paste0("≥ ", thresholds)))
  }))
}

## sd-based summary of iso_pred per region x cluster.
summarise_clusters <- function(long, basin) {
  long %>%
    group_by(region, cluster_min_order, cluster) %>%
    summarise(
      n_reaches   = dplyr::n(),
      mean_iso    = mean(iso_pred),
      median_iso  = median(iso_pred),
      sd_iso      = sd(iso_pred),
      cv_iso      = sd(iso_pred) / mean(iso_pred),
      iqr_iso     = IQR(iso_pred),
      q05_iso     = quantile(iso_pred, 0.05),
      q95_iso     = quantile(iso_pred, 0.95),
      min_iso     = min(iso_pred),
      max_iso     = max(iso_pred),
      .groups     = "drop"
    ) %>%
    mutate(basin = basin, .before = 1) %>%
    arrange(region, cluster_min_order)
}

# ---- core per-basin routine -------------------------------------------------

run_basin <- function(name, cfg) {
  reaches <- load_reaches(cfg)
  reaches$region <- factor(reaches$region, levels = cfg$region_levels)

  long <- expand_clusters(reaches, CLUSTER_THRESHOLDS)
  long$region <- factor(long$region, levels = cfg$region_levels)

  summ <- summarise_clusters(long, name)

  write_csv(summ, file.path(out_dir, paste0(name, "_iso_by_ordercluster.csv")))
  write_csv(long %>% dplyr::select(region, cluster_min_order, stream_order, iso_pred),
            file.path(out_dir, paste0(name, "_iso_cluster_reaches.csv")))

  message(sprintf("[%s] %d regions | clusters %s | reaches/region(>=%d): %s",
                  name, length(cfg$region_levels),
                  paste0(">=", range(CLUSTER_THRESHOLDS), collapse = ".."),
                  min(CLUSTER_THRESHOLDS),
                  paste(sprintf("%s=%d", cfg$region_levels,
                                tapply(reaches$iso_pred,
                                       droplevels(reaches$region), length)[cfg$region_levels]),
                        collapse = ", ")))

  list(long = long, summ = summ, cfg = cfg)
}

# ---- figure -----------------------------------------------------------------
# Distribution of iso_pred per cluster (boxplot + jittered reaches), faceted by
# genetic region. Mirrors the look of the nested-CV boxplot from 03e_NestedCatchmentCV.R.

region_cols <- c(lower = "#4D7298", middle = "#9AB87A", upper = "#C44536",
                 Kuskokwim = "#C44536")

make_figure <- function(res, name) {
  long <- res$long
  cfg  <- res$cfg

  facet_present <- length(cfg$region_levels) > 1
  cols <- region_cols[cfg$region_levels]
  cols[is.na(cols)] <- "#777777"

  p <- ggplot(long, aes(x = cluster, y = iso_pred)) +
    geom_jitter(aes(colour = region), width = 0.18, height = 0,
                size = 0.5, alpha = 0.10, show.legend = FALSE) +
    geom_boxplot(aes(colour = region, fill = region),
                 width = 0.55, alpha = 0.25, linewidth = 0.6,
                 outlier.shape = NA, show.legend = FALSE) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_fill_manual(values = cols, guide = "none") +
    labs(
      title    = sprintf("Isoscape Distribution by Stream-Order Cluster — %s", name),
      subtitle = "Each box = reach-level iso_pred for all reaches at or above the stream-order threshold (unweighted)",
      x = "Stream-order cluster (reaches of this Strahler order and higher)",
      y = expression(Predicted~isotope~value~(iso_pred))
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line   = element_line(colour = "grey70"),
      axis.ticks  = element_line(colour = "grey70"),
      panel.spacing = unit(2, "lines"),
      axis.title  = element_text(size = 16),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text   = element_text(size = 13, colour = "grey20"),
      strip.text  = element_text(face = "bold", size = 16),
      plot.title  = element_text(face = "bold", size = 20, hjust = 0.5,
                                 margin = margin(b = 4)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "grey30",
                                   margin = margin(b = 12)),
      plot.title.position = "plot"
    )

  if (facet_present) {
    p <- p + facet_wrap(~ region, nrow = 1,
                        labeller = as_labeller(function(x) paste(str_to_title(x), "Yukon")))
  }

  w <- if (facet_present) 13 else 7
  ggsave(file.path(fig_dir, paste0("iso_by_ordercluster_", name, ".png")), p,
         width = w, height = 6.5, dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, paste0("iso_by_ordercluster_", name, ".pdf")), p,
         width = w, height = 6.5, bg = "white")
  message("Figure written to ",
          file.path(fig_dir, paste0("iso_by_ordercluster_", name, ".png")))
}

# ---- run --------------------------------------------------------------------

res <- lapply(names(BASINS), function(nm) run_basin(nm, BASINS[[nm]]))
names(res) <- names(BASINS)

invisible(lapply(names(res), function(nm) make_figure(res[[nm]], nm)))

summ_all <- bind_rows(lapply(res, `[[`, "summ"))
write_csv(summ_all, file.path(out_dir, "iso_by_ordercluster_summary.csv"))
print(summ_all, n = nrow(summ_all))
