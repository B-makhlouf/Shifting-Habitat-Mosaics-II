################################################################################
# KUSKOKWIM & YUKON -- VARIANCE DAMPENING ANALYSIS
#
# QUESTION:
#   Does inter-annual variability in salmon production decrease as we look at
#   progressively larger drainage areas? If tributaries fluctuate independently,
#   aggregating production across more reaches should dampen variance (portfolio
#   effect). If they are synchronized by shared environmental drivers, variability
#   will persist even at large scales.
#
# APPROACH:
#   For each ReachBase reach (the mouth of a tributary system at a given stream
#   order), we sum cumulative production across all upstream reaches and compute
#   year-to-year % change across consecutive year pairs. We then compare the
#   spread of that variability at each stream order against two null simulations
#   run at empirically-derived CV levels:
#
#     cv_short  = empirical CV from the analysis years
#     cv_long   = empirical CV from 2010-present (all available years)
#
#   Proportional production (assignment_rescale) is used deliberately to isolate
#   spatial redistribution from basin-wide run size fluctuation.
#
# KUSKOKWIM:
#   Production years: 2017-2022 (5 consecutive pairs)
#   Uses TWO shapefiles bridged by spatial join (st_equals):
#     kusk_edges     = Kusko_upstream.shp  (network topology: rid, reachid)
#     kusko_analysis = Kusko_edges.shp     (Reachbase, Str_Order, reachids)
#
# YUKON (full basin):
#   Production years: 2015, 2016, 2018, 2021 (3 adjacent pairs)
#   Uses ONE shapefile: Yukon_edges.shp (reachid, up_rid, Reachbase, Str_Order)
#   Upstream traversal uses up_rid directly -- no bridge table needed.
#   (Based on FindUpstreamReachID_Yuk() in FINAL_UpstreamReachFunctions.R)
#   Production files: Outputs/ProductionData/Yukon_full/{yr}_Yukon_Full_Assignment_Results.csv
#
# OUTPUT:
#   Figures/Variance/Kusko_VarianceDampening.png  (Kusko-only, unchanged)
#   Figures/Variance/Yukon_VarianceDampening.png  (Yukon-only)
#   Figures/Variance/BothBasins_Facet.png         (side-by-side)
#   Figures/Variance/BothBasins_Overlay.png       (overlaid)
################################################################################


# ==============================================================================
# SECTION 1: LIBRARIES
# ==============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library(readxl)


# ==============================================================================
# SECTION 2: KUSKOKWIM -- LOAD SPATIAL DATA
# ==============================================================================

cat("Loading Kuskokwim spatial data...\n")

kusk_edges <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_upstream.shp"),
  quiet = TRUE
)

kusko_analysis <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  quiet = TRUE
)

KuskoNodes <- read.csv(
  here("Data", "UpstreamReaches", "kusko_noderelationships.csv"),
  stringsAsFactors = FALSE
)

KuskoNetwork <- KuskoNodes %>%
  rename(child_s = fromnode, parent_s = tonode)

kusko_analysis <- kusko_analysis %>%
  mutate(reach_length_m = as.numeric(st_length(geometry)))

total_basin_length_m <- sum(kusko_analysis$reach_length_m, na.rm = TRUE)

cat("  Network shapefile:", nrow(kusk_edges), "reaches\n")
cat("  Analysis shapefile:", nrow(kusko_analysis), "reaches\n")
cat("  Total basin length:", round(total_basin_length_m / 1000, 1), "km\n")


# ==============================================================================
# SECTION 3: LOAD ESCAPEMENT (shared) AND COMPUTE KUSKO CV VALUES
#
# cv_short = CV from the 2017-2022 analysis years
# cv_long  = CV from 2010 to the most recent available Kusko year
# ==============================================================================

cat("\nLoading escapement data and computing Kusko CV values...\n")

years      <- c(2017, 2018, 2019, 2020, 2021, 2022)
escapement <- read_excel(here("Data", "AYKEscapement.xlsx"))

kusko_all_esc <- escapement %>%
  filter(River == "Kusko") %>%
  arrange(Year)

# Short-term CV: 2017-2022 only
esc_short <- kusko_all_esc %>% filter(Year %in% years) %>% pull(Total_Run)
cv_short  <- sd(esc_short) / mean(esc_short)

# Long-term CV: 2010 to most recent available year
esc_long <- kusko_all_esc %>% filter(Year >= 2010) %>% pull(Total_Run)
cv_long  <- sd(esc_long) / mean(esc_long)

long_term_years <- kusko_all_esc %>% filter(Year >= 2010) %>% pull(Year)

cat("  Short-term CV (2017-2022):", round(cv_short, 3), "\n")
cat("  Long-term CV (", min(long_term_years), "-", max(long_term_years), "):",
    round(cv_long, 3), "\n", sep = "")

# Named list for loop-based simulation
cv_scenarios <- list(
  short_term = cv_short,
  long_term  = cv_long
)


# ==============================================================================
# SECTION 4: BUILD SPATIAL LOOKUP TABLE (network <-> analysis reachids)
#
# Bridges the two shapefiles via st_equals (identical geometry).
# Result: network_reachid <-> analysis_reachid lookup for all translation steps.
# ==============================================================================

cat("\nBuilding spatial lookup table...\n")

kusko_analysis <- st_transform(kusko_analysis, st_crs(kusk_edges))

network_to_analysis <- kusk_edges %>%
  select(network_reachid = reachid) %>%
  st_join(
    kusko_analysis %>% select(analysis_reachid = reachid),
    join = st_equals,
    left = TRUE
  ) %>%
  st_drop_geometry() %>%
  filter(!is.na(analysis_reachid)) %>%
  distinct(network_reachid, analysis_reachid)

cat("  Reaches matched:", nrow(network_to_analysis), "\n")
cat("  Network reaches unmatched:", nrow(kusk_edges) - nrow(network_to_analysis), "\n")


# ==============================================================================
# SECTION 5: IDENTIFY KUSKO REACHBASE REACHES
# ==============================================================================

cat("\nIdentifying Kusko ReachBase reaches...\n")

reachbase_reaches <- kusko_analysis %>%
  st_drop_geometry() %>%
  filter(Reachbase >= 4, Reachbase <= 7, !is.na(Str_Order)) %>%
  select(reachid, stream_order = Str_Order, Reachbase)

n_reachbases <- nrow(reachbase_reaches)

cat("  Total ReachBase reaches:", n_reachbases, "\n")
print(table(reachbase_reaches$stream_order))


# ==============================================================================
# SECTION 6: LOAD KUSKO PRODUCTION DATA (2017-2022)
# ==============================================================================

cat("\nLoading Kusko production data...\n")

prod_dir  <- here("Outputs", "ProductionData", "Kusko")
prod_list <- list()

for (yr in years) {
  prod_list[[as.character(yr)]] <- read_csv(
    file.path(prod_dir, paste0(yr, "_Kusko_Assignment_Results.csv")),
    show_col_types = FALSE
  ) %>%
    select(reachid, assignment_rescale) %>%
    rename(!!paste0("prod_", yr) := assignment_rescale)
}

prod_wide_all <- prod_list[[1]]
for (yr in years[-1]) {
  prod_wide_all <- prod_wide_all %>%
    left_join(prod_list[[as.character(yr)]], by = "reachid")
}

cat("  Loaded:", nrow(prod_wide_all), "reaches x", length(years), "years\n")


# ==============================================================================
# SECTION 7: ACCUMULATE KUSKO PRODUCTION PER REACHBASE DRAINAGE
#
# For each ReachBase reach:
#   1. Translate analysis reachid -> network reachid
#   2. Walk upstream via KuskoNetwork to collect all upstream network reachids
#   3. Translate back to analysis reachids
#   4. Sum production (proportion) across all matched reaches per year
#
# Upstream analysis_ids are cached to avoid redundant traversal in Section 9.
# ==============================================================================

cat("\nAccumulating Kusko production per drainage...\n")

# Cache upstream analysis_ids per ReachBase to reuse in null simulations
upstream_cache <- vector("list", n_reachbases)

accumulated_prod <- data.frame()

for (i in 1:n_reachbases) {

  rb_analysis_id <- reachbase_reaches$reachid[i]
  rb_order       <- reachbase_reaches$stream_order[i]

  if (i %% 50 == 0) cat("  Processing", i, "of", n_reachbases, "...\n")

  # Step 1: analysis -> network
  rb_network_id <- network_to_analysis %>%
    filter(analysis_reachid == rb_analysis_id) %>%
    pull(network_reachid)

  if (length(rb_network_id) != 1) next

  # Step 2: upstream traversal
  start_rid  <- kusk_edges$rid[kusk_edges$reachid == rb_network_id]
  trib_index <- KuskoNetwork$child_s[KuskoNetwork$rid == start_rid]

  if (length(trib_index) > 0) {
    children <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% trib_index]
    while (length(children) > 0) {
      trib_index <- c(trib_index, children)
      children   <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% children]
    }
    upstream_rids    <- KuskoNetwork$rid[match(trib_index, KuskoNetwork$child_s)]
    upstream_net_ids <- kusk_edges$reachid[match(upstream_rids, kusk_edges$rid)]
    upstream_net_ids <- upstream_net_ids[!is.na(upstream_net_ids)]
  } else {
    upstream_net_ids <- character(0)
  }

  all_network_ids <- unique(c(rb_network_id, upstream_net_ids))

  # Step 3: network -> analysis
  analysis_ids <- network_to_analysis %>%
    filter(network_reachid %in% all_network_ids) %>%
    pull(analysis_reachid) %>%
    unique()

  if (length(analysis_ids) == 0) next

  # Cache for reuse in Section 9
  upstream_cache[[i]] <- list(
    rb_analysis_id = rb_analysis_id,
    rb_order       = rb_order,
    analysis_ids   = analysis_ids
  )

  # Step 4: sum production
  drainage_prod <- prod_wide_all %>%
    filter(reachid %in% analysis_ids) %>%
    summarise(across(starts_with("prod_"), ~ sum(.x, na.rm = TRUE)))

  accumulated_prod <- bind_rows(
    accumulated_prod,
    data.frame(
      reachbase_id       = rb_analysis_id,
      stream_order       = rb_order,
      n_analysis_reaches = length(analysis_ids),
      drainage_prod
    )
  )
}

# Remove empty cache slots
upstream_cache <- Filter(Negate(is.null), upstream_cache)

cat("  Done. Drainages accumulated:", nrow(accumulated_prod), "\n")


# ==============================================================================
# SECTION 8: COMPUTE KUSKO PAIRWISE INTER-ANNUAL % CHANGES
#
# Consecutive year pairs only (5 pairs) to reduce non-independence.
# % change computed on proportional production.
# ==============================================================================

cat("\nComputing Kusko pairwise inter-annual changes...\n")

year_pairs    <- lapply(1:(length(years) - 1), function(i) c(years[i], years[i + 1]))
pairwise_list <- list()

for (pair in year_pairs) {

  yr_i  <- pair[1]
  yr_j  <- pair[2]
  col_i <- paste0("prod_", yr_i)
  col_j <- paste0("prod_", yr_j)

  pairwise_list[[paste0(yr_i, "_", yr_j)]] <- accumulated_prod %>%
    select(reachbase_id, stream_order, all_of(c(col_i, col_j))) %>%
    rename(prod_i = all_of(col_i), prod_j = all_of(col_j)) %>%
    mutate(
      interannual_pct = (prod_i - prod_j) / prod_j * 100,
      year_i          = yr_i,
      year_j          = yr_j
    ) %>%
    filter(prod_j > 0, is.finite(interannual_pct))
}

pairwise_all <- bind_rows(pairwise_list)

cat("  Total observations:", nrow(pairwise_all), "\n")


# ==============================================================================
# SECTION 9: KUSKO NULL SIMULATIONS -- TWO EMPIRICAL CV SCENARIOS
#
# For each CV scenario, simulates 20 years of independent-population production
# using a log-normal with mean = l/L (reach length fraction) and the given CV.
# Each year is normalized to sum to 1. Upstream accumulation uses cached
# analysis_ids from Section 7 to avoid redundant network traversal.
# Consecutive year pairs only, matching the empirical data structure.
# ==============================================================================

cat("\nRunning Kusko null simulations...\n")

set.seed(42)
n_sim_years <- 20

reach_length_fractions <- kusko_analysis %>%
  st_drop_geometry() %>%
  select(reachid, reach_length_m) %>%
  mutate(length_fraction = reach_length_m / total_basin_length_m)

n_sim_reaches <- nrow(reach_length_fractions)
all_envelopes <- data.frame()

for (scenario_name in names(cv_scenarios)) {

  cv_val    <- cv_scenarios[[scenario_name]]
  sigma_log <- sqrt(log(cv_val^2 + 1))

  cat("  Simulating scenario:", scenario_name, "(CV =", round(cv_val, 3), ")...\n")

  # Simulate reach-level production
  sim_matrix <- matrix(NA, nrow = n_sim_reaches, ncol = n_sim_years)

  for (i in 1:n_sim_reaches) {
    mu_i <- reach_length_fractions$length_fraction[i]
    if (mu_i <= 0) { sim_matrix[i, ] <- 0; next }
    mu_log_i        <- log(mu_i) - 0.5 * sigma_log^2
    sim_matrix[i, ] <- rlnorm(n_sim_years, meanlog = mu_log_i, sdlog = sigma_log)
  }

  for (col in 1:n_sim_years) {
    col_sum <- sum(sim_matrix[, col], na.rm = TRUE)
    if (col_sum > 0) sim_matrix[, col] <- sim_matrix[, col] / col_sum
  }

  sim_df           <- as.data.frame(sim_matrix)
  colnames(sim_df) <- paste0("simyr_", 1:n_sim_years)
  sim_df$reachid   <- reach_length_fractions$reachid

  # Accumulate using cached upstream analysis_ids
  sim_accumulated <- data.frame()

  for (cache in upstream_cache) {

    drainage_sim <- sim_df %>%
      filter(reachid %in% cache$analysis_ids) %>%
      summarise(across(starts_with("simyr_"), ~ sum(.x, na.rm = TRUE)))

    sim_accumulated <- bind_rows(
      sim_accumulated,
      data.frame(
        reachbase_id = cache$rb_analysis_id,
        stream_order = cache$rb_order,
        drainage_sim
      )
    )
  }

  # Consecutive pairwise % changes
  sim_pairs   <- lapply(1:(n_sim_years - 1), function(i) c(i, i + 1))
  sim_pw_list <- list()

  for (sp in sim_pairs) {
    ci <- paste0("simyr_", sp[1])
    cj <- paste0("simyr_", sp[2])

    sim_pw_list[[paste0(sp[1], "_", sp[2])]] <- sim_accumulated %>%
      select(reachbase_id, stream_order, all_of(c(ci, cj))) %>%
      rename(pi = all_of(ci), pj = all_of(cj)) %>%
      mutate(pct_change = (pi - pj) / pj * 100) %>%
      filter(pj > 0, is.finite(pct_change))
  }

  scenario_envelope <- bind_rows(sim_pw_list) %>%
    group_by(stream_order) %>%
    summarise(
      Q1            = quantile(pct_change, 0.25, na.rm = TRUE),
      Q3            = quantile(pct_change, 0.75, na.rm = TRUE),
      IQR           = Q3 - Q1,
      lower_whisker = Q1 - 1.5 * IQR,
      upper_whisker = Q3 + 1.5 * IQR,
      .groups       = "drop"
    ) %>%
    mutate(scenario = scenario_name, cv_val = cv_val)

  all_envelopes <- bind_rows(all_envelopes, scenario_envelope)
}

cat("  Kusko simulations complete.\n")


# ==============================================================================
# SECTION 10: KUSKO-ONLY PLOT AND EXPORT
# ==============================================================================

cat("\nBuilding Kusko figure...\n")

y_limit <- 100

envelopes_plot <- all_envelopes %>%
  mutate(scenario = factor(scenario, levels = c("short_term", "long_term")))

scenario_labels <- c(
  short_term = paste0("Short-term CV (2017\u20132022, CV = ", round(cv_short, 2), ")"),
  long_term  = paste0("Long-term CV (", min(long_term_years), "\u2013",
                      max(long_term_years), ", CV = ", round(cv_long, 2), ")")
)

scenario_colors    <- c(short_term = "#2C7BB6", long_term = "#1A9641")
scenario_linetypes <- c(short_term = "dotted",  long_term = "dashed")

# Envelope bounds: most restrictive across both scenarios
envelope_bounds <- envelopes_plot %>%
  group_by(stream_order) %>%
  summarise(
    inner_lower = max(lower_whisker),
    inner_upper = min(upper_whisker),
    .groups = "drop"
  )

plot_points <- pairwise_all %>%
  mutate(interannual_pct = pmax(pmin(interannual_pct, y_limit), -y_limit)) %>%
  left_join(envelope_bounds, by = "stream_order") %>%
  mutate(
    inside_envelope = interannual_pct >= inner_lower & interannual_pct <= inner_upper
  )

p_kusko <- ggplot() +

  geom_boxplot(
    data          = plot_points,
    aes(x = stream_order, y = interannual_pct, group = stream_order),
    fill          = "grey85",
    color         = "grey60",
    alpha         = 0.3,
    linewidth     = 0.4,
    width         = 0.5,
    outlier.shape = NA
  ) +

  geom_jitter(
    data  = filter(plot_points, !inside_envelope),
    aes(x = stream_order, y = interannual_pct),
    color = "grey70",
    size  = 2,
    alpha = 0.4,
    width = 0.15
  ) +

  geom_jitter(
    data  = filter(plot_points, inside_envelope),
    aes(x = stream_order, y = interannual_pct),
    color = "#E4572E",
    size  = 2,
    alpha = 0.55,
    width = 0.15
  ) +

  geom_line(
    data      = envelopes_plot,
    aes(x = stream_order, y = upper_whisker, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_line(
    data      = envelopes_plot,
    aes(x = stream_order, y = lower_whisker, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +

  scale_x_continuous(breaks = 4:7, labels = 4:7) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-100, 100, by = 50)
  ) +
  scale_color_manual(
    name   = "Null simulation",
    values = scenario_colors,
    labels = scenario_labels
  ) +
  scale_linetype_manual(
    name   = "Null simulation",
    values = scenario_linetypes,
    labels = scenario_labels
  ) +

  labs(
    title    = "Kuskokwim Chinook \u2014 Variance Dampening Across Spatial Scales",
    subtitle = paste0(
      "Colored points fall within both null envelopes; grey points fall outside\n",
      "(5 consecutive year-pairs, 2017\u20132022)"
    ),
    x = "Stream Order",
    y = "Inter-annual variability (% difference)"
  ) +

  theme_classic(base_size = 13) +
  theme(
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(color = "gray40", size = 9),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.width  = unit(1.5, "cm")
  )

print(p_kusko)

dir.create(here("Figures", "Variance"), showWarnings = FALSE, recursive = TRUE)
out_path <- here("Figures", "Variance", "Kusko_VarianceDampening.png")

ggsave(filename = out_path, plot = p_kusko, width = 10, height = 6, dpi = 300)
cat("\nKusko figure saved to:", out_path, "\n")


# ==============================================================================
# SECTION 11: YUKON -- LOAD SPATIAL DATA
#
# Yukon uses a single shapefile (Yukon_edges.shp) that carries both reach
# geometry and the up_rid field needed for upstream traversal.
# No second network shapefile or bridge table is needed.
# ==============================================================================

cat("\nLoading Yukon spatial data...\n")

yuk_edges <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges.shp"),
  quiet = TRUE
)

YukonNodes <- read.csv(
  here("Data", "UpstreamReaches", "yukon_noderelationships.csv"),
  stringsAsFactors = FALSE
)

YukonNetwork <- YukonNodes %>%
  rename(child_s = fromnode, parent_s = tonode)

yuk_edges <- yuk_edges %>%
  mutate(reach_length_m = as.numeric(st_length(geometry)))

total_basin_length_yukon <- sum(yuk_edges$reach_length_m, na.rm = TRUE)

cat("  Yukon shapefile:", nrow(yuk_edges), "reaches\n")
cat("  Total basin length:", round(total_basin_length_yukon / 1000, 1), "km\n")


# ==============================================================================
# SECTION 12: YUKON -- COMPUTE CV VALUES
# ==============================================================================

cat("\nComputing Yukon CV values...\n")

# Years with Yukon_Full production output files.
# Adjust if additional years are available.
yukon_years <- c(2015, 2016, 2018, 2021)

yukon_all_esc <- escapement %>%
  filter(River == "Yukon") %>%
  arrange(Year)

esc_short_y     <- yukon_all_esc %>% filter(Year %in% yukon_years) %>% pull(Total_Run)
cv_short_y      <- sd(esc_short_y) / mean(esc_short_y)

esc_long_y      <- yukon_all_esc %>% filter(Year >= 2010) %>% pull(Total_Run)
cv_long_y       <- sd(esc_long_y) / mean(esc_long_y)
long_term_yrs_y <- yukon_all_esc %>% filter(Year >= 2010) %>% pull(Year)

cat("  Short-term CV (", paste(range(yukon_years), collapse = "-"), "):",
    round(cv_short_y, 3), "\n", sep = "")
cat("  Long-term CV (", min(long_term_yrs_y), "-", max(long_term_yrs_y), "):",
    round(cv_long_y, 3), "\n", sep = "")

cv_scenarios_y <- list(
  short_term = cv_short_y,
  long_term  = cv_long_y
)


# ==============================================================================
# SECTION 13: YUKON -- IDENTIFY REACHBASE REACHES
# ==============================================================================

cat("\nIdentifying Yukon ReachBase reaches...\n")

reachbase_reaches_y <- yuk_edges %>%
  st_drop_geometry() %>%
  filter(Reachbase >= 4, Reachbase <= 7, !is.na(Str_Order)) %>%
  select(reachid, stream_order = Str_Order, Reachbase)

n_reachbases_y <- nrow(reachbase_reaches_y)
cat("  Total ReachBase reaches:", n_reachbases_y, "\n")
print(table(reachbase_reaches_y$stream_order))


# ==============================================================================
# SECTION 14: YUKON -- LOAD PRODUCTION DATA
#
# Mirrors the Kusko approach exactly.
# Files: Outputs/ProductionData/Yukon_full/{yr}_Yukon_Full_Assignment_Results.csv
# ==============================================================================

cat("\nLoading Yukon production data...\n")

prod_dir_y  <- here("Outputs", "ProductionData", "Yukon_full")
prod_list_y <- list()

for (yr in yukon_years) {
  prod_list_y[[as.character(yr)]] <- read_csv(
    file.path(prod_dir_y, paste0(yr, "_Yukon_Full_Assignment_Results.csv")),
    show_col_types = FALSE
  ) %>%
    select(reachid, assignment_rescale) %>%
    rename(!!paste0("prod_", yr) := assignment_rescale)
}

prod_wide_y <- prod_list_y[[1]]
for (yr in yukon_years[-1]) {
  prod_wide_y <- prod_wide_y %>%
    left_join(prod_list_y[[as.character(yr)]], by = "reachid")
}

cat("  Loaded:", nrow(prod_wide_y), "reaches x", length(yukon_years), "years\n")


# ==============================================================================
# SECTION 15: YUKON -- ACCUMULATE PRODUCTION PER REACHBASE DRAINAGE
#
# Seeds traversal from yuk_edges$up_rid for each ReachBase reachid, then walks
# upstream through YukonNetwork. Reach IDs are cached for reuse in Section 17.
# Same cache structure as Kusko (rb_analysis_id / analysis_ids field names).
# ==============================================================================

cat("\nAccumulating Yukon production per drainage...\n")

upstream_cache_y <- vector("list", n_reachbases_y)

accumulated_prod_y <- data.frame()

for (i in 1:n_reachbases_y) {

  rb_reachid <- reachbase_reaches_y$reachid[i]
  rb_order   <- reachbase_reaches_y$stream_order[i]

  if (i %% 50 == 0) cat("  Processing", i, "of", n_reachbases_y, "...\n")

  trib_start_rid <- yuk_edges$up_rid[yuk_edges$reachid == rb_reachid]

  if (length(trib_start_rid) != 1) next

  trib_index <- YukonNetwork$child_s[YukonNetwork$rid == trib_start_rid]

  if (length(trib_index) > 0) {
    children <- YukonNetwork$child_s[YukonNetwork$parent_s %in% trib_index]
    while (length(children) > 0) {
      trib_index <- c(trib_index, children)
      children   <- YukonNetwork$child_s[YukonNetwork$parent_s %in% children]
    }
    upstream_rids     <- YukonNetwork$rid[match(trib_index, YukonNetwork$child_s)]
    upstream_reachids <- yuk_edges$reachid[match(upstream_rids, yuk_edges$up_rid)]
    upstream_reachids <- upstream_reachids[!is.na(upstream_reachids)]
  } else {
    upstream_reachids <- character(0)
  }

  all_reach_ids <- unique(c(rb_reachid, upstream_reachids))

  if (length(all_reach_ids) == 0) next

  # Cache for reuse in Section 17 (same structure as Kusko cache)
  upstream_cache_y[[i]] <- list(
    rb_analysis_id = rb_reachid,
    rb_order       = rb_order,
    analysis_ids   = all_reach_ids
  )

  drainage_prod <- prod_wide_y %>%
    filter(reachid %in% all_reach_ids) %>%
    summarise(across(starts_with("prod_"), ~ sum(.x, na.rm = TRUE)))

  accumulated_prod_y <- bind_rows(
    accumulated_prod_y,
    data.frame(
      reachbase_id       = rb_reachid,
      stream_order       = rb_order,
      n_analysis_reaches = length(all_reach_ids),
      drainage_prod
    )
  )
}

upstream_cache_y <- Filter(Negate(is.null), upstream_cache_y)

cat("  Done. Drainages accumulated:", nrow(accumulated_prod_y), "\n")


# ==============================================================================
# SECTION 16: YUKON -- COMPUTE PAIRWISE INTER-ANNUAL % CHANGES
#
# Adjacent pairs in the year vector: 2015-2016, 2016-2018, 2018-2021.
# ==============================================================================

cat("\nComputing Yukon pairwise inter-annual changes...\n")

year_pairs_y    <- lapply(1:(length(yukon_years) - 1), function(i) c(yukon_years[i], yukon_years[i + 1]))
pairwise_list_y <- list()

for (pair in year_pairs_y) {

  yr_i  <- pair[1]
  yr_j  <- pair[2]
  col_i <- paste0("prod_", yr_i)
  col_j <- paste0("prod_", yr_j)

  pairwise_list_y[[paste0(yr_i, "_", yr_j)]] <- accumulated_prod_y %>%
    select(reachbase_id, stream_order, all_of(c(col_i, col_j))) %>%
    rename(prod_i = all_of(col_i), prod_j = all_of(col_j)) %>%
    mutate(
      interannual_pct = (prod_i - prod_j) / prod_j * 100,
      year_i          = yr_i,
      year_j          = yr_j
    ) %>%
    filter(prod_j > 0, is.finite(interannual_pct))
}

pairwise_all_y <- bind_rows(pairwise_list_y)

cat("  Total observations:", nrow(pairwise_all_y), "\n")


# ==============================================================================
# SECTION 17: YUKON -- NULL SIMULATIONS
# ==============================================================================

cat("\nRunning Yukon null simulations...\n")

reach_length_fractions_y <- yuk_edges %>%
  st_drop_geometry() %>%
  select(reachid, reach_length_m) %>%
  mutate(length_fraction = reach_length_m / total_basin_length_yukon)

n_sim_reaches_y <- nrow(reach_length_fractions_y)
all_envelopes_y <- data.frame()

for (scenario_name in names(cv_scenarios_y)) {

  cv_val    <- cv_scenarios_y[[scenario_name]]
  sigma_log <- sqrt(log(cv_val^2 + 1))

  cat("  Simulating scenario:", scenario_name, "(CV =", round(cv_val, 3), ")...\n")

  sim_matrix <- matrix(NA, nrow = n_sim_reaches_y, ncol = n_sim_years)

  for (i in 1:n_sim_reaches_y) {
    mu_i <- reach_length_fractions_y$length_fraction[i]
    if (mu_i <= 0) { sim_matrix[i, ] <- 0; next }
    mu_log_i        <- log(mu_i) - 0.5 * sigma_log^2
    sim_matrix[i, ] <- rlnorm(n_sim_years, meanlog = mu_log_i, sdlog = sigma_log)
  }

  for (col in 1:n_sim_years) {
    col_sum <- sum(sim_matrix[, col], na.rm = TRUE)
    if (col_sum > 0) sim_matrix[, col] <- sim_matrix[, col] / col_sum
  }

  sim_df           <- as.data.frame(sim_matrix)
  colnames(sim_df) <- paste0("simyr_", 1:n_sim_years)
  sim_df$reachid   <- reach_length_fractions_y$reachid

  sim_accumulated_y <- data.frame()

  for (cache in upstream_cache_y) {

    drainage_sim <- sim_df %>%
      filter(reachid %in% cache$analysis_ids) %>%
      summarise(across(starts_with("simyr_"), ~ sum(.x, na.rm = TRUE)))

    sim_accumulated_y <- bind_rows(
      sim_accumulated_y,
      data.frame(
        reachbase_id = cache$rb_analysis_id,
        stream_order = cache$rb_order,
        drainage_sim
      )
    )
  }

  sim_pairs   <- lapply(1:(n_sim_years - 1), function(i) c(i, i + 1))
  sim_pw_list <- list()

  for (sp in sim_pairs) {
    ci <- paste0("simyr_", sp[1])
    cj <- paste0("simyr_", sp[2])

    sim_pw_list[[paste0(sp[1], "_", sp[2])]] <- sim_accumulated_y %>%
      select(reachbase_id, stream_order, all_of(c(ci, cj))) %>%
      rename(pi = all_of(ci), pj = all_of(cj)) %>%
      mutate(pct_change = (pi - pj) / pj * 100) %>%
      filter(pj > 0, is.finite(pct_change))
  }

  scenario_envelope <- bind_rows(sim_pw_list) %>%
    group_by(stream_order) %>%
    summarise(
      Q1            = quantile(pct_change, 0.25, na.rm = TRUE),
      Q3            = quantile(pct_change, 0.75, na.rm = TRUE),
      IQR           = Q3 - Q1,
      lower_whisker = Q1 - 1.5 * IQR,
      upper_whisker = Q3 + 1.5 * IQR,
      .groups       = "drop"
    ) %>%
    mutate(scenario = scenario_name, cv_val = cv_val)

  all_envelopes_y <- bind_rows(all_envelopes_y, scenario_envelope)
}

cat("  Yukon simulations complete.\n")


# ==============================================================================
# SECTION 18: YUKON-ONLY PLOT AND EXPORT
# ==============================================================================

cat("\nBuilding Yukon figure...\n")

envelopes_plot_y <- all_envelopes_y %>%
  mutate(scenario = factor(scenario, levels = c("short_term", "long_term")))

scenario_labels_y <- c(
  short_term = paste0("Short-term CV (", paste(range(yukon_years), collapse = "\u2013"),
                      ", CV = ", round(cv_short_y, 2), ")"),
  long_term  = paste0("Long-term CV (", min(long_term_yrs_y), "\u2013",
                      max(long_term_yrs_y), ", CV = ", round(cv_long_y, 2), ")")
)

envelope_bounds_y <- envelopes_plot_y %>%
  group_by(stream_order) %>%
  summarise(
    inner_lower = max(lower_whisker),
    inner_upper = min(upper_whisker),
    .groups = "drop"
  )

plot_points_y <- pairwise_all_y %>%
  mutate(interannual_pct = pmax(pmin(interannual_pct, y_limit), -y_limit)) %>%
  left_join(envelope_bounds_y, by = "stream_order") %>%
  mutate(
    inside_envelope = interannual_pct >= inner_lower & interannual_pct <= inner_upper
  )

p_yukon <- ggplot() +

  geom_boxplot(
    data          = plot_points_y,
    aes(x = stream_order, y = interannual_pct, group = stream_order),
    fill          = "grey85",
    color         = "grey60",
    alpha         = 0.3,
    linewidth     = 0.4,
    width         = 0.5,
    outlier.shape = NA
  ) +

  geom_jitter(
    data  = filter(plot_points_y, !inside_envelope),
    aes(x = stream_order, y = interannual_pct),
    color = "grey70",
    size  = 2,
    alpha = 0.4,
    width = 0.15
  ) +

  geom_jitter(
    data  = filter(plot_points_y, inside_envelope),
    aes(x = stream_order, y = interannual_pct),
    color = "#E4572E",
    size  = 2,
    alpha = 0.55,
    width = 0.15
  ) +

  geom_line(
    data      = envelopes_plot_y,
    aes(x = stream_order, y = upper_whisker, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_line(
    data      = envelopes_plot_y,
    aes(x = stream_order, y = lower_whisker, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +

  scale_x_continuous(breaks = 4:7, labels = 4:7) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-100, 100, by = 50)
  ) +
  scale_color_manual(
    name   = "Null simulation",
    values = scenario_colors,
    labels = scenario_labels_y
  ) +
  scale_linetype_manual(
    name   = "Null simulation",
    values = scenario_linetypes,
    labels = scenario_labels_y
  ) +

  labs(
    title    = "Yukon Chinook \u2014 Variance Dampening Across Spatial Scales",
    subtitle = paste0(
      "Colored points fall within both null envelopes; grey points fall outside\n",
      "(3 adjacent year-pairs: ",
      paste(sapply(year_pairs_y, paste, collapse = "\u2013"), collapse = ", "), ")"
    ),
    x = "Stream Order",
    y = "Inter-annual variability (% difference)"
  ) +

  theme_classic(base_size = 13) +
  theme(
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(color = "gray40", size = 9),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.width  = unit(1.5, "cm")
  )

print(p_yukon)

out_path_y <- here("Figures", "Variance", "Yukon_VarianceDampening.png")
ggsave(filename = out_path_y, plot = p_yukon, width = 10, height = 6, dpi = 300)
cat("\nYukon figure saved to:", out_path_y, "\n")


# ==============================================================================
# SECTION 19: FIGURE 3 -- SIDE-BY-SIDE FACETS (Kuskokwim | Yukon)
#
# Inside/outside envelope flag is computed per basin using each basin's own
# envelope bounds (already present in plot_points and plot_points_y).
# CV values are annotated directly in each panel.
# ==============================================================================

cat("\nBuilding combined facet figure...\n")

basin_levels <- c("Kuskokwim", "Yukon")

scenario_labels_shared <- c(short_term = "Short-term CV", long_term = "Long-term CV")

# Combine empirical points with basin labels
plot_points_combined <- bind_rows(
  plot_points   %>% mutate(basin = "Kuskokwim"),
  plot_points_y %>% mutate(basin = "Yukon")
) %>%
  mutate(basin = factor(basin, levels = basin_levels))

# Combine envelopes with basin labels
envelopes_combined <- bind_rows(
  envelopes_plot   %>% mutate(basin = "Kuskokwim"),
  envelopes_plot_y %>% mutate(basin = "Yukon")
) %>%
  mutate(
    basin    = factor(basin, levels = basin_levels),
    scenario = factor(scenario, levels = c("short_term", "long_term"))
  )

# Per-panel CV value annotations
cv_annotations <- data.frame(
  basin = factor(c("Kuskokwim", "Yukon"), levels = basin_levels),
  label = c(
    paste0("Short-term: CV = ", round(cv_short, 2),
           " (2017\u20132022)\nLong-term: CV = ", round(cv_long, 2),
           " (", min(long_term_years), "\u2013", max(long_term_years), ")"),
    paste0("Short-term: CV = ", round(cv_short_y, 2),
           " (", paste(range(yukon_years), collapse = ", "), ")\nLong-term: CV = ",
           round(cv_long_y, 2),
           " (", min(long_term_yrs_y), "\u2013", max(long_term_yrs_y), ")")
  ),
  x = c(4, 4),
  y = c(94, 94)
)

p_facet <- ggplot() +

  geom_boxplot(
    data          = plot_points_combined,
    aes(x = stream_order, y = interannual_pct, group = stream_order),
    fill          = "grey85",
    color         = "grey60",
    alpha         = 0.3,
    linewidth     = 0.4,
    width         = 0.5,
    outlier.shape = NA
  ) +

  geom_jitter(
    data  = filter(plot_points_combined, !inside_envelope),
    aes(x = stream_order, y = interannual_pct),
    color = "grey70",
    size  = 1.8,
    alpha = 0.4,
    width = 0.15
  ) +

  geom_jitter(
    data  = filter(plot_points_combined, inside_envelope),
    aes(x = stream_order, y = interannual_pct),
    color = "#E4572E",
    size  = 1.8,
    alpha = 0.55,
    width = 0.15
  ) +

  geom_line(
    data      = envelopes_combined,
    aes(x = stream_order, y = upper_whisker, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_line(
    data      = envelopes_combined,
    aes(x = stream_order, y = lower_whisker, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +

  geom_text(
    data  = cv_annotations,
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1, size = 2.5, color = "gray40"
  ) +

  facet_wrap(~ basin, ncol = 2) +

  scale_x_continuous(breaks = 4:7, labels = 4:7) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-100, 100, by = 50)
  ) +
  scale_color_manual(
    name   = "Null simulation",
    values = scenario_colors,
    labels = scenario_labels_shared
  ) +
  scale_linetype_manual(
    name   = "Null simulation",
    values = scenario_linetypes,
    labels = scenario_labels_shared
  ) +

  labs(
    title    = "Chinook Variance Dampening Across Spatial Scales \u2014 Kuskokwim & Yukon",
    subtitle = "Orange points fall within both null envelopes; grey points fall outside",
    x = "Stream Order",
    y = "Inter-annual variability (% difference)"
  ) +

  theme_classic(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "gray40", size = 9),
    panel.grid       = element_blank(),
    strip.text       = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = NA),
    legend.position  = "bottom",
    legend.key.width = unit(1.4, "cm")
  )

print(p_facet)

out_path_facet <- here("Figures", "Variance", "BothBasins_Facet.png")
ggsave(filename = out_path_facet, plot = p_facet, width = 14, height = 7, dpi = 300)
cat("\nFacet figure saved to:", out_path_facet, "\n")


# ==============================================================================
# SECTION 20: FIGURE 4 -- OVERLAID (BOTH BASINS ON SAME AXES)
#
# Color = basin. Vivid points = inside envelope; faded = outside.
# Linetype = scenario for envelope lines.
# ==============================================================================

cat("\nBuilding overlaid figure...\n")

basin_colors <- c(Kuskokwim = "#E4572E", Yukon = "#2166AC")

p_overlay <- ggplot() +

  # Outside-envelope points (faded, colored by basin)
  geom_jitter(
    data  = filter(plot_points_combined, !inside_envelope),
    aes(x = stream_order, y = interannual_pct, color = basin),
    size  = 1.8,
    alpha = 0.20,
    width = 0.15
  ) +

  # Inside-envelope points (vivid, colored by basin)
  geom_jitter(
    data  = filter(plot_points_combined, inside_envelope),
    aes(x = stream_order, y = interannual_pct, color = basin),
    size  = 1.8,
    alpha = 0.55,
    width = 0.15
  ) +

  # Envelope lines: color = basin, linetype = scenario
  geom_line(
    data      = envelopes_combined,
    aes(x = stream_order, y = upper_whisker, color = basin, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_line(
    data      = envelopes_combined,
    aes(x = stream_order, y = lower_whisker, color = basin, linetype = scenario),
    linewidth = 0.9
  ) +

  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +

  scale_x_continuous(breaks = 4:7, labels = 4:7) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-100, 100, by = 50)
  ) +
  scale_color_manual(name = "Basin", values = basin_colors) +
  scale_linetype_manual(
    name   = "Null simulation",
    values = scenario_linetypes,
    labels = scenario_labels_shared
  ) +

  labs(
    title    = "Chinook Variance Dampening \u2014 Kuskokwim vs. Yukon",
    subtitle = paste0(
      "Kuskokwim: 2017\u20132022 (CV\u209b = ", round(cv_short, 2),
      ", CV\u2097 = ", round(cv_long, 2), ")   |   ",
      "Yukon: ", paste(yukon_years, collapse = ", "),
      " (CV\u209b = ", round(cv_short_y, 2),
      ", CV\u2097 = ", round(cv_long_y, 2), ")\n",
      "Vivid = inside envelope | Faded = outside | Color = basin | Linetype = CV scenario"
    ),
    x = "Stream Order",
    y = "Inter-annual variability (% difference)"
  ) +

  theme_classic(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "gray40", size = 8),
    panel.grid       = element_blank(),
    legend.position  = "right",
    legend.key.width = unit(1.4, "cm")
  )

print(p_overlay)

out_path_overlay <- here("Figures", "Variance", "BothBasins_Overlay.png")
ggsave(filename = out_path_overlay, plot = p_overlay, width = 10, height = 6, dpi = 300)
cat("\nOverlay figure saved to:", out_path_overlay, "\n")

cat("\nDone. All figures saved to Figures/Variance/\n")
