################################################################################
# KUSKOKWIM VARIANCE DAMPENING ANALYSIS
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
#   year-to-year % change across all 15 year pairs (2017-2022). We then compare
#   the spread of that variability at each stream order against two null
#   simulations run at empirically derived CV levels:
#
#     cv_short  = empirical CV from the 2017-2022 production years
#     cv_long   = empirical CV from 2010-present (all available Kusko years)
#
#   Each simulation produces an independent-population envelope. Both are
#   overlaid on a single plot, distinguished by color and linetype.
#
# SINGLE SHAPEFILE:
#   kusk_edges = Kusko_edges.shp
#     Has reachid, up_reachid, up_rid, Reachbase, Str_Order, geometry.
#     Upstream traversal uses up_reachid / up_rid fields directly —
#     no secondary network shapefile or spatial bridge needed.
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
# SECTION 2: LOAD SPATIAL AND NETWORK DATA
# ==============================================================================

cat("Loading spatial data...\n")

kusk_edges <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  quiet = TRUE
)

KuskoNodes <- read.csv(
  here("Data", "UpstreamReaches", "kusko_noderelationships.csv"),
  stringsAsFactors = FALSE
)

KuskoNetwork <- KuskoNodes %>%
  rename(child_s = fromnode, parent_s = tonode)

kusk_edges <- kusk_edges %>%
  mutate(reach_length_m = as.numeric(st_length(geometry)))

total_basin_length_m <- sum(kusk_edges$reach_length_m, na.rm = TRUE)

cat("  Reaches loaded:", nrow(kusk_edges), "\n")
cat("  Total basin length:", round(total_basin_length_m / 1000, 1), "km\n")


# ==============================================================================
# SECTION 3: UPSTREAM TRAVERSAL FUNCTION
#
# Uses up_rid and up_reachid fields on kusk_edges directly.
# Returns all upstream reachids for a given mouth reachid.
# ==============================================================================

FindUpstreamReachID_Kusk <- function(ReachID) {
  
  TribStartRID <- kusk_edges$up_rid[kusk_edges$up_reachid == ReachID]
  
  if (length(TribStartRID) != 1) {
    stop(paste("ReachID", ReachID, "does not resolve to a unique up_rid"))
  }
  
  TRIBindex <- KuskoNetwork$child_s[KuskoNetwork$rid == TribStartRID]
  ChildList  <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% TRIBindex]
  
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% ChildList]
  }
  
  upstream_rids     <- KuskoNetwork$rid[match(TRIBindex, KuskoNetwork$child_s)]
  upstream_reachids <- kusk_edges$up_reachid[match(upstream_rids, kusk_edges$up_rid)]
  return(upstream_reachids[!is.na(upstream_reachids)])
}


# ==============================================================================
# SECTION 4: LOAD ESCAPEMENT AND COMPUTE CV VALUES
#
# cv_short = CV from the 2017-2022 analysis years
# cv_long  = CV from 2010 to the most recent available Kusko year
# ==============================================================================

cat("\nLoading escapement data and computing CV values...\n")

years      <- c(2017, 2018, 2019, 2020, 2021, 2022)
escapement <- read_excel(here("Data", "AYKEscapement.xlsx"))

kusko_all_esc <- escapement %>%
  filter(River == "Kusko") %>%
  arrange(Year)

# Short-term CV: 2017-2022 only
esc_short <- kusko_all_esc %>% filter(Year %in% years) %>% pull(Total_Run)
cv_short  <- sd(esc_short) / mean(esc_short)

# Long-term CV: 2010 to most recent available year
esc_long        <- kusko_all_esc %>% filter(Year >= 2010) %>% pull(Total_Run)
cv_long         <- sd(esc_long) / mean(esc_long)
long_term_years <- kusko_all_esc %>% filter(Year >= 2010) %>% pull(Year)

cat("  Short-term CV (2017-2022):", round(cv_short, 3), "\n")
cat("  Long-term CV (", min(long_term_years), "-", max(long_term_years), "):",
    round(cv_long, 3), "\n", sep = "")

cv_scenarios <- list(
  short_term = cv_short,
  long_term  = cv_long
)


# ==============================================================================
# SECTION 5: IDENTIFY REACHBASE REACHES
#
# ReachBase reaches are the mouths of tributary systems — the last reach at
# a given stream order before the order increases.
# ==============================================================================

cat("\nIdentifying ReachBase reaches...\n")

reachbase_reaches <- kusk_edges %>%
  st_drop_geometry() %>%
  filter(Reachbase >= 3, Reachbase <= 7, !is.na(Str_Order)) %>%
  select(reachid, stream_order = Str_Order, Reachbase)

n_reachbases <- nrow(reachbase_reaches)

cat("  Total ReachBase reaches:", n_reachbases, "\n")
print(table(reachbase_reaches$stream_order))


# ==============================================================================
# SECTION 6: LOAD PRODUCTION DATA (2017-2022)
#
# Each CSV has one row per reach with assignment_rescale (sums to 1 basin-wide).
# Reachids match kusk_edges.
# ==============================================================================

cat("\nLoading production data...\n")

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
# SECTION 7: ACCUMULATE PRODUCTION PER REACHBASE DRAINAGE
#
# For each ReachBase reach:
#   1. Call FindUpstreamReachID_Kusk to collect all upstream reachids
#   2. Include the mouth reach itself
#   3. Sum production across all matched reaches per year
# ==============================================================================

cat("\nAccumulating production per drainage...\n")

accumulated_prod <- data.frame()

for (i in 1:n_reachbases) {
  
  rb_reachid <- reachbase_reaches$reachid[i]
  rb_order   <- reachbase_reaches$stream_order[i]
  
  if (i %% 50 == 0) cat("  Processing", i, "of", n_reachbases, "...\n")
  
  upstream_ids <- tryCatch(
    as.character(FindUpstreamReachID_Kusk(rb_reachid)),
    error = function(e) character(0)
  )
  
  all_ids <- unique(c(as.character(rb_reachid), upstream_ids))
  all_ids <- all_ids[all_ids %in% prod_wide_all$reachid]
  
  if (length(all_ids) == 0) next
  
  drainage_prod <- prod_wide_all %>%
    filter(reachid %in% all_ids) %>%
    summarise(across(starts_with("prod_"), ~ sum(.x, na.rm = TRUE)))
  
  accumulated_prod <- bind_rows(
    accumulated_prod,
    data.frame(
      reachbase_id       = rb_reachid,
      stream_order       = rb_order,
      n_analysis_reaches = length(all_ids),
      drainage_prod
    )
  )
}

cat("  Done. Drainages accumulated:", nrow(accumulated_prod), "\n")


# ==============================================================================
# SECTION 8: COMPUTE PAIRWISE INTER-ANNUAL % CHANGES
#
# % change computed on raw proportions (assignment_rescale sums).
# Point size is scaled by mean proportion across years.
# ==============================================================================

cat("\nComputing pairwise inter-annual changes...\n")

year_pairs    <- combn(years, 2, simplify = FALSE)
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

# Mean proportion per drainage for point sizing
drainage_mean <- accumulated_prod %>%
  select(reachbase_id, starts_with("prod_")) %>%
  pivot_longer(cols = starts_with("prod_"), names_to = "year", values_to = "proportion") %>%
  group_by(reachbase_id) %>%
  summarise(mean_proportion = mean(proportion, na.rm = TRUE), .groups = "drop")

pairwise_all <- pairwise_all %>%
  left_join(drainage_mean, by = "reachbase_id")

cat("  Total observations:", nrow(pairwise_all), "\n")


# ==============================================================================
# SECTION 9: NULL SIMULATIONS — TWO EMPIRICAL CV SCENARIOS
#
# For each CV scenario, simulates 20 years of independent-population production
# using a log-normal with mean = l/L (reach length fraction) and the given CV.
# Each year is normalized to sum to 1. The same downstream accumulation is
# applied. Whisker bounds (Q1 - 1.5*IQR, Q3 + 1.5*IQR) per stream order
# become the envelope for that scenario.
# ==============================================================================

cat("\nRunning null simulations for empirical CV scenarios...\n")

set.seed(42)
n_sim_years <- 20

reach_length_fractions <- kusk_edges %>%
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
  
  # Accumulate through each ReachBase drainage
  sim_accumulated <- data.frame()
  
  for (i in 1:n_reachbases) {
    
    rb_reachid <- reachbase_reaches$reachid[i]
    rb_order   <- reachbase_reaches$stream_order[i]
    
    upstream_ids <- tryCatch(
      as.character(FindUpstreamReachID_Kusk(rb_reachid)),
      error = function(e) character(0)
    )
    
    all_ids <- unique(c(as.character(rb_reachid), upstream_ids))
    all_ids <- all_ids[all_ids %in% sim_df$reachid]
    
    if (length(all_ids) == 0) next
    
    drainage_sim <- sim_df %>%
      filter(reachid %in% all_ids) %>%
      summarise(across(starts_with("simyr_"), ~ sum(.x, na.rm = TRUE)))
    
    sim_accumulated <- bind_rows(
      sim_accumulated,
      data.frame(reachbase_id = rb_reachid, stream_order = rb_order, drainage_sim)
    )
  }
  
  # Pairwise % changes and envelope for this scenario
  sim_pairs   <- combn(1:n_sim_years, 2, simplify = FALSE)
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

cat("  All simulations complete.\n")


# ==============================================================================
# SECTION 10: PLOT AND EXPORT — PUBLICATION QUALITY
# ==============================================================================

cat("\nBuilding figure...\n")

library(ggplot2)
library(scales)

y_limit <- 100

plot_points <- pairwise_all %>%
  mutate(interannual_pct = pmax(pmin(interannual_pct, y_limit), -y_limit))

# --- Scenario labels ----------------------------------------------------------
scenario_labels <- c(
  short_term = paste0("Short-term CV (2017\u20132022, CV = ", round(cv_short, 2), ")"),
  long_term  = paste0("Long-term CV (", min(long_term_years), "\u2013",
                      max(long_term_years), ", CV = ", round(cv_long, 2), ")")
)

scenario_colors    <- c(short_term = "#2166AC", long_term = "#4DAC26")
scenario_linetypes <- c(short_term = "dotted",  long_term = "dashed")

all_envelopes <- all_envelopes %>%
  mutate(scenario = factor(scenario, levels = names(scenario_labels)))

# --- Build plot ---------------------------------------------------------------
p <- ggplot() +
  
  # Soft ribbon between whiskers
  geom_ribbon(
    data = all_envelopes %>% filter(scenario == "short_term"),
    aes(x = stream_order, ymin = lower_whisker, ymax = upper_whisker),
    fill = "#2166AC", alpha = 0.07
  ) +
  geom_ribbon(
    data = all_envelopes %>% filter(scenario == "long_term"),
    aes(x = stream_order, ymin = lower_whisker, ymax = upper_whisker),
    fill = "#4DAC26", alpha = 0.07
  ) +
  
  # Boxplot
  geom_boxplot(
    data          = plot_points,
    aes(x = stream_order, y = interannual_pct, group = stream_order),
    fill          = "grey85",
    color         = "grey50",
    alpha         = 0.6,
    linewidth     = 0.4,
    width         = 0.45,
    outlier.shape = NA,
    staplewidth   = 0.3
  ) +
  
  # Jittered empirical points — uniform size
  geom_jitter(
    data   = plot_points,
    aes(x = stream_order, y = interannual_pct),
    color  = "#8C4A3C",
    fill   = "#C97D6E",
    size   = 1.2,
    alpha  = 0.30,
    width  = 0.18,
    height = 0,
    shape  = 21,
    stroke = 0.2
  ) +
  
  # Zero reference line
  geom_hline(
    yintercept = 0,
    color      = "black",
    linewidth  = 0.4,
    linetype   = "solid"
  ) +
  
  # Envelope lines — upper and lower
  geom_line(
    data      = all_envelopes,
    aes(x = stream_order, y = upper_whisker,
        color = scenario, linetype = scenario),
    linewidth = 0.85
  ) +
  geom_line(
    data      = all_envelopes,
    aes(x = stream_order, y = lower_whisker,
        color = scenario, linetype = scenario),
    linewidth = 0.85
  ) +
  
  # --- Scales -----------------------------------------------------------------
scale_x_continuous(
  breaks = 4:7,
  labels = as.character(4:7),
  expand = expansion(add = 0.5)
) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-100, 100, by = 25),
    expand = expansion(mult = 0.02),
    labels = function(x) paste0(x, "%")
  ) +
  scale_color_manual(
    name   = "Null simulation (independent populations)",
    values = scenario_colors,
    labels = scenario_labels
  ) +
  scale_linetype_manual(
    name   = "Null simulation (independent populations)",
    values = scenario_linetypes,
    labels = scenario_labels
  ) +
  
  # --- Labels -----------------------------------------------------------------
labs(
  title    = "Variance Dampening Across Spatial Scales \u2014 Kuskokwim Chinook",
  subtitle = "Each point = one ReachBase drainage's inter-annual % change in cumulative production (15 year-pairs, 2017\u20132022)\nLines = whisker bounds of null simulation under independent-population assumption",
  x        = "Stream order",
  y        = "Inter-annual variability (% difference)"
) +
  
  # --- Theme ------------------------------------------------------------------
theme_classic(base_size = 12) +
  theme(
    # Titles
    plot.title    = element_text(size = 12, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 8.5, color = "grey40",
                                 lineheight = 1.4, margin = margin(b = 12)),
    plot.margin   = margin(14, 16, 12, 14),
    
    # Axes
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 10, color = "black"),
    axis.line     = element_line(color = "black", linewidth = 0.4),
    axis.ticks    = element_line(color = "black", linewidth = 0.4),
    
    # Panel
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # Legend
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.key.width  = unit(1.8, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.margin     = margin(t = 6)
  ) +
  
  guides(
    color    = guide_legend(override.aes = list(linewidth = 1.2)),
    linetype = guide_legend(override.aes = list(linewidth = 1.2))
  )

print(p)

# --- Export ------------------------------------------------------------------
out_path <- here("Figures", "Kusko_VarianceDampening.png")
dir.create(here("Figures"), showWarnings = FALSE)

ggsave(
  filename = out_path,
  plot     = p,
  width    = 9,
  height   = 6,
  dpi      = 320,
  bg       = "white"
)

cat("\nFigure saved to:", out_path, "\n")
cat("Done.\n")