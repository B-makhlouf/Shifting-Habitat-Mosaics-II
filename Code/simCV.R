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
#   the spread of that variability at each stream order against a null simulation
#   where every reach fluctuates independently at the observed basin-wide CV.
#
#   Points inside the simulation envelope = variance dampening consistent with
#   independent populations.
#   Points outside = more volatile than independence alone would produce,
#   suggesting within-drainage synchrony.
#
# TWO SHAPEFILES — BRIDGED BY SPATIAL JOIN:
#
#   kusk_edges     = Kusko_upstream.shp  (NETWORK shapefile)
#     Has rid, reachid, and the network topology needed by the traversal
#     function. Must be named kusk_edges — the function uses it by name.
#     Returns reachids from THIS shapefile only.
#
#   kusko_analysis = Kusko_edges.shp  (ANALYSIS shapefile)
#     Has Reachbase, Str_Order, geometry, and reachids that match the
#     production CSVs. Used for everything analytical.
#
#   Because reachid values may differ between the two shapefiles, we bridge
#   them with a spatial join (st_equals) — matching reaches by identical
#   geometry. This gives a lookup table (network_reachid <-> analysis_reachid)
#   used in every accumulation step.
#
################################################################################


# ==============================================================================
# SECTION 1: LIBRARIES
# ==============================================================================

library(sf)      # shapefiles, spatial joins, reach length calculation
library(dplyr)   # data manipulation
library(tidyr)   # pivoting
library(ggplot2) # plotting
library(here)    # relative file paths
library(readr)   # reading production CSVs
library(readxl)  # reading escapement data


# ==============================================================================
# SECTION 2: LOAD SPATIAL DATA AND NETWORK TOPOLOGY
# ==============================================================================

cat("Loading spatial data...\n")

# NETWORK shapefile — used exclusively by FindUpstreamReachID_Kusk()
# Must be named kusk_edges in the environment
kusk_edges <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_upstream.shp"),
  quiet = TRUE
)

# ANALYSIS shapefile — has Reachbase, Str_Order, geometry, and reachids
# that match the production CSVs
kusko_analysis <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  quiet = TRUE
)

# Node relationship table — defines river network topology (parent/child nodes)
KuskoNodes <- read.csv(
  here("Data", "UpstreamReaches", "kusko_noderelationships.csv"),
  stringsAsFactors = FALSE
)

KuskoNetwork <- KuskoNodes %>%
  rename(child_s = fromnode, parent_s = tonode)

# Compute reach lengths from analysis shapefile geometry (used for l/L in simulation)
kusko_analysis <- kusko_analysis %>%
  mutate(reach_length_m = as.numeric(st_length(geometry)))

total_basin_length_m <- sum(kusko_analysis$reach_length_m, na.rm = TRUE)

cat("  Network shapefile (traversal):", nrow(kusk_edges), "reaches\n")
cat("  Analysis shapefile (production):", nrow(kusko_analysis), "reaches\n")
cat("  Total basin length:", round(total_basin_length_m / 1000, 1), "km\n")


# ==============================================================================
# SECTION 3: BUILD SPATIAL LOOKUP TABLE
#
# The traversal function returns reachids from kusk_edges, but our production
# data uses reachids from kusko_analysis. These may not match directly.
#
# We bridge them by spatial join using st_equals — matching reaches that
# occupy exactly the same geometry in space. Same approach as UpstreamTribGroups.R.
#
# Result: a two-column lookup table
#   network_reachid  = reachid from kusk_edges (returned by traversal function)
#   analysis_reachid = reachid from kusko_analysis (used to filter production)
#
# Both lookup directions (network->analysis and analysis->network) use this
# same table — we just filter on whichever column we need.
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
cat("  Network reaches with no geometry match:",
    nrow(kusk_edges) - nrow(network_to_analysis), "\n")


# ==============================================================================
# SECTION 4: UPSTREAM TRAVERSAL FUNCTION
#
# Given a reachid from kusk_edges, walks the network topology upstream and
# returns all reachids that drain into it (also from kusk_edges).
#
# Requires kusk_edges and KuskoNetwork to be loaded in the environment.
# ==============================================================================

FindUpstreamReachID_Kusk <- function(ReachID) {
  
  TribStartRID <- kusk_edges$rid[kusk_edges$reachid == ReachID]
  
  if (length(TribStartRID) != 1) {
    warning(paste("ReachID", ReachID, "does not resolve to a unique rid — skipping"))
    return(integer(0))
  }
  
  TRIBindex <- KuskoNetwork$child_s[KuskoNetwork$rid == TribStartRID]
  
  if (length(TRIBindex) == 0) return(integer(0))
  
  ChildList <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% TRIBindex]
  
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% ChildList]
  }
  
  upstream_rids     <- KuskoNetwork$rid[match(TRIBindex, KuskoNetwork$child_s)]
  upstream_reachids <- kusk_edges$reachid[match(upstream_rids, kusk_edges$rid)]
  upstream_reachids <- upstream_reachids[!is.na(upstream_reachids)]
  
  return(upstream_reachids)
}


# ==============================================================================
# SECTION 5: IDENTIFY REACHBASE REACHES
#
# ReachBase reaches are the mouths of tributary systems — the last reach at
# a given stream order before the order increases. Each one represents one
# "drainage unit" at its stream order.
#
# Reachbase and Str_Order live on kusko_analysis (Kusko_edges.shp).
# Reachbase values range from 4-7 in this dataset.
# ==============================================================================

cat("\nIdentifying ReachBase reaches...\n")

reachbase_reaches <- kusko_analysis %>%
  st_drop_geometry() %>%
  filter(Reachbase >= 4, Reachbase <= 7, !is.na(Str_Order)) %>%
  select(reachid, Str_Order, Reachbase) %>%
  rename(stream_order = Str_Order)

n_reachbases <- nrow(reachbase_reaches)

cat("  Total ReachBase reaches:", n_reachbases, "\n")
cat("  By stream order:\n")
print(table(reachbase_reaches$stream_order))


# ==============================================================================
# SECTION 6: LOAD PRODUCTION DATA (2017-2022)
#
# Each year's CSV has one row per reach with assignment_rescale (sums to 1
# across the basin). Reachids match kusko_analysis, not kusk_edges.
# ==============================================================================

cat("\nLoading production data...\n")

years    <- c(2017, 2018, 2019, 2020, 2021, 2022)
prod_dir <- here("Outputs", "ProductionData", "Kusko")

prod_list <- list()

for (yr in years) {
  cat("  Loading", yr, "...\n")
  prod_list[[as.character(yr)]] <- read_csv(
    file.path(prod_dir, paste0(yr, "_Kusko_Assignment_Results.csv")),
    show_col_types = FALSE
  ) %>%
    select(reachid, assignment_rescale) %>%
    rename(!!paste0("prod_", yr) := assignment_rescale)
}

# Wide format: one row per reach, one column per year
prod_wide_all <- prod_list[[1]]
for (yr in years[-1]) {
  prod_wide_all <- prod_wide_all %>%
    left_join(prod_list[[as.character(yr)]], by = "reachid")
}

cat("  Loaded:", nrow(prod_wide_all), "reaches x", length(years), "years\n")


# ==============================================================================
# SECTION 7: ACCUMULATE PRODUCTION DOWNSTREAM PER REACHBASE DRAINAGE
#
# For each ReachBase reach:
#   1. Translate its analysis reachid -> network reachid (via lookup table)
#   2. Call FindUpstreamReachID_Kusk() to get all upstream network reachids
#   3. Translate those network reachids -> analysis reachids (via lookup table)
#   4. Sum production across all matched analysis reaches for each year
#
# Result: one row per ReachBase, one cumulative production value per year.
# This represents "total production coming out of this entire drainage."
# ==============================================================================

cat("\nAccumulating production for each ReachBase drainage...\n")
cat("  (This may take several minutes)\n\n")

accumulated_prod <- data.frame()

for (i in 1:n_reachbases) {
  
  rb_analysis_reachid <- reachbase_reaches$reachid[i]
  rb_streamorder      <- reachbase_reaches$stream_order[i]
  
  if (i %% 50 == 0) cat("  Processing", i, "of", n_reachbases, "...\n")
  
  # Step 1: Translate ReachBase analysis reachid -> network reachid
  rb_network_reachid <- network_to_analysis %>%
    filter(analysis_reachid == rb_analysis_reachid) %>%
    pull(network_reachid)
  
  if (length(rb_network_reachid) != 1) next
  
  # Step 2: Get all upstream network reachids
  upstream_network_ids <- FindUpstreamReachID_Kusk(rb_network_reachid)
  all_network_ids      <- unique(c(rb_network_reachid, upstream_network_ids))
  
  # Step 3: Translate network reachids -> analysis reachids
  analysis_ids <- network_to_analysis %>%
    filter(network_reachid %in% all_network_ids) %>%
    pull(analysis_reachid) %>%
    unique()
  
  if (length(analysis_ids) == 0) next
  
  # Step 4: Sum production across all analysis reaches in this drainage
  drainage_prod <- prod_wide_all %>%
    filter(reachid %in% analysis_ids) %>%
    summarise(across(starts_with("prod_"), ~ sum(.x, na.rm = TRUE)))
  
  accumulated_prod <- bind_rows(
    accumulated_prod,
    data.frame(
      reachbase_id       = rb_analysis_reachid,
      stream_order       = rb_streamorder,
      n_analysis_reaches = length(analysis_ids),
      drainage_prod
    )
  )
}

cat("\n  Done! Accumulated production for", nrow(accumulated_prod), "drainage areas\n")


# ==============================================================================
# SECTION 8: COMPUTE PAIRWISE INTER-ANNUAL % CHANGES
#
# For each ReachBase drainage and each of the 15 year pairs:
#   % change = (prod_year_i - prod_year_j) / prod_year_j * 100
#
# Each drainage contributes 15 values. These become the dots in the figure,
# plotted at the drainage's stream order.
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
      year_i          = yr_i,
      year_j          = yr_j,
      interannual_pct = (prod_i - prod_j) / prod_j * 100
    ) %>%
    filter(prod_j > 0, is.finite(interannual_pct))
}

pairwise_all <- bind_rows(pairwise_list)

cat("  Total observations:", nrow(pairwise_all),
    "(", n_distinct(pairwise_all$reachbase_id), "drainages x",
    length(year_pairs), "year pairs)\n")


# ==============================================================================
# SECTION 9: NULL SIMULATION — INDEPENDENT POPULATIONS
#
# Simulates 20 years of production where every reach fluctuates independently,
# using the empirical basin CV as the per-reach variability parameter.
#
# For each reach:
#   - Expected mean production = reach_length / total_basin_length (l/L)
#   - Production drawn from log-normal with that mean and CV = basin_cv
#   - Each reach draws independently — no shared drivers, no synchrony
#
# We then run the same downstream accumulation on the simulated data and
# compute pairwise % changes. The whisker bounds (Q1-1.5*IQR, Q3+1.5*IQR)
# at each stream order become the dotted lines on the figure.
#
# Log-normal parameterization:
#   sigma_log = sqrt(log(CV^2 + 1))
#   mu_log    = log(mean) - 0.5 * sigma_log^2  <- ensures E[X] = l/L exactly
# ==============================================================================

cat("\nRunning null simulation...\n")

set.seed(42)
n_sim_years <- 20

# --- Empirical basin CV ---
escapement <- read_excel(here("Data", "AYKEscapement.xlsx"))

kusko_esc <- escapement %>%
  filter(River == "Kusko", Year %in% years) %>%
  pull(Total_Run)

basin_cv <- sd(kusko_esc) / mean(kusko_esc)

cat("  Empirical basin CV:", round(basin_cv, 3), "\n")

# --- Length fraction (l/L) per analysis reach ---
reach_length_fractions <- kusko_analysis %>%
  st_drop_geometry() %>%
  select(reachid, reach_length_m) %>%
  mutate(length_fraction = reach_length_m / total_basin_length_m)

# --- Simulate independent production ---
sigma_log     <- sqrt(log(basin_cv^2 + 1))
n_sim_reaches <- nrow(reach_length_fractions)

cat("  Simulating", n_sim_reaches, "reaches x", n_sim_years, "years...\n")

sim_matrix <- matrix(NA, nrow = n_sim_reaches, ncol = n_sim_years)

for (i in 1:n_sim_reaches) {
  mu_i <- reach_length_fractions$length_fraction[i]
  if (mu_i <= 0) { sim_matrix[i, ] <- 0; next }
  mu_log_i        <- log(mu_i) - 0.5 * sigma_log^2
  sim_matrix[i, ] <- rlnorm(n_sim_years, meanlog = mu_log_i, sdlog = sigma_log)
}

# Normalize each year to sum to 1 (matches empirical structure)
for (col in 1:n_sim_years) {
  col_sum <- sum(sim_matrix[, col], na.rm = TRUE)
  if (col_sum > 0) sim_matrix[, col] <- sim_matrix[, col] / col_sum
}

# Store with analysis reachids for lookup
sim_df           <- as.data.frame(sim_matrix)
colnames(sim_df) <- paste0("simyr_", 1:n_sim_years)
sim_df$reachid   <- reach_length_fractions$reachid

# --- Accumulate simulated production through each ReachBase drainage ---
# Identical logic to Section 7 — same lookup table, same translation steps
sim_accumulated <- data.frame()

for (i in 1:n_reachbases) {
  
  rb_analysis_reachid <- reachbase_reaches$reachid[i]
  rb_streamorder      <- reachbase_reaches$stream_order[i]
  
  rb_network_reachid <- network_to_analysis %>%
    filter(analysis_reachid == rb_analysis_reachid) %>%
    pull(network_reachid)
  
  if (length(rb_network_reachid) != 1) next
  
  upstream_network_ids <- FindUpstreamReachID_Kusk(rb_network_reachid)
  all_network_ids      <- unique(c(rb_network_reachid, upstream_network_ids))
  
  analysis_ids <- network_to_analysis %>%
    filter(network_reachid %in% all_network_ids) %>%
    pull(analysis_reachid) %>%
    unique()
  
  if (length(analysis_ids) == 0) next
  
  drainage_sim <- sim_df %>%
    filter(reachid %in% analysis_ids) %>%
    summarise(across(starts_with("simyr_"), ~ sum(.x, na.rm = TRUE)))
  
  sim_accumulated <- bind_rows(
    sim_accumulated,
    data.frame(reachbase_id = rb_analysis_reachid, stream_order = rb_streamorder, drainage_sim)
  )
}

# --- Pairwise % changes from simulated data ---
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

sim_pw_all <- bind_rows(sim_pw_list)

# --- Simulation envelope: whisker bounds per stream order ---
sim_envelope <- sim_pw_all %>%
  group_by(stream_order) %>%
  summarise(
    Q1            = quantile(pct_change, 0.25, na.rm = TRUE),
    Q3            = quantile(pct_change, 0.75, na.rm = TRUE),
    IQR           = Q3 - Q1,
    lower_whisker = Q1 - 1.5 * IQR,
    upper_whisker = Q3 + 1.5 * IQR,
    .groups       = "drop"
  )

cat("  Simulation complete.\n")


# ==============================================================================
# SECTION 10: PLOT AND EXPORT
#
# Each dot = one ReachBase drainage area's inter-annual % change for one year
# pair, plotted at its stream order (15 dots per drainage).
#
# Dotted lines = null simulation envelope. Points inside = portfolio buffering
# consistent with independence. Points outside = within-drainage synchrony.
# ==============================================================================

cat("\nBuilding figure...\n")

y_limit <- 100

plot_points <- pairwise_all %>%
  mutate(interannual_pct = pmax(pmin(interannual_pct, y_limit), -y_limit))

p <- ggplot() +
  
  # Violin plots — show distribution shape at each stream order
  # Drawn first so dots render on top
  geom_violin(
    data      = plot_points,
    aes(x = stream_order, y = interannual_pct, group = stream_order),
    fill      = "#AF7A6D",
    color     = "#AF7A6D",
    alpha     = 0.15,
    linewidth = 0.4,
    width     = 0.6
  ) +
  
  # Empirical dots — one per drainage per year pair, plotted over violins
  geom_jitter(
    data  = plot_points,
    aes(x = stream_order, y = interannual_pct),
    color = "#AF7A6D",
    alpha = 0.2,
    size  = 2.2,
    width = 0.15
  ) +
  
  # Simulation envelope — upper whisker
  geom_line(
    data      = sim_envelope,
    aes(x = stream_order, y = upper_whisker),
    linetype  = "dotted",
    linewidth = 1.0,
    color     = "black"
  ) +
  
  # Simulation envelope — lower whisker
  geom_line(
    data      = sim_envelope,
    aes(x = stream_order, y = lower_whisker),
    linetype  = "dotted",
    linewidth = 1.0,
    color     = "black"
  ) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  
  scale_x_continuous(breaks = 4:7, labels = 4:7) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-100, 100, by = 50)
  ) +
  
  labs(
    title    = "Kuskokwim Chinook - Variance Dampening Across Spatial Scales",
    subtitle = paste0(
      "Each point = one ReachBase drainage's inter-annual % change in cumulative production\n",
      "(15 year-pairs, 2017-2022) | Dotted lines = null simulation envelope",
      " (independent populations, empirical basin CV = ", round(basin_cv, 2), ")"
    ),
    x = "Stream Order",
    y = "Inter-annual variability (% difference)"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 9),
    panel.grid    = element_blank()
  )

print(p)

# --- Export ---
out_path <- here("Figures", "Kusko_VarianceDampening.png")
dir.create(here("Figures"), showWarnings = FALSE)

ggsave(
  filename = out_path,
  plot     = p,
  width    = 8,
  height   = 6,
  dpi      = 300
)

cat("\nFigure saved to:", out_path, "\n")
cat("Done.\n")