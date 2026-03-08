################################################################################
# COMBINED YUKON + KUSKOKWIM — TEMPERATURE vs SLOPE CONTOUR PLOTS
#
# Goal: Create contour plots showing temperature vs slope relationships for
#       high-productivity habitat (normalized production >= 0.7) from BOTH
#       basins on the same plot.
#       Uses first 50% of CPUE run timing.
#       Temperature sampling every 3 days (Blaskey NetCDF stream temp).
#       SNAP air temperature from shapefile columns (SnapTp20XX).
#
#       An AVERAGE contour (pooled across all years) is overlaid on each panel
#       as black contour lines to show the multi-year central tendency.
#
# Output: 2-column × N-year panel figure:
#         Column 1 = Stream Temperature (Blaskey NetCDF) vs Channel Slope
#         Column 2 = SNAP Air Temperature vs Channel Slope
#         Years as rows, year labels on left
#         Black average contour lines overlaid on every panel
#
# QC NOTE: This script is intentionally written without helper functions so
#          that every step can be inspected line by line. All processing is
#          done inline with explicit variable names and print statements.
################################################################################


# ==============================================================================
# LIBRARIES
# ==============================================================================
suppressPackageStartupMessages({
  library(ncdf4)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(tibble)
  library(here)
  library(ggplot2)
  library(patchwork)
  library(RColorBrewer)
  library(conflicted)
})

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# ==============================================================================
# PATHS
# ==============================================================================
PATHS <- list(

  # Kuskokwim shapefiles
  kusko_edges  = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  kusko_basin  = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),

  # Yukon shapefiles
  yukon_edges  = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges.shp"),
  yukon_basin  = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  yukon_ly_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_lYGen.shp"),
  yukon_my_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_mYGen.shp"),
  yukon_uy_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_uYGen.shp"),

  # NetCDF temperature directory (Blaskey hindcast; same folder covers both basins)
  nc_temp_dir = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "Production"),

  # Data inputs
  natal_data_dir = here("Data", "Natal Origins"),
  cpue_data_dir  = here("Data", "CPUE"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv"),

  # Outputs
  output_figures = here("Figures", "ContourPlots")
)

# Years with data in BOTH rivers
YEARS <- c(2017, 2018, 2019, 2021)

# Temperature sampling interval (days) — every 3rd day within the run window
TEMP_INTERVAL_DAYS <- 3

# Production threshold: only segments at or above this normalized value are used
PRODUCTION_THRESHOLD <- 0.7

# Basin-specific parameters for Bayesian isotope assignment
KUSKO_PARAMS <- list(
  min_stream_order      = 4,
  min_error             = 0.0006,
  sensitivity_threshold = 0.7
)

YUKON_PARAMS <- list(
  min_stream_order      = 4,
  min_error             = 0.0035,
  sensitivity_threshold = 0.7
)


# ==============================================================================
# LOAD DAILY GENETIC PROPORTIONS LOOKUP (Yukon only)
#
# This CSV has daily average genetic proportions (Lower / Middle / Upper) for
# each year and DOY. Fish in the Yukon dataset that are missing individual
# genetic assignments are imputed using these daily averages.
#
# Columns: sampleYear, DOY, genetic_assignment, n, proportion
# We pivot to wide format so each row is one year × DOY with columns
# avg_Lower, avg_Middle, avg_Upper.
# ==============================================================================
daily_gen_long <- read_csv(PATHS$daily_genetics, show_col_types = FALSE)

daily_gen_wide <- daily_gen_long %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(
    names_from  = genetic_assignment,
    values_from = proportion,
    values_fill = 0
  ) %>%
  rename(
    year       = sampleYear,
    avg_Lower  = Lower,
    avg_Middle = Middle,
    avg_Upper  = Upper
  )

cat("Daily genetics lookup loaded:", nrow(daily_gen_wide), "rows\n")


################################################################################
# PART 1: EXTRACT DAILY STREAM TEMPERATURE FROM NetCDF FILES
#
# Files cover both basins (same directory); basin separation happens later
# via COMID matching when joined to the shapefile data.
#
# NetCDF structure:
#   Variable : T_stream  [hru, no_seg, time]
#   hru      : reach/HRU identifiers (= COMID)
#   time     : days since January 1 of that file's year
#
# We keep only the downstream segment (no_seg index 2) and only June–July dates.
# Results are stacked into a single long data frame: COMID × date × temperature.
################################################################################

cat("\n================================================================\n")
cat("PART 1: EXTRACTING DAILY STREAM TEMPERATURE\n")
cat("================================================================\n")

# List all temperature NetCDF files that match the expected year pattern
nc_temp_files <- list.files(
  PATHS$nc_temp_dir,
  pattern    = "^\\d+_(2015|2016|2017|2018|2019|2020|2021)\\.nc$",
  full.names = TRUE
)

cat("Temperature NetCDF files found:", length(nc_temp_files), "\n")

# Initialize a list to hold one data frame per file
temp_daily_list <- vector("list", length(nc_temp_files))

# Loop through each NetCDF temperature file
for (i in seq_along(nc_temp_files)) {

  cat("  File", i, "of", length(nc_temp_files), ":", basename(nc_temp_files[i]), "\n")

  # Open file and pull out the three arrays we need
  nc        <- nc_open(nc_temp_files[i])
  vals      <- ncvar_get(nc, "T_stream")   # [hru, no_seg, time]
  reach_ids <- ncvar_get(nc, "hru")        # HRU/reach IDs (= COMIDs)
  time_vals <- ncvar_get(nc, "time")       # days since Jan 1 of this year
  nc_close(nc)

  # Parse the year from the filename (e.g., "12345_2017.nc" -> 2017)
  file_year <- as.numeric(sub(".*_(\\d{4})\\.nc$", "\\1", basename(nc_temp_files[i])))

  # Build a vector of calendar dates corresponding to the time dimension
  dates <- as.Date(paste0(file_year, "-01-01")) + time_vals

  # Use only the downstream segment (no_seg dimension 2, index position 2)
  # Result is a matrix: [hru, time]
  vals_downstream <- vals[, 2, ]

  # Identify which time steps fall in June (month 6) or July (month 7)
  june_july_idx <- which(month(dates) %in% 6:7)

  # Skip this file if it has no June/July data
  if (length(june_july_idx) == 0) {
    cat("    No June/July dates — skipping\n")
    next
  }

  dates_jj <- dates[june_july_idx]
  vals_jj  <- vals_downstream[, june_july_idx, drop = FALSE]  # [hru, june-july days]

  n_reach <- length(reach_ids)
  n_days  <- length(june_july_idx)

  # Flatten to a long data frame: one row per reach × date combination
  # rep(..., times = n_days)  repeats each reach ID once per time step
  # rep(..., each  = n_reach) repeats each date once per reach
  temp_daily_list[[i]] <- data.frame(
    COMID = rep(reach_ids, times = n_days),
    date  = rep(dates_jj,  each  = n_reach),
    value = as.vector(vals_jj)
  )

  cat("    Rows added:", n_reach * n_days, "\n")
}

# Combine all files; remove any duplicate COMID × date rows (should be rare)
temp_daily <- bind_rows(temp_daily_list) %>%
  distinct(COMID, date, .keep_all = TRUE)

cat("\nTotal temperature rows (all files combined):", nrow(temp_daily), "\n")


################################################################################
# PART 2: LOAD SPATIAL DATA
#
# Load stream network shapefiles for both basins.
# For Yukon: also load genetic LMU region shapefiles and tag each segment with
# its genetic assignment zone (lower / middle / upper / none). These zone
# indices are used as priors in the Bayesian assignment loop below.
################################################################################

cat("\n================================================================\n")
cat("PART 2: LOADING SPATIAL DATA\n")
cat("================================================================\n")

# --- Kuskokwim ---
kusko_edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
kusko_basin <- st_read(PATHS$kusko_basin, quiet = TRUE)

# Reproject edges to match basin CRS (ensures consistent coordinate system)
kusko_edges <- st_transform(kusko_edges, st_crs(kusko_basin))
kusko_shp   <- st_drop_geometry(kusko_edges)  # attribute table only (no geometry)

cat("Kuskokwim stream segments loaded:", nrow(kusko_edges), "\n")

# --- Yukon ---
yukon_edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
yukon_basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
yukon_edges <- st_transform(yukon_edges, st_crs(yukon_basin))
yukon_shp   <- st_drop_geometry(yukon_edges)

# Load the three genetic LMU region shapefiles
ly_gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
my_gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
uy_gen <- st_read(PATHS$yukon_uy_gen, quiet = TRUE)

# Tag each Yukon segment with its genetic LMU region
yukon_edges$GenLMU <- "none"
yukon_edges$GenLMU[yukon_edges$reachid %in% ly_gen$reachid] <- "lower"
yukon_edges$GenLMU[yukon_edges$reachid %in% my_gen$reachid] <- "middle"
yukon_edges$GenLMU[yukon_edges$reachid %in% uy_gen$reachid] <- "upper"

# Store row indices for each LMU — used as lookup vectors inside the fish loop
LYsites <- which(yukon_edges$GenLMU == "lower")
MYsites <- which(yukon_edges$GenLMU == "middle")
UYsites <- which(yukon_edges$GenLMU == "upper")

cat("Yukon stream segments loaded:", nrow(yukon_edges), "\n")
cat("  Lower:", length(LYsites), "| Middle:", length(MYsites),
    "| Upper:", length(UYsites), "\n")


# ==============================================================================
# COMPUTE LONG-TERM AVERAGE SNAP AIR TEMPERATURE PER REACH
#
# The shapefile contains one SNAP air temperature column per year
# (e.g., SnapTp2015 through SnapTp2021). We average across ALL available
# years to produce a single time-invariant mean temperature for each reach.
# This is used as the x-axis in Column 3 of the figure.
#
# Done here (before the year loop) because it does not vary by year —
# it is a fixed property of each reach across the full timeseries.
# Adding it directly to the sf objects means it flows through the year
# loop result tables automatically via st_drop_geometry().
# ==============================================================================

# Find all SnapTp year columns present in each shapefile
kusko_snap_all_cols <- grep("^SnapTp\\d{4}$", names(kusko_edges), value = TRUE)
yukon_snap_all_cols <- grep("^SnapTp\\d{4}$", names(yukon_edges), value = TRUE)

cat("Kuskokwim SNAP year columns found:", paste(kusko_snap_all_cols, collapse = ", "), "\n")
cat("Yukon SNAP year columns found:",     paste(yukon_snap_all_cols, collapse = ", "), "\n")

# Row-wise mean across all SNAP year columns — one value per reach
kusko_edges$SNAP_temp_avg <- rowMeans(
  st_drop_geometry(kusko_edges)[, kusko_snap_all_cols], na.rm = TRUE
)

yukon_edges$SNAP_temp_avg <- rowMeans(
  st_drop_geometry(yukon_edges)[, yukon_snap_all_cols], na.rm = TRUE
)

cat("Kuskokwim long-term avg SNAP temp range:",
    round(range(kusko_edges$SNAP_temp_avg, na.rm = TRUE), 2), "\n")
cat("Yukon long-term avg SNAP temp range:",
    round(range(yukon_edges$SNAP_temp_avg, na.rm = TRUE), 2), "\n")


################################################################################
# PART 3: YEAR LOOP — PRODUCTION + TEMPERATURE
#
# For each year we run the full pipeline independently for each basin, then
# combine the results.
#
# Steps per basin per year:
#   (a) Load natal origins data; filter out missing isotopes / CPUE
#   (b) Load CPUE curve; find the DOY at which 50% of the run has passed
#   (c) Filter natal observations to that first-50% window
#   (d) Compute 5-stratum weights (CPUE / otolith proportion ratio)
#   (e) Bayesian isotope assignment: for each fish, compute a Gaussian
#       likelihood across all segments × priors; normalize; apply threshold
#   (f) Sum weighted assignments across fish → normalized production per segment
#   (g) Match Blaskey stream temperatures to the date window (every 3 days)
#   (h) Join production, temperature, and SNAP air temperature into one table;
#       keep only segments at or above PRODUCTION_THRESHOLD
################################################################################

cat("\n================================================================\n")
cat("PART 3: RUNNING PRODUCTION + TEMPERATURE PER YEAR\n")
cat("================================================================\n")

# Storage: one entry per year, holding the combined Kusko + Yukon data frame
year_results <- list()

for (yr in YEARS) {

  cat("\n--- Year", yr, "---\n")

  # ============================================================================
  # (A) KUSKOKWIM
  # ============================================================================

  cat("\n  [KUSKOKWIM]\n")

  # --- Load natal origins ---
  # One row per fish; columns include natal_iso (otolith δ18O), dailyCPUEprop,
  # OtoPropDaily, and DOY
  kusko_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(yr, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))

  cat("    Raw natal observations:", nrow(kusko_natal_raw), "\n")

  # --- Load CPUE data and find the 50% run timing cutoff ---
  # cumCPUE is the cumulative CPUE through each date; we find the last date at
  # or before the halfway point of the total run
  kusko_cpue_raw <- read_csv(
    file.path(PATHS$cpue_data_dir, paste0("Kusko_CPUE_", yr, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Date), !is.na(cumCPUE))

  kusko_total_cpue  <- max(kusko_cpue_raw$cumCPUE, na.rm = TRUE)
  kusko_cutoff_date <- max(kusko_cpue_raw$Date[kusko_cpue_raw$cumCPUE <= kusko_total_cpue / 2])
  kusko_cutoff_doy  <- as.numeric(format(as.Date(kusko_cutoff_date), "%j"))

  cat("    50% CPUE cutoff DOY:", kusko_cutoff_doy, "\n")

  # Keep only fish that arrived before the 50% cutoff
  kusko_natal <- kusko_natal_raw %>% filter(DOY <= kusko_cutoff_doy)

  cat("    Natal observations (first 50% of run):", nrow(kusko_natal), "\n")

  # --- Compute 5-stratum weights ---
  # Strata are built from ALL sampling days (raw, not filtered), divided into
  # 5 equal-size groups by DOY. Each stratum's weight = CPUE sum / otolith sum.
  # Fish are then weighted by their stratum to correct for sampling imbalance.
  unique_days_k <- sort(unique(kusko_natal_raw$DOY))
  ndays_k       <- length(unique_days_k)
  strata_size_k <- ceiling(ndays_k / 5)

  day_strata_k <- tibble(
    DOY    = unique_days_k,
    strata = rep(1:5, each = strata_size_k, length.out = ndays_k)
  )

  strata_summary_k <- kusko_natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata_k, by = "DOY") %>%
    group_by(strata) %>%
    summarise(
      cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
      oto_sum  = sum(OtoPropDaily,  na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(weight = cpue_sum / oto_sum)

  # Attach stratum ID and weight to the filtered (50%) fish
  kusko_natal <- kusko_natal %>%
    left_join(day_strata_k,                                      by = "DOY")    %>%
    left_join(strata_summary_k %>% select(strata, weight),       by = "strata")

  # --- Bayesian isotope assignment: prepare priors and errors ---
  # kusko_pid_iso: predicted isoscape value for each segment (length = n segments)
  kusko_pid_iso   <- kusko_edges$iso_pred
  kusko_pid_isose <- kusko_edges$isose_pred

  # Floor the isoscape SE at a minimum threshold to avoid near-zero denominators
  kusko_pid_isose_mod <- ifelse(
    kusko_pid_isose < KUSKO_PARAMS$min_error,
    KUSKO_PARAMS$min_error,
    kusko_pid_isose
  )

  # Total assignment error: quadrature sum of spatial SE, measurement, and lab errors
  kusko_error <- sqrt(
    kusko_pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2
  )

  # Binary priors: 0 = exclude segment, 1 = allow assignment
  kusko_StreamOrderPrior <- ifelse(kusko_edges$Str_Order >= KUSKO_PARAMS$min_stream_order, 1, 0)
  kusko_PresencePrior    <- ifelse(
    (kusko_edges$Str_Order %in% c(7, 8)) & kusko_edges$SPAWNING_C == 0, 0, 1
  )
  kusko_NewHabitatPrior  <- ifelse(kusko_edges$Spawner_IP < 0.3, 0, 1)
  kusko_pid_prior        <- kusko_edges$UniPh2oNoE  # continuous spatial prior

  # --- Bayesian assignment loop ---
  # For each fish: Gaussian likelihood × all priors → normalize → threshold → weight
  n_kusko_segments        <- nrow(kusko_edges)
  n_kusko_fish            <- nrow(kusko_natal)
  kusko_assignment_matrix <- matrix(0, nrow = n_kusko_segments, ncol = n_kusko_fish)

  for (i in 1:n_kusko_fish) {

    fish_iso <- kusko_natal$natal_iso[i]

    # Gaussian likelihood: how well does each segment's predicted iso match this fish?
    assign <- (1 / sqrt(2 * pi * kusko_error^2)) *
      exp(-1 * (fish_iso - kusko_pid_iso)^2 / (2 * kusko_error^2)) *
      kusko_StreamOrderPrior * kusko_PresencePrior *
      kusko_pid_prior * kusko_NewHabitatPrior

    # Normalize so values sum to 1 across segments
    assign_norm     <- assign / sum(assign)

    # Re-scale so the best-matching segment = 1.0
    assign_rescaled <- assign_norm / max(assign_norm)

    # Zero out any segment below the sensitivity threshold
    assign_rescaled[assign_rescaled < KUSKO_PARAMS$sensitivity_threshold] <- 0

    # Weight the assignment by the fish's stratum weight and store
    kusko_assignment_matrix[, i] <- assign_rescaled * kusko_natal$weight[i]
  }

  # Sum the weighted assignments across all fish to get total production per segment
  kusko_basin_assign_sum <- apply(kusko_assignment_matrix, 1, sum, na.rm = TRUE)

  # Normalize production across segments so the highest = 1.0
  kusko_assign_norm <- kusko_basin_assign_sum / max(kusko_basin_assign_sum, na.rm = TRUE)

  cat("    Segments with production >= 0.7:", sum(kusko_assign_norm >= PRODUCTION_THRESHOLD), "\n")

  # --- Match stream temperatures to the date window ---
  # Determine the date range of fish in the (already filtered) natal data
  date_col_k             <- if ("date" %in% names(kusko_natal)) "date" else "Date"
  kusko_natal[[date_col_k]] <- as.Date(kusko_natal[[date_col_k]])
  kusko_date_range       <- range(kusko_natal[[date_col_k]], na.rm = TRUE)

  # Sample every TEMP_INTERVAL_DAYS days across that window
  kusko_date_seq <- seq(kusko_date_range[1], kusko_date_range[2], by = TEMP_INTERVAL_DAYS)

  # Pull temperature rows for those dates
  kusko_temp_subset <- temp_daily %>% filter(date %in% kusko_date_seq)

  # Average temperature per COMID across the sampled dates
  kusko_mean_temp <- kusko_temp_subset %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")

  # --- Assemble Kuskokwim result table ---
  # SNAP air temperature column name changes by year (e.g., "SnapTp2017")
  kusko_snap_temp_col <- paste0("SnapTp", yr)

  kusko_result <- st_drop_geometry(kusko_edges) %>%
    mutate(
      Production = kusko_assign_norm,
      Basin      = "Kuskokwim",
      year       = yr
    ) %>%
    left_join(kusko_mean_temp, by = "COMID") %>%
    rename(SNAP_temp = !!sym(kusko_snap_temp_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD)

  cat("    Reaches in final result (production >= 0.7):", nrow(kusko_result), "\n")


  # ============================================================================
  # (B) YUKON
  # ============================================================================

  cat("\n  [YUKON]\n")

  # --- Load raw natal data ---
  yukon_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(yr, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  )

  cat("    Raw natal observations:", nrow(yukon_natal_raw), "\n")

  # --- Impute missing genetic assignments from daily averages ---
  # Fish without individual genetic assignments get the population-average
  # proportions (Lower / Middle / Upper) for their sampling DOY from daily_gen_wide
  daily_gen_year <- daily_gen_wide %>% filter(year == yr)

  yukon_natal_raw <- yukon_natal_raw %>%
    left_join(
      daily_gen_year %>% select(DOY, avg_Lower, avg_Middle, avg_Upper),
      by = "DOY"
    ) %>%
    mutate(
      Lower  = ifelse(is.na(Lower),  avg_Lower,  Lower),
      Middle = ifelse(is.na(Middle), avg_Middle, Middle),
      Upper  = ifelse(is.na(Upper),  avg_Upper,  Upper)
    ) %>%
    select(-avg_Lower, -avg_Middle, -avg_Upper)

  # Remove rows still missing key fields after imputation
  yukon_natal_filtered <- yukon_natal_raw %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))

  # --- Load CPUE data and find 50% run timing cutoff ---
  yukon_cpue_raw <- read_csv(
    file.path(PATHS$cpue_data_dir, paste0("Yukon_CPUE_", yr, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Date), !is.na(cumCPUE))

  yukon_total_cpue  <- max(yukon_cpue_raw$cumCPUE, na.rm = TRUE)
  yukon_cutoff_date <- max(yukon_cpue_raw$Date[yukon_cpue_raw$cumCPUE <= yukon_total_cpue / 2])
  yukon_cutoff_doy  <- as.numeric(format(as.Date(yukon_cutoff_date), "%j"))

  cat("    50% CPUE cutoff DOY:", yukon_cutoff_doy, "\n")

  # Filter to first 50% of the run
  yukon_natal <- yukon_natal_filtered %>% filter(DOY <= yukon_cutoff_doy)

  cat("    Natal observations (first 50% of run):", nrow(yukon_natal), "\n")

  # --- Compute 5-stratum weights (same approach as Kuskokwim above) ---
  unique_days_y <- sort(unique(yukon_natal_raw$DOY))
  ndays_y       <- length(unique_days_y)
  strata_size_y <- ceiling(ndays_y / 5)

  day_strata_y <- tibble(
    DOY    = unique_days_y,
    strata = rep(1:5, each = strata_size_y, length.out = ndays_y)
  )

  strata_summary_y <- yukon_natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata_y, by = "DOY") %>%
    group_by(strata) %>%
    summarise(
      cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
      oto_sum  = sum(OtoPropDaily,  na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(weight = cpue_sum / oto_sum)

  yukon_natal <- yukon_natal %>%
    left_join(day_strata_y,                                      by = "DOY")    %>%
    left_join(strata_summary_y %>% select(strata, weight),       by = "strata")

  # --- Bayesian isotope assignment: prepare priors and errors ---
  yukon_pid_iso   <- yukon_edges$iso_pred
  yukon_pid_isose <- yukon_edges$isose_pred

  # Yukon uses the MEAN SE across all segments (rather than per-segment floor)
  yukon_pid_isose_mod <- rep(mean(yukon_pid_isose, na.rm = TRUE), length(yukon_pid_isose))
  yukon_error         <- sqrt(
    yukon_pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2
  )

  # Binary priors (Yukon differs slightly from Kusko in stream order range)
  yukon_StreamOrderPrior <- ifelse(yukon_edges$Str_Order >= YUKON_PARAMS$min_stream_order, 1, 0)
  yukon_PresencePrior    <- ifelse(
    (yukon_edges$Str_Order %in% c(6, 7, 8, 9)) & yukon_edges$SPAWNING_C == 0, 0, 1
  )
  yukon_newhabitatprior  <- ifelse(yukon_edges$Channel_sl > 2.3, 0, 1)

  # --- Bayesian assignment loop ---
  n_yukon_segments        <- nrow(yukon_edges)
  n_yukon_fish            <- nrow(yukon_natal)
  yukon_assignment_matrix <- matrix(0, nrow = n_yukon_segments, ncol = n_yukon_fish)

  for (i in 1:n_yukon_fish) {

    fish_iso <- yukon_natal$natal_iso[i]

    # Build the genetic prior for this fish:
    # Each segment gets the fish's probability for the LMU it belongs to.
    # Segments tagged "none" get 0 by default.
    gen_prior          <- rep(0, n_yukon_segments)
    gen_prior[LYsites] <- as.numeric(yukon_natal$Lower[i])
    gen_prior[MYsites] <- as.numeric(yukon_natal$Middle[i])
    gen_prior[UYsites] <- as.numeric(yukon_natal$Upper[i])

    # Gaussian likelihood × priors (genetic prior replaces the continuous spatial
    # prior used in Kuskokwim)
    assign <- (1 / sqrt(2 * pi * yukon_error^2)) *
      exp(-1 * (fish_iso - yukon_pid_iso)^2 / (2 * yukon_error^2)) *
      yukon_StreamOrderPrior * gen_prior * yukon_PresencePrior * yukon_newhabitatprior

    assign_norm     <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < YUKON_PARAMS$sensitivity_threshold] <- 0

    yukon_assignment_matrix[, i] <- assign_rescaled * yukon_natal$weight[i]
  }

  # Sum across fish
  yukon_basin_assign_sum <- apply(yukon_assignment_matrix, 1, sum, na.rm = TRUE)

  # Post-hoc spatial correction: Porcupine drainage segments get 30% weight.
  # Porc_off == 0 flags segments that should be down-weighted.
  yukon_basin_assign_sum <- ifelse(
    yukon_edges$Porc_off == 0,
    yukon_basin_assign_sum * 0.3,
    yukon_basin_assign_sum
  )

  # Normalize so the highest-production segment = 1.0
  yukon_assign_norm <- yukon_basin_assign_sum / max(yukon_basin_assign_sum, na.rm = TRUE)

  cat("    Segments with production >= 0.7:", sum(yukon_assign_norm >= PRODUCTION_THRESHOLD), "\n")

  # --- Match stream temperatures to the date window ---
  date_col_y             <- if ("date" %in% names(yukon_natal)) "date" else "Date"
  yukon_natal[[date_col_y]] <- as.Date(yukon_natal[[date_col_y]])
  yukon_date_range       <- range(yukon_natal[[date_col_y]], na.rm = TRUE)
  yukon_date_seq         <- seq(yukon_date_range[1], yukon_date_range[2], by = TEMP_INTERVAL_DAYS)

  yukon_temp_subset <- temp_daily %>% filter(date %in% yukon_date_seq)

  yukon_mean_temp <- yukon_temp_subset %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")

  # --- Assemble Yukon result table ---
  yukon_snap_temp_col <- paste0("SnapTp", yr)

  yukon_result <- st_drop_geometry(yukon_edges) %>%
    mutate(
      Production = yukon_assign_norm,
      Basin      = "Yukon",
      year       = yr
    ) %>%
    left_join(yukon_mean_temp, by = "COMID") %>%
    rename(SNAP_temp = !!sym(yukon_snap_temp_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD)

  cat("    Reaches in final result (production >= 0.7):", nrow(yukon_result), "\n")


  # ============================================================================
  # COMBINE BOTH BASINS FOR THIS YEAR
  # ============================================================================

  combined_result <- bind_rows(kusko_result, yukon_result)
  year_results[[as.character(yr)]] <- combined_result

  cat("\n  Combined reaches (both basins, production >= 0.7):", nrow(combined_result), "\n")
}


################################################################################
# PART 4: 2-COLUMN CONTOUR FIGURE
#
# Layout:
#   Rows    = one per year in YEARS
#   Column 1 = Stream Temperature (Blaskey NetCDF) vs Channel Slope
#   Column 2 = SNAP Air Temperature vs Channel Slope
#   Far left = year label panels (narrow)
#
# Each panel contains:
#   (1) A filled kernel density contour for that year (warm color palette)
#   (2) Black contour lines showing the density of the ALL-YEARS pooled data
#       — this is the "average" contour requested in QC feedback
#
# Panels are built one at a time inside a for loop (no helper functions) so
# each line is easy to inspect and trace back to a specific panel.
################################################################################

cat("\n================================================================\n")
cat("PART 4: BUILDING CONTOUR FIGURE\n")
cat("================================================================\n")

# ------------------------------------------------------------------
# Collect per-year data frames
# ------------------------------------------------------------------
filtered_list <- list()
for (yr in YEARS) {
  filtered_list[[as.character(yr)]] <- year_results[[as.character(yr)]]
  cat("  Year", yr, "—", nrow(filtered_list[[as.character(yr)]]), "rows\n")
}

# ------------------------------------------------------------------
# Pool all years: used for the average (cross-year) contour overlay
# Remove rows with NA in either axis variable before overlaying
# ------------------------------------------------------------------
all_years_combined <- bind_rows(filtered_list)

cat("\nAll-years combined rows (for average contour):", nrow(all_years_combined), "\n")

# Pre-filter the pooled data for each column variable (avoids NA warnings in ggplot)
avg_contour_stream <- all_years_combined %>%
  filter(!is.na(mean_summer_temp), !is.na(Channel_sl))

avg_contour_snap <- all_years_combined %>%
  filter(!is.na(SNAP_temp), !is.na(Channel_sl))

# Average SNAP temp column — same value per reach regardless of year,
# but different reaches appear per year depending on production threshold
avg_contour_snap_avg <- all_years_combined %>%
  filter(!is.na(SNAP_temp_avg), !is.na(Channel_sl))

cat("Average contour rows — stream temp:", nrow(avg_contour_stream),
    "| SNAP temp:", nrow(avg_contour_snap),
    "| avg SNAP temp:", nrow(avg_contour_snap_avg), "\n")

# ------------------------------------------------------------------
# Fixed axis limits — identical across all panels for direct comparability
# ------------------------------------------------------------------
x_lim_stream_temp <- c(5, 13)    # Blaskey stream temperature (°C)
x_lim_snap_temp   <- c(11, 17)   # SNAP air temperature, year-specific (°C)
y_lim_slope       <- c(0, 3)     # Channel slope (%)

# Long-term avg SNAP temp limits: derived from the data, rounded outward
x_lim_snap_avg <- c(
  floor(min(all_years_combined$SNAP_temp_avg,   na.rm = TRUE)),
  ceiling(max(all_years_combined$SNAP_temp_avg, na.rm = TRUE))
)
cat("Long-term avg SNAP temp axis limits:", x_lim_snap_avg, "\n")

# ------------------------------------------------------------------
# Color palette for filled density contours (YlOrRd, drop lightest bin)
# ------------------------------------------------------------------
fill_colors_warm <- brewer.pal(9, "YlOrRd")[-1]

# ------------------------------------------------------------------
# Shared base theme applied to every panel
# ------------------------------------------------------------------
base_theme <- theme_minimal() +
  theme(
    axis.text        = element_text(size = 16, color = "grey30"),
    axis.title       = element_blank(),
    legend.position  = "none",
    panel.grid.major = element_line(color = alpha("grey50", 0.3), linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.ontop      = TRUE,
    panel.background = element_rect(fill = NA, color = NA),
    plot.margin      = margin(2, 8, 2, 8),
    plot.title       = element_text(size = 18, face = "bold", hjust = 0.5,
                                    color = "grey10", margin = margin(b = 4))
  )

# ------------------------------------------------------------------
# Build all panels — one iteration per year
# Each iteration produces one Column 1 panel and one Column 2 panel.
# ------------------------------------------------------------------
plots_col1 <- list()  # Stream temperature vs slope
plots_col2 <- list()  # SNAP air temperature vs slope (year-specific)
plots_col3 <- list()  # Long-term average SNAP air temperature vs slope

for (i in seq_along(YEARS)) {

  yr        <- YEARS[i]
  df        <- filtered_list[[as.character(yr)]]
  is_top    <- (i == 1)            # top row: show column title
  is_bottom <- (i == length(YEARS))  # bottom row: show x-axis labels; include 0 on y

  cat("  Building panels for year", yr, "\n")

  # Y-axis breaks: suppress 0 on interior rows so tick labels don't crowd at seams
  y_breaks <- if (is_bottom) c(0, 1, 2, 3) else c(1, 2, 3)


  # ----------------------------------------------------------------
  # Column 1: STREAM TEMPERATURE (Blaskey NetCDF) vs Channel Slope
  # ----------------------------------------------------------------

  p1 <- ggplot(df, aes(x = mean_summer_temp, y = Channel_sl)) +

    # White background rectangle — drawn first so it sits behind the density fill
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +

    # Per-year filled density contour (warm palette, 6 density bins)
    # Fewer bins than default keeps the fill from looking too cluttered
    stat_density_2d_filled(bins = 6) +
    scale_fill_manual(values = fill_colors_warm) +

    # --- AVERAGE CONTOUR OVERLAY ---
    # Uses the all-years pooled data (avg_contour_stream).
    # Dark solid lines at 4 density levels — visible against both the light
    # yellow outer areas and the dark red core.
    geom_density_2d(
      data      = avg_contour_stream,
      aes(x     = mean_summer_temp, y = Channel_sl),
      color     = "grey40",
      linetype  = "solid",
      linewidth = 1.0,
      bins      = 4,
      alpha     = 0.5
    ) +

    scale_x_continuous(
      limits = x_lim_stream_temp,
      expand = c(0, 0),
      labels = if (is_bottom) waiver() else NULL   # x labels only on bottom row
    ) +
    scale_y_continuous(
      limits = y_lim_slope,
      expand = c(0, 0),
      breaks = y_breaks,
      labels = waiver()                            # y labels shown on left (col 1) always
    ) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (is_bottom)
        element_text(size = 16, color = "grey30")
      else
        element_blank(),
      axis.text.y = element_text(size = 16, color = "grey30")
    )

  # Column title appears only above the top row
  if (is_top) {
    p1 <- p1 + ggtitle("Stream Temp vs Slope")
  }

  plots_col1[[i]] <- p1


  # ----------------------------------------------------------------
  # Column 2: SNAP AIR TEMPERATURE vs Channel Slope
  # ----------------------------------------------------------------

  p2 <- ggplot(df, aes(x = SNAP_temp, y = Channel_sl)) +

    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +

    stat_density_2d_filled(bins = 6) +
    scale_fill_manual(values = fill_colors_warm) +

    # --- AVERAGE CONTOUR OVERLAY (SNAP air temperature) ---
    geom_density_2d(
      data      = avg_contour_snap,
      aes(x     = SNAP_temp, y = Channel_sl),
      color     = "grey40",
      linetype  = "solid",
      linewidth = 1.0,
      bins      = 4,
      alpha     = 0.5
    ) +

    scale_x_continuous(
      limits = x_lim_snap_temp,
      expand = c(0, 0),
      labels = if (is_bottom) waiver() else NULL
    ) +
    scale_y_continuous(
      limits = y_lim_slope,
      expand = c(0, 0),
      breaks = y_breaks,
      labels = NULL                               # y labels suppressed on right columns
    ) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (is_bottom)
        element_text(size = 16, color = "grey30")
      else
        element_blank(),
      axis.text.y = element_blank()
    )

  if (is_top) {
    p2 <- p2 + ggtitle("Air Temp vs Slope")
  }

  plots_col2[[i]] <- p2


  # ----------------------------------------------------------------
  # Column 3: LONG-TERM AVERAGE SNAP AIR TEMPERATURE vs Channel Slope
  #
  # x-axis is SNAP_temp_avg — the mean SNAP air temperature for each
  # reach averaged across ALL years in the shapefile timeseries.
  # This is time-invariant per reach; year-to-year variation in the
  # panel reflects which reaches clear the production threshold each year.
  # ----------------------------------------------------------------

  p3 <- ggplot(df, aes(x = SNAP_temp_avg, y = Channel_sl)) +

    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +

    stat_density_2d_filled(bins = 6) +
    scale_fill_manual(values = fill_colors_warm) +

    # --- AVERAGE CONTOUR OVERLAY (long-term avg SNAP temp) ---
    geom_density_2d(
      data      = avg_contour_snap_avg,
      aes(x     = SNAP_temp_avg, y = Channel_sl),
      color     = "grey40",
      linetype  = "solid",
      linewidth = 1.0,
      bins      = 4,
      alpha     = 0.5
    ) +

    scale_x_continuous(
      limits = x_lim_snap_avg,
      expand = c(0, 0),
      labels = if (is_bottom) waiver() else NULL
    ) +
    scale_y_continuous(
      limits = y_lim_slope,
      expand = c(0, 0),
      breaks = y_breaks,
      labels = NULL
    ) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (is_bottom)
        element_text(size = 16, color = "grey30")
      else
        element_blank(),
      axis.text.y = element_blank()
    )

  if (is_top) {
    p3 <- p3 + ggtitle("Avg Air Temp vs Slope")
  }

  plots_col3[[i]] <- p3
}


# ------------------------------------------------------------------
# Year label panels — one per row, narrow column on the far left
# ------------------------------------------------------------------
year_label_panels <- list()

for (i in seq_along(YEARS)) {
  yr <- YEARS[i]

  year_label_panels[[i]] <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = yr,
             hjust = 0.5, size = 8, fontface = "bold", color = "grey20") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}


# ------------------------------------------------------------------
# Assemble flat list in row-major order:
#   [ year label | col1 | col2 | col3 ]  repeated for each year
# ------------------------------------------------------------------
flat_list <- list()

for (i in seq_along(YEARS)) {
  flat_list <- c(flat_list, list(
    year_label_panels[[i]],
    plots_col1[[i]],
    plots_col2[[i]],
    plots_col3[[i]]
  ))
}

# Arrange with patchwork: 4 columns — year label (narrow) + 3 equal data columns
combined_plot <- wrap_plots(flat_list, ncol = 4,
                            widths = c(0.25, 1, 1, 1)) +
  plot_layout(heights = rep(1, length(YEARS)))


# ------------------------------------------------------------------
# Shared y-axis label: "Channel Slope", rotated 90°, placed far left
# ------------------------------------------------------------------
final_plot <- wrap_elements(combined_plot) +
  labs(tag = "Channel Slope") +
  theme(
    plot.tag          = element_text(size = 18, angle = 90, color = "grey20",
                                     face = "bold"),
    plot.tag.position = "left"
  )


# ------------------------------------------------------------------
# Shared x-axis caption — labels spaced to roughly align with each column
# ------------------------------------------------------------------
final_with_xlab <- final_plot +
  plot_annotation(
    caption = "Mean Summer Stream Temp (\u00B0C)                    SNAP Air Temp (\u00B0C)                    Long-term Avg Air Temp (\u00B0C)",
    theme   = theme(
      plot.caption = element_text(size = 16, hjust = 0.55, color = "grey20",
                                  margin = margin(t = 4))
    )
  )


# ------------------------------------------------------------------
# Save figure to disk
# ------------------------------------------------------------------
dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(PATHS$output_figures, "50pct_BothBasins.png"),
  plot   = final_with_xlab,
  width  = 14,    # 3 data columns + year label
  height = 12,
  dpi    = 300,
  bg     = "white"
)

print(final_with_xlab)

cat("\n================================================================\n")
cat("CONTOUR PLOTS COMPLETE\n")
cat("================================================================\n")
