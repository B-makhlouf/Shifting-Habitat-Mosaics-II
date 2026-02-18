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
# Output: 2-column × N-year panel figure:
#         Column 1 = Stream Temperature (Blaskey) vs Slope
#         Column 2 = SNAP Air Temperature vs Slope
#         Years as rows, year labels on left
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
  
  # NetCDF temperature directories
  kusko_nc_temp_dir = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "Production"),
  yukon_nc_temp_dir = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "Production"),
  
  # NetCDF discharge directory (mizuRoute output, shared across basins)
  nc_disch_dir = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "mizuRoute_Output"),
  
  # Data inputs
  natal_data_dir = here("Data", "Natal Origins"),
  cpue_data_dir  = here("Data", "CPUE"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv"),
  
  # Outputs
  output_figures = here("Figures", "ContourPlots")
)

# Years with data in BOTH rivers
YEARS <- c(2017, 2018, 2019, 2021)

# Temperature sampling interval (days)
TEMP_INTERVAL_DAYS <- 3

# Production threshold (normalized 0-1 scale)
PRODUCTION_THRESHOLD <- 0.7

# Basin-specific parameters
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
# Columns: sampleYear, DOY, genetic_assignment (Lower/Middle/Upper), n, proportion
# Used to impute genetic values for fish missing individual genetics
# ==============================================================================
daily_gen_long <- read_csv(PATHS$daily_genetics, show_col_types = FALSE)

daily_gen_wide <- daily_gen_long %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(names_from = genetic_assignment, values_from = proportion,
              values_fill = 0) %>%
  rename(year      = sampleYear,
         avg_Lower  = Lower,
         avg_Middle = Middle,
         avg_Upper  = Upper)


################################################################################
# PART 1: EXTRACT DAILY STREAM TEMPERATURE FROM NetCDF FILES
################################################################################

cat("\n================================================================\n")
cat("PART 1: EXTRACTING DAILY STREAM TEMPERATURE\n")
cat("================================================================\n")

# Function to extract temperature data for a basin
extract_temp_data <- function(nc_dir, basin_name) {
  nc_temp_files <- list.files(
    nc_dir,
    pattern = "^\\d+_(2015|2016|2017|2018|2019|2020|2021)\\.nc$",
    full.names = TRUE
  )
  
  cat("\n", basin_name, "temperature files found:", length(nc_temp_files), "\n")
  
  temp_daily_list <- vector("list", length(nc_temp_files))
  
  for (i in seq_along(nc_temp_files)) {
    nc <- nc_open(nc_temp_files[i])
    
    vals      <- ncvar_get(nc, "T_stream")  # [hru, no_seg, time]
    reach_ids <- ncvar_get(nc, "hru")
    time_vals <- ncvar_get(nc, "time")
    nc_close(nc)
    
    yr    <- as.numeric(sub(".*_(\\d{4})\\.nc$", "\\1", basename(nc_temp_files[i])))
    dates <- as.Date(paste0(yr, "-01-01")) + time_vals
    vals2 <- vals[, 2, ]  # downstream segment: [hru, time]
    jj    <- which(month(dates) %in% 6:7)
    
    if (length(jj) == 0) next
    
    dates_jj <- dates[jj]
    vals_jj  <- vals2[, jj, drop = FALSE]
    
    n_reach <- length(reach_ids)
    n_days  <- length(jj)
    
    temp_daily_list[[i]] <- data.frame(
      COMID = rep(reach_ids, times = n_days),
      date  = rep(dates_jj, each = n_reach),
      value = as.vector(vals_jj)
    )
  }
  
  temp_daily <- bind_rows(temp_daily_list) %>% 
    distinct(COMID, date, .keep_all = TRUE)
  
  cat("  ", basin_name, "temperature rows:", nrow(temp_daily), "\n")
  
  return(temp_daily)
}

# Extract temperature for both basins
kusko_temp_daily <- extract_temp_data(PATHS$kusko_nc_temp_dir, "Kuskokwim")
yukon_temp_daily <- extract_temp_data(PATHS$yukon_nc_temp_dir, "Yukon")


cat("\n================================================================\n")
cat("PART 1b: EXTRACTING DAILY DISCHARGE FROM NetCDF FILES\n")
cat("================================================================\n")

# Discharge files: variable = IRFroutedRunoff, dim = reachID
# Structure: [reach, time], origin date = 1989-06-01
# File pattern: AK_Rivers_*.h.(year).nc
extract_disch_data <- function(nc_dir, basin_name) {
  nc_disch_files <- list.files(
    nc_dir,
    pattern = "AK_Rivers_.*\\.h\\.(2015|2016|2017|2018|2019|2020|2021).*\\.nc$",
    full.names = TRUE
  )
  
  cat("\n", basin_name, "discharge files found:", length(nc_disch_files), "\n")
  
  origin_date <- as.Date("1989-06-01")
  all_data <- list()
  
  for (i in seq_along(nc_disch_files)) {
    nc        <- nc_open(nc_disch_files[i])
    vals      <- ncvar_get(nc, "IRFroutedRunoff")  # [reach, time]
    reach_ids <- ncvar_get(nc, "reachID")
    time_vals <- ncvar_get(nc, "time")
    nc_close(nc)
    
    dates <- origin_date + time_vals
    jj    <- which(month(dates) %in% 6:7)
    
    if (length(jj) == 0) next
    
    dates_jj    <- dates[jj]
    data_filtered <- vals[, jj]
    
    for (j in seq_along(reach_ids)) {
      all_data[[length(all_data) + 1]] <- data.frame(
        COMID = reach_ids[j],
        date  = dates_jj,
        value = data_filtered[j, ]
      )
    }
  }
  
  disch_daily <- bind_rows(all_data) %>%
    distinct(COMID, date, .keep_all = TRUE)
  
  cat("  ", basin_name, "discharge rows:", nrow(disch_daily), "\n")
  
  return(disch_daily)
}

# Discharge is a basin-wide model output — load once, shared across both basins
disch_daily <- extract_disch_data(PATHS$nc_disch_dir, "Both basins")


################################################################################
# PART 2: LOAD SPATIAL DATA
################################################################################

cat("\n================================================================\n")
cat("PART 2: LOADING SPATIAL DATA\n")
cat("================================================================\n")

# Kuskokwim
kusko_edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
kusko_basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
kusko_edges <- st_transform(kusko_edges, st_crs(kusko_basin))
kusko_shp   <- st_drop_geometry(kusko_edges)

cat("  Kuskokwim stream segments:", nrow(kusko_edges), "\n")

# Yukon
yukon_edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
yukon_basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
yukon_edges <- st_transform(yukon_edges, st_crs(yukon_basin))
yukon_shp   <- st_drop_geometry(yukon_edges)

# Load genetic regions
ly_gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
my_gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
uy_gen <- st_read(PATHS$yukon_uy_gen, quiet = TRUE)

yukon_edges$GenLMU <- "none"
yukon_edges$GenLMU[yukon_edges$reachid %in% ly_gen$reachid] <- "lower"
yukon_edges$GenLMU[yukon_edges$reachid %in% my_gen$reachid] <- "middle"
yukon_edges$GenLMU[yukon_edges$reachid %in% uy_gen$reachid] <- "upper"

LYsites <- which(yukon_edges$GenLMU == "lower")
MYsites <- which(yukon_edges$GenLMU == "middle")
UYsites <- which(yukon_edges$GenLMU == "upper")

cat("  Yukon stream segments:", nrow(yukon_edges), "\n")
cat("    Lower:", length(LYsites), "| Middle:", length(MYsites), 
    "| Upper:", length(UYsites), "\n")


################################################################################
# PART 3: YEAR LOOP — PRODUCTION + TEMPERATURE
################################################################################

cat("\n================================================================\n")
cat("PART 3: RUNNING PRODUCTION + TEMPERATURE PER YEAR\n")
cat("================================================================\n")

year_results <- list()

for (yr in YEARS) {
  
  cat("\n--- Year", yr, "---\n")
  
  # ============================================================================
  # KUSKOKWIM
  # ============================================================================
  
  cat("\n  KUSKOKWIM:\n")
  
  # Load natal data
  kusko_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(yr, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  
  # Apply 50% CPUE cutoff
  kusko_cpue_raw <- read_csv(
    file.path(PATHS$cpue_data_dir, paste0("Kusko_CPUE_", yr, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Date), !is.na(cumCPUE))
  
  kusko_total_cpue <- max(kusko_cpue_raw$cumCPUE, na.rm = TRUE)
  kusko_cutoff_date <- max(kusko_cpue_raw$Date[kusko_cpue_raw$cumCPUE <= kusko_total_cpue/2])
  kusko_cutoff_doy <- as.numeric(format(as.Date(kusko_cutoff_date), "%j"))
  
  kusko_natal <- kusko_natal_raw %>% filter(DOY <= kusko_cutoff_doy)
  
  cat("    50% CPUE cutoff DOY:", kusko_cutoff_doy, "\n")
  cat("    Natal observations:", nrow(kusko_natal), "\n")
  
  # Stratum weights
  unique_days_k <- sort(unique(kusko_natal_raw$DOY))
  ndays_k       <- length(unique_days_k)
  strata_size_k <- ceiling(ndays_k / 5)
  day_strata_k  <- tibble(
    DOY    = unique_days_k,
    strata = rep(1:5, each = strata_size_k, length.out = ndays_k)
  )
  strata_summary_k <- kusko_natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata_k, by = "DOY") %>%
    group_by(strata) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
              oto_sum  = sum(OtoPropDaily,  na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = cpue_sum / oto_sum)
  kusko_natal <- kusko_natal %>%
    left_join(day_strata_k, by = "DOY") %>%
    left_join(strata_summary_k %>% select(strata, weight), by = "strata")
  
  # Calculate error and priors
  kusko_pid_iso    <- kusko_edges$iso_pred
  kusko_pid_isose  <- kusko_edges$isose_pred
  kusko_pid_isose_mod <- ifelse(kusko_pid_isose < KUSKO_PARAMS$min_error,
                                KUSKO_PARAMS$min_error, kusko_pid_isose)
  kusko_error <- sqrt(kusko_pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  kusko_StreamOrderPrior <- ifelse(kusko_edges$Str_Order >= KUSKO_PARAMS$min_stream_order, 1, 0)
  kusko_PresencePrior    <- ifelse((kusko_edges$Str_Order %in% c(7, 8)) &
                                     kusko_edges$SPAWNING_C == 0, 0, 1)
  kusko_NewHabitatPrior  <- ifelse(kusko_edges$Spawner_IP < 0.3, 0, 1)
  kusko_pid_prior        <- kusko_edges$UniPh2oNoE
  
  # Bayesian assignment
  n_kusko_segments <- nrow(kusko_edges)
  n_kusko_fish <- nrow(kusko_natal)
  kusko_assignment_matrix <- matrix(0, nrow = n_kusko_segments, ncol = n_kusko_fish)
  
  for (i in 1:n_kusko_fish) {
    fish_iso <- kusko_natal$natal_iso[i]
    
    assign <- (1 / sqrt(2 * pi * kusko_error^2)) *
      exp(-1 * (fish_iso - kusko_pid_iso)^2 / (2 * kusko_error^2)) *
      kusko_StreamOrderPrior * kusko_PresencePrior *
      kusko_pid_prior * kusko_NewHabitatPrior
    
    assign_norm     <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < KUSKO_PARAMS$sensitivity_threshold] <- 0
    
    kusko_assignment_matrix[, i] <- assign_rescaled * kusko_natal$weight[i]
  }
  
  kusko_basin_assign_sum <- apply(kusko_assignment_matrix, 1, sum, na.rm = TRUE)
  kusko_assign_norm <- kusko_basin_assign_sum / max(kusko_basin_assign_sum, na.rm = TRUE)
  
  n_above_threshold <- sum(kusko_assign_norm >= PRODUCTION_THRESHOLD)
  cat("    Segments with production >= 0.7:", n_above_threshold, "\n")
  
  # Temperature matching (every 3 days)
  date_col <- if ("date" %in% names(kusko_natal)) "date" else "Date"
  kusko_natal[[date_col]] <- as.Date(kusko_natal[[date_col]])
  kusko_date_range <- range(kusko_natal[[date_col]], na.rm = TRUE)
  kusko_date_seq <- seq(kusko_date_range[1], kusko_date_range[2], by = TEMP_INTERVAL_DAYS)
  
  kusko_temp_subset <- kusko_temp_daily %>%
    filter(date %in% kusko_date_seq)
  
  kusko_mean_temp <- kusko_temp_subset %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Build Kusko results — include SNAP temp and precip from shapefile
  kusko_snap_temp_col <- paste0("SnapTp", yr)
  kusko_snap_prec_col <- paste0("SnapPr", yr)
  
  # Mean discharge over the same date window (every 3 days)
  kusko_disch_subset <- disch_daily %>%
    filter(date %in% kusko_date_seq)
  
  kusko_mean_disch <- kusko_disch_subset %>%
    group_by(COMID) %>%
    summarise(mean_summer_disch = mean(value, na.rm = TRUE), .groups = "drop")
  
  kusko_result <- st_drop_geometry(kusko_edges) %>%
    mutate(
      Production = kusko_assign_norm,
      Basin = "Kuskokwim",
      year = yr
    ) %>%
    left_join(kusko_mean_temp, by = "COMID") %>%
    left_join(kusko_mean_disch, by = "COMID") %>%
    rename(SNAP_temp = !!sym(kusko_snap_temp_col),
           SNAP_prec = !!sym(kusko_snap_prec_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD)
  
  cat("    Reaches with production >= 0.7 and temperature:", nrow(kusko_result), "\n")
  
  
  # ============================================================================
  # YUKON
  # ============================================================================
  
  cat("\n  YUKON:\n")
  
  # Load natal data (raw, before filtering — needed for imputation and strata)
  yukon_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(yr, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  )
  
  # Impute missing genetics from daily averages
  daily_gen_year <- daily_gen_wide %>% filter(year == yr)
  yukon_natal_raw <- yukon_natal_raw %>%
    left_join(daily_gen_year %>% select(DOY, avg_Lower, avg_Middle, avg_Upper), by = "DOY") %>%
    mutate(
      Lower  = ifelse(is.na(Lower),  avg_Lower,  Lower),
      Middle = ifelse(is.na(Middle), avg_Middle, Middle),
      Upper  = ifelse(is.na(Upper),  avg_Upper,  Upper)
    ) %>%
    select(-avg_Lower, -avg_Middle, -avg_Upper)
  
  yukon_natal_filtered <- yukon_natal_raw %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  # Apply 50% CPUE cutoff
  yukon_cpue_raw <- read_csv(
    file.path(PATHS$cpue_data_dir, paste0("Yukon_CPUE_", yr, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Date), !is.na(cumCPUE))
  
  yukon_total_cpue  <- max(yukon_cpue_raw$cumCPUE, na.rm = TRUE)
  yukon_cutoff_date <- max(yukon_cpue_raw$Date[yukon_cpue_raw$cumCPUE <= yukon_total_cpue/2])
  yukon_cutoff_doy  <- as.numeric(format(as.Date(yukon_cutoff_date), "%j"))
  
  yukon_natal <- yukon_natal_filtered %>% filter(DOY <= yukon_cutoff_doy)
  
  cat("    50% CPUE cutoff DOY:", yukon_cutoff_doy, "\n")
  cat("    Natal observations:", nrow(yukon_natal), "\n")
  
  # Stratum weights
  unique_days_y <- sort(unique(yukon_natal_raw$DOY))
  ndays_y       <- length(unique_days_y)
  strata_size_y <- ceiling(ndays_y / 5)
  day_strata_y  <- tibble(
    DOY    = unique_days_y,
    strata = rep(1:5, each = strata_size_y, length.out = ndays_y)
  )
  strata_summary_y <- yukon_natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata_y, by = "DOY") %>%
    group_by(strata) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
              oto_sum  = sum(OtoPropDaily,  na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = cpue_sum / oto_sum)
  yukon_natal <- yukon_natal %>%
    left_join(day_strata_y, by = "DOY") %>%
    left_join(strata_summary_y %>% select(strata, weight), by = "strata")
  
  # Calculate error and priors
  yukon_pid_iso    <- yukon_edges$iso_pred
  yukon_pid_isose  <- yukon_edges$isose_pred
  yukon_pid_isose_mod <- rep(mean(yukon_pid_isose, na.rm = TRUE), length(yukon_pid_isose))
  yukon_error <- sqrt(yukon_pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  yukon_StreamOrderPrior <- ifelse(yukon_edges$Str_Order >= YUKON_PARAMS$min_stream_order, 1, 0)
  yukon_PresencePrior    <- ifelse((yukon_edges$Str_Order %in% c(6, 7, 8, 9)) &
                                     yukon_edges$SPAWNING_C == 0, 0, 1)
  yukon_newhabitatprior  <- ifelse(yukon_edges$Channel_sl > 2.3, 0, 1)
  
  # Bayesian assignment
  n_yukon_segments <- nrow(yukon_edges)
  n_yukon_fish     <- nrow(yukon_natal)
  yukon_assignment_matrix <- matrix(0, nrow = n_yukon_segments, ncol = n_yukon_fish)
  
  for (i in 1:n_yukon_fish) {
    fish_iso <- yukon_natal$natal_iso[i]
    
    gen_prior <- rep(0, n_yukon_segments)
    gen_prior[LYsites] <- as.numeric(yukon_natal$Lower[i])
    gen_prior[MYsites] <- as.numeric(yukon_natal$Middle[i])
    gen_prior[UYsites] <- as.numeric(yukon_natal$Upper[i])
    
    assign <- (1 / sqrt(2 * pi * yukon_error^2)) *
      exp(-1 * (fish_iso - yukon_pid_iso)^2 / (2 * yukon_error^2)) *
      yukon_StreamOrderPrior * gen_prior * yukon_PresencePrior * yukon_newhabitatprior
    
    assign_norm     <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < YUKON_PARAMS$sensitivity_threshold] <- 0
    
    yukon_assignment_matrix[, i] <- assign_rescaled * yukon_natal$weight[i]
  }
  
  yukon_basin_assign_sum <- apply(yukon_assignment_matrix, 1, sum, na.rm = TRUE)
  
  # Downweight Porcupine drainage segments post-hoc
  yukon_basin_assign_sum <- ifelse(yukon_edges$Porc_off == 0,
                                   yukon_basin_assign_sum * 0.3,
                                   yukon_basin_assign_sum)
  
  yukon_assign_norm <- yukon_basin_assign_sum / max(yukon_basin_assign_sum, na.rm = TRUE)
  
  n_above_threshold <- sum(yukon_assign_norm >= PRODUCTION_THRESHOLD)
  cat("    Segments with production >= 0.7:", n_above_threshold, "\n")
  
  # Temperature matching (every 3 days)
  date_col <- if ("date" %in% names(yukon_natal)) "date" else "Date"
  yukon_natal[[date_col]] <- as.Date(yukon_natal[[date_col]])
  yukon_date_range <- range(yukon_natal[[date_col]], na.rm = TRUE)
  yukon_date_seq <- seq(yukon_date_range[1], yukon_date_range[2], by = TEMP_INTERVAL_DAYS)
  
  yukon_temp_subset <- yukon_temp_daily %>%
    filter(date %in% yukon_date_seq)
  
  yukon_mean_temp <- yukon_temp_subset %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Build Yukon results — include SNAP temp and precip from shapefile
  yukon_snap_temp_col <- paste0("SnapTp", yr)
  yukon_snap_prec_col <- paste0("SnapPr", yr)
  
  # Mean discharge over the same date window (every 3 days)
  yukon_disch_subset <- disch_daily %>%
    filter(date %in% yukon_date_seq)
  
  yukon_mean_disch <- yukon_disch_subset %>%
    group_by(COMID) %>%
    summarise(mean_summer_disch = mean(value, na.rm = TRUE), .groups = "drop")
  
  yukon_result <- st_drop_geometry(yukon_edges) %>%
    mutate(
      Production = yukon_assign_norm,
      Basin = "Yukon",
      year = yr
    ) %>%
    left_join(yukon_mean_temp, by = "COMID") %>%
    left_join(yukon_mean_disch, by = "COMID") %>%
    rename(SNAP_temp = !!sym(yukon_snap_temp_col),
           SNAP_prec = !!sym(yukon_snap_prec_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD)
  
  cat("    Reaches with production >= 0.7 and temperature:", nrow(yukon_result), "\n")
  
  
  # ============================================================================
  # COMBINE BASINS
  # ============================================================================
  
  combined_result <- bind_rows(kusko_result, yukon_result)
  year_results[[as.character(yr)]] <- combined_result
  
  cat("\n  Combined reaches with production >= 0.7:", nrow(combined_result), "\n")
}



################################################################################
# PART 4: 4-COLUMN CONTOUR FIGURE
#   Column 1: Stream Temperature (Blaskey NetCDF) vs Channel Slope  [YlOrRd]
#   Column 2: SNAP Air Temperature vs Channel Slope                  [YlOrRd]
#   Column 3: Log10 Discharge (Blaskey NetCDF) vs Channel Slope      [Blues]
#   Column 4: Log10 SNAP Precipitation vs Channel Slope              [Blues]
#   Rows: one per year, year labels on left
################################################################################

cat("\n================================================================\n")
cat("PART 4: BUILDING CONTOUR FIGURE\n")
cat("================================================================\n")

# ------------------------------------------------------------------
# Prepare filtered data list
# ------------------------------------------------------------------
filtered_list <- lapply(YEARS, function(yr) {
  year_results[[as.character(yr)]]
})
names(filtered_list) <- as.character(YEARS)

# ------------------------------------------------------------------
# Global axis limits
# ------------------------------------------------------------------
x_lim_temp  <- c(5, 13)
y_lim_slope <- c(0, 3)
x_lim_air   <- c(11, 17)

# Derive discharge and precip limits from data (log10, positive values only)
all_combined <- bind_rows(filtered_list)

x_lim_disch <- range(
  log10(all_combined$mean_summer_disch[all_combined$mean_summer_disch > 0]),
  na.rm = TRUE
)
x_lim_prec  <- range(
  log10(all_combined$SNAP_prec[all_combined$SNAP_prec > 0]),
  na.rm = TRUE
)

# ------------------------------------------------------------------
# Color palettes
# ------------------------------------------------------------------
fill_colors_warm <- brewer.pal(9, "YlOrRd")[-1]   # temperature columns
fill_colors_blue <- brewer.pal(9, "Blues")[-1]      # discharge / precip columns

# ------------------------------------------------------------------
# Shared theme
# ------------------------------------------------------------------
base_theme <- theme_minimal() +
  theme(
    axis.text        = element_text(size = 8, color = "grey30"),
    axis.title       = element_blank(),
    legend.position  = "none",
    panel.grid.major = element_line(color = alpha("grey50", 0.3), linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.ontop      = TRUE,
    panel.background = element_rect(fill = NA, color = NA),
    plot.margin      = margin(1, 2, 1, 2),
    plot.title       = element_blank()
  )

# ------------------------------------------------------------------
# Helper: build one contour panel
# ------------------------------------------------------------------
make_panel <- function(df, x_var, x_lim, fill_colors,
                       show_x_labels, show_y_labels = TRUE) {
  p <- ggplot(df, aes(.data[[x_var]], Channel_sl)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +
    stat_density_2d_filled(bins = 8) +
    scale_fill_manual(values = fill_colors) +
    scale_x_continuous(
      limits = x_lim,
      expand = c(0, 0),
      labels = if (show_x_labels) waiver() else NULL
    ) +
    scale_y_continuous(
      limits = y_lim_slope,
      expand = c(0, 0),
      labels = if (show_y_labels) waiver() else NULL
    ) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (show_x_labels)
        element_text(size = 8, color = "grey30")
      else element_blank(),
      axis.text.y = if (show_y_labels)
        element_text(size = 8, color = "grey30")
      else element_blank()
    )
  p
}

# ------------------------------------------------------------------
# Add log-transformed discharge and precip to each year's data
# ------------------------------------------------------------------
filtered_list <- lapply(filtered_list, function(df) {
  df %>%
    mutate(
      log_disch = ifelse(mean_summer_disch > 0, log10(mean_summer_disch), NA_real_),
      log_prec  = ifelse(SNAP_prec > 0,         log10(SNAP_prec),         NA_real_)
    )
})

# ------------------------------------------------------------------
# Build all panels
# ------------------------------------------------------------------
plots_col1 <- lapply(seq_along(YEARS), function(i)
  make_panel(filtered_list[[i]], "mean_summer_temp", x_lim_temp,
             fill_colors_warm,
             show_x_labels = (i == length(YEARS)), show_y_labels = TRUE))

plots_col2 <- lapply(seq_along(YEARS), function(i)
  make_panel(filtered_list[[i]], "SNAP_temp", x_lim_air,
             fill_colors_warm,
             show_x_labels = (i == length(YEARS)), show_y_labels = FALSE))

plots_col3 <- lapply(seq_along(YEARS), function(i)
  make_panel(filtered_list[[i]], "log_disch", x_lim_disch,
             fill_colors_blue,
             show_x_labels = (i == length(YEARS)), show_y_labels = FALSE))

plots_col4 <- lapply(seq_along(YEARS), function(i)
  make_panel(filtered_list[[i]], "log_prec", x_lim_prec,
             fill_colors_blue,
             show_x_labels = (i == length(YEARS)), show_y_labels = FALSE))

# ------------------------------------------------------------------
# Year label panels
# ------------------------------------------------------------------
year_labels <- lapply(YEARS, function(yr) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = yr,
             hjust = 0.5, size = 4, fontface = "bold", color = "grey20") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
})

# ------------------------------------------------------------------
# Assemble — 5-column grid (year label | col1 | col2 | col3 | col4)
# ------------------------------------------------------------------
flat_list <- list()
for (i in seq_along(YEARS)) {
  flat_list <- c(flat_list, list(
    year_labels[[i]],
    plots_col1[[i]],
    plots_col2[[i]],
    plots_col3[[i]],
    plots_col4[[i]]
  ))
}

combined_plot <- wrap_plots(flat_list, ncol = 5,
                            widths = c(0.15, 1, 1, 1, 1)) +
  plot_layout(heights = rep(1, length(YEARS)))

# ------------------------------------------------------------------
# Column titles
# ------------------------------------------------------------------
combined_plot <- combined_plot +
  plot_annotation(
    title = "Stream Temp vs Slope        Air Temp vs Slope        Log Discharge vs Slope        Log Precip vs Slope",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5,
                                color = "grey10", margin = margin(b = 4))
    )
  )

# ------------------------------------------------------------------
# Shared y-axis label
# ------------------------------------------------------------------
final_plot <- wrap_elements(combined_plot) +
  labs(tag = "Channel Slope") +
  theme(
    plot.tag          = element_text(size = 11, angle = 90, color = "grey20"),
    plot.tag.position = "left"
  )

# ------------------------------------------------------------------
# Shared x-axis caption
# ------------------------------------------------------------------
final_with_xlab <- final_plot +
  plot_annotation(
    caption = "Mean Summer Stream Temp (\u00B0C)          SNAP Air Temp (\u00B0C)          Log\u2081\u2080 Discharge (m\u00B3/s)          Log\u2081\u2080 SNAP Precip (mm)",
    theme = theme(
      plot.caption = element_text(size = 9, hjust = 0.55, color = "grey20",
                                  margin = margin(t = 2))
    )
  )

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------
dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(PATHS$output_figures, "50pct_BothBasins.png"),
  plot   = final_with_xlab,
  width  = 14,
  height = 12,
  dpi    = 300,
  bg     = "white"
)

print(final_with_xlab)

cat("\n================================================================\n")
cat("CONTOUR PLOTS COMPLETE\n")
cat("================================================================\n")