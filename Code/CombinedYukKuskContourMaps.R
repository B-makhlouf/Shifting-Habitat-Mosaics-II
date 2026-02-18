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
#
# Production calculation matches regional analysis exactly:
#   - Strata-based CPUE weights (5 strata over full run, applied to 50% subset)
#   - Kusko: per-reach isose with 0.0006 floor + Spawner_IP prior
#   - Yukon: mean isose + Porc_off prior + genetic imputation from daily averages
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
  min_stream_order      = 5,
  sensitivity_threshold = 0.0
)


################################################################################
# PART 1: EXTRACT DAILY STREAM TEMPERATURE FROM NetCDF FILES
################################################################################

cat("\n================================================================\n")
cat("PART 1: EXTRACTING DAILY STREAM TEMPERATURE\n")
cat("================================================================\n")

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

kusko_temp_daily <- extract_temp_data(PATHS$kusko_nc_temp_dir, "Kuskokwim")
yukon_temp_daily <- extract_temp_data(PATHS$yukon_nc_temp_dir, "Yukon")


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

cat("  Kuskokwim stream segments:", nrow(kusko_edges), "\n")

# Yukon
yukon_edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
yukon_basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
yukon_edges <- st_transform(yukon_edges, st_crs(yukon_basin))

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

# ==============================================================================
# Load daily genetic proportions for Yukon imputation
# (computed on full run; applied before 50% CPUE filter)
# ==============================================================================
daily_gen_long <- read_csv(PATHS$daily_genetics, show_col_types = FALSE)

daily_gen_wide <- daily_gen_long %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(names_from  = genetic_assignment,
              values_from = proportion,
              values_fill = 0) %>%
  rename(year       = sampleYear,
         avg_Lower  = Lower,
         avg_Middle = Middle,
         avg_Upper  = Upper)


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
  
  # ── Load natal data ──────────────────────────────────────────────────────────
  kusko_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir,
              paste0(yr, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  
  # ── 50% CPUE cutoff ──────────────────────────────────────────────────────────
  kusko_cpue_raw <- read_csv(
    file.path(PATHS$cpue_data_dir, paste0("Kusko_CPUE_", yr, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Date), !is.na(cumCPUE))
  
  kusko_total_cpue  <- max(kusko_cpue_raw$cumCPUE, na.rm = TRUE)
  kusko_cutoff_date <- max(kusko_cpue_raw$Date[kusko_cpue_raw$cumCPUE <= kusko_total_cpue / 2])
  kusko_cutoff_doy  <- as.numeric(format(as.Date(kusko_cutoff_date), "%j"))
  
  cat("    50% CPUE cutoff DOY:", kusko_cutoff_doy, "\n")
  
  # ── Strata weights (built from FULL run, then applied to 50% subset) ─────────
  kusko_unique_days <- sort(unique(kusko_natal_raw$DOY))
  kusko_ndays       <- length(kusko_unique_days)
  kusko_strata_size <- ceiling(kusko_ndays / 5)
  
  kusko_day_strata <- tibble(
    DOY    = kusko_unique_days,
    strata = rep(1:5, each = kusko_strata_size, length.out = kusko_ndays)
  )
  
  kusko_strata_summary <- kusko_natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(kusko_day_strata, by = "DOY") %>%
    group_by(strata) %>%
    summarise(
      cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
      oto_sum  = sum(OtoPropDaily,  na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(weight = cpue_sum / oto_sum)
  
  # Apply 50% cutoff and join weights
  kusko_natal <- kusko_natal_raw %>%
    filter(DOY <= kusko_cutoff_doy) %>%
    left_join(kusko_day_strata,                              by = "DOY") %>%
    left_join(kusko_strata_summary %>% select(strata, weight), by = "strata")
  
  cat("    Natal observations (50% window):", nrow(kusko_natal), "\n")
  
  # ── Error (per-reach isose with floor) ───────────────────────────────────────
  kusko_pid_iso       <- kusko_edges$iso_pred
  kusko_pid_isose     <- kusko_edges$isose_pred
  kusko_pid_isose_mod <- ifelse(kusko_pid_isose < KUSKO_PARAMS$min_error,
                                KUSKO_PARAMS$min_error,
                                kusko_pid_isose)
  kusko_error <- sqrt(kusko_pid_isose_mod^2 +
                        (0.0003133684 / 1.96)^2 +
                        (0.00011 / 2)^2)
  
  # ── Priors ───────────────────────────────────────────────────────────────────
  kusko_StreamOrderPrior <- ifelse(kusko_edges$Str_Order >= KUSKO_PARAMS$min_stream_order, 1, 0)
  kusko_PresencePrior    <- ifelse((kusko_edges$Str_Order %in% c(6, 7)) &
                                     kusko_edges$SPAWNING_C == 0, 0, 1)
  kusko_NewHabitatPrior  <- ifelse(kusko_edges$Spawner_IP < 0.3, 0, 1)   # Spawner_IP, not Channel_sl
  kusko_pid_prior        <- kusko_edges$UniPh2oNoE
  
  # ── Bayesian assignment ──────────────────────────────────────────────────────
  n_kusko_segments        <- nrow(kusko_edges)
  n_kusko_fish            <- nrow(kusko_natal)
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
  kusko_assign_norm      <- kusko_basin_assign_sum / max(kusko_basin_assign_sum, na.rm = TRUE)
  
  cat("    Segments with production >= 0.7:",
      sum(kusko_assign_norm >= PRODUCTION_THRESHOLD), "\n")
  
  # ── Temperature matching (every 3 days, within 50% window dates) ─────────────
  date_col_k            <- if ("date" %in% names(kusko_natal)) "date" else "Date"
  kusko_natal[[date_col_k]] <- as.Date(kusko_natal[[date_col_k]])
  kusko_date_range      <- range(kusko_natal[[date_col_k]], na.rm = TRUE)
  kusko_date_seq        <- seq(kusko_date_range[1], kusko_date_range[2],
                               by = TEMP_INTERVAL_DAYS)
  
  kusko_mean_temp <- kusko_temp_daily %>%
    filter(date %in% kusko_date_seq) %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
  
  # ── Build result ─────────────────────────────────────────────────────────────
  kusko_snap_col <- paste0("SnapTp", yr)
  
  kusko_result <- st_drop_geometry(kusko_edges) %>%
    mutate(
      Production = kusko_assign_norm,
      Basin      = "Kuskokwim",
      year       = yr
    ) %>%
    left_join(kusko_mean_temp, by = "COMID") %>%
    rename(SNAP_temp = !!sym(kusko_snap_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD)
  
  cat("    Reaches in final dataset:", nrow(kusko_result), "\n")
  
  
  # ============================================================================
  # YUKON
  # ============================================================================
  
  cat("\n  YUKON:\n")
  
  # ── Load natal data ──────────────────────────────────────────────────────────
  yukon_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir,
              paste0(yr, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  
  # ── Impute missing genetics BEFORE applying 50% cutoff ──────────────────────
  # (so strata weights also see the imputed full-run data)
  daily_gen_year <- daily_gen_wide %>% filter(year == yr)
  
  yukon_natal_raw <- yukon_natal_raw %>%
    left_join(daily_gen_year %>% select(DOY, avg_Lower, avg_Middle, avg_Upper),
              by = "DOY") %>%
    mutate(
      Lower  = ifelse(is.na(Lower),  avg_Lower,  Lower),
      Middle = ifelse(is.na(Middle), avg_Middle, Middle),
      Upper  = ifelse(is.na(Upper),  avg_Upper,  Upper)
    ) %>%
    select(-avg_Lower, -avg_Middle, -avg_Upper)
  
  # ── 50% CPUE cutoff ──────────────────────────────────────────────────────────
  yukon_cpue_raw <- read_csv(
    file.path(PATHS$cpue_data_dir, paste0("Yukon_CPUE_", yr, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Date), !is.na(cumCPUE))
  
  yukon_total_cpue  <- max(yukon_cpue_raw$cumCPUE, na.rm = TRUE)
  yukon_cutoff_date <- max(yukon_cpue_raw$Date[yukon_cpue_raw$cumCPUE <= yukon_total_cpue / 2])
  yukon_cutoff_doy  <- as.numeric(format(as.Date(yukon_cutoff_date), "%j"))
  
  cat("    50% CPUE cutoff DOY:", yukon_cutoff_doy, "\n")
  
  # ── Strata weights (built from FULL run after imputation) ────────────────────
  yukon_unique_days <- sort(unique(yukon_natal_raw$DOY))
  yukon_ndays       <- length(yukon_unique_days)
  yukon_strata_size <- ceiling(yukon_ndays / 5)
  
  yukon_day_strata <- tibble(
    DOY    = yukon_unique_days,
    strata = rep(1:5, each = yukon_strata_size, length.out = yukon_ndays)
  )
  
  yukon_strata_summary <- yukon_natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(yukon_day_strata, by = "DOY") %>%
    group_by(strata) %>%
    summarise(
      cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
      oto_sum  = sum(OtoPropDaily,  na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(weight = cpue_sum / oto_sum)
  
  # Apply 50% cutoff, drop rows still missing genetics after imputation, join weights
  yukon_natal <- yukon_natal_raw %>%
    filter(!is.na(Lower), !is.na(Middle), !is.na(Upper),
           DOY <= yukon_cutoff_doy) %>%
    left_join(yukon_day_strata,                              by = "DOY") %>%
    left_join(yukon_strata_summary %>% select(strata, weight), by = "strata")
  
  cat("    Natal observations (50% window):", nrow(yukon_natal), "\n")
  
  # ── Error (mean isose across all reaches — matches Yukon regional analyses) ───
  yukon_pid_iso       <- yukon_edges$iso_pred
  yukon_pid_isose     <- yukon_edges$isose_pred
  yukon_pid_isose_mod <- rep(mean(yukon_pid_isose, na.rm = TRUE),
                             length(yukon_pid_isose))
  yukon_error <- sqrt(yukon_pid_isose_mod^2 +
                        (0.0003133684 / 1.96)^2 +
                        (0.00011 / 2)^2)
  
  # ── Priors ───────────────────────────────────────────────────────────────────
  yukon_StreamOrderPrior <- ifelse(yukon_edges$Str_Order >= YUKON_PARAMS$min_stream_order, 1, 0)
  yukon_PresencePrior    <- ifelse((yukon_edges$Str_Order %in% c(7, 8, 9)) &
                                     yukon_edges$SPAWNING_C == 0, 0, 1)
  yukon_porcpupinepr     <- yukon_edges$Porc_off
  
  # ── Bayesian assignment ──────────────────────────────────────────────────────
  n_yukon_segments        <- nrow(yukon_edges)
  n_yukon_fish            <- nrow(yukon_natal)
  yukon_assignment_matrix <- matrix(0, nrow = n_yukon_segments, ncol = n_yukon_fish)
  
  for (i in 1:n_yukon_fish) {
    fish_iso <- yukon_natal$natal_iso[i]
    
    gen_prior <- rep(0, n_yukon_segments)
    gen_prior[LYsites] <- as.numeric(yukon_natal$Lower[i])
    gen_prior[MYsites] <- as.numeric(yukon_natal$Middle[i])
    gen_prior[UYsites] <- as.numeric(yukon_natal$Upper[i])
    
    assign <- (1 / sqrt(2 * pi * yukon_error^2)) *
      exp(-1 * (fish_iso - yukon_pid_iso)^2 / (2 * yukon_error^2)) *
      yukon_StreamOrderPrior * gen_prior * yukon_PresencePrior *
      yukon_porcpupinepr
    
    assign_norm     <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < YUKON_PARAMS$sensitivity_threshold] <- 0
    
    yukon_assignment_matrix[, i] <- assign_rescaled * yukon_natal$weight[i]
  }
  
  yukon_basin_assign_sum <- apply(yukon_assignment_matrix, 1, sum, na.rm = TRUE)
  yukon_assign_norm      <- yukon_basin_assign_sum / max(yukon_basin_assign_sum, na.rm = TRUE)
  
  cat("    Segments with production >= 0.7:",
      sum(yukon_assign_norm >= PRODUCTION_THRESHOLD), "\n")
  
  # ── Temperature matching (every 3 days, within 50% window dates) ─────────────
  date_col_y            <- if ("date" %in% names(yukon_natal)) "date" else "Date"
  yukon_natal[[date_col_y]] <- as.Date(yukon_natal[[date_col_y]])
  yukon_date_range      <- range(yukon_natal[[date_col_y]], na.rm = TRUE)
  yukon_date_seq        <- seq(yukon_date_range[1], yukon_date_range[2],
                               by = TEMP_INTERVAL_DAYS)
  
  yukon_mean_temp <- yukon_temp_daily %>%
    filter(date %in% yukon_date_seq) %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
  
  # ── Build result ─────────────────────────────────────────────────────────────
  yukon_snap_col <- paste0("SnapTp", yr)
  
  yukon_result <- st_drop_geometry(yukon_edges) %>%
    mutate(
      Production = yukon_assign_norm,
      Basin      = "Yukon",
      year       = yr
    ) %>%
    left_join(yukon_mean_temp, by = "COMID") %>%
    rename(SNAP_temp = !!sym(yukon_snap_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD)
  
  cat("    Reaches in final dataset:", nrow(yukon_result), "\n")
  
  
  # ============================================================================
  # COMBINE BASINS
  # ============================================================================
  
  combined_result           <- bind_rows(kusko_result, yukon_result)
  year_results[[as.character(yr)]] <- combined_result
  
  cat("\n  Combined reaches with production >= 0.7:", nrow(combined_result), "\n")
}


################################################################################
# PART 4: 2-COLUMN CONTOUR FIGURE
#   Column 1: Stream Temperature (Blaskey NetCDF) vs Channel Slope
#   Column 2: SNAP Air Temperature vs Channel Slope
#   Rows: one per year, year labels on left
################################################################################

cat("\n================================================================\n")
cat("PART 4: BUILDING CONTOUR FIGURE\n")
cat("================================================================\n")

# ------------------------------------------------------------------
# Prepare filtered data list
# ------------------------------------------------------------------
filtered_list        <- lapply(YEARS, function(yr) year_results[[as.character(yr)]])
names(filtered_list) <- as.character(YEARS)

# ------------------------------------------------------------------
# Global axis limits
# ------------------------------------------------------------------
x_lim_temp  <- c(5, 13)
y_lim_slope <- c(0, 3)
x_lim_air   <- c(11, 17)

# ------------------------------------------------------------------
# Colors
# ------------------------------------------------------------------
fill_colors <- brewer.pal(9, "YlOrRd")[-1]

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
# Column 1: Stream Temperature (Blaskey) vs Channel Slope
# ------------------------------------------------------------------
plots_col1 <- lapply(seq_along(YEARS), function(i) {
  df        <- filtered_list[[as.character(YEARS[i])]]
  is_bottom <- (i == length(YEARS))
  
  ggplot(df, aes(mean_summer_temp, Channel_sl)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +
    stat_density_2d_filled(bins = 8) +
    scale_fill_manual(values = fill_colors) +
    scale_x_continuous(
      limits = x_lim_temp,
      expand = c(0, 0),
      labels = if (is_bottom) waiver() else NULL
    ) +
    scale_y_continuous(
      limits = y_lim_slope,
      expand = c(0, 0)
    ) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (is_bottom)
        element_text(size = 8, color = "grey30")
      else
        element_blank()
    )
})

# ------------------------------------------------------------------
# Column 2: SNAP Air Temperature vs Channel Slope
# ------------------------------------------------------------------
plots_col2 <- lapply(seq_along(YEARS), function(i) {
  df        <- filtered_list[[as.character(YEARS[i])]]
  is_bottom <- (i == length(YEARS))
  
  ggplot(df, aes(SNAP_temp, Channel_sl)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +
    stat_density_2d_filled(bins = 8) +
    scale_fill_manual(values = fill_colors) +
    scale_x_continuous(
      limits = x_lim_air,
      expand = c(0, 0),
      labels = if (is_bottom) waiver() else NULL
    ) +
    scale_y_continuous(
      limits = y_lim_slope,
      expand = c(0, 0),
      labels = NULL
    ) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (is_bottom)
        element_text(size = 8, color = "grey30")
      else
        element_blank(),
      axis.text.y = element_blank()
    )
})

# ------------------------------------------------------------------
# Year label panels
# ------------------------------------------------------------------
year_labels <- lapply(YEARS, function(yr) {
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label    = yr,
      hjust    = 0.5,
      size     = 4,
      fontface = "bold",
      color    = "grey20"
    ) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
})

# ------------------------------------------------------------------
# Assemble flat 3-column grid: year label | col1 | col2
# ------------------------------------------------------------------
flat_list <- list()
for (i in seq_along(YEARS)) {
  flat_list <- c(flat_list, list(
    year_labels[[i]],
    plots_col1[[i]],
    plots_col2[[i]]
  ))
}

combined_plot <- wrap_plots(flat_list, ncol = 3,
                            widths = c(0.15, 1, 1)) +
  plot_layout(heights = rep(1, length(YEARS)))

# ------------------------------------------------------------------
# Column titles
# ------------------------------------------------------------------
combined_plot <- combined_plot +
  plot_annotation(
    title = expression(
      paste("Stream Temperature vs Slope",
            "                         ",
            "Air Temperature vs Slope")
    ),
    theme = theme(
      plot.title = element_text(
        size = 12, face = "bold", hjust = 0.5,
        color = "grey10", margin = margin(b = 4)
      )
    )
  )

# ------------------------------------------------------------------
# Shared y-axis label (rotated on left)
# ------------------------------------------------------------------
final_plot <- wrap_elements(combined_plot) +
  labs(tag = "Channel Slope") +
  theme(
    plot.tag          = element_text(size = 11, angle = 90, color = "grey20"),
    plot.tag.position = "left"
  )

# ------------------------------------------------------------------
# Shared x-axis label (bottom caption)
# ------------------------------------------------------------------
final_with_xlab <- final_plot +
  plot_annotation(
    caption = expression(
      paste("Mean Summer Stream Temperature (\u00B0C)",
            "                                     ",
            "SNAP Air Temperature (\u00B0C)")
    ),
    theme = theme(
      plot.caption = element_text(
        size = 10, hjust = 0.55, color = "grey20",
        margin = margin(t = 2)
      )
    )
  )

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------
dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(PATHS$output_figures, "50pct_BothBasins.png"),
  plot   = final_with_xlab,
  width  = 8.5,
  height = 12,
  dpi    = 300,
  bg     = "white"
)

print(final_with_xlab)

cat("\n================================================================\n")
cat("CONTOUR PLOTS COMPLETE\n")
cat("================================================================\n")