################################################################################
# BASIN-SEPARATED CONTOUR PLOTS — FULL RUN TIMING
#
# Yukon:      2015, 2016, 2021  (3 years × 4 plots)
# Kuskokwim:  2017, 2020, 2021  (3 years × 4 plots)
#
# Final figure layout:
#   - Yukon 3×4 panel (rows = years, cols = stream temp, air temp, discharge, precip)
#   - Blank gap row
#   - Kuskokwim 3×4 panel (same format)
#
# Changes from combined script:
#   1. Basin-specific year vectors
#   2. Full run timing (no 50% CPUE cutoff)
#   3. Basins processed independently and plotted separately
#   4. Final figure assembled with gap row between basins
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
  kusko_edges  = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  kusko_basin  = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  
  yukon_edges  = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges.shp"),
  yukon_basin  = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  yukon_ly_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_lYGen.shp"),
  yukon_my_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_mYGen.shp"),
  yukon_uy_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_uYGen.shp"),
  
  kusko_nc_temp_dir = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "Production"),
  yukon_nc_temp_dir = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "Production"),
  nc_disch_dir      = here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", "mizuRoute_Output"),
  
  natal_data_dir = here("Data", "Natal Origins"),
  cpue_data_dir  = here("Data", "CPUE"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv"),
  
  output_figures = here("Figures", "ContourPlots")
)

# Basin-specific year vectors
YUKON_YEARS <- c(2015, 2016, 2021)
KUSKO_YEARS <- c(2017, 2020, 2021)
ALL_YEARS   <- sort(unique(c(YUKON_YEARS, KUSKO_YEARS)))

# Temperature sampling interval (days) — every 3 days
TEMP_INTERVAL_DAYS <- 3

# Production threshold
PRODUCTION_THRESHOLD <- 0.7

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
# DAILY GENETIC PROPORTIONS (Yukon)
# ==============================================================================
daily_gen_long <- read_csv(PATHS$daily_genetics, show_col_types = FALSE)

daily_gen_wide <- daily_gen_long %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(names_from = genetic_assignment, values_from = proportion,
              values_fill = 0) %>%
  rename(year       = sampleYear,
         avg_Lower  = Lower,
         avg_Middle = Middle,
         avg_Upper  = Upper)


################################################################################
# PART 1: EXTRACT DAILY STREAM TEMPERATURE
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
    nc        <- nc_open(nc_temp_files[i])
    vals      <- ncvar_get(nc, "T_stream")
    reach_ids <- ncvar_get(nc, "hru")
    time_vals <- ncvar_get(nc, "time")
    nc_close(nc)
    
    yr    <- as.numeric(sub(".*_(\\d{4})\\.nc$", "\\1", basename(nc_temp_files[i])))
    dates <- as.Date(paste0(yr, "-01-01")) + time_vals
    vals2 <- vals[, 2, ]
    jj    <- which(month(dates) %in% 6:7)
    if (length(jj) == 0) next
    
    n_reach <- length(reach_ids)
    n_days  <- length(jj)
    temp_daily_list[[i]] <- data.frame(
      COMID = rep(reach_ids, times = n_days),
      date  = rep(dates[jj],  each  = n_reach),
      value = as.vector(vals2[, jj, drop = FALSE])
    )
  }
  bind_rows(temp_daily_list) %>% distinct(COMID, date, .keep_all = TRUE)
}

kusko_temp_daily <- extract_temp_data(PATHS$kusko_nc_temp_dir, "Kuskokwim")
yukon_temp_daily <- extract_temp_data(PATHS$yukon_nc_temp_dir, "Yukon")


################################################################################
# PART 1b: EXTRACT DAILY DISCHARGE
################################################################################

cat("\n================================================================\n")
cat("PART 1b: EXTRACTING DAILY DISCHARGE\n")
cat("================================================================\n")

extract_disch_data <- function(nc_dir) {
  nc_disch_files <- list.files(
    nc_dir,
    pattern = "AK_Rivers_.*\\.h\\.(2015|2016|2017|2018|2019|2020|2021).*\\.nc$",
    full.names = TRUE
  )
  cat("  Discharge files found:", length(nc_disch_files), "\n")
  
  origin_date <- as.Date("1989-06-01")
  all_data    <- list()
  
  for (i in seq_along(nc_disch_files)) {
    nc        <- nc_open(nc_disch_files[i])
    vals      <- ncvar_get(nc, "IRFroutedRunoff")
    reach_ids <- ncvar_get(nc, "reachID")
    time_vals <- ncvar_get(nc, "time")
    nc_close(nc)
    
    dates <- origin_date + time_vals
    jj    <- which(month(dates) %in% 6:7)
    if (length(jj) == 0) next
    
    for (j in seq_along(reach_ids)) {
      all_data[[length(all_data) + 1]] <- data.frame(
        COMID = reach_ids[j],
        date  = dates[jj],
        value = vals[j, jj]
      )
    }
  }
  bind_rows(all_data) %>% distinct(COMID, date, .keep_all = TRUE)
}

disch_daily <- extract_disch_data(PATHS$nc_disch_dir)


################################################################################
# PART 2: LOAD SPATIAL DATA
################################################################################

cat("\n================================================================\n")
cat("PART 2: LOADING SPATIAL DATA\n")
cat("================================================================\n")

kusko_edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
kusko_basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
kusko_edges <- st_transform(kusko_edges, st_crs(kusko_basin))
cat("  Kuskokwim stream segments:", nrow(kusko_edges), "\n")

yukon_edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
yukon_basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
yukon_edges <- st_transform(yukon_edges, st_crs(yukon_basin))

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
# PART 3: PRODUCTION LOOP — KUSKOKWIM (full run, years 2017, 2020, 2021)
################################################################################

cat("\n================================================================\n")
cat("PART 3a: KUSKOKWIM PRODUCTION — FULL RUN\n")
cat("================================================================\n")

kusko_year_results <- list()

for (yr in KUSKO_YEARS) {
  cat("\n--- Kuskokwim Year", yr, "---\n")
  
  # Load natal data — no CPUE cutoff; use ALL fish
  kusko_natal <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(yr, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  
  cat("  Natal observations (full run):", nrow(kusko_natal), "\n")
  
  # Stratum weights (computed from full natal data)
  unique_days_k <- sort(unique(kusko_natal$DOY))
  ndays_k       <- length(unique_days_k)
  strata_size_k <- ceiling(ndays_k / 5)
  day_strata_k  <- tibble(
    DOY    = unique_days_k,
    strata = rep(1:5, each = strata_size_k, length.out = ndays_k)
  )
  strata_summary_k <- kusko_natal %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata_k, by = "DOY") %>%
    group_by(strata) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
              oto_sum  = sum(OtoPropDaily,  na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = cpue_sum / oto_sum)
  kusko_natal <- kusko_natal %>%
    left_join(day_strata_k, by = "DOY") %>%
    left_join(strata_summary_k %>% select(strata, weight), by = "strata")
  
  # Error and priors
  kusko_pid_iso        <- kusko_edges$iso_pred
  kusko_pid_isose      <- kusko_edges$isose_pred
  kusko_pid_isose_mod  <- ifelse(kusko_pid_isose < KUSKO_PARAMS$min_error,
                                 KUSKO_PARAMS$min_error, kusko_pid_isose)
  kusko_error          <- sqrt(kusko_pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  kusko_StreamOrderPrior <- ifelse(kusko_edges$Str_Order >= KUSKO_PARAMS$min_stream_order, 1, 0)
  kusko_PresencePrior    <- ifelse((kusko_edges$Str_Order %in% c(7, 8)) &
                                     kusko_edges$SPAWNING_C == 0, 0, 1)
  kusko_NewHabitatPrior  <- ifelse(kusko_edges$Spawner_IP < 0.3, 0, 1)
  kusko_pid_prior        <- kusko_edges$UniPh2oNoE
  
  # Bayesian assignment
  n_k_seg  <- nrow(kusko_edges)
  n_k_fish <- nrow(kusko_natal)
  kusko_mat <- matrix(0, nrow = n_k_seg, ncol = n_k_fish)
  
  for (i in 1:n_k_fish) {
    fish_iso <- kusko_natal$natal_iso[i]
    assign   <- (1 / sqrt(2 * pi * kusko_error^2)) *
      exp(-1 * (fish_iso - kusko_pid_iso)^2 / (2 * kusko_error^2)) *
      kusko_StreamOrderPrior * kusko_PresencePrior *
      kusko_pid_prior * kusko_NewHabitatPrior
    assign_norm     <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < KUSKO_PARAMS$sensitivity_threshold] <- 0
    kusko_mat[, i]  <- assign_rescaled * kusko_natal$weight[i]
  }
  
  kusko_assign_norm <- {
    s <- apply(kusko_mat, 1, sum, na.rm = TRUE)
    s / max(s, na.rm = TRUE)
  }
  cat("  Segments >= 0.7:", sum(kusko_assign_norm >= PRODUCTION_THRESHOLD), "\n")
  
  # Temperature date window — full natal date range, every 3 days
  date_col <- if ("date" %in% names(kusko_natal)) "date" else "Date"
  kusko_natal[[date_col]] <- as.Date(kusko_natal[[date_col]])
  dr       <- range(kusko_natal[[date_col]], na.rm = TRUE)
  date_seq <- seq(dr[1], dr[2], by = TEMP_INTERVAL_DAYS)
  
  kusko_mean_temp  <- kusko_temp_daily %>%
    filter(date %in% date_seq) %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
  
  kusko_mean_disch <- disch_daily %>%
    filter(date %in% date_seq) %>%
    group_by(COMID) %>%
    summarise(mean_summer_disch = mean(value, na.rm = TRUE), .groups = "drop")
  
  snap_temp_col <- paste0("SnapTp", yr)
  snap_prec_col <- paste0("SnapPr", yr)
  
  kusko_year_results[[as.character(yr)]] <- st_drop_geometry(kusko_edges) %>%
    mutate(Production = kusko_assign_norm, Basin = "Kuskokwim", year = yr) %>%
    left_join(kusko_mean_temp,  by = "COMID") %>%
    left_join(kusko_mean_disch, by = "COMID") %>%
    rename(SNAP_temp = !!sym(snap_temp_col),
           SNAP_prec = !!sym(snap_prec_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD) %>%
    mutate(
      log_disch = ifelse(mean_summer_disch > 0, log10(mean_summer_disch), NA_real_),
      log_prec  = ifelse(SNAP_prec > 0,         log10(SNAP_prec),         NA_real_)
    )
  
  cat("  Reaches retained:", nrow(kusko_year_results[[as.character(yr)]]), "\n")
}


################################################################################
# PART 3b: PRODUCTION LOOP — YUKON (full run, years 2015, 2016, 2021)
################################################################################

cat("\n================================================================\n")
cat("PART 3b: YUKON PRODUCTION — FULL RUN\n")
cat("================================================================\n")

yukon_year_results <- list()

for (yr in YUKON_YEARS) {
  cat("\n--- Yukon Year", yr, "---\n")
  
  # Load natal data — no CPUE cutoff; use ALL fish
  yukon_natal_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(yr, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  )
  
  # Impute missing genetics
  daily_gen_year <- daily_gen_wide %>% filter(year == yr)
  yukon_natal_raw <- yukon_natal_raw %>%
    left_join(daily_gen_year %>% select(DOY, avg_Lower, avg_Middle, avg_Upper), by = "DOY") %>%
    mutate(
      Lower  = ifelse(is.na(Lower),  avg_Lower,  Lower),
      Middle = ifelse(is.na(Middle), avg_Middle, Middle),
      Upper  = ifelse(is.na(Upper),  avg_Upper,  Upper)
    ) %>%
    select(-avg_Lower, -avg_Middle, -avg_Upper)
  
  yukon_natal <- yukon_natal_raw %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  cat("  Natal observations (full run):", nrow(yukon_natal), "\n")
  
  # Stratum weights (full run)
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
  
  # Error and priors
  yukon_pid_iso        <- yukon_edges$iso_pred
  yukon_pid_isose      <- yukon_edges$isose_pred
  yukon_pid_isose_mod  <- rep(mean(yukon_pid_isose, na.rm = TRUE), length(yukon_pid_isose))
  yukon_error          <- sqrt(yukon_pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  yukon_StreamOrderPrior <- ifelse(yukon_edges$Str_Order >= YUKON_PARAMS$min_stream_order, 1, 0)
  yukon_PresencePrior    <- ifelse((yukon_edges$Str_Order %in% c(6, 7, 8, 9)) &
                                     yukon_edges$SPAWNING_C == 0, 0, 1)
  yukon_newhabitatprior  <- ifelse(yukon_edges$Channel_sl > 2.3, 0, 1)
  
  # Bayesian assignment
  n_y_seg  <- nrow(yukon_edges)
  n_y_fish <- nrow(yukon_natal)
  yukon_mat <- matrix(0, nrow = n_y_seg, ncol = n_y_fish)
  
  for (i in 1:n_y_fish) {
    fish_iso  <- yukon_natal$natal_iso[i]
    gen_prior <- rep(0, n_y_seg)
    gen_prior[LYsites] <- as.numeric(yukon_natal$Lower[i])
    gen_prior[MYsites] <- as.numeric(yukon_natal$Middle[i])
    gen_prior[UYsites] <- as.numeric(yukon_natal$Upper[i])
    
    assign <- (1 / sqrt(2 * pi * yukon_error^2)) *
      exp(-1 * (fish_iso - yukon_pid_iso)^2 / (2 * yukon_error^2)) *
      yukon_StreamOrderPrior * gen_prior * yukon_PresencePrior * yukon_newhabitatprior
    
    assign_norm     <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < YUKON_PARAMS$sensitivity_threshold] <- 0
    yukon_mat[, i]  <- assign_rescaled * yukon_natal$weight[i]
  }
  
  yukon_basin_assign_sum <- apply(yukon_mat, 1, sum, na.rm = TRUE)
  yukon_basin_assign_sum <- ifelse(yukon_edges$Porc_off == 0,
                                   yukon_basin_assign_sum * 0.3,
                                   yukon_basin_assign_sum)
  yukon_assign_norm <- yukon_basin_assign_sum / max(yukon_basin_assign_sum, na.rm = TRUE)
  
  cat("  Segments >= 0.7:", sum(yukon_assign_norm >= PRODUCTION_THRESHOLD), "\n")
  
  # Temperature date window — full natal date range, every 3 days
  date_col <- if ("date" %in% names(yukon_natal)) "date" else "Date"
  yukon_natal[[date_col]] <- as.Date(yukon_natal[[date_col]])
  dr       <- range(yukon_natal[[date_col]], na.rm = TRUE)
  date_seq <- seq(dr[1], dr[2], by = TEMP_INTERVAL_DAYS)
  
  yukon_mean_temp  <- yukon_temp_daily %>%
    filter(date %in% date_seq) %>%
    group_by(COMID) %>%
    summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
  
  yukon_mean_disch <- disch_daily %>%
    filter(date %in% date_seq) %>%
    group_by(COMID) %>%
    summarise(mean_summer_disch = mean(value, na.rm = TRUE), .groups = "drop")
  
  snap_temp_col <- paste0("SnapTp", yr)
  snap_prec_col <- paste0("SnapPr", yr)
  
  yukon_year_results[[as.character(yr)]] <- st_drop_geometry(yukon_edges) %>%
    mutate(Production = yukon_assign_norm, Basin = "Yukon", year = yr) %>%
    left_join(yukon_mean_temp,  by = "COMID") %>%
    left_join(yukon_mean_disch, by = "COMID") %>%
    rename(SNAP_temp = !!sym(snap_temp_col),
           SNAP_prec = !!sym(snap_prec_col)) %>%
    filter(Production >= PRODUCTION_THRESHOLD) %>%
    mutate(
      log_disch = ifelse(mean_summer_disch > 0, log10(mean_summer_disch), NA_real_),
      log_prec  = ifelse(SNAP_prec > 0,         log10(SNAP_prec),         NA_real_)
    )
  
  cat("  Reaches retained:", nrow(yukon_year_results[[as.character(yr)]]), "\n")
}


################################################################################
# PART 4: BUILD FIGURE
#   Layout:
#     [Year label] [Stream Temp] [Air Temp] [Log Discharge] [Log Precip]
#   Yukon (3 rows) → gap row → Kuskokwim (3 rows)
################################################################################

cat("\n================================================================\n")
cat("PART 4: BUILDING FIGURE\n")
cat("================================================================\n")

# ------------------------------------------------------------------
# Axis limits — derived separately per basin for tighter scaling
# ------------------------------------------------------------------
all_yukon  <- bind_rows(yukon_year_results)
all_kusko  <- bind_rows(kusko_year_results)

make_limits <- function(df) {
  list(
    x_stream = range(df$mean_summer_temp,  na.rm = TRUE),
    x_air    = range(df$SNAP_temp,         na.rm = TRUE),
    y_slope  = c(0, 3),
    x_disch  = range(df$log_disch[is.finite(df$log_disch)], na.rm = TRUE),
    x_prec   = range(df$log_prec[is.finite(df$log_prec)],   na.rm = TRUE)
  )
}

lim_y <- make_limits(all_yukon)
lim_k <- make_limits(all_kusko)

# ------------------------------------------------------------------
# Palettes
# ------------------------------------------------------------------
fill_warm <- brewer.pal(9, "YlOrRd")[-1]   # 8 levels — temperature
fill_blue <- brewer.pal(9, "Blues")[-1]    # 8 levels — hydrology

# ------------------------------------------------------------------
# Shared base theme
# ------------------------------------------------------------------
base_theme <- theme_minimal() +
  theme(
    axis.text        = element_text(size = 14, color = "grey30"),
    axis.title       = element_blank(),
    legend.position  = "none",
    panel.grid.major = element_line(color = alpha("grey50", 0.3), linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.ontop      = TRUE,
    panel.background = element_rect(fill = NA, color = NA),
    plot.margin      = margin(2, 6, 2, 6),
    plot.title       = element_text(size = 16, face = "bold", hjust = 0.5,
                                    color = "grey10", margin = margin(b = 4))
  )

# ------------------------------------------------------------------
# Helper: single contour panel
# ------------------------------------------------------------------
make_panel <- function(df, x_var, x_lim, y_lim, fill_colors,
                       show_x_labels, show_y_labels,
                       is_top_row    = FALSE,
                       is_bottom_row = FALSE,
                       col_title     = NULL) {
  
  y_breaks <- if (is_bottom_row) c(0, 1, 2, 3) else c(1, 2, 3)
  
  p <- ggplot(df, aes(.data[[x_var]], Channel_sl)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +
    stat_density_2d_filled(bins = 8) +
    scale_fill_manual(values = fill_colors) +
    scale_x_continuous(limits = x_lim, expand = c(0, 0),
                       labels = if (show_x_labels) waiver() else NULL) +
    scale_y_continuous(limits = y_lim, expand = c(0, 0),
                       breaks = y_breaks,
                       labels = if (show_y_labels) waiver() else NULL) +
    coord_cartesian(clip = "off") +
    base_theme +
    theme(
      axis.text.x = if (show_x_labels) element_text(size = 14, color = "grey30")
      else element_blank(),
      axis.text.y = if (show_y_labels) element_text(size = 14, color = "grey30")
      else element_blank()
    )
  
  if (is_top_row && !is.null(col_title)) p <- p + ggtitle(col_title)
  p
}

# ------------------------------------------------------------------
# Build panel grid for one basin
# Returns a flat list of ggplots (year_label + 4 panels) × n_years
# ------------------------------------------------------------------
build_basin_panels <- function(year_results, years, lim, basin_label) {
  flat <- list()
  
  for (i in seq_along(years)) {
    yr  <- years[i]
    df  <- year_results[[as.character(yr)]]
    is_top    <- (i == 1)
    is_bottom <- (i == length(years))
    
    # Year label (with basin name on first year)
    yr_label_text <- if (i == 1) paste0(basin_label, "\n", yr) else as.character(yr)
    yr_label <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = yr_label_text,
               hjust = 0.5, size = if (i == 1) 5.5 else 5,
               fontface = if (i == 1) "bold" else "plain",
               color = "grey20") +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    p1 <- make_panel(df, "mean_summer_temp", lim$x_stream, lim$y_slope,
                     fill_warm,
                     show_x_labels = is_bottom, show_y_labels = TRUE,
                     is_top_row = is_top, is_bottom_row = is_bottom,
                     col_title = "Stream Temp vs Slope")
    
    p2 <- make_panel(df, "SNAP_temp", lim$x_air, lim$y_slope,
                     fill_warm,
                     show_x_labels = is_bottom, show_y_labels = FALSE,
                     is_top_row = is_top, is_bottom_row = is_bottom,
                     col_title = "Air Temp vs Slope")
    
    p3 <- make_panel(df, "log_disch", lim$x_disch, lim$y_slope,
                     fill_blue,
                     show_x_labels = is_bottom, show_y_labels = FALSE,
                     is_top_row = is_top, is_bottom_row = is_bottom,
                     col_title = "Log Discharge vs Slope")
    
    p4 <- make_panel(df, "log_prec", lim$x_prec, lim$y_slope,
                     fill_blue,
                     show_x_labels = is_bottom, show_y_labels = FALSE,
                     is_top_row = is_top, is_bottom_row = is_bottom,
                     col_title = "Log Precip vs Slope")
    
    flat <- c(flat, list(yr_label, p1, p2, p3, p4))
  }
  flat
}

yukon_panels <- build_basin_panels(yukon_year_results, YUKON_YEARS, lim_y, "Yukon")
kusko_panels <- build_basin_panels(kusko_year_results, KUSKO_YEARS, lim_k, "Kuskokwim")

# ------------------------------------------------------------------
# Gap row — 5 blank spacer plots
# ------------------------------------------------------------------
blank_spacer <- function() {
  ggplot() + theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
}
gap_row <- replicate(5, blank_spacer(), simplify = FALSE)

# ------------------------------------------------------------------
# Assemble full grid:
#   Yukon (3 × 5) + gap (1 × 5) + Kusko (3 × 5) = 7 × 5
# ------------------------------------------------------------------
all_panels <- c(yukon_panels, gap_row, kusko_panels)
n_rows     <- length(YUKON_YEARS) + 1 + length(KUSKO_YEARS)   # 3 + 1 + 3 = 7

combined_plot <- wrap_plots(all_panels, ncol = 5,
                            widths  = c(0.3, 1, 1, 1, 1),
                            heights = c(rep(1, length(YUKON_YEARS)),
                                        0.25,          # gap row height
                                        rep(1, length(KUSKO_YEARS))))

# ------------------------------------------------------------------
# Shared y-axis label
# ------------------------------------------------------------------
final_plot <- wrap_elements(combined_plot) +
  labs(tag = "Channel Slope") +
  theme(
    plot.tag          = element_text(size = 16, angle = 90, color = "grey20",
                                     face = "bold"),
    plot.tag.position = "left"
  )

# ------------------------------------------------------------------
# Shared x-axis caption
# ------------------------------------------------------------------
final_with_xlab <- final_plot +
  plot_annotation(
    caption = paste0(
      "Mean Summer Stream Temp (\u00B0C)",
      "                    SNAP Air Temp (\u00B0C)",
      "                    Log\u2081\u2080 Discharge (m\u00B3/s)",
      "                    Log\u2081\u2080 SNAP Precip (mm)"
    ),
    theme = theme(
      plot.caption = element_text(size = 14, hjust = 0.55, color = "grey20",
                                  margin = margin(t = 4))
    )
  )

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------
dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

out_path <- file.path(PATHS$output_figures, "FullRun_BothBasins.png")

ggsave(
  out_path,
  plot   = final_with_xlab,
  width  = 16,
  height = 16,   # taller to accommodate 7-row layout
  dpi    = 300,
  bg     = "white"
)

print(final_with_xlab)

cat("\n================================================================\n")
cat("CONTOUR PLOTS COMPLETE\n")
cat("Output:", out_path, "\n")
cat("================================================================\n")