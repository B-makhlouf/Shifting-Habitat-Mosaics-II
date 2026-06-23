################################################################################
# FULL BASIN PRODUCTION MAPS -- FIRST 50% OF CPUE ONLY
#
# Identical to 01_FullBasinProductionEstimates.R, except the natal data for
# each year/basin is restricted to days that fall within the FIRST 50% of the
# cumulative CPUE for that basin/year. All other parameters (priors, slope
# cutoff, stream order cutoff, sensitivity threshold, Porcupine target) are
# unchanged.
#
# Two functions drive all annual production analyses:
#   run_kusko(year)  -> Kuskokwim (first 50% CPUE only)
#   run_yukon(year)  -> Full Yukon basin (Lower + Middle + Upper, first 50% CPUE only)
#
# Outputs per call:
#   - Outputs/ProductionData/<region>_50cpue/<year>_<region>_50cpue_Assignment_Results.csv
#   - Figures/Maps/FullBasin50Cpue/<region>/<region>_<year>.png
################################################################################

suppressPackageStartupMessages({
  library(sf);       library(dplyr);       library(readr)
  library(readxl);   library(tibble);      library(tidyr)
  library(ggplot2);  library(RColorBrewer); library(here)
})

# ---- Paths -------------------------------------------------------------------
# Edge shapefiles already include geomorphology (slope, mean_elev, z1, z2,
# length_m) -- produced by Code/Geospatial/BuildGeomorphEdges.R. Re-run that
# script if the underlying streams or DEMs change.
PATHS <- list(
  kusko_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges_geomorphAdded.shp"),
  kusko_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges_geomorphAdded.shp"),
  yukon_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  natal_dir      = here("Data", "Natal Origins"),
  runsize        = here("Data", "AYKEscapement.xlsx"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv"),
  out_kusko      = here("Outputs", "ProductionData", "Kusko_50cpue"),
  out_yukon_full = here("Outputs", "ProductionData", "Yukon_full_50cpue"),
  map_kusko      = here("Figures", "Maps","FullBasin50Cpue", "Kusko"),
  map_yukon_full = here("Figures", "Maps","FullBasin50Cpue", "Yukon")
)

KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

# CPUE cutoff: keep DOYs whose cumulative dailyCPUEprop reaches this fraction
CPUE_CUTOFF <- 0.50

source(here("Code", "Analysis","00_ProvenanceEstimates", "params.R"))

# ---- Spatial layers (loaded once, reused across years) -----------------------
KUSKO_EDGES <- st_read(PATHS$kusko_edges, quiet = TRUE)
KUSKO_BASIN <- st_read(PATHS$kusko_basin, quiet = TRUE)
KUSKO_EDGES <- st_transform(KUSKO_EDGES, st_crs(KUSKO_BASIN))

YUKON_EDGES <- st_read(PATHS$yukon_edges, quiet = TRUE)
YUKON_BASIN <- st_read(PATHS$yukon_basin, quiet = TRUE)
YUKON_EDGES <- st_transform(YUKON_EDGES, st_crs(YUKON_BASIN))

# Common map assets
PALETTE       <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)
LEGEND_LABELS <- c("0.0-0.4", "0.4-0.7", "0.7-0.8", "0.8-0.9", "0.9-0.95", "0.95-1.0")
LEGEND_COLORS <- PALETTE[c(2, 5, 7, 8, 9, 10)]

# Yukon imputation lookup (wide: year x DOY x avg_Lower/avg_Middle/avg_Upper)
daily_gen_wide <- read_csv(PATHS$daily_genetics, show_col_types = FALSE) %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(names_from = genetic_assignment, values_from = proportion, values_fill = 0) %>%
  rename(year = sampleYear, avg_Lower = Lower, avg_Middle = Middle, avg_Upper = Upper)


# ---- First-50%-CPUE helper ---------------------------------------------------
# Given the full natal_raw data frame for a basin/year, keep only rows whose
# DOY falls within the first `cutoff` fraction of the cumulative dailyCPUEprop.
# Inclusive of the day on which the cumulative sum first crosses the cutoff,
# so that we always retain at least the requested fraction of the run.
filter_first_pct_cpue <- function(natal_raw, cutoff = 0.50) {
  doy_cpue <- natal_raw %>%
    distinct(DOY, dailyCPUEprop) %>%
    arrange(DOY) %>%
    mutate(dailyCPUEprop = tidyr::replace_na(dailyCPUEprop, 0)) %>%
    mutate(cum_cpue = cumsum(dailyCPUEprop))

  if (nrow(doy_cpue) == 0) return(natal_raw)

  # Index of the first day at which the running total reaches the cutoff.
  cross_idx <- which(doy_cpue$cum_cpue >= cutoff)[1]
  if (is.na(cross_idx)) cross_idx <- nrow(doy_cpue)  # never reaches cutoff -> keep all

  keep_doys <- doy_cpue$DOY[seq_len(cross_idx)]
  cat(sprintf("  CPUE filter (<= %.0f%%): keeping %d / %d DOYs (cum CPUE = %.3f)\n",
              100 * cutoff, length(keep_doys), nrow(doy_cpue),
              doy_cpue$cum_cpue[cross_idx]))

  natal_raw %>% filter(DOY %in% keep_doys)
}


# ==============================================================================
# KUSKOKWIM
# ==============================================================================
run_kusko <- function(year) {
  cat(sprintf("\n=== Kusko %d (first 50%% CPUE) ===\n", year))

  # ---- Spatial ---------------------------------------------------------------
  edges <- KUSKO_EDGES
  basin <- KUSKO_BASIN

  # ---- Natal + stratum weights ----------------------------------------------

  # Read in the raw natal origins
  natal_raw <- read_csv(file.path(PATHS$natal_dir,
                                  sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
                        show_col_types = FALSE)

  # Restrict to the first 50% of cumulative CPUE for the basin/year
  natal_raw <- filter_first_pct_cpue(natal_raw, cutoff = CPUE_CUTOFF)

  # Filter out days that dont have a natal origin or CPUE
  natal <- natal_raw %>% filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  if (nrow(natal) == 0) stop("No data available!")



  ### produce strata weighted CPUE vs oto collection

  unique_days <- sort(unique(natal_raw$DOY))

  day_strata  <- tibble(
    DOY    = unique_days,
    strata = rep(1:5, each = ceiling(length(unique_days) / 5), length.out = length(unique_days))
  )

  # Ok, so days with less otoliths collected than the proportional CPUE are upweighted to account for less
  # representation here

  strata_summary <- natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata, by = "DOY") %>%
    group_by(strata) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
              oto_sum  = sum(OtoPropDaily,  na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = cpue_sum / oto_sum)

  # Add back in the weights
  natal <- natal %>%
    left_join(day_strata, by = "DOY") %>%
    left_join(strata_summary %>% select(strata, weight), by = "strata")


  cat(sprintf("  Segments: %d | Observations: %d\n", nrow(edges), nrow(natal)))

  # ---- Error + priors --------------------------------------------------------

  pid_iso       <- edges$iso_pred # Isotope ratio of the environment

  # Raise the lower limit to avoid disparity in assignment based off of error
 
   pid_isose_mod <- pmax(edges$isose_pred, KUSKO_PARAMS$min_error)
  
  
  #pid_isose_mod<- edges$isose_pred
  
  
  # if we want to just use the mean error value
  #mean_min_error <- mean(edges$isose_pred, na.rm = TRUE)
  #pid_isose_mod <- mean_min_error

  # Combined error sources.
  error         <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  # Limit assigment above a given stream order
  stream_order_prior <- ifelse(edges$Str_Order  >= KUSKO_PARAMS$min_stream_order, 1, 0)

  # Limit assignment below a given channel slope cutoff
  new_habitat_prior  <- ifelse(edges$slope >  KUSKO_PARAMS$channel_slope_cutoff, 0, 1)

  #new_habitat_prior  <- ifelse(edges$Spawner_IP < .5, 0, 1)

  # Limit assigment to places where observed Chinook have been seen
  presence_prior     <- ifelse(edges$Str_Order %in% c(7, 8) & edges$SPAWNING_C == 0, 0, 1)
  fixed_prior        <- stream_order_prior * edges$UniPh2oNoE *new_habitat_prior * presence_prior

  # ---- Bayesian assignment ---------------------------------------------------
  A <- matrix(0, nrow = nrow(edges), ncol = nrow(natal))


  for (i in seq_len(nrow(natal))) {
    lik  <- (1 / sqrt(2 * pi * error^2)) * exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a    <- lik * fixed_prior
    an   <- a / sum(a)
    resc <- an / max(an)
    resc[resc < KUSKO_PARAMS$sensitivity_threshold] <- 0
   # resc[resc > KUSKO_PARAMS$sensitivity_threshold] <- 1
    A[, i] <- resc * natal$weight[i]
  }

  # ---- Finalize --------------------------------------------------------------
  basin_sum  <- rowSums(A, na.rm = TRUE)
  runsizedat <- read_excel(PATHS$runsize)
  runsize    <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Kusko" & runsizedat$Year == year])
  total      <- sum(basin_sum, na.rm = TRUE)
  if (total > 0) {
    rescale     <- basin_sum / total
    norm        <- rescale / max(rescale, na.rm = TRUE)
    individuals <- rescale * runsize
  } else {
    rescale <- norm <- individuals <- rep(0, length(basin_sum))
  }
  cat(sprintf("  Segments with assignment > 0: %d / %d\n", sum(basin_sum > 0), nrow(edges)))

  # ---- CSV -------------------------------------------------------------------
  dir.create(PATHS$out_kusko, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order, iso_pred = edf$iso_pred,
               assignment_sum = basin_sum, assignment_rescale = rescale,
               assignment_norm = norm, assignment_individuals = individuals),
    file.path(PATHS$out_kusko, sprintf("%d_Kusko_50cpue_Assignment_Results.csv", year))
  )

  # ---- Map -------------------------------------------------------------------
  colcode <- rep("gray90", length(norm))
  colcode[norm == 0]                  <- "gray50"
  colcode[norm > 0.0 & norm <= 0.1]   <- PALETTE[1]
  colcode[norm > 0.1 & norm <= 0.2]   <- PALETTE[2]
  colcode[norm > 0.2 & norm <= 0.3]   <- PALETTE[3]
  colcode[norm > 0.3 & norm <= 0.4]   <- PALETTE[4]
  colcode[norm > 0.4 & norm <= 0.5]   <- PALETTE[5]
  colcode[norm > 0.5 & norm <= 0.6]   <- PALETTE[6]
  colcode[norm > 0.6 & norm <= 0.7]   <- PALETTE[7]
  colcode[norm > 0.7 & norm <= 0.8]   <- PALETTE[8]
  colcode[norm > 0.8 & norm <= 0.9]   <- PALETTE[9]
  colcode[norm > 0.9]   <- PALETTE[10]

  # colcode[norm > 0.0 & norm <= 0.7]   <- "grey50"
  # colcode[norm > 0.7 & norm <= 0.8]   <- PALETTE[5]
  # colcode[norm > 0.8 & norm <= 0.9]   <- PALETTE[7]
  # colcode[norm > 0.9]   <- PALETTE[9]

  colcode[stream_order_prior == 0]    <- "gray70"


  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 5,
        ifelse(so >= 8, 6,
        ifelse(so >= 7, 5,
        ifelse(so >= 6, 3.0,
        ifelse(so >= 5, 2.7,
        ifelse(so >= 4, 2.7,
        ifelse(so >= 3, 2.5,
        ifelse(so >= 2, 0,
               0))))))))
  lw[so < KUSKO_PARAMS$min_stream_order] <- 0

  dir.create(PATHS$map_kusko, recursive = TRUE, showWarnings = FALSE)
  png(file.path(PATHS$map_kusko, sprintf("Kusko_%d.png", year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Annual Production - Kuskokwim (first 50%% CPUE)\nYear: %d", year), bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = lw)
  legend("topleft", legend = LEGEND_LABELS, col = LEGEND_COLORS, lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")

  invisible(NULL)
}


# ==============================================================================
# YUKON  (Full basin: Lower + Middle + Upper)
# ==============================================================================
run_yukon <- function(year) {
  region <- "Yukon_Full_50cpue"
  cat(sprintf("\n=== %s %d ===\n", region, year))

  gen_cols <- c("Lower", "Middle", "Upper")

  # ---- Spatial --------------------------------------------------------------
  edges <- YUKON_EDGES
  basin <- YUKON_BASIN

  LY <- which(tolower(edges$GenLMU) == "lower")
  MY <- which(tolower(edges$GenLMU) == "middle")
  UY <- which(tolower(edges$GenLMU) == "upper")

  # ---- Natal + genetic imputation + stratum weights -------------------------

  natal_raw <- read_csv(file.path(PATHS$natal_dir,
                                  sprintf("%d_Yukon_Natal_Origins_Genetics_CPUE.csv", year)),
                        show_col_types = FALSE)

  # Restrict to the first 50% of cumulative CPUE for the basin/year.
  # Done BEFORE genetic imputation so the imputation only fills DOYs that
  # remain in the analysis window.
  natal_raw <- filter_first_pct_cpue(natal_raw, cutoff = CPUE_CUTOFF)

  avg_cols  <- paste0("avg_", gen_cols)
  dgen_year <- daily_gen_wide %>% filter(year == !!year) %>%
                 select(DOY, all_of(avg_cols))
  natal_raw <- natal_raw %>% left_join(dgen_year, by = "DOY")
  for (col in gen_cols) {
    ac <- paste0("avg_", col)
    natal_raw[[col]] <- ifelse(is.na(natal_raw[[col]]), natal_raw[[ac]], natal_raw[[col]])
  }
  natal_raw <- natal_raw %>% select(-all_of(avg_cols))

  natal <- natal_raw %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop),
           if_all(all_of(gen_cols), ~ !is.na(.x)))
  if (nrow(natal) == 0) stop("No data available!")

  unique_days <- sort(unique(natal_raw$DOY))
  day_strata  <- tibble(
    DOY    = unique_days,
    strata = rep(1:5, each = ceiling(length(unique_days) / 5), length.out = length(unique_days))
  )
  strata_summary <- natal_raw %>%
    distinct(DOY, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata, by = "DOY") %>%
    group_by(strata) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
              oto_sum  = sum(OtoPropDaily,  na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = cpue_sum / oto_sum)
  natal <- natal %>%
    left_join(day_strata, by = "DOY") %>%
    left_join(strata_summary %>% select(strata, weight), by = "strata")

  cat(sprintf("  Segments: %d (L=%d, M=%d, U=%d) | Observations: %d\n",
              nrow(edges), length(LY), length(MY), length(UY), nrow(natal)))

  # ---- Error + priors --------------------------------------------------------
  pid_iso       <- edges$iso_pred
  #pid_isose_mod <- rep(mean(edges$isose_pred, na.rm = TRUE), length(pid_iso))

  mean_min_error <- mean(edges$isose_pred, na.rm = TRUE)

  pid_isose_mod <- mean_min_error

  error         <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order  >= YUKON_PARAMS$min_stream_order, 1, 0)
  #presence_prior     <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
  new_habitat_prior  <- ifelse(edges$slope >  YUKON_PARAMS$channel_slope_cutoff, 0, 1)
  base_prior         <- stream_order_prior * new_habitat_prior #* presence_prior

  # ---- Bayesian assignment ---------------------------------------------------
  A <- matrix(0, nrow = nrow(edges), ncol = nrow(natal))

  for (i in seq_len(nrow(natal))) {
    gen_prior <- rep(0, nrow(edges))
    gen_prior[LY] <- as.numeric(natal$Lower[i])
    gen_prior[MY] <- as.numeric(natal$Middle[i])
    gen_prior[UY] <- as.numeric(natal$Upper[i])

    lik  <- (1 / sqrt(2 * pi * error^2)) * exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a    <- lik * base_prior * gen_prior
    an   <- a / sum(a)
    resc <- an / max(an)
    resc[resc < YUKON_PARAMS$sensitivity_threshold] <- 0
    #resc[resc > YUKON_PARAMS$sensitivity_threshold] <- 1
    A[, i] <- resc * natal$weight[i]
  }

  # ---- Finalize (post-hoc porcupine adjustment) -----------------------------
  basin_sum <- rowSums(A, na.rm = TRUE)

  # Rescale Porcupine so it represents YUKON_PARAMS$porcupine_target (e.g. 15%)
  # of the Canadian (Upper) basin total.
  #   1) Identify Canadian segments (UY = Upper) and the Porcupine subset.
  #   2) Compute the current Porcupine and non-Porcupine Canadian totals.
  #   3) Solve new_porc / (non_porc_canada + new_porc) = target
  #        => new_porc = target / (1 - target) * non_porc_canada
  #   4) Apply that multiplier only to Porcupine segments.
  porc_idx          <- which(edges$Porc_off == 0)
  canada_idx        <- UY                              # Upper Yukon = Canada
  porc_canada_idx   <- intersect(canada_idx, porc_idx) # Porcupine within Canada

  porc_total        <- sum(basin_sum[porc_canada_idx], na.rm = TRUE)
  canada_total      <- sum(basin_sum[canada_idx],      na.rm = TRUE)
  non_porc_canada   <- canada_total - porc_total

  target            <- YUKON_PARAMS$porcupine_target
  if (porc_total > 0 && non_porc_canada > 0 && target > 0 && target < 1) {
    new_porc_total  <- (target / (1 - target)) * non_porc_canada
    porc_multiplier <- new_porc_total / porc_total
    basin_sum[porc_canada_idx] <- basin_sum[porc_canada_idx] * porc_multiplier
    cat(sprintf("  Porcupine rescaled to %.1f%% of Canadian total (multiplier = %.4f)\n",
                100 * target, porc_multiplier))
  }

  scale_factor <- 1

  runsizedat <- read_excel(PATHS$runsize)

  runsize    <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Yukon" & runsizedat$Year == year])

  total      <- sum(basin_sum, na.rm = TRUE)

  if (total > 0) {
    rescale     <- basin_sum / total
    norm        <- rescale / max(rescale, na.rm = TRUE)
    individuals <- rescale * runsize * scale_factor
  } else {
    rescale <- norm <- individuals <- rep(0, length(basin_sum))
  }

  cat(sprintf("  Segments with assignment > 0: %d / %d\n", sum(basin_sum > 0), nrow(edges)))

  # ---- CSV -------------------------------------------------------------------
  dir.create(PATHS$out_yukon_full, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order, iso_pred = edf$iso_pred,
               assignment_sum = basin_sum, assignment_rescale = rescale,
               assignment_norm = norm, assignment_individuals = individuals,
               GENLMU = edf$GenLMU),
    file.path(PATHS$out_yukon_full, sprintf("%d_%s_Assignment_Results.csv", year, region))
  )

  # ---- Map -------------------------------------------------------------------
  below_min <- !is.na(edges$Str_Order) & edges$Str_Order < YUKON_PARAMS$min_stream_order

  colcode <- rep("gray90", length(norm))
  colcode[norm == 0] <- "grey85"
  for (k in 1:9) colcode[norm > (k - 1) / 10 & norm <= k / 10] <- PALETTE[k]
  colcode[norm > 0.9] <- PALETTE[10]
  colcode[below_min] <- NA

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 3.7,
               ifelse(so >= 8, 5,
                      ifelse(so >= 7, 3.0,
                             ifelse(so >= 6, 2.0,
                                    ifelse(so >= 5, 1.5,
                                           ifelse(so >= 4, 1.5,
                                                   ifelse(so >= 3, 1.2, 0)))))))
  lw[so < YUKON_PARAMS$min_stream_order] <- 0

  dir.create(PATHS$map_yukon_full, recursive = TRUE, showWarnings = FALSE)
  png(file.path(PATHS$map_yukon_full, sprintf("%s_%d.png", region, year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Annual Production - Full Yukon Basin (first 50%% CPUE)\nYear: %d", year), bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = lw)
  legend("topleft", legend = LEGEND_LABELS, col = LEGEND_COLORS, lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")

  invisible(NULL)
}

# ==============================================================================
# DRIVER
# ==============================================================================
#
cat("\n### KUSKOKWIM (first 50% CPUE) ###\n")
for (yr in KUSKO_YEARS) {
  tryCatch(run_kusko(yr),
           error = function(e) cat("ERROR Kusko", yr, ":", e$message, "\n"))
}

cat("\n### YUKON_FULL (first 50% CPUE) ###\n")
for (yr in YUKON_YEARS) {
  tryCatch(run_yukon(yr),
           error = function(e) cat("ERROR Yukon_Full", yr, ":", e$message, "\n"))
}
