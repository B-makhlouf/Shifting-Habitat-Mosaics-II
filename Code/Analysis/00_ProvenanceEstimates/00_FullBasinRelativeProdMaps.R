################################################################################
# FULL BASIN FULL YEAR PRODUCTION MAPS — CONTINUOUS RELATIVE PRODUCTION
#
# Identical to 04_FullBasinIndividualsMaps.R in all assignment logic and
# map style (continuous YlOrRd ramp, gradient colorbar).
#
# Key difference from the discrete relative-abundance maps
# (01_FullBasinProductionEstimates.R):
#   - Color encodes `rescale` = basin_sum / total  (each segment's true share
#     of the run, proportional production), NOT `norm` = rescale / max(rescale).
#   - Because rescale is NOT divided by its annual maximum, the color scale
#     reflects real proportional values. The ramp auto-scales to each year's
#     actual maximum rescale value; the colorbar labels are shown as
#     percentages (rescale * 100) so values are directly comparable across years.
#
# Outputs per call:
#   - Figures/Maps/FullBasin_RelProd/<region>/<region>_<year>_relprod.png
#   (CSV outputs reuse the same paths as the original script)
################################################################################

suppressPackageStartupMessages({
  library(sf);       library(dplyr);       library(readr)
  library(readxl);   library(tibble);      library(tidyr)
  library(ggplot2);  library(RColorBrewer); library(here)
})

# ---- Paths -------------------------------------------------------------------
PATHS <- list(
  kusko_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges_geomorphAdded.shp"),
  kusko_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges_geomorphAdded.shp"),
  yukon_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  natal_dir      = here("Data", "Natal Origins"),
  runsize        = here("Data", "AYKEscapement.xlsx"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv"),
  out_kusko      = here("Outputs", "ProductionData", "Kusko"),
  out_yukon_full = here("Outputs", "ProductionData", "Yukon_full"),
  map_kusko      = here("Figures", "Maps", "FullBasin_RelProd", "Kusko"),
  map_yukon_full = here("Figures", "Maps", "FullBasin_RelProd", "Yukon")
)

KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2021)

source(here("Code", "Analysis", "00_ProvenanceEstimates", "params.R"))

# ---- Per-basin stream-order minimums (set inside each run_* function) --------
# Yukon: MIN_STREAM_ORDER = 4  |  Kuskokwim: MIN_STREAM_ORDER = 3

# ---- Continuous color helpers ------------------------------------------------
N_PAL    <- 500
PAL_CONT <- colorRampPalette(brewer.pal(9, "YlOrRd"))(N_PAL)

# Map a vector of proportional production values to colors, scaling to max.
color_continuous <- function(rescale_vals) {
  max_val <- max(rescale_vals, na.rm = TRUE)
  cols    <- rep("grey85", length(rescale_vals))
  if (max_val > 0) {
    has_prod       <- rescale_vals > 0
    idx            <- pmax(1L, ceiling(rescale_vals[has_prod] / max_val * N_PAL))
    cols[has_prod] <- PAL_CONT[idx]
  }
  cols
}

# Draw a vertical gradient colorbar. Labels show relative scale (0-1).
draw_colorbar <- function(n_steps = 200,
                          title = "Relative production") {
  usr <- par("usr")
  pw  <- usr[2] - usr[1]
  ph  <- usr[4] - usr[3]

  bx0 <- usr[1] + 0.030 * pw
  bx1 <- bx0    + 0.022 * pw
  by0 <- usr[3] + 0.55  * ph
  by1 <- usr[3] + 0.88  * ph

  pal  <- colorRampPalette(brewer.pal(9, "YlOrRd"))(n_steps)
  step <- (by1 - by0) / n_steps
  for (k in seq_len(n_steps)) {
    rect(bx0, by0 + (k - 1) * step, bx1, by0 + k * step,
         col = pal[k], border = NA)
  }
  rect(bx0, by0, bx1, by1, border = "black", lwd = 0.5)

  tick_fracs <- c(0, 0.25, 0.5, 0.75, 1.0)
  tick_y     <- by0 + tick_fracs * (by1 - by0)
  text(bx1 + 0.008 * pw, tick_y,
       tick_fracs, adj = 0, cex = 0.62)

  text((bx0 + bx1) / 2, by1 + 0.030 * ph,
       title, adj = 0.5, cex = 0.70, font = 2)
}

# ---- Spatial layers (loaded once) -------------------------------------------
KUSKO_EDGES <- st_read(PATHS$kusko_edges, quiet = TRUE)
KUSKO_BASIN <- st_read(PATHS$kusko_basin, quiet = TRUE)
KUSKO_EDGES <- st_transform(KUSKO_EDGES, st_crs(KUSKO_BASIN))

YUKON_EDGES <- st_read(PATHS$yukon_edges, quiet = TRUE)
YUKON_BASIN <- st_read(PATHS$yukon_basin, quiet = TRUE)
YUKON_EDGES <- st_transform(YUKON_EDGES, st_crs(YUKON_BASIN))

daily_gen_wide <- read_csv(PATHS$daily_genetics, show_col_types = FALSE) %>%
  select(sampleYear, DOY, genetic_assignment, proportion) %>%
  pivot_wider(names_from = genetic_assignment, values_from = proportion,
              values_fill = 0) %>%
  rename(year = sampleYear,
         avg_Lower = Lower, avg_Middle = Middle, avg_Upper = Upper)


# ==============================================================================
# KUSKOKWIM
# ==============================================================================
run_kusko <- function(year) {
  cat(sprintf("\n=== Kusko %d ===\n", year))
  MIN_STREAM_ORDER <- 3
  edges <- KUSKO_EDGES
  basin <- KUSKO_BASIN

  # ---- Natal + stratum weights -----------------------------------------------
  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )

  natal <- natal_raw %>% filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  if (nrow(natal) == 0) stop("No data available!")

  unique_days <- sort(unique(natal_raw$DOY))
  day_strata  <- tibble(
    DOY    = unique_days,
    strata = rep(1:5, each = ceiling(length(unique_days) / 5),
                 length.out = length(unique_days))
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

  cat(sprintf("  Segments: %d | Observations: %d\n", nrow(edges), nrow(natal)))

  # ---- Error + priors --------------------------------------------------------
  pid_iso        <- edges$iso_pred

  pid_isose     <- edges$isose_pred
  
  #min_error<- .0005
  #pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose) #Raise the lower limit

  pid_isose_mod  <- mean(edges$isose_pred, na.rm = TRUE) # Set the error as the mean error across the basin
  error          <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  #habitatprior       <- ifelse(edges$Spawner_IP > 0, 1, 0)
  presence_prior     <- ifelse(edges$Str_Order %in% c(7, 8) & edges$SPAWNING_C == 0, 0, 1)
  fixed_prior        <- stream_order_prior * edges$UniPh2oNoE * presence_prior

  # ---- Bayesian assignment ---------------------------------------------------
  A <- matrix(0, nrow = nrow(edges), ncol = nrow(natal))

  for (i in seq_len(nrow(natal))) {
    lik  <- (1 / sqrt(2 * pi * error^2)) *
              exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a    <- lik * fixed_prior
    an   <- a / sum(a)
    resc <- an / max(an)
    resc[resc < KUSKO_PARAMS$sensitivity_threshold] <- 0
    A[, i] <- resc
  }

  # ---- Finalize --------------------------------------------------------------
  basin_sum  <- rowSums(A, na.rm = TRUE)
  runsizedat <- read_excel(PATHS$runsize)
  runsize    <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Kusko" &
                                                 runsizedat$Year  == year])
  total      <- sum(basin_sum, na.rm = TRUE)
  if (total > 0) {
    rescale     <- basin_sum / total          # proportional production (NOT normalised)
    norm        <- rescale / max(rescale, na.rm = TRUE)
    individuals <- rescale * runsize
  } else {
    rescale <- norm <- individuals <- rep(0, length(basin_sum))
  }
  cat(sprintf("  Segments with assignment > 0: %d / %d\n",
              sum(basin_sum > 0), nrow(edges)))
  cat(sprintf("  Max proportional production per segment: %.4f (%.3f%%)\n",
              max(rescale), max(rescale) * 100))

  # ---- CSV -------------------------------------------------------------------
  dir.create(PATHS$out_kusko, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order,
               iso_pred = edf$iso_pred, assignment_sum = basin_sum,
               assignment_rescale = rescale, assignment_norm = norm,
               assignment_individuals = individuals),
    file.path(PATHS$out_kusko, sprintf("%d_Kusko_Assignment_Results.csv", year))
  )

  # ---- Map -------------------------------------------------------------------
  colcode <- color_continuous(rescale)
  colcode[stream_order_prior == 0] <- "gray70"

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 5.0,
        ifelse(so >= 8, 6.0,
        ifelse(so >= 7, 5.0,
        ifelse(so >= 6, 3.0,
        ifelse(so >= 5, 2.7,
        ifelse(so >= 4, 2.7,
        ifelse(so >= 3, 2.5,
        ifelse(so >= 2, 1.5, 0))))))))
  lw[so < MIN_STREAM_ORDER] <- 0
  lw[norm > 0.7 & lw > 0] <- lw[norm > 0.7 & lw > 0] + 0.8

  dir.create(PATHS$map_kusko, recursive = TRUE, showWarnings = FALSE)
  png(file.path(PATHS$map_kusko, sprintf("Kusko_%d_relprod.png", year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Annual Production - Kuskokwim\nYear: %d  |  Run size: %s",
                      year, format(round(runsize), big.mark = ",")),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")

  invisible(NULL)
}


# ==============================================================================
# YUKON  (Full basin: Lower + Middle + Upper)
# ==============================================================================
run_yukon <- function(year) {
  region <- "Yukon_Full"
  cat(sprintf("\n=== %s %d ===\n", region, year))
  MIN_STREAM_ORDER <- 4

  gen_cols <- c("Lower", "Middle", "Upper")

  edges <- YUKON_EDGES
  basin <- YUKON_BASIN

  LY <- which(tolower(edges$GenLMU) == "lower")
  MY <- which(tolower(edges$GenLMU) == "middle")
  UY <- which(tolower(edges$GenLMU) == "upper")

  # ---- Natal + genetic imputation + stratum weights -------------------------
  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Yukon_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )

  avg_cols  <- paste0("avg_", gen_cols)
  dgen_year <- daily_gen_wide %>%
    filter(year == !!year) %>%
    select(DOY, all_of(avg_cols))
  natal_raw <- natal_raw %>% left_join(dgen_year, by = "DOY")
  for (col in gen_cols) {
    ac <- paste0("avg_", col)
    natal_raw[[col]] <- ifelse(is.na(natal_raw[[col]]),
                               natal_raw[[ac]], natal_raw[[col]])
  }
  natal_raw <- natal_raw %>% select(-all_of(avg_cols))

  natal <- natal_raw %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop),
           if_all(all_of(gen_cols), ~ !is.na(.x)))
  if (nrow(natal) == 0) stop("No data available!")

  unique_days <- sort(unique(natal_raw$DOY))
  day_strata  <- tibble(
    DOY    = unique_days,
    strata = rep(1:5, each = ceiling(length(unique_days) / 5),
                 length.out = length(unique_days))
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
  pid_iso        <- edges$iso_pred

  pid_isose     <- edges$isose_pred
 # min_error<- 
  #pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose) #Raise the lower limit

  pid_isose_mod  <- mean(edges$isose_pred, na.rm = TRUE) # Set the error as the mean error across the basin
  error          <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  new_habitat_prior  <- ifelse(edges$slope > YUKON_PARAMS$channel_slope_cutoff, 0, 1)
  base_prior         <- stream_order_prior * new_habitat_prior

  # ---- Bayesian assignment ---------------------------------------------------
  A <- matrix(0, nrow = nrow(edges), ncol = nrow(natal))

  for (i in seq_len(nrow(natal))) {
    gen_prior     <- rep(0, nrow(edges))
    gen_prior[LY] <- as.numeric(natal$Lower[i])
    gen_prior[MY] <- as.numeric(natal$Middle[i])
    gen_prior[UY] <- as.numeric(natal$Upper[i])

    lik  <- (1 / sqrt(2 * pi * error^2)) *
              exp(-(natal$natal_iso[i] - pid_iso)^2 / (2 * error^2))
    a    <- lik * base_prior * gen_prior
    an   <- a / sum(a)
    resc <- an / max(an)
    resc[resc < YUKON_PARAMS$sensitivity_threshold] <- 0
    A[, i] <- resc * natal$weight[i]
  }

  # ---- Finalize (post-hoc Porcupine adjustment) ------------------------------
  basin_sum <- rowSums(A, na.rm = TRUE)

  porc_idx        <- which(edges$Porc_off == 0)
  canada_idx      <- UY
  porc_canada_idx <- intersect(canada_idx, porc_idx)

  porc_total      <- sum(basin_sum[porc_canada_idx], na.rm = TRUE)
  canada_total    <- sum(basin_sum[canada_idx],      na.rm = TRUE)
  non_porc_canada <- canada_total - porc_total

  target <- YUKON_PARAMS$porcupine_target
  if (porc_total > 0 && non_porc_canada > 0 && target > 0 && target < 1) {
    new_porc_total             <- (target / (1 - target)) * non_porc_canada
    porc_multiplier            <- new_porc_total / porc_total
    basin_sum[porc_canada_idx] <- basin_sum[porc_canada_idx] * porc_multiplier
    cat(sprintf("  Porcupine rescaled to %.1f%% of Canadian total (multiplier = %.4f)\n",
                100 * target, porc_multiplier))
  }

  runsizedat <- read_excel(PATHS$runsize)
  runsize    <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Yukon" &
                                                 runsizedat$Year  == year])
  total      <- sum(basin_sum, na.rm = TRUE)
  if (total > 0) {
    rescale     <- basin_sum / total          # proportional production (NOT normalised)
    norm        <- rescale / max(rescale, na.rm = TRUE)
    individuals <- rescale * runsize
  } else {
    rescale <- norm <- individuals <- rep(0, length(basin_sum))
  }
  cat(sprintf("  Segments with assignment > 0: %d / %d\n",
              sum(basin_sum > 0), nrow(edges)))
  cat(sprintf("  Max proportional production per segment: %.4f (%.3f%%)\n",
              max(rescale), max(rescale) * 100))

  # ---- CSV -------------------------------------------------------------------
  dir.create(PATHS$out_yukon_full, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order,
               iso_pred = edf$iso_pred, assignment_sum = basin_sum,
               assignment_rescale = rescale, assignment_norm = norm,
               assignment_individuals = individuals, GENLMU = edf$GenLMU),
    file.path(PATHS$out_yukon_full,
              sprintf("%d_%s_Assignment_Results.csv", year, region))
  )

  # ---- Map -------------------------------------------------------------------
  below_min <- !is.na(edges$Str_Order) & edges$Str_Order < MIN_STREAM_ORDER

  colcode <- color_continuous(rescale)
  colcode[below_min] <- NA

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 3.7,
        ifelse(so >= 8, 5.0,
        ifelse(so >= 7, 3.0,
        ifelse(so >= 6, 2.0,
        ifelse(so >= 5, 1.5,
        ifelse(so >= 4, 1.5,
        ifelse(so >= 3, 1.2,
        ifelse(so >= 2, 0.8, 0))))))))
  lw[so < MIN_STREAM_ORDER] <- 0
  lw[norm > 0.7 & lw > 0] <- lw[norm > 0.7 & lw > 0] + 0.8

  dir.create(PATHS$map_yukon_full, recursive = TRUE, showWarnings = FALSE)
  png(file.path(PATHS$map_yukon_full,
                sprintf("%s_%d_relprod.png", region, year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Annual Production - Full Yukon Basin\nYear: %d  |  Run size: %s",
                      year, format(round(runsize), big.mark = ",")),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")

  invisible(NULL)
}


# ==============================================================================
# DRIVER
# ==============================================================================
cat("\n### KUSKOKWIM ###\n")
for (yr in KUSKO_YEARS) {
  tryCatch(run_kusko(yr),
           error = function(e) cat("ERROR Kusko", yr, ":", e$message, "\n"))
}

cat("\n### YUKON_FULL ###\n")
for (yr in YUKON_YEARS) {
  tryCatch(run_yukon(yr),
           error = function(e) cat("ERROR Yukon_Full", yr, ":", e$message, "\n"))
}
