################################################################################
# FULL BASIN FULL YEAR PRODUCTION MAPS — COLORED BY NUMBER OF FISH
#   with a GLOBAL color scale across all years and both rivers
#
# Identical to 01_FullBasinProductionEstimates.R in all assignment logic.
# Key differences:
#   - Color encodes estimated fish count per segment (rescale * runsize).
#   - The color ramp is anchored to the GLOBAL maximum individuals value
#     across ALL years and BOTH rivers, so a segment that received more fish
#     in an absolute sense always appears darker, regardless of within-year
#     relative production.
#   - Continuous YlOrRd ramp with a gradient colorbar legend.
#   - MIN_STREAM_ORDER = 2 (local override; does not affect params.R).
#
# Two-pass structure:
#   Pass 1 – compute_kusko() / compute_yukon(): run the Bayesian assignment
#             for every year and return results as a list (no plotting).
#   Pass 2 – map_kusko()    / map_yukon()     : draw maps using the global
#             maximum derived from all Pass-1 results.
#
# Outputs:
#   - Figures/Maps/FullBasin_Individuals/<region>/<region>_<year>_individuals.png
#   - Outputs/ProductionData/<region>/<year>_*_Assignment_Results.csv
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
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  yukon_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  natal_dir      = here("Data", "Natal Origins"),
  runsize        = here("Data", "AYKEscapement.xlsx"),
  daily_genetics = here("Data", "Genetics", "daily_genetic_proportions.csv"),
  out_kusko           = here("Outputs", "ProductionData", "Kusko"),
  out_yukon_full      = here("Outputs", "ProductionData", "Yukon_full"),
  # Global-scale maps (all years normalized to the same cap)
  map_kusko           = here("Figures", "Maps", "FullBasin_Individuals", "Kusko"),
  map_yukon_full      = here("Figures", "Maps", "FullBasin_Individuals", "Yukon"),
  # Per-year maps (each year normalized to its own cap)
  map_kusko_byyear    = here("Figures", "Maps", "FullBasin_Individuals_ByYear", "Kusko"),
  map_yukon_byyear    = here("Figures", "Maps", "FullBasin_Individuals_ByYear", "Yukon")
)

KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2018, 2021)

source(here("Code", "Analysis", "00_ProvenanceEstimates", "params.R"))

# ---- Local stream-order override (does NOT affect params.R) ------------------
MIN_STREAM_ORDER <- 3
min_error<- .0005

# ---- Continuous color helpers ------------------------------------------------
N_PAL    <- 500
PAL_CONT <- colorRampPalette(brewer.pal(9, "YlOrRd"))(N_PAL)

# Quantile used to set the global cap. Values at or above this percentile of
# the non-zero distribution are shown in the darkest color. Reducing this (e.g.
# to 0.95) compresses the scale further and pulls more color out of lean years.
SCALE_QUANTILE <- 0.99

# Map individuals to colors using a supplied global cap value.
# Full YlOrRd ramp across the entire 0-1 range; anything above global_max
# is clipped to the darkest color.
color_continuous <- function(individuals, global_max) {
  cols <- rep("grey85", length(individuals))   # zero-fish segments
  if (global_max > 0) {
    has_fish       <- individuals > 0
    norm           <- pmin(individuals[has_fish] / global_max, 1)
    idx            <- pmax(1L, ceiling(norm * N_PAL))
    cols[has_fish] <- PAL_CONT[idx]
  }
  cols
}

# Vertical gradient colorbar matching the two-zone color scheme:
#   bottom half (0-0.5) -> solid dark grey
#   top half    (0.5-1) -> continuous YlOrRd ramp
draw_colorbar <- function(global_max, n_steps = 200) {
  usr <- par("usr")
  pw  <- usr[2] - usr[1]
  ph  <- usr[4] - usr[3]

  bx0 <- usr[1] + 0.030 * pw
  bx1 <- bx0    + 0.022 * pw
  by0 <- usr[3] + 0.55  * ph
  by1 <- usr[3] + 0.88  * ph

  # Full continuous color ramp from bottom to top
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
       "Relative production\n(global scale)", adj = 0.5, cex = 0.65, font = 2)
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
# PASS 1 — COMPUTE ASSIGNMENTS (no plotting)
# ==============================================================================

compute_kusko <- function(year) {
  cat(sprintf("\n  [Kusko %d] computing...\n", year))

  edges <- KUSKO_EDGES

  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )
  natal <- natal_raw %>% filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  if (nrow(natal) == 0) stop("No data available!")

  unique_days    <- sort(unique(natal_raw$DOY))
  day_strata     <- tibble(
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

  pid_iso        <- edges$iso_pred

  #pid_isose     <- edges$isose_pred
  #pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose) #Raise the lower limit

  pid_isose_mod  <- mean(edges$isose_pred, na.rm = TRUE) # Set the error as the mean error across the basin
  error          <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  presence_prior     <- ifelse(edges$Str_Order %in% c(7, 8) & edges$SPAWNING_C == 0, 0, 1)
  fixed_prior        <- stream_order_prior * edges$UniPh2oNoE * presence_prior

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

  basin_sum  <- rowSums(A, na.rm = TRUE)
  runsizedat <- read_excel(PATHS$runsize)
  runsize    <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Kusko" &
                                                 runsizedat$Year  == year])
  total      <- sum(basin_sum, na.rm = TRUE)
  if (total > 0) {
    rescale     <- basin_sum / total
    norm        <- rescale / max(rescale, na.rm = TRUE)
    individuals <- rescale * runsize
  } else {
    rescale <- norm <- individuals <- rep(0, length(basin_sum))
  }
  cat(sprintf("    Max individuals: %.0f\n", max(individuals)))

  # Write CSV
  dir.create(PATHS$out_kusko, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order,
               iso_pred = edf$iso_pred, assignment_sum = basin_sum,
               assignment_rescale = rescale, assignment_norm = norm,
               assignment_individuals = individuals),
    file.path(PATHS$out_kusko, sprintf("%d_Kusko_Assignment_Results.csv", year))
  )

  list(year = year, river = "Kusko",
       edges = edges, basin = KUSKO_BASIN,
       individuals = individuals,
       stream_order_prior = stream_order_prior,
       runsize = runsize)
}


compute_yukon <- function(year) {
  region   <- "Yukon_Full"
  gen_cols <- c("Lower", "Middle", "Upper")
  cat(sprintf("\n  [%s %d] computing...\n", region, year))

  edges <- YUKON_EDGES
  basin <- YUKON_BASIN
  LY    <- which(tolower(edges$GenLMU) == "lower")
  MY    <- which(tolower(edges$GenLMU) == "middle")
  UY    <- which(tolower(edges$GenLMU) == "upper")

  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Yukon_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )
  avg_cols  <- paste0("avg_", gen_cols)
  dgen_year <- daily_gen_wide %>% filter(year == !!year) %>%
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

  unique_days    <- sort(unique(natal_raw$DOY))
  day_strata     <- tibble(
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

  pid_iso        <- edges$iso_pred
  
  #pid_isose<- edges$isose_pred
  #pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose) #Raise the lower limit
  
  pid_isose_mod <- mean(edges$isose_pred, na.rm = TRUE) # Set the error as the mean error across the basin 
  error          <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  base_prior         <- stream_order_prior 

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

  basin_sum       <- rowSums(A, na.rm = TRUE)
  porc_idx        <- which(edges$Porc_off == 0)
  porc_canada_idx <- intersect(UY, porc_idx)
  porc_total      <- sum(basin_sum[porc_canada_idx], na.rm = TRUE)
  non_porc_canada <- sum(basin_sum[UY], na.rm = TRUE) - porc_total
  target          <- YUKON_PARAMS$porcupine_target
  
  if (porc_total > 0 && non_porc_canada > 0 && target > 0 && target < 1) {
    porc_multiplier            <- (target / (1 - target)) * non_porc_canada / porc_total
    basin_sum[porc_canada_idx] <- basin_sum[porc_canada_idx] * porc_multiplier
    cat(sprintf("    Porcupine rescaled (multiplier = %.4f)\n", porc_multiplier))
  }

  runsizedat <- read_excel(PATHS$runsize)
  runsize    <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Yukon" &
                                                 runsizedat$Year  == year])
  total      <- sum(basin_sum, na.rm = TRUE)
  if (total > 0) {
    rescale     <- basin_sum / total
    norm        <- rescale / max(rescale, na.rm = TRUE)
    individuals <- rescale * runsize
  } else {
    rescale <- norm <- individuals <- rep(0, length(basin_sum))
  }
  cat(sprintf("    Max individuals: %.0f\n", max(individuals)))

  # Write CSV
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

  list(year = year, river = region,
       edges = edges, basin = basin,
       individuals = individuals,
       stream_order_prior = stream_order_prior,
       runsize = runsize)
}


# ==============================================================================
# PASS 2 — MAKE MAPS using the global maximum
# ==============================================================================

map_kusko <- function(res, global_max, outdir) {
  year     <- res$year
  edges    <- res$edges
  basin    <- res$basin
  indiv    <- res$individuals
  so_prior <- res$stream_order_prior
  runsize  <- res$runsize

  colcode <- color_continuous(indiv, global_max)
  colcode[so_prior == 0] <- "gray70"

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

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  png(file.path(outdir, sprintf("Kusko_%d_individuals.png", year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Annual Production - Kuskokwim\nYear: %d  |  Run size: %s",
                      year, format(round(runsize), big.mark = ",")),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar(global_max)
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved Kusko %d -> %s\n", year, outdir))
}


map_yukon <- function(res, global_max, outdir) {
  year     <- res$year
  region   <- res$river
  edges    <- res$edges
  basin    <- res$basin
  indiv    <- res$individuals
  so_prior <- res$stream_order_prior
  runsize  <- res$runsize

  below_min <- !is.na(edges$Str_Order) & edges$Str_Order < MIN_STREAM_ORDER
  colcode   <- color_continuous(indiv, global_max)
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

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  png(file.path(outdir, sprintf("%s_%d_individuals.png", region, year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf("Annual Production - Full Yukon Basin\nYear: %d  |  Run size: %s",
                      year, format(round(runsize), big.mark = ",")),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar(global_max)
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved %s %d -> %s\n", region, year, outdir))
}


# ==============================================================================
# STACKED PANEL FIGURES
# ==============================================================================

# Stacked single-column figure for all Kusko years.
make_stacked_kusko <- function(results, years, global_max, outdir) {
  valid <- Filter(Negate(is.null), results[as.character(years)])
  n     <- length(valid)
  if (n == 0) return(invisible(NULL))

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  png(file.path(outdir, "Kusko_all_years.png"),
      width = 9, height = 4.5 * n, units = "in", res = 300, bg = "white")
  par(mfrow = c(n, 1), mar = c(1.5, 1.5, 2.5, 1.5), bg = "white")

  for (res in valid) {
    edges    <- res$edges
    basin    <- res$basin
    indiv    <- res$individuals
    so_prior <- res$stream_order_prior
    runsize  <- res$runsize
    year     <- res$year

    colcode <- color_continuous(indiv, global_max)
    colcode[so_prior == 0] <- "gray70"

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

    plot(st_geometry(basin), col = "gray60", border = "gray60",
         main = sprintf("Kuskokwim %d  |  Run size: %s",
                        year, format(round(runsize), big.mark = ",")),
         bg = "white")
    plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
    draw_colorbar(global_max)
  }

  dev.off()
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved Kusko stacked (%d years) -> %s\n", n, outdir))
}


# Stacked single-column figure for all Yukon years.
make_stacked_yukon <- function(results, years, global_max, outdir) {
  valid <- Filter(Negate(is.null), results[as.character(years)])
  n     <- length(valid)
  if (n == 0) return(invisible(NULL))

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  png(file.path(outdir, "Yukon_all_years.png"),
      width = 9, height = 4.5 * n, units = "in", res = 300, bg = "white")
  par(mfrow = c(n, 1), mar = c(1.5, 1.5, 2.5, 1.5), bg = "white")

  for (res in valid) {
    edges    <- res$edges
    basin    <- res$basin
    indiv    <- res$individuals
    runsize  <- res$runsize
    year     <- res$year

    below_min <- !is.na(edges$Str_Order) & edges$Str_Order < MIN_STREAM_ORDER
    colcode   <- color_continuous(indiv, global_max)
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

    plot(st_geometry(basin), col = "gray60", border = "gray60",
         main = sprintf("Yukon %d  |  Run size: %s",
                        year, format(round(runsize), big.mark = ",")),
         bg = "white")
    plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
    draw_colorbar(global_max)
  }

  dev.off()
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved Yukon stacked (%d years) -> %s\n", n, outdir))
}


# ==============================================================================
# DRIVER
# ==============================================================================

# -- Pass 1: compute all assignments -------------------------------------------
cat("\n### PASS 1: COMPUTING ASSIGNMENTS ###\n")

kusko_results <- list()
for (yr in KUSKO_YEARS) {
  kusko_results[[as.character(yr)]] <-
    tryCatch(compute_kusko(yr),
             error = function(e) { cat("ERROR Kusko", yr, ":", e$message, "\n"); NULL })
}

yukon_results <- list()
for (yr in YUKON_YEARS) {
  yukon_results[[as.character(yr)]] <-
    tryCatch(compute_yukon(yr),
             error = function(e) { cat("ERROR Yukon", yr, ":", e$message, "\n"); NULL })
}

# -- Find global scale cap across all years and both rivers --------------------
# Use SCALE_QUANTILE of the non-zero distribution rather than the absolute max,
# so a single outlier year doesn't wash out the rest of the maps.
all_individuals <- c(
  unlist(lapply(Filter(Negate(is.null), kusko_results), `[[`, "individuals")),
  unlist(lapply(Filter(Negate(is.null), yukon_results), `[[`, "individuals"))
)
nonzero_individuals <- all_individuals[all_individuals > 0]
GLOBAL_MAX <- quantile(nonzero_individuals, SCALE_QUANTILE, na.rm = TRUE)
cat(sprintf("\n### GLOBAL SCALE CAP (%.0f%% quantile): %.0f fish/segment ###\n",
            SCALE_QUANTILE * 100, GLOBAL_MAX))
cat(sprintf("    (absolute max was %.0f; %.1f%% of segments are clipped to max color)\n",
            max(nonzero_individuals),
            100 * mean(nonzero_individuals > GLOBAL_MAX)))

# -- Pass 2a: global-scale individual maps + stacked panel ---------------------
cat("\n### PASS 2a: GLOBAL-SCALE MAPS ###\n")

for (yr in KUSKO_YEARS) {
  res <- kusko_results[[as.character(yr)]]
  if (!is.null(res)) map_kusko(res, GLOBAL_MAX, PATHS$map_kusko)
}
for (yr in YUKON_YEARS) {
  res <- yukon_results[[as.character(yr)]]
  if (!is.null(res)) map_yukon(res, GLOBAL_MAX, PATHS$map_yukon_full)
}

make_stacked_kusko(kusko_results, KUSKO_YEARS, GLOBAL_MAX, PATHS$map_kusko)
make_stacked_yukon(yukon_results, YUKON_YEARS, GLOBAL_MAX, PATHS$map_yukon_full)

# -- Pass 2b: per-year maps + stacked panel ------------------------------------
cat("\n### PASS 2b: PER-YEAR MAPS ###\n")

for (yr in KUSKO_YEARS) {
  res <- kusko_results[[as.character(yr)]]
  if (!is.null(res)) {
    nz       <- res$individuals[res$individuals > 0]
    year_max <- quantile(nz, SCALE_QUANTILE, na.rm = TRUE)
    cat(sprintf("  Kusko %d year cap: %.0f fish/segment\n", yr, year_max))
    map_kusko(res, year_max, PATHS$map_kusko_byyear)
  }
}
for (yr in YUKON_YEARS) {
  res <- yukon_results[[as.character(yr)]]
  if (!is.null(res)) {
    nz       <- res$individuals[res$individuals > 0]
    year_max <- quantile(nz, SCALE_QUANTILE, na.rm = TRUE)
    cat(sprintf("  Yukon %d year cap: %.0f fish/segment\n", yr, year_max))
    map_yukon(res, year_max, PATHS$map_yukon_byyear)
  }
}

# Stacked per-year panels: each year's own cap, so recompute per-year maxes
kusko_year_maxes <- sapply(KUSKO_YEARS, function(yr) {
  res <- kusko_results[[as.character(yr)]]
  if (is.null(res)) return(NA)
  quantile(res$individuals[res$individuals > 0], SCALE_QUANTILE, na.rm = TRUE)
})
yukon_year_maxes <- sapply(YUKON_YEARS, function(yr) {
  res <- yukon_results[[as.character(yr)]]
  if (is.null(res)) return(NA)
  quantile(res$individuals[res$individuals > 0], SCALE_QUANTILE, na.rm = TRUE)
})

# For the stacked by-year figure the panels share the same colour scale only
# within that figure — we use each year's own cap but pass it consistently.
# The simplest approach: use each year's cap inside a custom stacked call.
make_stacked_kusko_byyear <- function(results, years, year_maxes, outdir) {
  valid_idx <- which(!sapply(results[as.character(years)], is.null))
  n         <- length(valid_idx)
  if (n == 0) return(invisible(NULL))

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  png(file.path(outdir, "Kusko_all_years.png"),
      width = 9, height = 4.5 * n, units = "in", res = 300, bg = "white")
  par(mfrow = c(n, 1), mar = c(1.5, 1.5, 2.5, 1.5), bg = "white")

  for (i in valid_idx) {
    yr        <- years[i]
    res       <- results[[as.character(yr)]]
    gmax      <- year_maxes[i]
    edges     <- res$edges;  basin <- res$basin
    indiv     <- res$individuals;  runsize <- res$runsize
    so_prior  <- res$stream_order_prior

    colcode <- color_continuous(indiv, gmax)
    colcode[so_prior == 0] <- "gray70"

    so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
    lw <- ifelse(so >= 9, 5.0, ifelse(so >= 8, 6.0, ifelse(so >= 7, 5.0,
          ifelse(so >= 6, 3.0, ifelse(so >= 5, 2.7, ifelse(so >= 4, 2.7,
          ifelse(so >= 3, 2.5, ifelse(so >= 2, 1.5, 0))))))))
    lw[so < MIN_STREAM_ORDER] <- 0

    plot(st_geometry(basin), col = "gray60", border = "gray60",
         main = sprintf("Kuskokwim %d  |  Run size: %s",
                        yr, format(round(runsize), big.mark = ",")),
         bg = "white")
    plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
    draw_colorbar(gmax)
  }

  dev.off()
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved Kusko by-year stacked -> %s\n", outdir))
}

make_stacked_yukon_byyear <- function(results, years, year_maxes, outdir) {
  valid_idx <- which(!sapply(results[as.character(years)], is.null))
  n         <- length(valid_idx)
  if (n == 0) return(invisible(NULL))

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  png(file.path(outdir, "Yukon_all_years.png"),
      width = 9, height = 4.5 * n, units = "in", res = 300, bg = "white")
  par(mfrow = c(n, 1), mar = c(1.5, 1.5, 2.5, 1.5), bg = "white")

  for (i in valid_idx) {
    yr    <- years[i]
    res   <- results[[as.character(yr)]]
    gmax  <- year_maxes[i]
    edges <- res$edges;  basin <- res$basin
    indiv <- res$individuals;  runsize <- res$runsize

    below_min <- !is.na(edges$Str_Order) & edges$Str_Order < MIN_STREAM_ORDER
    colcode   <- color_continuous(indiv, gmax)
    colcode[below_min] <- NA

    so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
    lw <- ifelse(so >= 9, 3.7, ifelse(so >= 8, 5.0, ifelse(so >= 7, 3.0,
          ifelse(so >= 6, 2.0, ifelse(so >= 5, 1.5, ifelse(so >= 4, 1.5,
          ifelse(so >= 3, 1.2, ifelse(so >= 2, 0.8, 0))))))))
    lw[so < MIN_STREAM_ORDER] <- 0

    plot(st_geometry(basin), col = "gray60", border = "gray60",
         main = sprintf("Yukon %d  |  Run size: %s",
                        yr, format(round(runsize), big.mark = ",")),
         bg = "white")
    plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
    draw_colorbar(gmax)
  }

  dev.off()
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved Yukon by-year stacked -> %s\n", outdir))
}

make_stacked_kusko_byyear(kusko_results, KUSKO_YEARS, kusko_year_maxes, PATHS$map_kusko_byyear)
make_stacked_yukon_byyear(yukon_results, YUKON_YEARS, yukon_year_maxes, PATHS$map_yukon_byyear)

cat("\nDone.\n")
