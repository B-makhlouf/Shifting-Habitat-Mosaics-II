################################################################################
# FULL BASIN FULL YEAR PRODUCTION MAPS — BINNED RELATIVE PRODUCTION
#
# Same assignment logic as 04_FullBasinIndividualsMaps.R, but the map style
# uses 10 discrete YlOrRd bins (relative production binned 0-1 by 0.1) with a
# stepped colorbar. All eligible reaches, including zero-production reaches,
# use the palette; below-min-stream-order segments are not drawn.
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
#   - Figures/01_ProdMaps/<region>/<region>_<year>_relprod.png
#   (CSV outputs reuse the same paths as the original script)
################################################################################

suppressPackageStartupMessages({
  library(sf);       library(dplyr);       library(readr)
  library(readxl);   library(tibble);      library(tidyr)
  library(ggplot2);  library(RColorBrewer); library(here)
})

# Retry transient Windows file-lock failures before skipping an entire year.
write_csv_retry <- function(x, path, attempts = 5L, wait_seconds = 0.5) {
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    ok <- tryCatch({
      readr::write_csv(x, path)
      TRUE
    }, error = function(e) {
      last_error <<- e
      FALSE
    })
    if (ok) return(invisible(path))
    if (attempt < attempts) Sys.sleep(wait_seconds)
  }
  stop(last_error)
}

# ---- Paths -------------------------------------------------------------------
PATHS <- list(
  kusko_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges_geomorphAdded.shp"),
  kusko_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges_geomorphAdded.shp"),
  yukon_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  natal_dir      = here("Data", "Natal Origins"),
  runsize        = here("Data", "AYKEscapement.xlsx"),
  out_kusko      = here("Outputs", "ProductionData", "Kusko"),
  out_yukon_full = here("Outputs", "ProductionData", "Yukon_full"),
  contour_kusko  = here("Outputs", "SensitivitySweep", "t0.9", "Kusko"),
  contour_yukon  = here("Outputs", "SensitivitySweep", "t0.9", "Yukon"),
  map_kusko      = here("Figures", "01_ProdMaps", "Kusko"),
  map_yukon_full = here("Figures", "01_ProdMaps", "Yukon")
)

source(here("Code", "Analysis", "params.R"))

# ---- Per-basin stream-order minimums come from params.R ----------------------

# ---- 01_ProdMaps-only aesthetics --------------------------------------------
# Ten equal bins span each map's 0-1 relative-production scale. Line width is
# based only on stream order and is never increased based on production.
BIN_BREAKS <- seq(0, 1, by = 0.1)
N_BINS     <- length(BIN_BREAKS) - 1
PAL_BINS  <- colorRampPalette(brewer.pal(9, "YlOrRd"))(N_BINS)

# Map relative-production values to ten colors after scaling to the map maximum.
color_binned <- function(rescale_vals) {
  max_val <- max(rescale_vals, na.rm = TRUE)
  norm_vals <- if (is.finite(max_val) && max_val > 0) {
    rescale_vals / max_val
  } else rep(0, length(rescale_vals))
  valid <- which(is.finite(norm_vals))
  cols <- rep(NA_character_, length(rescale_vals))
  idx <- cut(norm_vals[valid], breaks = BIN_BREAKS, include.lowest = TRUE,
             right = TRUE, labels = FALSE)
  cols[valid] <- PAL_BINS[pmax(1L, pmin(N_BINS, idx))]
  cols
}

# Draw a vertical discrete colorbar: N_BINS stacked blocks of the bin colors
# with tick labels at each break (matching color_binned).
# Sized large and left-aligned so it stays legible from a distance in a
# multi-panel figure and the title never clips off the left edge.
# Font sizes are driven by CBAR_CEX so every map scales together; bump this
# one value if the legend still reads too small in the assembled figure.
CBAR_CEX <- 2.0

draw_colorbar <- function(title = "Relative\nproduction") {
  usr <- par("usr")
  pw  <- usr[2] - usr[1]
  ph  <- usr[4] - usr[3]

  # Bar geometry (fractions of the plot region). Wider + taller than before.
  bx0 <- usr[1] + 0.045 * pw
  bx1 <- bx0    + 0.055 * pw
  by0 <- usr[3] + 0.46  * ph
  by1 <- usr[3] + 0.90  * ph

  bar_h <- by1 - by0
  step  <- bar_h / N_BINS

  for (k in seq_len(N_BINS)) {
    rect(bx0, by0 + (k - 1) * step, bx1, by0 + k * step,
         col = PAL_BINS[k], border = "white", lwd = 1.4)
  }
  rect(bx0, by0, bx1, by1, border = "grey30", lwd = 1.0)

  # Tick labels to the right of the bar.
  tick_y <- by0 + seq(0, N_BINS) * step
  text(bx1 + 0.014 * pw, tick_y,
       formatC(BIN_BREAKS, format = "f", digits = 2),
       adj = 0, cex = CBAR_CEX * 0.85)

  # Title left-aligned at the bar's left edge (two lines) so it cannot clip.
  text(bx0, by1 + 0.03 * ph, title,
       adj = c(0, 0), cex = CBAR_CEX, font = 2)
}

# ---- Spatial layers (loaded once) -------------------------------------------
KUSKO_EDGES <- st_read(PATHS$kusko_edges, quiet = TRUE)
KUSKO_BASIN <- st_read(PATHS$kusko_basin, quiet = TRUE)
KUSKO_EDGES <- st_transform(KUSKO_EDGES, st_crs(KUSKO_BASIN))

YUKON_EDGES <- st_read(PATHS$yukon_edges, quiet = TRUE)
YUKON_BASIN <- st_read(PATHS$yukon_basin, quiet = TRUE)
YUKON_EDGES <- st_transform(YUKON_EDGES, st_crs(YUKON_BASIN))

# ==============================================================================
# KUSKOKWIM
# ==============================================================================
run_kusko <- function(year,
                      sens_thresh = KUSKO_PARAMS$sensitivity_threshold,
                      out_dir     = PATHS$out_kusko,
                      draw_map    = TRUE) {
  cat(sprintf("\n=== Kusko %d ===\n", year))
  MIN_STREAM_ORDER <- KUSKO_PARAMS$min_stream_order
  edges <- KUSKO_EDGES
  basin <- KUSKO_BASIN

  # ---- Natal + stratum weights -----------------------------------------------
  natal_raw <- read_csv(
    file.path(PATHS$natal_dir,
              sprintf("%d_Kusko_Natal_Origins_Genetics_CPUE.csv", year)),
    show_col_types = FALSE
  )

  natal <- natal_raw %>% dplyr::filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
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
    left_join(strata_summary %>% dplyr::select(strata, weight), by = "strata")

  cat(sprintf("  Segments: %d | Observations: %d\n", nrow(edges), nrow(natal)))

  # ---- Error + priors --------------------------------------------------------
  pid_iso        <- edges$iso_pred

  pid_isose     <- edges$isose_pred
  hist(edges$isose_pred)
  
  
  min_error<- KUSKO_PARAMS$min_error
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose) #Raise the lower limit

  #pid_isose_mod  <- mean(edges$isose_pred, na.rm = TRUE) # Set the error as the mean error across the basin

  # OPTIONAL: limit isoscape error to within +/- 1 SD of the basin mean (winsorize
  # the per-reach isose_pred). Comment out the next line to run with the default above.
  #pid_isose_mod <- pmin(pmax(pid_isose, mean(pid_isose, na.rm = TRUE) - ( sd(pid_isose, na.rm = TRUE))), mean(pid_isose, na.rm = TRUE) + ( sd(pid_isose, na.rm = TRUE)))

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
    resc[resc < sens_thresh] <- 0
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
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv_retry(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order,
               iso_pred = edf$iso_pred, assignment_sum = basin_sum,
               assignment_rescale = rescale, assignment_norm = norm,
               assignment_individuals = individuals),
    file.path(out_dir, sprintf("%d_Kusko_Assignment_Results.csv", year))
  )

  if (!draw_map) return(invisible(NULL))

  # ---- Map -------------------------------------------------------------------
  below_min <- is.na(edges$Str_Order) | edges$Str_Order < MIN_STREAM_ORDER
  colcode <- color_binned(rescale)
  colcode[below_min] <- NA          # not drawn at all (lwd = 0 still renders a hairline)

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 5.0,
        ifelse(so >= 8, 6.0,
        ifelse(so >= 7, 5.0,
        ifelse(so >= 6, 3.5,
        ifelse(so >= 5, 3.0,
        ifelse(so >= 4, 2.2,
        ifelse(so >= 3, 2.5,
        ifelse(so >= 2, 1.5, 0))))))))
  lw[below_min] <- 0

  dir.create(PATHS$map_kusko, recursive = TRUE, showWarnings = FALSE)
  png(file.path(PATHS$map_kusko, sprintf("Kusko_%d_relprod.png", year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  # No per-map title and near-zero margins: the map fills the frame so it can be
  # tiled with the contour panels without wasted space at the top. The year is
  # supplied as a row label when the panels are assembled.
  par(mar = c(0.5, 0.5, 0.5, 0.5), bg = "white")
  plot(st_geometry(basin), col = "gray52", border = "#444444",
       lwd = 1.5, bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  # Legend is now drawn once (horizontal, shared) in 06_CombinedMapContour.R so
  # the map fills the frame. Uncomment for a stand-alone map with its own legend.
  # draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")

  invisible(NULL)
}


# ==============================================================================
# YUKON  (Full basin: Lower + Middle + Upper)
# ==============================================================================
run_yukon <- function(year,
                      sens_thresh = YUKON_PARAMS$sensitivity_threshold,
                      out_dir     = PATHS$out_yukon_full,
                      draw_map    = TRUE) {
  region <- "Yukon_Full"
  cat(sprintf("\n=== %s %d ===\n", region, year))
  MIN_STREAM_ORDER <- YUKON_PARAMS$min_stream_order

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

  # Use the final 2016 otolith-collection date (June 30; DOY 182) as the
  # common seasonal endpoint for every Yukon year.
  # To restore the full sampling period, comment out the next four lines.
  yukon_rows_before_date_cutoff <- nrow(natal_raw)
  natal_raw <- natal_raw %>% dplyr::filter(!is.na(DOY), DOY <= 182)
  cat(sprintf("  Fish/records after 2016 date cutoff (DOY 182): %d (excluded %d)\n",
              nrow(natal_raw), yukon_rows_before_date_cutoff - nrow(natal_raw)))

  # Treat any incomplete genetics row as missing.
  missing_genetics <- !stats::complete.cases(natal_raw[, gen_cols])

  # Current approach: assign fish without a complete genetic assignment equal
  # weight in all three reporting regions.
  natal_raw[missing_genetics, gen_cols] <- 1 / length(gen_cols)
  cat(sprintf("  Missing genetics assigned equal regional weights: %d\n",
              sum(missing_genetics)))

  # Alternative approach (kept here so it can be restored): exclude fish
  # without complete genetic data.
  # natal_raw <- natal_raw[!missing_genetics, , drop = FALSE]
  # cat(sprintf("  Fish excluded due to missing genetics: %d\n",
  #             sum(missing_genetics)))

  natal <- natal_raw %>%
    dplyr::filter(!is.na(natal_iso), !is.na(dailyCPUEprop),
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
    left_join(strata_summary %>% dplyr::select(strata, weight), by = "strata")

  cat(sprintf("  Segments: %d (L=%d, M=%d, U=%d) | Observations: %d\n",
              nrow(edges), length(LY), length(MY), length(UY), nrow(natal)))

  # ---- Error + priors --------------------------------------------------------
  pid_iso        <- edges$iso_pred

  pid_isose     <- edges$isose_pred
  
  hist(edges$isose_pred)
  min_error<- YUKON_PARAMS$min_error
  
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose) #Raise the lower limit

  #pid_isose_mod  <- mean(edges$isose_pred, na.rm = TRUE) # Set the error as the mean error across the basin

  # OPTIONAL: limit isoscape error to within +/- 1 SD of the basin mean (winsorize
  # the per-reach isose_pred). Comment out the next line to run with the default above.
  #pid_isose_mod <- pmin(pmax(pid_isose, mean(pid_isose, na.rm = TRUE) - ( sd(pid_isose, na.rm = TRUE))), mean(pid_isose, na.rm = TRUE) + ( sd(pid_isose, na.rm = TRUE)))
  
  error          <- sqrt(pid_isose_mod^2 + (0.0003133684 / 1.96)^2 + (0.00011 / 2)^2)
  presence_prior     <- ifelse(edges$Str_Order %in% c(8, 9) & edges$SPAWNING_C == 0, 0, 1)
  # Presence prior applies only to the Lower + Middle portions; the Canadian
  # (Upper) reaches are not impacted by it.
  presence_prior[UY] <- 1
  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
  new_habitat_prior  <- ifelse(edges$slope > YUKON_PARAMS$channel_slope_cutoff, 0, 1)
  base_prior         <- stream_order_prior * new_habitat_prior *presence_prior

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
    resc[resc < sens_thresh] <- 0
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
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  edf <- st_drop_geometry(edges)
  write_csv_retry(
    data.frame(reachid = edf$reachid, Str_Order = edf$Str_Order,
               iso_pred = edf$iso_pred, assignment_sum = basin_sum,
               assignment_rescale = rescale, assignment_norm = norm,
               assignment_individuals = individuals, GENLMU = edf$GenLMU),
    file.path(out_dir,
              sprintf("%d_%s_Assignment_Results.csv", year, region))
  )

  if (!draw_map) return(invisible(NULL))

  # ---- Map -------------------------------------------------------------------
  below_min <- is.na(edges$Str_Order) | edges$Str_Order < MIN_STREAM_ORDER

  colcode <- color_binned(rescale)
  colcode[below_min] <- NA

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 3.7,
        ifelse(so >= 8, 5.0,
        ifelse(so >= 7, 3.0,
        ifelse(so >= 6, 2.0,
        ifelse(so >= 5, 2.0,
        ifelse(so >= 4, 2.0,
        ifelse(so >= 3, 1.2,
        ifelse(so >= 2, 0.8, 0))))))))
  lw[below_min] <- 0

  dir.create(PATHS$map_yukon_full, recursive = TRUE, showWarnings = FALSE)
  png(file.path(PATHS$map_yukon_full,
                sprintf("%s_%d_relprod.png", region, year)),
      width = 9, height = 8, units = "in", res = 300, bg = "white")
  # No per-map title and near-zero margins: the map fills the frame so it can be
  # tiled with the contour panels without wasted space at the top. The year is
  # supplied as a row label when the panels are assembled.
  par(mar = c(0.5, 0.5, 0.5, 0.5), bg = "white")
  plot(st_geometry(basin), col = "gray52", border = "#444444",
       lwd = 1.5, bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  # Legend is now drawn once (horizontal, shared) in 06_CombinedMapContour.R so
  # the map fills the frame. Uncomment for a stand-alone map with its own legend.
  # draw_colorbar()
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

# ==============================================================================
# CONTOUR DATA PASS  (sensitivity threshold = 0.9)
#
# The density-contour figures (02_ContourThreshnew.R) read a sensitivity-
# thresholded (tau = 0.9) version of the assignment results from
# Outputs/SensitivitySweep/t0.9/. Re-running the SAME assignment computation
# here at tau = 0.9 regenerates that data from the current production code, so
# no contour-pass maps are drawn.
# ==============================================================================
CONTOUR_SENS_THRESHOLD <- 0.9

cat("\n### CONTOUR DATA (tau = 0.9): KUSKOKWIM ###\n")
for (yr in KUSKO_YEARS) {
  tryCatch(run_kusko(yr, sens_thresh = CONTOUR_SENS_THRESHOLD,
                     out_dir = PATHS$contour_kusko, draw_map = FALSE),
           error = function(e) cat("ERROR Kusko contour", yr, ":", e$message, "\n"))
}

cat("\n### CONTOUR DATA (tau = 0.9): YUKON_FULL ###\n")
for (yr in YUKON_YEARS) {
  tryCatch(run_yukon(yr, sens_thresh = CONTOUR_SENS_THRESHOLD,
                     out_dir = PATHS$contour_yukon, draw_map = FALSE),
           error = function(e) cat("ERROR Yukon contour", yr, ":", e$message, "\n"))
}

cat("\nDone.\n")
