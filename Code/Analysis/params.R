################################################################################
# SHARED ANALYSIS PARAMETERS
#
# Single source of truth for all analysis and figure scripts in Code/Analysis.
# Add or remove years only here; changes propagate on next source().
################################################################################

# ---- Watershed-year coverage -------------------------------------------------
# These need not contain the same years. Downstream analyses and figures use
# only the configured years, even if old output CSVs remain on disk.
ANALYSIS_YEARS <- list(
  Kuskokwim = c(2017L, 2018L, 2019L, 2020L, 2021L, 2022L),
  Yukon     = c(2015L, 2016L, 2018L, 2021L)
)

# Validate once, near the configuration, so typos fail with a useful message.
if (!identical(sort(names(ANALYSIS_YEARS)), sort(c("Kuskokwim", "Yukon")))) {
  stop("ANALYSIS_YEARS must contain Kuskokwim and Yukon entries")
}
ANALYSIS_YEARS <- lapply(ANALYSIS_YEARS, function(x) {
  x <- sort(unique(as.integer(x)))
  if (!length(x) || anyNA(x) || any(x < 1900L | x > 2200L)) {
    stop("Each ANALYSIS_YEARS entry must contain valid calendar years")
  }
  x
})

# Backward-compatible aliases used throughout the scripts.
KUSKO_YEARS <- ANALYSIS_YEARS$Kuskokwim
YUKON_YEARS <- ANALYSIS_YEARS$Yukon

# ---- Kuskokwim ---------------------------------------------------------------
KUSKO_PARAMS <- list(
  min_stream_order      = 3,        # Minimum Strahler stream order included
  min_error             = 0.0000,   # Lower-bound clamp on pid_isose error
  max_error             = 0.00089,  # Upper-bound clamp (Quartiles analysis)
  sensitivity_threshold = 0,
  channel_slope_cutoff  = 2.0     # NewHabitatPrior: Channel_sl > this -> excluded
)

# ---- Yukon -------------------------------------------------------------------
YUKON_PARAMS <- list(
  min_stream_order      = 4,
  min_error             = 0.0016,
  sensitivity_threshold = 0.0,
  channel_slope_cutoff  = 2.0,
  porcupine_target      = 0.10     # Target proportion of Canadian basin assigned to Porcupine
)

# ---- Contour figures ---------------------------------------------------------
# assignment_norm minimum for a reach to be included in the density-contour
# figures (02_ContourThreshnewR). Applies to both basins. Raise to keep only
# higher-confidence assignments; lower to include more reaches.
CONTOUR_FILT_THRESH <- 0.0

