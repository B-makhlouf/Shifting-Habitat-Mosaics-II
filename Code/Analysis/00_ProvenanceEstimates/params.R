################################################################################
# SHARED ANALYSIS PARAMETERS
#
# Single source of truth for all production-map and contour-map scripts in
# Code/NewAnalysis. Edit values here; changes propagate on next source().
################################################################################

# ---- Kuskokwim ---------------------------------------------------------------
KUSKO_PARAMS <- list(
  min_stream_order      = 2,        # Minimum Strahler stream order included
  min_error             = 0.0006,   # Lower-bound clamp on pid_isose error
  max_error             = 0.00089,  # Upper-bound clamp (Quartiles analysis)
  sensitivity_threshold = 0.9,      # Rescaled assignment values below this -> 0
  channel_slope_cutoff  = 2.0     # NewHabitatPrior: Channel_sl > this -> excluded
)

# ---- Yukon -------------------------------------------------------------------
YUKON_PARAMS <- list(
  min_stream_order      = 3,
  min_error             = 0.0035,
  sensitivity_threshold = 0.9,
  channel_slope_cutoff  = 2.0,
  porcupine_target      = 0.10      # Target proportion of Canadian basin assigned to Porcupine
)

# ---- Shared ------------------------------------------------------------------
PRODUCTION_THRESHOLD <- 0.7  # Minimum normalised production value to include a segment
TEMP_INTERVAL_DAYS   <- 3    # Temporal sampling interval for temperature extraction (days)
