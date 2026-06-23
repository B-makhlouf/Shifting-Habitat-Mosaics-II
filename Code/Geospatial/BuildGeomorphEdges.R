################################################################################
# BUILD GEOMORPH-ADDED EDGE SHAPEFILES
#
# One-time (or on-demand) preprocessing step.
#
# For each source edge shapefile listed in JOBS, this script:
#   1. Reads the shapefile from Data/Spatial Data/AnalysisShapefiles
#   2. Computes per-segment geomorphology from the matching DEM:
#        mean_elev, z1, z2, length_m, slope (percent, unsigned, NA -> 0)
#   3. Writes a new shapefile to the same folder with "_geomorphAdded"
#      appended to the name (e.g. Kusko_edges_geomorphAdded.shp).
#
# The geomorph-added shapefiles are the inputs consumed by the production
# and contour analyses, so slope no longer has to be recomputed at run time.
#
# Re-running this script ALWAYS overwrites the existing _geomorphAdded
# shapefiles (delete_dsn = TRUE), so it is safe to use as a refresh step.
################################################################################

suppressPackageStartupMessages({
  library(sf); library(terra); library(exactextractr); library(lwgeom); library(here)
})

# ---- Paths -------------------------------------------------------------------
SHP_DIR <- here("Data", "Spatial Data", "AnalysisShapefiles")
DEM_DIR <- here("Data", "Spatial Data", "DEMs_rAnalysis")

# ---- Jobs --------------------------------------------------------------------
# Each job: source shapefile, output shapefile, DEM used for slope/elevation.
JOBS <- list(
  list(input = "Kusko_HighIP2.shp",
       output = "Kusko_edges_geomorphAdded.shp",
       dem    = "KK_DEM.tif"),

list(input = "Yukon_HighIP.shp",
       output = "Yukon_edges_geomorphAdded.shp",
       dem    = "YkDem.tif")

  #list(input = "Yukon_edges2.shp",
       #output = "Yukon_edges2_geomorphAdded.shp",
       #dem    = "YkDem.tif")
)

# ---- Slope / elevation helper ------------------------------------------------
# Returns the streams sf with columns appended:
#   mean_elev, z1, z2, length_m, slope (percent, unsigned; NA replaced with 0).
# Computation runs in the DEM's CRS; the streams' original CRS is preserved.

add_slope_elev <- function(streams, dem_path) {
  dem <- terra::rast(dem_path)
  s   <- st_transform(streams, crs(dem))

  streams$mean_elev <- exactextractr::exact_extract(dem, s, "mean", progress = FALSE)

  p1 <- st_startpoint(s)
  p2 <- st_endpoint(s)
  streams$z1 <- terra::extract(dem, vect(p1))[, 2]
  streams$z2 <- terra::extract(dem, vect(p2))[, 2]

  streams$length_m <- as.numeric(st_length(s))
  streams$slope    <- (abs(streams$z1 - streams$z2) / streams$length_m) * 100

  # NA slopes -> 0 so downstream analyses do not need to special-case missingness.
  streams$slope[is.na(streams$slope)] <- 0

  streams
}

# ---- Per-job runner ----------------------------------------------------------
# Removes any existing shapefile sidecars before writing so re-runs cleanly
# overwrite without GDAL "does not appear to be a file" warnings.
remove_shapefile <- function(out_path) {
  stem <- tools::file_path_sans_ext(out_path)
  exts <- c("shp", "shx", "dbf", "prj", "cpg", "sbn", "sbx", "shp.xml")
  files <- paste0(stem, ".", exts)
  unlink(files[file.exists(files)])
}

build_one <- function(job) {
  in_path  <- file.path(SHP_DIR, job$input)
  out_path <- file.path(SHP_DIR, job$output)
  dem_path <- file.path(DEM_DIR, job$dem)

  cat(sprintf("\n-> %s\n", job$output))
  streams <- st_read(in_path, quiet = TRUE)
  streams <- add_slope_elev(streams, dem_path)

  remove_shapefile(out_path)
  st_write(streams, out_path, quiet = TRUE)

  cat(sprintf("   wrote %d features  ->  %s\n", nrow(streams), out_path))
}

# ---- Driver ------------------------------------------------------------------
cat("Building geomorph-added edge shapefiles...\n")
invisible(lapply(JOBS, build_one))
cat("\nDONE\n")
