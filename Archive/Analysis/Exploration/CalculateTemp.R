################################################################################
# CALCULATE MEAN AIR TEMPERATURE
#
# add_mean_temp(streams, temp_path) returns the input streams sf with a new
# column appended: mean_temp (in the raster's native units, typically degC).
# The extraction is performed in the temperature raster's CRS; the streams'
# original CRS is preserved on return.
#
# temp_raster_path_for_year(year) returns the file path to the SNAP air-
# temperature raster for that year. Currently 2022TEMP.tif is hard-coded
# for every year as a placeholder; eventually each year will have its own
# raster (e.g. 2017TEMP.tif, 2018TEMP.tif, ...). When per-year rasters are
# ready, just flip the path back to the year-templated form below.
################################################################################

suppressPackageStartupMessages({
  library(sf); library(terra); library(exactextractr); library(here)
})

temp_raster_path_for_year <- function(year) {
  # ---- Placeholder: use 2022 for everything until per-year rasters exist ----
  here("Data", "Spatial Data", "SNAP_Rasters", "2022TEMP.tif")

  # ---- Future: one raster per year ----
  # here("Data", "Spatial Data", "SNAP_Rasters", sprintf("%dTEMP.tif", year))
}

add_mean_temp <- function(streams, temp_path) {
  r <- terra::rast(temp_path)
  s <- st_transform(streams, crs(r))
  streams$mean_temp <- exactextractr::exact_extract(r, s, "mean",
                                                    progress = FALSE)
  streams
}
