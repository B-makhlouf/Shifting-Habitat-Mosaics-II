# convert_lines_to_midpoints.R
# Simple script to convert line shapefiles to midpoints

library(sf)

#' Convert line shapefile to midpoints and export as shapefile
#'
#' @param input_shapefile Path to the input line shapefile
#' @param output_shapefile Path to save the output point shapefile
#' @return Path to the created shapefile
convert_lines_to_midpoints <- function(input_shapefile, output_shapefile) {
  # Read the input shapefile
  message(paste("Reading input shapefile:", input_shapefile))
  lines_sf <- st_read(input_shapefile, quiet = TRUE)
  
  # Print summary info
  message(paste("Input features:", nrow(lines_sf), "lines"))
  
  # Calculate midpoints (guaranteed to fall on lines)
  message("Calculating midpoints...")
  
  # Use line_sample at 0.5 position (middle of the line)
  midpoints_sf <- st_line_sample(lines_sf, position = 0.5)
  
  # Convert the sfc object to sf with original attributes
  attributes_df <- st_drop_geometry(lines_sf)
  midpoints_sf <- st_sf(attributes_df, geometry = midpoints_sf)
  
  # Create output directory if it doesn't exist
  output_dir <- dirname(output_shapefile)
  if (!dir.exists(output_dir)) {
    message(paste("Creating output directory:", output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Export the midpoint shapefile
  message(paste("Writing output shapefile:", output_shapefile))
  st_write(midpoints_sf, output_shapefile, append = FALSE)
  
  message("Conversion complete!")
  message(paste("Output features:", nrow(midpoints_sf), "points"))
  
  return(output_shapefile)
}

# Example usage - replace with your actual file paths
input_file <- "path/to/your/line_shapefile.shp"
output_file <- "path/to/output/midpoint_shapefile.shp"

convert_lines_to_midpoints(input_file, output_file)
# Execute the script - replace with your actual file paths
input_file <- here("/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp")
output_file <- here("/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonPOINTS.shp")

convert_lines_to_centroids(input_file, output_file)
