################################################################################
# 02_ANNUAL_TRIBUTARY_MAPS.R - MAIN ANNUAL PRODUCTION MAPPING SCRIPT
################################################################################
# Creates simple annual production maps showing total production by year
# No quartiles, no management units - just annual tributary production maps
# This script auto-loads the setup and visualization scripts
# ADDED: CSV export of raw tributary production data
################################################################################

cat("=== ANNUAL TRIBUTARY MAPPING ANALYSIS ===\n")

# Auto-load required scripts
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
if (script_dir == "") script_dir <- getwd()  # fallback if not in RStudio

# Load setup script
setup_path <- file.path(script_dir, "00_setup_annual.R")
if (file.exists(setup_path)) {
  source(setup_path)
  cat("✓ Loaded setup script\n")
} else {
  stop("Cannot find 00_setup_annual.R - make sure it's in the same directory")
}

# Load visualization script  
viz_path <- file.path(script_dir, "01_visualization_annual.R")
if (file.exists(viz_path)) {
  source(viz_path)
  cat("✓ Loaded visualization script\n")
} else {
  stop("Cannot find 01_visualization_annual.R - make sure it's in the same directory")
}

################################################################################
# MAIN ANNUAL PRODUCTION ANALYSIS FUNCTION
################################################################################

#' Run annual tributary mapping analysis
#' Creates maps showing total annual production distribution
run_annual_tributary_analysis <- function(years = CONFIG$years, 
                                          watersheds = CONFIG$watersheds) {
  
  cat("Starting annual tributary mapping analysis...\n")
  cat("Years:", paste(years, collapse = ", "), "\n")
  cat("Watersheds:", paste(watersheds, collapse = ", "), "\n\n")
  
  # Create output directories
  create_output_dirs()
  
  # Create CSV export directory
  csv_export_dir <- "/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/AnnualProdData"
  dir.create(csv_export_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (watershed in watersheds) {
    params <- WATERSHED_PARAMS[[watershed]]
    
    # Create watershed-specific output directory
    watershed_output_dir <- file.path(PATHS$maps_dir, "Annual_Production", watershed)
    dir.create(watershed_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Create watershed-specific CSV directory
    watershed_csv_dir <- file.path(csv_export_dir, watershed)
    dir.create(watershed_csv_dir, recursive = TRUE, showWarnings = FALSE)
    
    cat(glue("=== Processing {watershed} watershed ===\n"))
    
    # Storage for multi-year summary
    annual_summary_data <- data.frame()
    
    for (year in years) {
      cat(glue("\nProcessing {watershed} {year}...\n"))
      
      # Load data
      spatial_data <- load_spatial_data(watershed)
      natal_data <- load_natal_data(year, watershed)
      
      cat(glue("  Loaded {nrow(natal_data)} fish observations\n"))
      
      # Setup assignment parameters
      pid_iso <- spatial_data$edges$iso_pred
      pid_isose <- spatial_data$edges$isose_pred
      error <- calculate_error(pid_isose, params$min_error)
      priors <- setup_priors(spatial_data$edges, watershed, natal_data)
      
      #------------------------------------------------------------------------
      # PERFORM ANNUAL ASSIGNMENT (ALL DATA FOR THE YEAR)
      #------------------------------------------------------------------------
      
      cat("  Performing Bayesian assignment...\n")
      
      # Use entire annual dataset - no quartile division
      assignment_matrix <- perform_assignment(
        natal_data, spatial_data$edges, watershed, priors, pid_iso, error, params$sensitivity_threshold
      )
      
      # Calculate basin-level production
      basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
      total_annual_production <- sum(basin_assign_sum, na.rm = TRUE)
      
      cat(glue("  Total annual production: {round(total_annual_production, 2)}\n"))
      
      #------------------------------------------------------------------------
      # EXPORT TRIBUTARY PRODUCTION DATA TO CSV
      #------------------------------------------------------------------------
      
      cat("  Exporting tributary production data to CSV...\n")
      
      # Create tributary production data frame
      tributary_production <- data.frame(
        tributary_id = 1:length(basin_assign_sum),
        year = year,
        watershed = watershed,
        raw_production = basin_assign_sum,
        stream_order = spatial_data$edges$Str_Order,
        stringsAsFactors = FALSE
      )
      
      # Add coordinates (centroid of each tributary)
      coords <- st_coordinates(st_centroid(spatial_data$edges))
      tributary_production$longitude <- coords[,1]
      tributary_production$latitude <- coords[,2]
      
      # Calculate normalized values (same as used in mapping)
      basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE)
      basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
      
      tributary_production$production_proportion <- basin_assign_rescale
      tributary_production$production_normalized <- basin_assign_norm
      
      # Sort by production (highest first)
      tributary_production <- tributary_production[order(tributary_production$raw_production, decreasing = TRUE), ]
      
      # Export to CSV
      csv_filename <- file.path(watershed_csv_dir, paste0("TributaryProduction_", year, "_", watershed, ".csv"))
      write_csv(tributary_production, csv_filename)
      
      cat(glue("  ✓ Exported: {basename(csv_filename)} ({nrow(tributary_production)} tributaries)\n"))
      
      # Store summary data
      annual_summary_data <- rbind(annual_summary_data, 
                                   data.frame(year = year, 
                                              watershed = watershed,
                                              total_production = total_annual_production))
      
      #------------------------------------------------------------------------
      # CREATE ANNUAL MAP
      #------------------------------------------------------------------------
      
      cat("  Creating annual tributary map...\n")
      
      # Create map filename
      map_filename <- file.path(watershed_output_dir, 
                                paste0("Annual_Production_", year, "_", watershed, ".png"))
      
      # Create the annual map with histogram
      create_annual_tributary_map(
        edges = spatial_data$edges,
        basin = spatial_data$basin,
        year = year,
        watershed = watershed,
        map_filename = map_filename,
        basin_assign_sum = basin_assign_sum,
        natal_data = natal_data  # Pass natal data for histogram
      )
      
      cat(glue("  ✓ Completed {year}\n"))
    }
    
    #--------------------------------------------------------------------------
    # CREATE MULTI-YEAR SUMMARY OUTPUTS
    #--------------------------------------------------------------------------
    
    cat(glue("\nCreating multi-year summaries for {watershed}...\n"))
    
    # Create comparison plot
    create_multiyear_comparison(annual_summary_data, watershed, watershed_output_dir)
    
    # Create summary table and data exports
    create_annual_summary_table(annual_summary_data, watershed, watershed_output_dir)
    
    cat(glue("✓ Completed {watershed} watershed analysis\n"))
    cat(glue("  - Maps: {watershed_output_dir}\n"))
    cat(glue("  - CSV data: {watershed_csv_dir}\n\n"))
  }
  
  cat("=== ANALYSIS COMPLETE ===\n")
  cat("Check output directories:\n")
  for (watershed in watersheds) {
    output_path <- file.path(PATHS$maps_dir, "Annual_Production", watershed)
    csv_path <- file.path(csv_export_dir, watershed)
    cat(glue("  {watershed} Maps: {output_path}\n"))
    cat(glue("  {watershed} CSV:  {csv_path}\n"))
  }
}

################################################################################
# EXECUTION SECTION
################################################################################

# Run the analysis if this script is executed directly
if (interactive() || !exists(".annual_script_executed")) {
  cat("\n=== READY TO RUN ANALYSIS ===\n")
  cat("All scripts loaded successfully!\n")
  cat("To run the analysis, execute:\n")
  cat("  run_annual_tributary_analysis()\n\n")
  
  cat("CSV files will be exported to:\n")
  cat("  /Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/AnnualProdData/\n\n")
  
  cat("Or uncomment the line below to run automatically:\n")
  # Uncomment the line below to run the analysis automatically
  run_annual_tributary_analysis()
  
  .annual_script_executed <- TRUE
}

cat("✓ Annual tributary mapping script loaded\n")
cat("Run: run_annual_tributary_analysis() to execute the analysis\n")