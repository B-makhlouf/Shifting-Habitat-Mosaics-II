################################################################################
# SALMON ANALYSIS - YUKON LOWER & MIDDLE COMBINED (EXCLUDING UPPER)
# Function: run_yukon_lower_middle_analysis()
# Plus create_map() for visualization
################################################################################

# ==============================================================================
# LIBRARIES
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(RColorBrewer)
  library(readxl)
  library(here)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

PATHS <- list(
  
  # ── Shapefiles ─────────────────────────────────────────────
  yukon_edges  = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges2.shp"
  ),
  
  yukon_basin  = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"
  ),
  
  # ── Data inputs ────────────────────────────────────────────
  natal_data_dir = here("Data","Natal Origins"),
  
  runsize_data = here("Data","AYKEscapement.xlsx"),
  
  # ── Outputs ────────────────────────────────────────────────
  output_yukon = "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Outputs/ProductionData/CanadaOnly"
)

MAP_OUTPUT_DIR <- "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Figures/Maps/CanadaOnly"

# ==============================================================================
# FUNCTION: YUKON LOWER + MIDDLE ANALYSIS (EXCLUDING UPPER)
# ==============================================================================

run_yukon_lower_middle_analysis <- function(year, verbose = TRUE) {
  
  # Parameters
  min_stream_order <- 4
  min_error <- 0.0035
  sensitivity_threshold <- 0.7
  
  if (verbose) cat(paste("\n=== Processing Yukon (Lower + Middle only)", year, "===\n"))
  
  # Load spatial data
  edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
  basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  edges <- st_transform(edges, st_crs(basin))
  
  # Identify genetic regions from existing GENLMU attribute
  LYsites <- which(tolower(edges$GenLMU) == "lower")
  MYsites <- which(tolower(edges$GenLMU) == "middle")
  
  # Filter edges to ONLY Lower and Middle sites (exclude Upper)
  LM_sites <- c(LYsites, MYsites)
  edges <- edges[LM_sites, ]
  
  # Re-identify which rows are Lower vs Middle in filtered dataset
  LYsites_filtered <- which(tolower(edges$GenLMU) == "lower")
  MYsites_filtered <- which(tolower(edges$GenLMU) == "middle")
  
  if (verbose) {
    cat(paste("  Loaded", nrow(edges), "stream segments (Lower + Middle only)\n"))
    cat(paste("    Lower sites:", length(LYsites_filtered), "\n"))
    cat(paste("    Middle sites:", length(MYsites_filtered), "\n"))
  }
  
  # Load natal data
  natal_data <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(Lower), !is.na(Middle), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  if (verbose) cat(paste("  Observations:", nrow(natal_data), "\n"))
  
  if (nrow(natal_data) == 0) stop("No data available!")
  
  # Calculate error
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # Setup priors
  StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
  PresencePrior <- ifelse((edges$Str_Order %in% c(7,8,9)) & edges$SPAWNING_C == 0, 0, 1)
  newhabitatprior <- ifelse(edges$Channel_sl > 2.3, 0, 1)
  porcpupinepr <- edges$Porc_off
  
  # Bayesian assignment
  if (verbose) cat("  Performing Bayesian assignment (Lower + Middle)...\n")
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    # Set genetic prior based on site location (Lower or Middle)
    gen_prior <- rep(0, n_basins)
    gen_prior[LYsites_filtered] <- as.numeric(natal_data$Lower[i])
    gen_prior[MYsites_filtered] <- as.numeric(natal_data$Middle[i])
    
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      StreamOrderPrior * gen_prior * PresencePrior * porcpupinepr * newhabitatprior
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # Process results
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  total_sum <- sum(basin_assign_sum, na.rm = TRUE)
  
  if (total_sum > 0) {
    basin_assign_rescale <- basin_assign_sum / total_sum
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
    
    runsizedat <- read_excel(PATHS$runsize_data)
    runsize <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Yukon" & runsizedat$Year == year])
    
    # Scale by combined Lower + Middle proportion
    avg_lower_middle_prop <- mean(natal_data$Lower + natal_data$Middle, na.rm = TRUE)
    basin_assign_individuals <- basin_assign_rescale * runsize * avg_lower_middle_prop
  } else {
    basin_assign_rescale <- basin_assign_norm <- basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  if (verbose) {
    cat(paste("  Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
  }
  
  # Export CSV
  dir.create(PATHS$output_yukon, recursive = TRUE, showWarnings = FALSE)
  
  edges_df <- st_drop_geometry(edges)
  output_data <- data.frame(
    reachid = edges_df$reachid,
    Str_Order = edges_df$Str_Order,
    iso_pred = edges_df$iso_pred,
    assignment_sum = basin_assign_sum,
    assignment_rescale = basin_assign_rescale,
    assignment_norm = basin_assign_norm,
    assignment_individuals = basin_assign_individuals,
    GENLMU = edges_df$GenLMU
  )
  
  filepath <- file.path(PATHS$output_yukon, paste0(year, "_Yukon_LowerMiddle_Assignment_Results.csv"))
  write_csv(output_data, filepath)
  if (verbose) cat(paste("  ✓ Exported:", filepath, "\n"))
  
  return(list(
    edges = edges,
    basin = basin,
    results = output_data,
    natal_data = natal_data
  ))
}

# ==============================================================================
# FUNCTION: CREATE MAP
# ==============================================================================

create_map <- function(analysis_results, year) {
  
  edges <- analysis_results$edges
  basin <- analysis_results$basin
  basin_assign_norm <- analysis_results$results$assignment_norm
  
  # --------------------------------------------------------------------------
  # COLOR SCHEME
  # --------------------------------------------------------------------------
  
  palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)
  
  colcode <- rep("gray90", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- "white"
  colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette[1]
  colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette[2]
  colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette[3]
  colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette[4]
  colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette[5]
  colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette[6]
  colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette[7]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[8]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[9]
  colcode[basin_assign_norm > 0.9] <- palette[10]
  
  legend_labels <- c("0.0-0.4", "0.4-0.7", "0.7-0.8", "0.8-0.9", "0.9-0.95", "0.95-1.0")
  legend_colors <- palette[c(2, 5, 7, 8, 9, 10)]
  
  # --------------------------------------------------------------------------
  # LINE WIDTHS (Yukon-specific)
  # --------------------------------------------------------------------------
  
  stream_order <- edges$Str_Order
  stream_order[is.na(stream_order)] <- 1
  
  linewidths <- ifelse(stream_order >= 9, 3.7,
                       ifelse(stream_order >= 8, 5,
                              ifelse(stream_order >= 7, 2.0,
                                     ifelse(stream_order >= 6, 1.5,
                                            ifelse(stream_order >= 5, 1.4,
                                                   ifelse(stream_order >= 4, 1.0, 0))))))
  
  # Emphasize high production areas
  linewidths[basin_assign_norm > 0.8] <- linewidths[basin_assign_norm > 0.8] * 1.5
  
  # --------------------------------------------------------------------------
  # OUTPUT
  # --------------------------------------------------------------------------
  
  dir.create(MAP_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  map_filename <- file.path(MAP_OUTPUT_DIR, paste0("Yukon_LowerMiddle_", year, ".png"))
  
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = paste0("Annual Production - Lower & Middle Yukon\nYear: ", year), 
       bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("  ✓ Saved:", map_filename, "\n"))
  
  return(map_filename)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

cat("✓ Script loaded. Functions available:\n")
cat("  - run_yukon_lower_middle_analysis(year)\n")
cat("  - create_map(results, year)\n\n")

# Run analysis for Lower + Middle Yukon (excluding Upper)
for (year in c(2015, 2016, 2018, 2021)) {
  tryCatch({
    results <- run_yukon_lower_middle_analysis(year)
    create_map(results, year)
  }, error = function(e) cat("ERROR Yukon Lower+Middle", year, ":", e$message, "\n"))
}