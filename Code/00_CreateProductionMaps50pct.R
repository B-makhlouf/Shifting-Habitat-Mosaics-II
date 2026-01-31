################################################################################
# YUKON SALMON - 50% CPUE ANALYSIS
# Analyzes first half of run using cumulative CPUE cutoff
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
  # Shapefiles
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges.shp"),
  yukon_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  yukon_ly_gen   = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_lYGen.shp"),
  yukon_my_gen   = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_mYGen.shp"),
  
  # Data inputs
  natal_data_dir = here("Data", "Natal Origins"),
  runsize_data   = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx",
  
  # Outputs
  output_dir     = here("Outputs", "ProductionData"),
  map_output_dir = "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Figures/Maps/FirstHalfProd"
)

# Analysis parameters
PARAMS <- list(
  min_stream_order      = 4,
  min_error             = 0.0035,
  sensitivity_threshold = 0.0
)

# ==============================================================================
# FUNCTION: APPLY 50% CPUE CUTOFF
# ==============================================================================

apply_cpue_50_cutoff <- function(natal_data) {
  
  # Calculate cumulative CPUE by day
  daily_cpue <- natal_data %>%
    group_by(DOY) %>%
    summarise(daily_total = sum(COratio, na.rm = TRUE), .groups = 'drop') %>%
    arrange(DOY) %>%
    mutate(cumsum_proportion = cumsum(daily_total) / sum(daily_total))
  
  # Find cutoff DOY (50% threshold)
  cutoff_doy <- max(daily_cpue$DOY[daily_cpue$cumsum_proportion <= 0.5])
  
  # Filter data
  filtered_data <- natal_data %>% filter(DOY <= cutoff_doy)
  
  # Attach metadata
  attr(filtered_data, "cutoff_doy") <- cutoff_doy
  attr(filtered_data, "original_n") <- nrow(natal_data)
  attr(filtered_data, "filtered_n") <- nrow(filtered_data)
  attr(filtered_data, "percent_retained") <- round(nrow(filtered_data) / nrow(natal_data) * 100, 1)
  
  return(filtered_data)
}

# ==============================================================================
# FUNCTION: YUKON 50% ANALYSIS
# ==============================================================================

run_yukon_50pct_analysis <- function(year, verbose = TRUE) {
  
  if (verbose) cat(paste("\n=== Yukon", year, "- 50% CPUE Analysis ===\n"))
  
  # --------------------------------------------------------------------------
  # 1. LOAD SPATIAL DATA
  # --------------------------------------------------------------------------
  
  edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
  basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  edges <- st_transform(edges, st_crs(basin))
  
  # Load genetic regions
  ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
  my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
  
  edges$GenLMU <- "none"
  edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
  
  LYsites <- which(edges$GenLMU == "lower")
  MYsites <- which(edges$GenLMU == "middle")
  
  if (verbose) cat(paste("  Loaded", nrow(edges), "stream segments\n"))
  
  # --------------------------------------------------------------------------
  # 2. LOAD AND FILTER NATAL DATA
  # --------------------------------------------------------------------------
  
  natal_data_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  )
  
  natal_data_clean <- natal_data_raw %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  natal_data <- apply_cpue_50_cutoff(natal_data_clean)
  
  if (verbose) {
    cat(paste("  Initial observations:", nrow(natal_data_clean), "\n"))
    cat(paste("  50% CPUE cutoff at DOY:", attr(natal_data, "cutoff_doy"), "\n"))
    cat(paste("  Retained:", attr(natal_data, "filtered_n"), 
              "(", attr(natal_data, "percent_retained"), "%)\n"))
  }
  
  if (nrow(natal_data) == 0) stop("No data remaining after filtering!")
  
  # --------------------------------------------------------------------------
  # 3. CALCULATE PREDICTION ERROR
  # --------------------------------------------------------------------------
  
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < PARAMS$min_error, PARAMS$min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # --------------------------------------------------------------------------
  # 4. SETUP PRIORS
  # --------------------------------------------------------------------------
  
  StreamOrderPrior <- ifelse(edges$Str_Order >= PARAMS$min_stream_order, 1, 0)
  PresencePrior <- ifelse((edges$Str_Order %in% c(7,8,9)) & edges$SPAWNING_C == 0, 0, 1)
  
  # --------------------------------------------------------------------------
  # 5. BAYESIAN ASSIGNMENT
  # --------------------------------------------------------------------------
  
  if (verbose) cat("  Performing Bayesian assignment...\n")
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    # Genetic prior
    gen_prior <- rep(0, length(pid_iso))
    gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
    gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])
    
    # Assignment probability
    assign <- (1/sqrt(2*pi*error^2)) * 
      exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      StreamOrderPrior * gen_prior * PresencePrior
    
    # Normalize and rescale
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < PARAMS$sensitivity_threshold] <- 0
    
    # Weight by CPUE
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # --------------------------------------------------------------------------
  # 6. PROCESS RESULTS
  # --------------------------------------------------------------------------
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  total_sum <- sum(basin_assign_sum, na.rm = TRUE)
  
  if (total_sum > 0) {
    basin_assign_rescale <- basin_assign_sum / total_sum
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
    
    # Scale to individuals (half of total run)
    runsizedat <- read_excel(PATHS$runsize_data)
    runsize_full <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Yukon" & runsizedat$Year == year])
    runsize_half <- runsize_full / 2
    basin_assign_individuals <- basin_assign_rescale * runsize_half
  } else {
    basin_assign_rescale <- basin_assign_norm <- basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  if (verbose) {
    cat(paste("  Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
  }
  
  # --------------------------------------------------------------------------
  # 7. EXPORT RESULTS
  # --------------------------------------------------------------------------
  
  dir.create(PATHS$output_dir, recursive = TRUE, showWarnings = FALSE)
  
  edges_df <- st_drop_geometry(edges)
  output_data <- data.frame(
    reachid = edges_df$reachid,
    Str_Order = edges_df$Str_Order,
    iso_pred = edges_df$iso_pred,
    assignment_sum = basin_assign_sum,
    assignment_rescale = basin_assign_rescale,
    assignment_norm = basin_assign_norm,
    assignment_individuals = basin_assign_individuals,
    GenLMU = edges_df$GenLMU
  )
  
  filepath <- file.path(PATHS$output_dir, paste0("CPUE50pct_", year, "_Yukon_Assignment_Results.csv"))
  write_csv(output_data, filepath)
  
  if (verbose) cat(paste("  ✓ Exported:", filepath, "\n"))
  
  # --------------------------------------------------------------------------
  # 8. RETURN RESULTS
  # --------------------------------------------------------------------------
  
  return(list(
    edges = edges,
    basin = basin,
    results = output_data,
    natal_data = natal_data,
    cutoff_doy = attr(natal_data, "cutoff_doy")
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
  colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.4] <- palette[2]
  colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.7] <- palette[5]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[7]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[8]
  colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 0.95] <- palette[9]
  colcode[basin_assign_norm > 0.95] <- palette[10]
  
  legend_labels <- c("0.0-0.4", "0.4-0.7", "0.7-0.8", "0.8-0.9", "0.9-0.95", "0.95-1.0")
  legend_colors <- palette[c(2, 5, 7, 8, 9, 10)]
  
  # --------------------------------------------------------------------------
  # LINE WIDTHS
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
  # GENERATE MAP
  # --------------------------------------------------------------------------
  
  dir.create(PATHS$map_output_dir, recursive = TRUE, showWarnings = FALSE)
  map_filename <- file.path(PATHS$map_output_dir, paste0("Yukon_", year, "_50pct.png"))
  
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = paste0("First 50% of Run Production\nYear: ", year, " River: Yukon"), 
       bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, 
       add = TRUE, lwd = linewidths)
  
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("  ✓ Map saved:", map_filename, "\n"))
  
  return(map_filename)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

cat("\n✓ Yukon 50% CPUE Analysis Script Loaded\n")
cat("  Functions:\n")
cat("    - run_yukon_50pct_analysis(year)\n")
cat("    - create_map(results, year)\n\n")

# Run analysis for all available years
YEARS <- c(2015, 2016, 2018, 2021)

for (year in YEARS) {
  tryCatch({
    results <- run_yukon_50pct_analysis(year)
    create_map(results, year)
  }, error = function(e) {
    cat(paste("ERROR -", year, ":", e$message, "\n"))
  })
}

cat("\n✓ Analysis complete\n")