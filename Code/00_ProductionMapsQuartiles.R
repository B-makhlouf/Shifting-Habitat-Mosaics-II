################################################################################
# SALMON ANALYSIS - FULL RUN
# Two functions: run_kusko_analysis(), run_yukon_analysis()
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
  kusko_edges  = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"
  ),
  
  kusko_basin  = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"
  ),
  
  yukon_edges  = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges2.shp"
  ),
  
  yukon_basin  = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"
  ),
  
  yukon_ly_gen = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "edges_lYGen.shp"
  ),
  
  yukon_my_gen = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "edges_mYGen.shp"
  ),
  
  yukon_uy_gen = here(
    "Data", "Spatial Data", "AnalysisShapefiles", "edges_UYGen.shp"
  ),
  
  # ── Data inputs (external repo) ────────────────────────────
  natal_data_dir = here("Data","Natal Origins"),
  
  runsize_data = here("Data","AYKEscapement.xlsx"),
  
  # ── Outputs ────────────────────────────────────────────────
  output_kusko = here("Outputs", "ProductionData","Quartiles"),
  output_yukon = here("Outputs", "ProductionData")
)

MAP_OUTPUT_DIR <- here("Figures","Maps")

# ==============================================================================
# FUNCTION 1: KUSKOKWIM ANALYSIS WITH QUARTILE ANALYSIS (CPUE-WEIGHTED)
# ==============================================================================

run_kusko_analysis_quartiles <- function(year, verbose = TRUE) {
  
  # Parameters
  min_stream_order <- 3
  min_error <- 0.00057
  max_error <- 0.00089
  sensitivity_threshold <- 0.7
  
  if (verbose) cat(paste("\n=== Processing Kusko", year, "with Quartiles ===\n"))
  
  # Load spatial data
  edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
  basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  edges <- st_transform(edges, st_crs(basin))
  
  if (verbose) cat(paste("  Loaded", nrow(edges), "stream segments\n"))
  
  # Load natal data
  natal_data <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(year, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  
  if (verbose) cat(paste("  Total observations:", nrow(natal_data), "\n"))
  
  if (nrow(natal_data) == 0) stop("No data available!")
  
  # Determine date quartiles
  date_col <- if("date" %in% names(natal_data)) "date" else if("Date" %in% names(natal_data)) "Date" else stop("Cannot find date column")
  
  natal_data[[date_col]] <- as.Date(natal_data[[date_col]])
  date_range <- range(natal_data[[date_col]], na.rm = TRUE)
  
  # Create 4 equal date ranges
  date_breaks <- seq(date_range[1], date_range[2], length.out = 5)
  
  natal_data$quartile <- cut(natal_data[[date_col]], 
                             breaks = date_breaks, 
                             labels = c("Q1", "Q2", "Q3", "Q4"),
                             include.lowest = TRUE)
  
  if (verbose) {
    cat(paste("  Date range:", date_range[1], "to", date_range[2], "\n"))
    cat("  Quartile breakdown:\n")
    print(table(natal_data$quartile))
  }
  
  # Get total run size
  runsizedat <- read_excel(PATHS$runsize_data)
  total_runsize <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Kusko" & runsizedat$Year == year])
  
  # Calculate CPUE proportion for each quartile
  total_cpue <- sum(natal_data$dailyCPUEprop, na.rm = TRUE)
  quartile_cpue <- natal_data %>%
    group_by(quartile) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE), .groups = 'drop') %>%
    mutate(cpue_proportion = cpue_sum / total_cpue,
           quartile_runsize = cpue_proportion * total_runsize)
  
  if (verbose) {
    cat(paste("  Total run size:", total_runsize, "\n"))
    cat("  CPUE-based quartile run sizes:\n")
    print(quartile_cpue)
  }
  
  # Calculate error (same for all quartiles)
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- pmax(pmin(pid_isose, max_error), min_error)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # Setup priors (same for all quartiles)
  StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
  PresencePrior <- ifelse((edges$Str_Order %in% c(6,7)) & edges$SPAWNING_C == 0, 0, 1)
  NewHabitatPrior <- ifelse(edges$Channel_sl > 2.5, 0, 1)
  pid_prior <- edges$UniPh2oNoE
  
  # Initialize output dataframe
  output_data <- data.frame(
    reachid = st_drop_geometry(edges)$reachid,
    Str_Order = st_drop_geometry(edges)$Str_Order,
    iso_pred = st_drop_geometry(edges)$iso_pred
  )
  
  # Loop through each quartile
  for (q in c("Q1", "Q2", "Q3", "Q4")) {
    
    if (verbose) cat(paste("\n  Processing", q, "...\n"))
    
    # Subset data for this quartile
    natal_q <- natal_data %>% filter(quartile == q)
    
    # Get the run size for this quartile
    q_runsize <- quartile_cpue$quartile_runsize[quartile_cpue$quartile == q]
    if (length(q_runsize) == 0) q_runsize <- 0
    
    if (nrow(natal_q) == 0) {
      if (verbose) cat(paste("    No data for", q, "- assigning zeros\n"))
      output_data[[paste0(q, "_assignment_sum")]] <- 0
      output_data[[paste0(q, "_assignment_rescale")]] <- 0
      output_data[[paste0(q, "_assignment_norm")]] <- 0
      output_data[[paste0(q, "_assignment_individuals")]] <- 0
      output_data[[paste0(q, "_cpue_proportion")]] <- 0
      next
    }
    
    if (verbose) cat(paste("    Fish in", q, ":", nrow(natal_q), "| Run size:", round(q_runsize), "\n"))
    
    # Bayesian assignment for this quartile
    n_basins <- nrow(edges)
    n_fish <- nrow(natal_q)
    assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
    
    for (i in 1:n_fish) {
      fish_iso <- natal_q$natal_iso[i]
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        StreamOrderPrior * PresencePrior * pid_prior * NewHabitatPrior
      
      assign_norm <- assign / sum(assign)
      assign_rescaled <- assign_norm / max(assign_norm)
      assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
      assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_q$COratio[i])
    }
    
    # Process results for this quartile
    basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
    total_sum <- sum(basin_assign_sum, na.rm = TRUE)
    
    if (total_sum > 0) {
      basin_assign_rescale <- basin_assign_sum / total_sum
      basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
      basin_assign_individuals <- basin_assign_rescale * q_runsize  # <-- Now using CPUE-weighted run size
    } else {
      basin_assign_rescale <- basin_assign_norm <- basin_assign_individuals <- rep(0, length(basin_assign_sum))
    }
    
    if (verbose) {
      cat(paste("    Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
    }
    
    # Add to output dataframe
    output_data[[paste0(q, "_assignment_sum")]] <- basin_assign_sum
    output_data[[paste0(q, "_assignment_rescale")]] <- basin_assign_rescale
    output_data[[paste0(q, "_assignment_norm")]] <- basin_assign_norm
    output_data[[paste0(q, "_assignment_individuals")]] <- basin_assign_individuals
    output_data[[paste0(q, "_cpue_proportion")]] <- quartile_cpue$cpue_proportion[quartile_cpue$quartile == q]
  }
  
  # Calculate totals across all quartiles
  output_data$total_individuals <- rowSums(output_data[, grep("_individuals$", names(output_data))], na.rm = TRUE)
  
  # Export CSV
  dir.create(PATHS$output_kusko, recursive = TRUE, showWarnings = FALSE)
  
  filepath <- file.path(PATHS$output_kusko, paste0(year, "_Kusko_Quartile_Assignment_Results.csv"))
  write_csv(output_data, filepath)
  if (verbose) cat(paste("\n  ✓ Exported:", filepath, "\n"))
  
  return(list(
    edges = edges,
    basin = basin,
    results = output_data,
    natal_data = natal_data,
    date_breaks = date_breaks,
    quartile_cpue = quartile_cpue
  ))
}

results_2017 <- run_kusko_analysis_quartiles(2017)
results_2018 <- run_kusko_analysis_quartiles(2018)
results_2019 <- run_kusko_analysis_quartiles(2019)
results_2020 <- run_kusko_analysis_quartiles(2020)
results_2021 <- run_kusko_analysis_quartiles(2021)
results_2022 <- run_kusko_analysis_quartiles(2022)