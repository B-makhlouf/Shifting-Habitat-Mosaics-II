################################################################################
# SALMON ANALYSIS MASTER SCRIPT - COMPLETE CONSOLIDATED VERSION
# Combines 00_Assignment_noCA.R + 00_Visualization.R + 00_Run_Analysis.R
# 
# Preserves ALL exact functionality from original three scripts
# Linear workflow - just load this file and execute at the bottom
################################################################################

# ============================================================================
# SECTION 1: LOAD ALL REQUIRED LIBRARIES
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(RColorBrewer)
  library(scales)
  library(grid)
  library(tidyr)
  library(readxl)
})

# ============================================================================
# SECTION 2: MAIN MAPPING FUNCTION (from 00_Visualization.R)
# ============================================================================

#' Create annual map with scenario-based directory structure
#'
#' @param analysis_results List output from run_annual_analysis()
#' @param base_output_dir Base directory for output (scenario subdirs created automatically)
#' @param year Year of analysis
#' @param watershed Watershed name
#' @param filter_type Type of filter applied
#' @param cpue_lower Lower CPUE percentile (if applicable)
#' @param cpue_upper Upper CPUE percentile (if applicable)
#' @param date_start Start DOY (if applicable)
#' @param date_end End DOY (if applicable)
#'
create_annual_map <- function(analysis_results, 
                              base_output_dir, 
                              year, 
                              watershed,
                              filter_type = "none",
                              cpue_lower = NULL,
                              cpue_upper = NULL,
                              date_start = NULL,
                              date_end = NULL) {
  
  edges <- analysis_results$edges
  basin <- analysis_results$basin
  results <- analysis_results$results
  natal_data <- analysis_results$natal_data
  
  basin_assign_norm <- results$assignment_norm
  
  # ========================================================================
  # COLOR CODING (watershed-specific)
  # ========================================================================
  
  palette <- brewer.pal(9, "YlOrRd")
  palette_expanded <- colorRampPalette(palette)(10)
  
  colcode <- rep("gray90", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'white'
  
  if (watershed == "Yukon") {
    
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.4] <- palette_expanded[2]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.7] <- palette_expanded[5]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[7]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[8]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 0.95] <- palette_expanded[9]
    colcode[basin_assign_norm > 0.95 & basin_assign_norm <= 1.0] <- palette_expanded[10]
    
    
    legend_labels <- c("0.0-0.4", "0.4-0.7", "0.7-0.8", "0.8-0.9", "0.9-0.95", "0.95-1.0")
    legend_colors <- c(palette_expanded[2], palette_expanded[5], palette_expanded[7], 
                       palette_expanded[8], palette_expanded[9], palette_expanded[10])
    
  } else if (watershed == "Kusko") {
    
    # colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.4] <- palette_expanded[2]
    # colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.7] <- palette_expanded[5]
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.7] <- palette_expanded[2]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[7]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[8]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 0.95] <- palette_expanded[9]
    colcode[basin_assign_norm > 0.95 & basin_assign_norm <= 1.0] <- palette_expanded[10]
    
    
    legend_labels <- c("0.0-0.4", "0.4-0.7", "0.7-0.8", "0.8-0.9", "0.9-0.95", "0.95-1.0")
    legend_colors <- c(palette_expanded[2], palette_expanded[5], palette_expanded[7], 
                       palette_expanded[8], palette_expanded[9], palette_expanded[10])
  }
  
  # ========================================================================
  # LINE WIDTHS (watershed-specific stream order emphasis)
  # ========================================================================
  
  stream_order <- edges$Str_Order
  stream_order[is.na(stream_order)] <- 1
  
  if (watershed == "Yukon") {
    # Conservative Yukon linewidths
    linewidths <- ifelse(stream_order >= 9, 3.7,
                         ifelse(stream_order >= 8, 5,
                                ifelse(stream_order >= 7, 2.0,
                                       ifelse(stream_order >= 6, 1.5,
                                              ifelse(stream_order >= 5, 1.4,
                                                     ifelse(stream_order >= 4, 1.0, 
                                                            ifelse(stream_order >= 3, 0, 0)))))))
  } else if (watershed == "Kusko") {
    # Dramatic Kusko linewidths
    linewidths <- ifelse(stream_order >= 9, 5,
                         ifelse(stream_order >= 8, 6,
                                ifelse(stream_order >= 7, 5,
                                       ifelse(stream_order >= 6, 3.0,
                                              ifelse(stream_order >= 5, 2.7,
                                                     ifelse(stream_order >= 4, 2.7,
                                                            ifelse(stream_order >= 3, 1.2, 0)))))))
  }
  
  # Highlight high production areas with slightly thicker lines
  linewidths[basin_assign_norm > 0.8] <- linewidths[basin_assign_norm > 0.8] * 1.5
  
  # ========================================================================
  # DETERMINE SCENARIO SUBDIRECTORY AND CREATE OUTPUT FILENAME
  # ========================================================================
  
  if (filter_type == "none") {
    scenario_dir <- "Full_Year"
    map_filename <- file.path(base_output_dir, "Production", scenario_dir, paste0(year, "_", watershed, "_Annual_Production.png"))
  } else if (filter_type == "cpue_50_cutoff") {
    scenario_dir <- "Half_Year"
    map_filename <- file.path(base_output_dir, "Production", scenario_dir, paste0(year, "_", watershed, "_Annual_Production.png"))
  } else if (filter_type == "cpue_percentile") {
    scenario_dir <- paste0("CPUE_", cpue_lower, "-", cpue_upper, "pct")
    map_filename <- file.path(base_output_dir, "Production", scenario_dir, paste0(year, "_", watershed, "_Annual_Production.png"))
  } else if (filter_type == "date_range") {
    scenario_dir <- paste0("DOY_", date_start, "-", date_end)
    map_filename <- file.path(base_output_dir, "Production", scenario_dir, paste0(year, "_", watershed, "_Annual_Production.png"))
  } else if (filter_type == "both") {
    scenario_dir <- paste0("CPUE_", cpue_lower, "-", cpue_upper, "pct_DOY_", date_start, "-", date_end)
    map_filename <- file.path(base_output_dir, "Production", scenario_dir, paste0(year, "_", watershed, "_Annual_Production.png"))
  }
  
  # Create output directory
  output_dir <- dirname(map_filename)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ========================================================================
  # CREATE PNG FILE
  # ========================================================================
  
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  # PLOT BASE MAP
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = paste0("Annual Production\nYear: ", year, " River: ", watershed), bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # ADD LEGEND
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5, 
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("  ✓ Saved:", basename(map_filename), "\n"))
  cat(paste("  ✓ Location:", output_dir, "\n"))
  cat(paste("  ✓ Scenario:", scenario_dir, "\n"))
  cat(paste("  ✓ Map includes ALL stream orders (white = zero assignment, colors = assignment values)\n"))
  
  return(map_filename)
}

# ============================================================================
# SECTION 3: CORE ANALYSIS FUNCTION (from 00_Assignment_noCA.R)
# ============================================================================

#' Run complete salmon assignment analysis for a given year and watershed
#'
#' @param year Numeric year to analyze
#' @param watershed Character: "Kusko" or "Yukon"
#' @param filter_type Character: "none", "cpue_50_cutoff", "cpue_percentile", "date_range", "both"
#' @param cpue_lower Numeric: Lower CPUE percentile (for cpue_percentile or both filters)
#' @param cpue_upper Numeric: Upper CPUE percentile (for cpue_percentile or both filters)
#' @param date_start Numeric: Starting DOY (for date_range or both filters)
#' @param date_end Numeric: Ending DOY (for date_range or both filters)
#' @param verbose Logical: Print progress messages
#'
#' @return List containing: edges, basin, results (data frame), natal_data, filter_metadata
#'
run_annual_analysis <- function(year, 
                                watershed,
                                filter_type = "none",
                                cpue_lower = NULL,
                                cpue_upper = NULL,
                                date_start = NULL,
                                date_end = NULL,
                                verbose = TRUE) {
  
  # ========================================================================
  # CONFIGURATION
  # ========================================================================
  
  PATHS <- list(
    kusko_edges = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_Reachbase_complete2.shp",
    kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
    yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
    yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
    yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
    yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
    natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
    output_kusko = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko",
    output_yukon = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
  )
  
  PARAMS <- list(
    Kusko = list(min_stream_order = 3, min_error = 0.00057, sensitivity_threshold = 0.7, max_error = 0.00089),
    Yukon = list(min_stream_order = 4, min_error = 0.0035, sensitivity_threshold = 0.000, max_error = NULL)
  )
  
  # ========================================================================
  # VALIDATION & INITIALIZATION
  # ========================================================================
  
  if (!(watershed %in% c("Kusko", "Yukon"))) {
    stop("Watershed must be 'Kusko' or 'Yukon'")
  }
  
  if (verbose) cat(paste("\n=== Processing", watershed, year, "===\n"))
  
  params <- PARAMS[[watershed]]
  
  # ========================================================================
  # APPLY FILTERS (inline helper function)
  # ========================================================================
  
  apply_filters <- function(natal_data, filter_type, cpue_lower, cpue_upper, date_start, date_end, watershed) {
    
    filtered_data <- natal_data
    attr(filtered_data, "original_n") <- nrow(natal_data)
    attr(filtered_data, "original_cpue") <- sum(natal_data$COratio, na.rm = TRUE)
    
    if (filter_type == "cpue_50_cutoff") {
      sorted_data <- filtered_data %>% arrange(DOY)
      daily_cpue <- sorted_data %>%
        group_by(DOY) %>%
        summarise(daily_total = sum(COratio, na.rm = TRUE), .groups = 'drop') %>%
        arrange(DOY) %>%
        mutate(cumsum_cpue = cumsum(daily_total),
               total_cpue = sum(daily_total),
               cumsum_proportion = cumsum_cpue / total_cpue)
      
      cutoff_doy <- daily_cpue %>%
        filter(cumsum_proportion <= 0.5) %>%
        pull(DOY) %>%
        max()
      
      filtered_data <- filtered_data %>% filter(DOY <= cutoff_doy)
      filter_description <- paste0("Up to 50% cumulative CPUE (DOY <= ", cutoff_doy, ")")
      
    } else if (filter_type %in% c("cpue_percentile", "both")) {
      if (is.null(cpue_lower)) cpue_lower <- 0
      if (is.null(cpue_upper)) cpue_upper <- 100
      
      daily_cpue <- filtered_data %>%
        group_by(DOY) %>%
        summarise(mean_cpue = mean(dailyCPUEprop, na.rm = TRUE), .groups = 'drop') %>%
        mutate(cpue_percentile = rank(mean_cpue) / n() * 100)
      
      target_doys <- daily_cpue %>%
        filter(cpue_percentile >= cpue_lower & cpue_percentile <= cpue_upper) %>%
        pull(DOY)
      
      filtered_data <- filtered_data %>% filter(DOY %in% target_doys)
      filter_description <- paste0("CPUE percentile: ", cpue_lower, "-", cpue_upper, "%")
    } else {
      filter_description <- ""
    }
    
    if (filter_type %in% c("date_range", "both")) {
      if (!is.null(date_start)) {
        filtered_data <- filtered_data %>% filter(DOY >= date_start)
        filter_description <- paste0(filter_description, ifelse(filter_description != "", " & ", ""),
                                     "DOY >= ", date_start)
      }
      if (!is.null(date_end)) {
        filtered_data <- filtered_data %>% filter(DOY <= date_end)
        filter_description <- paste0(filter_description, ifelse(filter_description != "", " & ", ""),
                                     "DOY <= ", date_end)
      }
    }
    
    attr(filtered_data, "filtered_n") <- nrow(filtered_data)
    attr(filtered_data, "filtered_cpue") <- sum(filtered_data$COratio, na.rm = TRUE)
    attr(filtered_data, "percent_retained") <- round(nrow(filtered_data) / nrow(natal_data) * 100, 1)
    attr(filtered_data, "cpue_retained") <- round(sum(filtered_data$COratio, na.rm = TRUE) / 
                                                    sum(natal_data$COratio, na.rm = TRUE) * 100, 1)
    attr(filtered_data, "filter_description") <- filter_description
    attr(filtered_data, "filter_type") <- filter_type
    
    return(filtered_data)
  }
  
  # ========================================================================
  # LOAD SPATIAL DATA
  # ========================================================================
  
  if (watershed == "Kusko") {
    edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  } else if (watershed == "Yukon") {
    edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  }
  
  edges <- st_transform(edges, st_crs(basin))
  
  if (verbose) {
    cat(paste("  Loaded", nrow(edges), "total stream segments (all stream orders)\n"))
    meets_threshold <- edges$Str_Order >= params$min_stream_order
    cat(paste("  Segments meeting min stream order threshold:", sum(meets_threshold), "\n"))
  }
  
  # ========================================================================
  # LOAD & CLEAN NATAL DATA
  # ========================================================================
  
  natal_data_raw <- read_csv(file.path(PATHS$natal_data_dir, 
                                       paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv")), 
                             show_col_types = FALSE)
  
  if (watershed == "Yukon") {
    natal_data_clean <- filter(natal_data_raw, !is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  } else {
    natal_data_clean <- filter(natal_data_raw, !is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  
  # ========================================================================
  # APPLY FILTERING
  # ========================================================================
  
  natal_data <- apply_filters(natal_data_clean, filter_type, cpue_lower, cpue_upper, 
                              date_start, date_end, watershed)
  
  if (verbose) {
    cat(paste("  Initial observations: ", nrow(natal_data_clean), "\n", sep = ""))
    cat("  Filter applied: ", attr(natal_data, "filter_description"), "\n", sep = "")
    cat("    Filtered observations: ", attr(natal_data, "filtered_n"), "\n", sep = "")
    cat("    Percent retained: ", attr(natal_data, "percent_retained"), "%\n", sep = "")
    cat("    CPUE retained: ", attr(natal_data, "cpue_retained"), "%\n", sep = "")
  }
  
  if (nrow(natal_data) == 0) stop("No data remaining after filtering!")
  
  cat(paste("  Using", nrow(natal_data), "fish for assignment\n"))
  
  # ========================================================================
  # CALCULATE ERROR
  # ========================================================================
  
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  
  if (!is.null(params$max_error)) {
    pid_isose_mod <- ifelse(pid_isose_mod > params$max_error, params$max_error, pid_isose_mod)
  }
  
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # ========================================================================
  # SETUP PRIORS
  # ========================================================================
  
  StreamOrderPrior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  
  if (watershed == "Kusko") {
    pid_prior <- edges$UniPh2oNoE
    PresencePrior <- ifelse((edges$Str_Order %in% c(6,7)) & edges$SPAWNING_C == 0, 0, 1)
    #NewHabitatPrior <- ifelse(edges$Spawner_IP < .3, 0, 1)
    NewHabitatPrior <- ifelse(edges$Avg_Slop_1 > 2.5, 0, 1)
    
    
  } else if (watershed == "Yukon") {
    pid_prior <- edges$PriorSl2
    PresencePrior <- ifelse((edges$Str_Order %in% c(7,8,9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == .3, 0, 1)
    
    ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
    
    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
    
    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
  }
  
  # ========================================================================
  # BAYESIAN ASSIGNMENT
  # ========================================================================
  
  if (verbose) cat("  Performing Bayesian assignment...\n")
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    if (watershed == "Kusko") {
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        StreamOrderPrior * PresencePrior * pid_prior * NewHabitatPrior
      
    } else if (watershed == "Yukon") {
      gen_prior <- rep(0, length(pid_iso))
      gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
      gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        StreamOrderPrior * gen_prior * PresencePrior  #* NewHabitatPrior #pid_prior
    }
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # ========================================================================
  # PROCESS RESULTS
  # ========================================================================
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  
  total_sum <- sum(basin_assign_sum, na.rm = TRUE)
  if (total_sum > 0) {
    basin_assign_rescale <- basin_assign_sum / total_sum
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
    
    runsizedat <- read_excel("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx")
    runsize <- runsizedat %>%
      filter(River == watershed & Year == year) %>%
      pull(Total_Run) %>%
      as.numeric()
    
    if (filter_type == "cpue_50_cutoff") runsize <- runsize / 2
    
    basin_assign_individuals <- basin_assign_rescale * runsize
    
  } else {
    basin_assign_rescale <- rep(0, length(basin_assign_sum))
    basin_assign_norm <- rep(0, length(basin_assign_sum))
    basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  cat(paste("  Total production:", round(sum(basin_assign_sum), 2), "\n"))
  cat(paste("  Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
  
  # ========================================================================
  # EXPORT TO CSV
  # ========================================================================
  
  output_dir <- if (watershed == "Kusko") PATHS$output_kusko else PATHS$output_yukon
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  filename_base <- if (filter_type == "cpue_50_cutoff") 
    paste0("CPUE50pct_", year, "_", watershed, "_Assignment_Results")
  else if (filter_type == "cpue_percentile") 
    paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_", year, "_", watershed, "_Assignment_Results")
  else if (filter_type == "date_range") 
    paste0("DOY", date_start, "-", date_end, "_", year, "_", watershed, "_Assignment_Results")
  else if (filter_type == "both") 
    paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_DOY", date_start, "-", date_end, "_", year, "_", watershed, "_Assignment_Results")
  else 
    paste0(year, "_", watershed, "_Assignment_Results")
  
  edges_df <- st_drop_geometry(edges)
  output_data <- data.frame(
    reachid = edges_df$reachid,
    Str_Order = edges_df$Str_Order,
    iso_pred = edges_df$iso_pred,
    assignment_sum = basin_assign_sum,
    assignment_rescale = basin_assign_rescale,
    assignment_norm = basin_assign_norm,
    assignment_individuals = basin_assign_individuals
  )
  
  if (watershed == "Yukon") output_data$GenLMU <- edges_df$GenLMU
  
  filepath <- file.path(output_dir, paste0(filename_base, ".csv"))
  write_csv(output_data, filepath)
  
  cat(paste("  ✓ Exported:", filepath, "\n"))
  
  # ========================================================================
  # RETURN RESULTS
  # ========================================================================
  
  filter_metadata <- list(
    filter_type = filter_type,
    cpue_lower = cpue_lower,
    cpue_upper = cpue_upper,
    date_start = date_start,
    date_end = date_end,
    original_n = attr(natal_data, "original_n"),
    filtered_n = attr(natal_data, "filtered_n"),
    percent_retained = attr(natal_data, "percent_retained"),
    cpue_retained = attr(natal_data, "cpue_retained"),
    filter_description = attr(natal_data, "filter_description")
  )
  
  return(list(
    edges = edges,
    basin = basin, 
    results = output_data,
    natal_data = natal_data,
    filter_metadata = filter_metadata
  ))
}

# ============================================================================
# SECTION 4: EXECUTION EXAMPLES (from 00_Run_Analysis.R)
# ============================================================================

cat("✓ Master script loaded successfully!\n")
cat("Available functions:\n")
cat("  - run_annual_analysis(year, watershed, filter_type, ...)\n")
cat("  - create_annual_map(analysis_results, base_output_dir, year, watershed, filter_type, ...)\n")
cat("\n")

# Define BASE output directories
BASE_KUSKO_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/Maps/Kusko_Annual"
BASE_YUKON_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/Maps/Yukon_Annual"

# ============================================================================
# EXAMPLE 1: FULL YEAR ANALYSIS (uncomment to run)
# ============================================================================

#KUSKOKWIM FULL YEAR
for (year in c(2017, 2018, 2019, 2020, 2021, 2022)) {
  cat("\n--- Kuskokwim", year, "---\n")
  tryCatch({
    results <- run_annual_analysis(year, "Kusko")
    create_annual_map(results, BASE_KUSKO_DIR, year, "Kusko", filter_type = "none")
  }, error = function(e) {
    cat("ERROR processing Kusko", year, ":", e$message, "\n")
  })
}

# # #YUKON FULL YEAR
# for (year in c(2015, 2016, 2018, 2021)) {
#   cat("\n--- Yukon", year, "---\n")
#   tryCatch({
#     results <- run_annual_analysis(year, "Yukon")
#     create_annual_map(results, BASE_YUKON_DIR, year, "Yukon", filter_type = "none")
#   }, error = function(e) {
#     cat("ERROR processing Yukon", year, ":", e$message, "\n")
#   })
# }

# ============================================================================
# EXAMPLE 2: HALF YEAR (50% CUMULATIVE CPUE CUTOFF) (uncomment to run)
# ============================================================================

# YUKON - UP TO 50% CPUE
# for (year in c(2015, 2016, 2018, 2021)) {
#   cat("\n--- Yukon", year, "(50% CPUE cutoff) ---\n")
#   tryCatch({
#     results <- run_annual_analysis(year, "Yukon", filter_type = "cpue_50_cutoff")
#     create_annual_map(results, BASE_YUKON_DIR, year, "Yukon", filter_type = "cpue_50_cutoff")
#   }, error = function(e) {
#     cat("ERROR processing Yukon", year, ":", e$message, "\n")
#   })
# }

cat("\n")
cat("To run analysis, uncomment examples at bottom of script\n")
cat("Or use:\n")
cat("  results <- run_annual_analysis(2017, 'Kusko')\n")
cat("  create_annual_map(results, BASE_KUSKO_DIR, 2017, 'Kusko')\n")