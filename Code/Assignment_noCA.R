################################################################################
# CONSOLIDATED SALMON ASSIGNMENT ANALYSIS WITH FLEXIBLE FILTERING
################################################################################

library(sf); library(dplyr); library(readr)

#------------------------------------------------------------------------------
# CONFIGURATION
#------------------------------------------------------------------------------
PATHS <- list(
  kusko_edges = "/Users/benjaminmakhlouf/Spatial Data/KuskoUSGS_HUC.shp",
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
  Kusko = list(min_stream_order = 3, min_error = 0.0006, sensitivity_threshold = 0.6),
  Yukon = list(min_stream_order = 5, min_error = 0.003, sensitivity_threshold = 0.0001)
)

################################################################################
# FILTERING FUNCTIONS
################################################################################

#' Apply flexible filtering to natal data
#'
#' @param natal_data Raw natal data frame
#' @param filter_type Character: "none", "cpue_percentile", "date_range", or "both"
#' @param cpue_lower Numeric: Lower percentile (0-100) for CPUE filtering (e.g., 25 for top 75%)
#' @param cpue_upper Numeric: Upper percentile (0-100) for CPUE filtering (e.g., 100 for full range)
#' @param date_start Numeric: Starting day of year (1-365)
#' @param date_end Numeric: Ending day of year (1-365)
#' @param watershed Character: "Kusko" or "Yukon" (for data validation)
#'
#' @return Filtered natal data frame with attributes about filtering applied
#'
#' @examples
#' # Full annual analysis (no filtering)
#' filtered_data <- apply_filters(natal_data, filter_type = "none")
#'
#' # Top 50% CPUE (by daily average)
#' filtered_data <- apply_filters(natal_data, filter_type = "cpue_percentile", 
#'                                 cpue_lower = 50, cpue_upper = 100)
#'
#' # Peak season (DOY 160-183)
#' filtered_data <- apply_filters(natal_data, filter_type = "date_range",
#'                                 date_start = 160, date_end = 183)
#'
#' # Top 50% CPUE during specific dates
#' filtered_data <- apply_filters(natal_data, filter_type = "both",
#'                                 cpue_lower = 50, cpue_upper = 100,
#'                                 date_start = 160, date_end = 183)
apply_filters <- function(natal_data, 
                          filter_type = "none",
                          cpue_lower = NULL,
                          cpue_upper = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          watershed = NULL) {
  
  filtered_data <- natal_data
  filter_description <- ""
  
  # Initialize attributes
  attr(filtered_data, "original_n") <- nrow(natal_data)
  attr(filtered_data, "original_cpue") <- sum(natal_data$COratio, na.rm = TRUE)
  
  # CPUE PERCENTILE FILTERING
  if (filter_type %in% c("cpue_percentile", "both")) {
    if (is.null(cpue_lower)) cpue_lower <- 0
    if (is.null(cpue_upper)) cpue_upper <- 100
    
    # Calculate CPUE quantiles per day
    daily_cpue <- filtered_data %>%
      group_by(DOY) %>%
      summarise(mean_cpue = mean(dailyCPUEprop, na.rm = TRUE), .groups = 'drop') %>%
      mutate(cpue_percentile = rank(mean_cpue) / n() * 100)
    
    # Find DOYs in requested percentile range
    target_doys <- daily_cpue %>%
      filter(cpue_percentile >= cpue_lower & cpue_percentile <= cpue_upper) %>%
      pull(DOY)
    
    filtered_data <- filtered_data %>% filter(DOY %in% target_doys)
    
    filter_description <- paste0("CPUE percentile: ", cpue_lower, "-", cpue_upper, "%")
  }
  
  # DATE RANGE FILTERING
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
  
  # Calculate statistics
  attr(filtered_data, "filtered_n") <- nrow(filtered_data)
  attr(filtered_data, "filtered_cpue") <- sum(filtered_data$COratio, na.rm = TRUE)
  attr(filtered_data, "percent_retained") <- round(nrow(filtered_data) / nrow(natal_data) * 100, 1)
  attr(filtered_data, "cpue_retained") <- round(sum(filtered_data$COratio, na.rm = TRUE) / 
                                                  sum(natal_data$COratio, na.rm = TRUE) * 100, 1)
  attr(filtered_data, "filter_description") <- filter_description
  attr(filtered_data, "filter_type") <- filter_type
  
  return(filtered_data)
}

#' Print filtering summary
#'
#' @param filtered_data Output from apply_filters()
print_filter_summary <- function(filtered_data) {
  cat("  Filter applied: ", attr(filtered_data, "filter_description"), "\n", sep = "")
  cat("    Original observations: ", attr(filtered_data, "original_n"), "\n", sep = "")
  cat("    Filtered observations: ", attr(filtered_data, "filtered_n"), "\n", sep = "")
  cat("    Percent retained: ", attr(filtered_data, "percent_retained"), "%\n", sep = "")
  cat("    Original CPUE: ", round(attr(filtered_data, "original_cpue"), 4), "\n", sep = "")
  cat("    Filtered CPUE: ", round(attr(filtered_data, "filtered_cpue"), 4), "\n", sep = "")
  cat("    CPUE retained: ", attr(filtered_data, "cpue_retained"), "%\n", sep = "")
}

################################################################################
# MAIN FUNCTION (UPDATED)
################################################################################

#' Run annual analysis with optional filtering
#'
#' @param year Numeric year to analyze
#' @param watershed Character: "Kusko" or "Yukon"
#' @param filter_type Character: "none", "cpue_percentile", "date_range", or "both"
#' @param cpue_lower Numeric: Lower percentile for CPUE (0-100)
#' @param cpue_upper Numeric: Upper percentile for CPUE (0-100)
#' @param date_start Numeric: Starting DOY (1-365)
#' @param date_end Numeric: Ending DOY (1-365)
#' @param verbose Logical: Print detailed output
#'
#' @return List containing edges, basin, results, natal_data, and filter_metadata
#'
#' @examples
#' # Full annual analysis
#' results <- run_annual_analysis(2017, "Kusko")
#'
#' # Top 50% of CPUE days
#' results <- run_annual_analysis(2017, "Kusko", 
#'                                filter_type = "cpue_percentile",
#'                                cpue_lower = 50, cpue_upper = 100)
#'
#' # Peak season only
#' results <- run_annual_analysis(2017, "Kusko",
#'                                filter_type = "date_range",
#'                                date_start = 160, date_end = 183)
run_annual_analysis <- function(year, 
                                watershed,
                                filter_type = "none",
                                cpue_lower = NULL,
                                cpue_upper = NULL,
                                date_start = NULL,
                                date_end = NULL,
                                verbose = TRUE) {
  
  if (verbose) {
    cat(paste("\n=== Processing", watershed, year, "===\n"))
  }
  
  params <- PARAMS[[watershed]]
  
  # 1. LOAD SPATIAL DATA
  if (watershed == "Kusko") {
    edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  } else {
    edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  }
  edges <- st_transform(edges, st_crs(basin)) %>% filter(Str_Order >= params$min_stream_order)
  
  # 2. LOAD NATAL DATA
  natal_data_raw <- read_csv(file.path(PATHS$natal_data_dir, 
                                       paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv")), 
                             show_col_types = FALSE)
  
  # Clean data
  natal_data_clean <- if (watershed == "Yukon") {
    filter(natal_data_raw, !is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  } else {
    filter(natal_data_raw, !is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  
  # 3. APPLY FILTERING
  natal_data <- apply_filters(natal_data_clean,
                              filter_type = filter_type,
                              cpue_lower = cpue_lower,
                              cpue_upper = cpue_upper,
                              date_start = date_start,
                              date_end = date_end,
                              watershed = watershed)
  
  if (verbose) {
    cat(paste("  Initial observations: ", nrow(natal_data_clean), "\n", sep = ""))
    print_filter_summary(natal_data)
  }
  
  if (nrow(natal_data) == 0) {
    stop("No data remaining after filtering!")
  }
  
  cat(paste("  Loaded", nrow(natal_data), "fish,", nrow(edges), "segments\n"))
  
  # 4. CALCULATE ERROR
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # 5. SETUP PRIORS
  StreamOrderPrior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  
  if (watershed == "Kusko") {
    pid_prior <- edges$UniPh2oNoE
    PresencePrior <- ifelse((edges$Str_Order %in% c(6,7,8)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
  } else {
    pid_prior <- edges$PriorSl2
    PresencePrior <-  ifelse((edges$Str_Order %in% c(6,7,8,9)) & edges$SPAWNING_C == 0, 0, 1)
    
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)
    
    ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)

    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"

    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
  }
  
  # 6. BAYESIAN ASSIGNMENT
  if (verbose) cat("  Performing Bayesian assignment...\n")
  n_basins <- length(pid_iso)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(NA, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    if (watershed == "Kusko") {
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * NewHabitatPrior * PresencePrior
    } else {
      gen_prior <- rep(0, length(pid_iso))
      gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
      gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])

      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * gen_prior * NewHabitatPrior * PresencePrior
    }
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # 7. PROCESS RESULTS
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE) #Sum across all individuals 
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE) #Rescale to sum to 1 across the basin
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE) #normalize to range from 0-1 
  
  cat(paste("  Total production:", round(sum(basin_assign_sum), 2), "\n"))
  
  # 8. EXPORT TO CSV
  output_dir <- if (watershed == "Kusko") PATHS$output_kusko else PATHS$output_yukon
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create filename suffix based on filter
  filename_suffix <- ""
  if (filter_type != "none") {
    if (filter_type == "cpue_percentile") {
      filename_suffix <- paste0("_CPUE", cpue_lower, "-", cpue_upper)
    } else if (filter_type == "date_range") {
      filename_suffix <- paste0("_DOY", date_start, "-", date_end)
    } else if (filter_type == "both") {
      filename_suffix <- paste0("_CPUE", cpue_lower, "-", cpue_upper, "_DOY", date_start, "-", date_end)
    }
  }
  
  edges_df <- st_drop_geometry(edges)
  output_data <- data.frame(
    reachid = edges_df$reachid,
    Str_Order = edges_df$Str_Order,
    iso_pred = edges_df$iso_pred,
    assignment_sum = basin_assign_sum,
    assignment_rescale = basin_assign_rescale,
    assignment_norm = basin_assign_norm
  )
  
  if (watershed == "Yukon") output_data$GenLMU <- edges_df$GenLMU
  
  filepath <- file.path(output_dir, paste0(year, "_", watershed, "_Assignment_Results", filename_suffix, ".csv"))
  write_csv(output_data, filepath)
  
  cat(paste("  ✓ Exported:", filepath, "\n"))
  cat(paste("  ✓ Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(output_data), "\n"))
  
  # Store filter metadata
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

cat("✓ Enhanced Assignment.R loaded with filtering functions\n")
cat("Available filter types: 'none', 'cpue_percentile', 'date_range', 'both'\n")
cat("See ?run_annual_analysis for detailed parameter descriptions\n")