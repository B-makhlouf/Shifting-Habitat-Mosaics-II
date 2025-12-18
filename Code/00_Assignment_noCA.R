################################################################################
# CONSOLIDATED SALMON ASSIGNMENT ANALYSIS WITH FLEXIBLE FILTERING
# UPDATED: Adds Nushagak watershed alongside Kusko and Yukon
# UPDATED: Keeps all stream orders, assigns 0 to below-threshold streams
# NEW: Added "cpue_50_cutoff" filter type for 50% cumulative CPUE analysis
################################################################################

library(sf); library(dplyr); library(readr)

#------------------------------------------------------------------------------
# CONFIGURATION
#------------------------------------------------------------------------------
PATHS <- list(
  # KUSKOKWIM PATHS
  kusko_edges = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp",
  kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  
  # YUKON PATHS
  yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
  yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
  yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
  yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
  
  # NUSHAGAK PATHS - PLACEHOLDERS (UPDATE THESE WITH ACTUAL PATHS)
  nushagak_edges = "/Users/benjaminmakhlouf/Spatial Data/NushagakUSGS.shp",  # TODO: UPDATE PATH
  nushagak_basin = "/Users/benjaminmakhlouf/Spatial Data/Nushagak_basin.shp",  # TODO: UPDATE PATH
  # NOTE: Add genetic group paths if Nushagak has genetic data similar to Yukon
  # nushagak_gen_group1 = "PATH_TO_GEN_GROUP_1.shp",  # TODO: If applicable
  # nushagak_gen_group2 = "PATH_TO_GEN_GROUP_2.shp",  # TODO: If applicable
  
  # DATA DIRECTORIES
  natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
  
  # OUTPUT DIRECTORIES
  output_kusko = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko",
  output_yukon = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon",
  output_nushagak = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Nushagak"  # TODO: CREATE THIS DIR
)

# Watershed-specific analysis parameters
# TODO: Update Nushagak parameters based on your analysis requirements
PARAMS <- list(
  Kusko = list(
    min_stream_order = 3, 
    min_error = 0.0000, 
    sensitivity_threshold = 0.0000, 
    max_error = NULL
  ),
  Yukon = list(
    min_stream_order = 4, 
    min_error = 0.003, 
    sensitivity_threshold = 0.7, 
    max_error = NULL
  ),
  Nushagak = list(
    min_stream_order = 3,        # TODO: Confirm minimum stream order for Nushagak
    min_error = 0.0000,          # TODO: Confirm minimum isoscape error threshold
    sensitivity_threshold = 0.0, # TODO: Confirm Bayesian assignment sensitivity threshold
    max_error = NULL,
    has_genetic_data = FALSE     # TODO: Set to TRUE if Nushagak has genetic data
  )
)

################################################################################
# FILTERING FUNCTIONS
################################################################################

#' Apply flexible filtering to natal data
#'
#' @param natal_data Raw natal data frame
#' @param filter_type Character: "none", "cpue_percentile", "date_range", "both", or "cpue_50_cutoff"
#' @param cpue_lower Numeric: Lower percentile (0-100) for CPUE filtering (e.g., 25 for top 75%)
#' @param cpue_upper Numeric: Upper percentile (0-100) for CPUE filtering (e.g., 100 for full range)
#' @param date_start Numeric: Starting day of year (1-365)
#' @param date_end Numeric: Ending day of year (1-365)
#' @param watershed Character: "Kusko", "Yukon", or "Nushagak" (for data validation)
#'
#' @return Filtered natal data frame with attributes about filtering applied
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
  
  # 50% CUMULATIVE CPUE CUTOFF (NEW)
  if (filter_type == "cpue_50_cutoff") {
    # Sort by DOY
    sorted_data <- filtered_data %>% arrange(DOY)
    
    # Calculate cumulative CPUE by day (summing COratio for each DOY)
    daily_cpue <- sorted_data %>%
      group_by(DOY) %>%
      summarise(daily_total = sum(COratio, na.rm = TRUE), .groups = 'drop') %>%
      arrange(DOY) %>%
      mutate(cumsum_cpue = cumsum(daily_total),
             total_cpue = sum(daily_total),
             cumsum_proportion = cumsum_cpue / total_cpue)
    
    # Find the DOY where 50% cumulative CPUE is reached
    cutoff_doy <- daily_cpue %>%
      filter(cumsum_proportion <= 0.5) %>%
      pull(DOY) %>%
      max()
    
    # Include the day where 50% is reached
    filtered_data <- filtered_data %>% filter(DOY <= cutoff_doy)
    
    filter_description <- paste0("Up to 50% cumulative CPUE (DOY <= ", cutoff_doy, ")")
  }
  
  # CPUE PERCENTILE FILTERING
  else if (filter_type %in% c("cpue_percentile", "both")) {
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
# MAIN FUNCTION (UPDATED - KEEPS ALL STREAM ORDERS, SUPPORTS NUSHAGAK)
################################################################################

#' Run annual analysis with optional filtering for Kusko, Yukon, or Nushagak
#' UPDATED: Keeps ALL stream orders in output (lower order streams get 0 assignment)
#' NEW: Supports Nushagak watershed
#'
#' @param year Numeric year to analyze
#' @param watershed Character: "Kusko", "Yukon", or "Nushagak"
#' @param filter_type Character: "none", "cpue_percentile", "date_range", "both", or "cpue_50_cutoff"
#' @param cpue_lower Numeric: Lower percentile for CPUE (0-100)
#' @param cpue_upper Numeric: Upper percentile for CPUE (0-100)
#' @param date_start Numeric: Starting DOY (1-365)
#' @param date_end Numeric: Ending DOY (1-365)
#' @param verbose Logical: Print detailed output
#'
#' @return List containing edges, basin, results, natal_data, and filter_metadata
#'
#' @examples
#' # Full annual analysis - Nushagak
#' results <- run_annual_analysis(2020, "Nushagak")
#'
#' # Up to 50% cumulative CPUE - Nushagak
#' results <- run_annual_analysis(2020, "Nushagak", filter_type = "cpue_50_cutoff")
run_annual_analysis <- function(year, 
                                watershed,
                                filter_type = "none",
                                cpue_lower = NULL,
                                cpue_upper = NULL,
                                date_start = NULL,
                                date_end = NULL,
                                verbose = TRUE) {
  
  if (!(watershed %in% c("Kusko", "Yukon", "Nushagak"))) {
    stop("Watershed must be 'Kusko', 'Yukon', or 'Nushagak'")
  }
  
  if (verbose) {
    cat(paste("\n=== Processing", watershed, year, "===\n"))
  }
  
  params <- PARAMS[[watershed]]
  
  # 1. LOAD SPATIAL DATA - KEEP ALL STREAM ORDERS
  if (watershed == "Kusko") {
    edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  } else if (watershed == "Yukon") {
    edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  } else if (watershed == "Nushagak") {
    # NUSHAGAK: Load spatial data
    edges <- st_read(PATHS$nushagak_edges, quiet = TRUE)
    basin <- st_read(PATHS$nushagak_basin, quiet = TRUE)
  }
  
  # Transform CRS but DO NOT filter by stream order yet
  edges <- st_transform(edges, st_crs(basin))
  
  cat(paste("  Loaded", nrow(edges), "total stream segments (all stream orders)\n"))
  
  # Create a logical vector for streams that meet minimum stream order
  meets_threshold <- edges$Str_Order >= params$min_stream_order
  cat(paste("  Segments meeting min stream order threshold:", sum(meets_threshold), "\n"))
  
  # 2. LOAD NATAL DATA
  natal_data_raw <- read_csv(file.path(PATHS$natal_data_dir, 
                                       paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv")), 
                             show_col_types = FALSE)
  
  # Clean data - watershed-specific requirements
  if (watershed == "Yukon") {
    natal_data_clean <- filter(natal_data_raw, !is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  } else if (watershed == "Nushagak" && params$has_genetic_data) {
    # TODO: Update genetic column names if Nushagak has different genetic structure
    natal_data_clean <- filter(natal_data_raw, !is.na(natal_iso), !is.na(dailyCPUEprop))
    # Add genetic column filtering here if applicable
  } else {
    # Kusko and Nushagak (if no genetic data) - simple filtering
    natal_data_clean <- filter(natal_data_raw, !is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  
  # 3. APPLY FILTERING
  natal_data <- apply_filters(natal_data_clean,
                              filter_type = filter_type,
                              cpue_lower = cpue_lower,
                              cpue_upper = cpue_upper,
                              date_start = date_start,
                              date_end = date_end,
                              watershed = watershed)
  
  ##############################################################################
  ### ADD FW Growth Calculation 
  ### Growth dataset 
  
  GrowthDat<- read.csv("/Users/benjaminmakhlouf/Research_repos/03_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_Combined.csv")
  
  # because its a huge dataset, lets filter to the first 6 columns 
  GrowthDat<- GrowthDat[,c(1,2,3,4,5,6)]
  
  
  
  
  
  
  if (verbose) {
    cat(paste("  Initial observations: ", nrow(natal_data_clean), "\n", sep = ""))
    print_filter_summary(natal_data)
  }
  
  if (nrow(natal_data) == 0) {
    stop("No data remaining after filtering!")
  }
  
  cat(paste("  Using", nrow(natal_data), "fish for assignment\n"))
  
  # 4. CALCULATE ERROR
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Apply lower bound
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  
  # Apply upper bound if specified
  if (!is.null(params$max_error)) {
    pid_isose_mod <- ifelse(pid_isose_mod > params$max_error, params$max_error, pid_isose_mod)
  }
  
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # 5. SETUP PRIORS - WATERSHED SPECIFIC
  StreamOrderPrior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  
  if (watershed == "Kusko") {
    pid_prior <- edges$UniPh2oNoE
    PresencePrior <- ifelse((edges$Str_Order %in% c(7)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)
    
  } else if (watershed == "Yukon") {
    pid_prior <- edges$PriorSl2
    PresencePrior <- ifelse((edges$Str_Order %in% c(8,9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)
    
    ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
    
    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
    
    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
    
  } else if (watershed == "Nushagak") {
    # TODO: Define Nushagak-specific priors
    # Following Kusko pattern as template (update column names as needed)
    pid_prior <- edges$UniPh2oNoE         # TODO: Confirm correct column name
    PresencePrior <- ifelse((edges$Str_Order %in% c(6)) & edges$SPAWNING_C == 0, 0, 1)  # TODO: Adjust stream order thresholds
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)  # TODO: Confirm column names
    
    # TODO: If Nushagak has genetic data, load and setup genetic priors here
    # Example structure (update paths and column names):
    # if (params$has_genetic_data) {
    #   gen_group1 <- st_read(PATHS$nushagak_gen_group1, quiet = TRUE)
    #   gen_group2 <- st_read(PATHS$nushagak_gen_group2, quiet = TRUE)
    #   
    #   edges$GenGroup <- 0
    #   edges$GenGroup[edges$reachid %in% gen_group1$reachid] <- "group1"
    #   edges$GenGroup[edges$reachid %in% gen_group2$reachid] <- "group2"
    #   
    #   Group1sites <- which(edges$GenGroup == "group1")
    #   Group2sites <- which(edges$GenGroup == "group2")
    # }
  }
  
  # 6. BAYESIAN ASSIGNMENT - Initialize with zeros for all streams
  if (verbose) cat("  Performing Bayesian assignment...\n")
  n_basins <- nrow(edges)  # Use ALL streams, not just filtered ones
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  # Only perform assignment for streams meeting threshold
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    if (watershed == "Kusko") {
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * NewHabitatPrior * PresencePrior
      
    } else if (watershed == "Yukon") {
      gen_prior <- rep(0, length(pid_iso))
      gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
      gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * gen_prior * NewHabitatPrior * PresencePrior
      
    } else if (watershed == "Nushagak") {
      # TODO: Implement Nushagak assignment logic
      # Option 1: If no genetic data (like Kusko):
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * NewHabitatPrior * PresencePrior
      
      # Option 2: If genetic data exists (like Yukon):
      # Uncomment and modify as needed
      # gen_prior <- rep(0, length(pid_iso))
      # gen_prior[Group1sites] <- as.numeric(natal_data$GenGroup1[i])  # TODO: Update column name
      # gen_prior[Group2sites] <- as.numeric(natal_data$GenGroup2[i])  # TODO: Update column name
      # 
      # assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      #   pid_prior * StreamOrderPrior * gen_prior * NewHabitatPrior * PresencePrior
    }
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  
  }
  
  # 7. PROCESS RESULTS
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  
  # Handle case where sum is 0 (avoid division by zero)
  total_sum <- sum(basin_assign_sum, na.rm = TRUE)
  if (total_sum > 0) {
    
    basin_assign_rescale <- basin_assign_sum / total_sum # Everything needs to sum to 1 
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE) # Normalize to max of 1
    
    # Read excel file here 
    library(readxl)
    runsizedat <- read_excel("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx")
    
    # filter for the correct watershed and year 
    runsize<- runsizedat %>%
      filter(River == watershed & Year == year)
    
    runsize<- as.numeric(runsize$Total_Run)
    
    # If its a half run, use only half the runsize, if its full use the whole run size 
    if (filter_type == "cpue_50_cutoff") {
      runsize <- runsize / 2
    }
    
  
    # multiply the basin_assign_rescale by the total run size to get estimated production
    basin_assign_individuals <- basin_assign_rescale * runsize
  
  } else {
    basin_assign_rescale <- rep(0, length(basin_assign_sum))
    basin_assign_norm <- rep(0, length(basin_assign_sum))
    basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  cat(paste("  Total production:", round(sum(basin_assign_sum), 2), "\n"))
  cat(paste("  Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
  
  # 8. EXPORT TO CSV
  if (watershed == "Kusko") {
    output_dir <- PATHS$output_kusko
  } else if (watershed == "Yukon") {
    output_dir <- PATHS$output_yukon
  } else {
    output_dir <- PATHS$output_nushagak
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create filename based on filter type
  filename_base <- ""
  if (filter_type == "cpue_50_cutoff") {
    filename_base <- paste0("CPUE50pct_", year, "_", watershed, "_Assignment_Results")
  } else if (filter_type == "cpue_percentile") {
    filename_base <- paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_", year, "_", watershed, "_Assignment_Results")
  } else if (filter_type == "date_range") {
    filename_base <- paste0("DOY", date_start, "-", date_end, "_", year, "_", watershed, "_Assignment_Results")
  } else if (filter_type == "both") {
    filename_base <- paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_DOY", date_start, "-", date_end, "_", year, "_", watershed, "_Assignment_Results")
  } else {
    # Default for "none" filter type
    filename_base <- paste0(year, "_", watershed, "_Assignment_Results")
  }
  
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
  
  # Add genetic info if applicable
  if (watershed == "Yukon" && exists("GenLMU")) {
    output_data$GenLMU <- edges_df$GenLMU
  } else if (watershed == "Nushagak" && params$has_genetic_data && exists("GenGroup")) {
    output_data$GenGroup <- edges_df$GenGroup  # TODO: Verify this column name
  }
  
  filepath <- file.path(output_dir, paste0(filename_base, ".csv"))
  write_csv(output_data, filepath)
  
  cat(paste("  ✓ Exported:", filepath, "\n"))
  cat(paste("  ✓ Output includes ALL", nrow(output_data), "stream segments (with zeros for below-threshold streams)\n"))
  
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

