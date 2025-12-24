################################################################################
# YUKON TOP 20% GROWTH SALMON PRODUCTION MAPPING
# Complete workflow in a single function for easy walkthrough
################################################################################

library(sf); library(dplyr); library(readr); library(readxl)
library(RColorBrewer); library(grid)

#==============================================================================
# CONSOLIDATED ANALYSIS FUNCTION
#==============================================================================

analyze_yukon_salmon_production <- function(year) {
  
  #============================================================================
  # 1. CONFIGURATION & SETUP
  #============================================================================
  
  cat(paste("\n=== YEAR", year, "===\n"))
  
  # Define all file paths
  paths <- list(
    yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
    yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
    yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
    yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
    natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
    master_genetics = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Genetic Data/01_Raw/LYTF_2015-2022_Otoliths_Genetics_data.csv",
    escapement_data = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx",
    growth_data = "/Users/benjaminmakhlouf/Research_repos/03_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_Combined.csv",
    output_dir = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual/Growth"
  )
  
  # Analysis parameters
  params <- list(
    min_stream_order = 4,
    min_error = 0.003,
    sensitivity_threshold = 0.7,
    growth_percentile = 0.75
  )
  
  # Mapping parameters
  mapping_params <- list(
    color_bins = c(0.0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0),
    linewidth_order_9 = 3.7,
    linewidth_order_8 = 3.0,
    linewidth_order_7 = 3.2,
    linewidth_order_6 = 2.2,
    linewidth_order_5 = 1.5,
    linewidth_order_4 = 1.2,
    linewidth_order_3 = 0.5
  )
  
  dir.create(paths$output_dir, recursive = TRUE, showWarnings = FALSE)
  
  #============================================================================
  # 2. LOAD SPATIAL DATA
  #============================================================================
  
  cat("Step 1: Loading spatial data...\n")
  
  edges <- st_read(paths$yukon_edges, quiet = TRUE)
  basin <- st_read(paths$yukon_basin, quiet = TRUE)
  
  # Transform edges to match basin CRS
  edges <- st_transform(edges, st_crs(basin))
  
  # Load genetic group boundaries
  ly_gen <- st_read(paths$yukon_ly_gen, quiet = TRUE)
  my_gen <- st_read(paths$yukon_my_gen, quiet = TRUE)
  
  # Assign genetic groups to edges
  edges$GenLMU <- 0
  edges$GenLMU[edges$reachid %in% ly_gen$reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my_gen$reachid] <- "middle"
  
  cat("  Loaded edges and basin\n")
  
  #============================================================================
  # 3. LOAD NATAL DATA
  #============================================================================
  
  cat("Step 2: Loading natal origin data...\n")
  
  file_path <- file.path(paths$natal_data_dir, 
                         paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  
  if (!file.exists(file_path)) {
    stop("Natal data file not found: ", file_path)
  }
  
  natal_data_raw <- read_csv(file_path, show_col_types = FALSE) %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  cat(paste("  Loaded", nrow(natal_data_raw), "records\n"))
  
  #============================================================================
  # 4. LOAD AND PROCESS GROWTH DATA
  #============================================================================
  
  cat("Step 3: Loading and processing growth data...\n")
  
  # Load growth data (first 6 columns only)
  allgrowthdata <- read.csv(paths$growth_data)[, 1:6]
  
  # Calculate growth from natal to marine
  allgrowthdata$growthfw <- allgrowthdata$Marine_Start - allgrowthdata$Natal_Start
  
  
  # Add growth data to natal data by matching fish_id
  natal_data_raw <- natal_data_raw %>%
    left_join(
      allgrowthdata %>% select(Fish_id, growthfw),
      by = c("Fish_id" = "Fish_id")  # Adjust column name if needed
    )
  
  # list out all the valid growth values 
  growth_valid <- natal_data_raw$growthfw[!is.na(natal_data_raw$growthfw)]
  
  # Calculate threshold for top 20% of growth
  growth_threshold <- quantile(growth_valid, params$growth_percentile, na.rm = TRUE)
  cat(paste("  Growth threshold (80th percentile):", round(growth_threshold, 2), "\n"))
  
  
  
  
  #============================================================================
  # 5. FILTER BY TOP 20% GROWTH
  #============================================================================
  
  cat("Step 4: Filtering for top 20% growth...\n")
  
  natal_data <- natal_data_raw %>%
    filter(growthfw >= growth_threshold)
  
  # Remove NA 
  natal_data <- natal_data %>%
    filter(!is.na(growthfw))
  
  initial_count <- nrow(natal_data_raw)
  final_count <- nrow(natal_data)
  cat(paste("  Records before filter:", initial_count, "\n"))
  cat(paste("  Records after growth filter:", final_count, "\n"))
  
  if (final_count == 0) {
    stop("No data found with top 20% growth")
  }
  
  #============================================================================
  # 6. CALCULATE STRATUM WEIGHTS (CPUE adjustment by season)
  #============================================================================
  
  cat("Step 5: Calculating stratum weights for seasonal adjustment...\n")
  
  # Create 5 equal time strata from raw data (before filtering)
  unique_days <- sort(unique(natal_data_raw$Date))
  ndays <- length(unique_days)
  strata_size <- ceiling(ndays / 5)
  
  day_strata <- tibble(
    Date = unique_days,
    strata = rep(1:5, each = strata_size, length.out = ndays)
  )
  
  # Calculate CPUE proportions by strata
  strata_summary <- natal_data_raw %>%
    distinct(Date, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata, by = "Date") %>%
    group_by(strata) %>%
    summarise(
      cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
      oto_sum = sum(OtoPropDaily, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      weight = cpue_sum / oto_sum
    ) %>%
    select(strata, weight)
  
  # Join weights to filtered natal data
  day_strata_with_weights <- day_strata %>%
    left_join(strata_summary, by = "strata")
  
  natal_data <- natal_data %>%
    left_join(day_strata_with_weights %>% select(Date, strata, weight), by = "Date")
  
  cat(paste("  Stratum weights calculated for", nrow(strata_summary), "strata\n"))
  
  #============================================================================
  # 7. LOAD ESCAPEMENT DATA
  #============================================================================
  
  cat("Step 6: Loading escapement data...\n")
  
  escapement <- read_excel(paths$escapement_data) %>%
    filter(River == "Yukon", Year == year) %>%
    pull(Total_Run)
  
  if (length(escapement) == 0) {
    stop("Escapement data not found for Yukon, year ", year)
  }
  
  escapement <- as.numeric(escapement)
  cat(paste("  Total escapement:", escapement, "\n"))
  
  #============================================================================
  # 8. PERFORM BAYESIAN ASSIGNMENT
  #============================================================================
  
  cat("Step 7: Performing Bayesian assignment for natal origin...\n")
  
  # Extract isoscape and genetic data from edges
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Calculate error variance
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # Setup priors
  stream_order_prior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  pid_prior <- edges$PriorSl2
  presence_prior <- ifelse((edges$Str_Order %in% c(8, 9)) & edges$SPAWNING_C == 0, 0, 1)
  #habitat_prior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)
  
  # Get genetic group site indices
  ly_sites <- which(edges$GenLMU == "lower")
  my_sites <- which(edges$GenLMU == "middle")
  
  # Initialize assignment matrix
  n_reaches <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_reaches, ncol = n_fish)
  
  # Perform assignment for each fish
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    # Set up genetic prior for this fish
    gen_prior <- rep(0, n_reaches)
    gen_prior[ly_sites] <- as.numeric(natal_data$Lower[i])
    gen_prior[my_sites] <- as.numeric(natal_data$Middle[i])
    
    # Calculate assignment probability using Bayesian framework
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      pid_prior * stream_order_prior * gen_prior *  presence_prior #habitat_prior *
    
    # Normalize and apply sensitivity threshold
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    
    # Weight by CPUE
    assignment_matrix[, i] <- assign_rescaled * natal_data$weight[i]
  }
  
  # Sum across all fish to get reach-level production
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  
  cat(paste("  Assignment complete for", n_fish, "fish\n"))
  
  #============================================================================
  # 9. PROCESS RESULTS
  #============================================================================
  
  cat("Step 8: Processing and normalizing results...\n")
  
  total_sum <- sum(basin_assign_sum, na.rm = TRUE)
  
  if (total_sum > 0) {
    basin_assign_rescale <- basin_assign_sum / total_sum
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
    basin_assign_individuals <- basin_assign_rescale * escapement
  } else {
    basin_assign_rescale <- rep(0, length(basin_assign_sum))
    basin_assign_norm <- rep(0, length(basin_assign_sum))
    basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  cat(paste("  Total production:", round(sum(basin_assign_sum), 2), "\n"))
  
  #============================================================================
  # 10. CREATE MAP
  #============================================================================
  
  cat("Step 9: Creating map visualization...\n")
  
  # Setup color palette
  palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)
  
  # Color coding based on normalized assignment (10 bins)
  colcode <- rep("gray90", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'grey95'
  # colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette[1]
  # colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette[2]
  # colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette[3]
  # colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette[4]
  # colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette[5]
  # colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette[6]
  # colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette[7]
  # colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[8]
  # colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[9]
  # colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette[10]
 
  colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette[5]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[7]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[9]
  colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette[10]
  
  # Apply stream order filters and linewidths
  stream_order <- edges$Str_Order
  colcode[stream_order < params$min_stream_order] <- "gray50"
  
  linewidths <- rep(0.5, length(stream_order))
  linewidths[stream_order == 9] <- mapping_params$linewidth_order_9
  linewidths[stream_order == 8] <- mapping_params$linewidth_order_8
  linewidths[stream_order == 7] <- mapping_params$linewidth_order_7
  linewidths[stream_order == 6] <- mapping_params$linewidth_order_6
  linewidths[stream_order == 5] <- mapping_params$linewidth_order_5
  linewidths[stream_order == 4] <- mapping_params$linewidth_order_4
  linewidths[stream_order == 3] <- 0
  linewidths[stream_order <= 2] <- 0
  
  # Create output filename
  output_filename <- file.path(paths$output_dir, 
                               paste0(year, "_Top20Growth_Yukon.png"))
  
  # Generate map
  png(file = output_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  par(mar = c(3, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = paste0("Salmon Production - Year ", year, 
                     "\nYukon River (Top 20% Growth)"),
       bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add legend
  legend_labels <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                     "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
  legend_colors <- palette
  
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  par(mar = c(1, 1, 1, 1), bg = "white")
  
  cat(paste("✓ Saved map:", basename(output_filename), "\n"))
  
  #============================================================================
  # 11. RETURN RESULTS SUMMARY
  #============================================================================
  
  cat("\n=== ANALYSIS COMPLETE ===\n\n")
  
  return(list(
    year = year,
    n_fish = n_fish,
    growth_threshold = growth_threshold,
    escapement = escapement,
    total_production = sum(basin_assign_sum),
    basin_assign_sum = basin_assign_sum,
    basin_assign_norm = basin_assign_norm,
    basin_assign_individuals = basin_assign_individuals,
    map_file = output_filename
  ))
}

#==============================================================================
# EXECUTION
#==============================================================================

# Run analysis for multiple years
for (year in c(2015, 2016, 2021)) {
  tryCatch({
    result <- analyze_yukon_salmon_production(year)
  }, error = function(e) {
    cat("ERROR - Year", year, ":", e$message, "\n")
  })
}