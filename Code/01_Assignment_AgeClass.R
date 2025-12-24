################################################################################
# YUKON AGE CLASS MAPPING ANALYSIS - CONSOLIDATED
# Single function version for easy step-by-step walkthrough
################################################################################

library(sf); library(dplyr); library(readr); library(readxl)
library(RColorBrewer); library(grid)

#==============================================================================
# CONSOLIDATED MAIN FUNCTION
#==============================================================================

run_yukon_analysis <- function(year, age_class) {
  
  # ============================================================================
  # SECTION 1: CONFIGURATION
  # ============================================================================
  
  PATHS <- list(
    yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
    yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
    yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
    yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
    
    natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
    master_genetics = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Genetic Data/01_Raw/LYTF_2015-2022_Otoliths_Genetics_data.csv",
    escapement_data = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx",
    
    output_dir = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual/AgeClass"
  )
  
  PARAMS <- list(
    min_stream_order = 4, 
    min_error = 0.003, 
    sensitivity_threshold = 0.7
  )
  
  MAPPING_PARAMS <- list(
    color_bins = c(0.0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0),
    linewidth_order_9 = 3.7,
    linewidth_order_8 = 3.0,
    linewidth_order_7 = 3.2,
    linewidth_order_6 = 2.2,
    linewidth_order_5 = 1.5,
    linewidth_order_4 = 1.2,
    linewidth_order_3 = 0.5
  )
  
  dir.create(PATHS$output_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat(paste("\n=== Year", year, "- Age", age_class, "===\n"))
  
  # ============================================================================
  # SECTION 2: LOAD YUKON SPATIAL DATA
  # ============================================================================
  
  cat("Loading spatial data...\n")
  
  edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
  basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  
  # Transform and prepare edges
  edges <- st_transform(edges, st_crs(basin))
  
  # Load genetic group data
  ly_gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
  my_gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
  
  # Assign genetic groups
  edges$GenLMU <- 0
  edges$GenLMU[edges$reachid %in% ly_gen$reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my_gen$reachid] <- "middle"
  
  # ============================================================================
  # SECTION 3: LOAD NATAL ORIGIN DATA
  # ============================================================================
  
  cat("Loading natal origin data...\n")
  
  file_path <- file.path(PATHS$natal_data_dir, 
                         paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  
  if (!file.exists(file_path)) {
    stop("Natal data file not found: ", file_path)
  }
  
  natal_data_raw <- read_csv(file_path, show_col_types = FALSE) %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  # ============================================================================
  # SECTION 4: ADD AGE DATA
  # ============================================================================
  
  cat("Loading age data...\n")
  
  master_gen <- read.csv(PATHS$master_genetics) %>%
    filter(sampleYear == year, Genotyped. == "Yes") %>%
    select(Otolith.Number, totalAge) %>%
    distinct(Otolith.Number, .keep_all = TRUE)
  
  natal_data <- natal_data_raw %>%
    left_join(master_gen, by = c("OtoNum" = "Otolith.Number"))
  
  # ============================================================================
  # SECTION 5: GET ESCAPEMENT DATA
  # ============================================================================
  
  cat("Loading escapement data...\n")
  
  escapement <- read_excel(PATHS$escapement_data) %>%
    filter(River == "Yukon", Year == year) %>%
    pull(Total_Run)
  
  if (length(escapement) == 0) {
    stop("Escapement data not found for Yukon, year ", year)
  }
  
  escapement <- as.numeric(escapement)
  
  # ============================================================================
  # SECTION 6: FILTER BY AGE CLASS
  # ============================================================================
  
  cat("Filtering by age class...\n")
  
  if (!("totalAge" %in% names(natal_data))) {
    stop("Age data not found in natal_data.")
  }
  
  natal_data <- natal_data %>% filter(totalAge == age_class)
  
  if (nrow(natal_data) == 0) {
    stop("No data found for age class: ", age_class)
  }
  
  cat(paste("  Age", age_class, ":", nrow(natal_data), "fish\n"))
  
  # ============================================================================
  # SECTION 7: CALCULATE STRATUM WEIGHTS (CPUE adjustment by season)
  # ============================================================================
  
  cat("Calculating stratum weights...\n")
  
  # Create 5 equal time strata
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
  
  # Join weights back to raw data
  day_strata_with_weights <- day_strata %>%
    left_join(strata_summary, by = "strata")
  
  natal_data_raw <- natal_data_raw %>%
    left_join(day_strata_with_weights %>% select(Date, strata, weight), by = "Date")
  
  # Extract unique weight values per OtoNum
  weight_data <- natal_data_raw %>%
    select(OtoNum, weight) %>%
    distinct(OtoNum, .keep_all = TRUE)
  
  natal_data <- natal_data %>%
    left_join(weight_data, by = "OtoNum")
  
  # ============================================================================
  # SECTION 8: PERFORM BAYESIAN ASSIGNMENT
  # ============================================================================
  
  cat("Performing Bayesian assignment...\n")
  
  # Extract isoscape and genetic data
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  # Calculate error
  pid_isose_mod <- ifelse(pid_isose < PARAMS$min_error, PARAMS$min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # Setup priors
  stream_order_prior <- ifelse(edges$Str_Order >= PARAMS$min_stream_order, 1, 0)
  pid_prior <- edges$PriorSl2
  presence_prior <- ifelse((edges$Str_Order %in% c(8,9)) & edges$SPAWNING_C == 0, 0, 1)
  habitat_prior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)
  
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
    
    # Calculate assignment probability
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      pid_prior * stream_order_prior * gen_prior * presence_prior #* habitat_prior 
    
    # Normalize and apply threshold
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < PARAMS$sensitivity_threshold] <- 0
    
    # Weight by CPUE
    assignment_matrix[,i] <- assign_rescaled * natal_data$weight[i]
  }
  
  # Sum across all fish to get reach-level production
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  
  # ============================================================================
  # SECTION 9: PROCESS ASSIGNMENTS TO NORMALIZED AND INDIVIDUAL COUNTS
  # ============================================================================
  
  cat("Processing assignment results...\n")
  
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
  
  # ============================================================================
  # SECTION 10: CREATE MAP
  # ============================================================================
  
  cat("Creating map...\n")
  
  # Setup color palette
  palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)
  

  # Color coding based on normalized assignment
  colcode <- rep("gray90", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'grey95'
  
  # colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette[1]
  # colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette[2]
  # colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette[3]
  # colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette[4]
  # colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette[5]
  # colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette[6]
  # colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette[7]
  # 
  # colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.7] <- 'grey95'
  # colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[8]
  # colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[9]
  # colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette[10]
  
  # colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette[1]
  #   colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette[2]
  #   colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette[3]
  #   colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette[4]
  #   colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette[5]
  #   colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette[6]
  #   colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette[7]
  #   colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[8]
  #   colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[9]
  #   colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette[10]
  
  colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette[5]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[7]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette[9]
  colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette[10]
  
  # Setup linewidths by stream order
  stream_order <- edges$Str_Order
  colcode[stream_order < PARAMS$min_stream_order] <- "gray50"
  
  linewidths <- rep(0.5, length(stream_order))
  linewidths[stream_order == 9] <- MAPPING_PARAMS$linewidth_order_9
  linewidths[stream_order == 8] <- MAPPING_PARAMS$linewidth_order_8
  linewidths[stream_order == 7] <- MAPPING_PARAMS$linewidth_order_7
  linewidths[stream_order == 6] <- MAPPING_PARAMS$linewidth_order_6
  linewidths[stream_order == 5] <- MAPPING_PARAMS$linewidth_order_5
  linewidths[stream_order == 4] <- MAPPING_PARAMS$linewidth_order_4
  linewidths[stream_order == 3] <- 0
  linewidths[stream_order <= 2] <- 0
  
  # Create map
  map_filename <- file.path(PATHS$output_dir, 
                            paste0(year, "_", age_class, "_AgeClass_Yukon.png"))
  
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  par(mar = c(8, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = paste0("Age ", age_class, " Production - Year ", year, "\nYukon River"),
       bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add legend
  legend_labels <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", 
                     "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
  legend_colors <- palette
  
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("✓ Saved map:", basename(map_filename), "\n"))
  
  # ============================================================================
  # SECTION 11: RETURN SUMMARY
  # ============================================================================
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  
  return(list(
    year = year,
    age_class = age_class,
    n_fish = n_fish,
    escapement = escapement,
    total_production = sum(basin_assign_sum),
    basin_assign_sum = basin_assign_sum,
    basin_assign_norm = basin_assign_norm,
    basin_assign_individuals = basin_assign_individuals,
    map_filename = map_filename
  ))
}

#==============================================================================
# EXECUTION
#==============================================================================

# Run single analysis
# result <- run_yukon_analysis(2015, 1.3)

# Or run loop
for (year in c(2015, 2016, 2021)) {
  for (age in c(1.3, 1.4)) {
    tryCatch({
      run_yukon_analysis(year, age)
    }, error = function(e) {
      cat("ERROR - Year", year, "Age", age, ":", e$message, "\n")
    })
  }
}