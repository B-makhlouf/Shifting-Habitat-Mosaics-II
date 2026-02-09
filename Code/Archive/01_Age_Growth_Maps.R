################################################################################
# YUKON SALMON ASSIGNMENT ANALYSIS - COMBINED AGECLASS & GROWTH
# Single function supports analysis by: Age Class OR Top 20% Growth
# Complete workflow in ~300 lines for easy step-by-step walkthrough
################################################################################

library(sf); library(dplyr); library(readr); library(readxl)
library(RColorBrewer); library(grid)

#==============================================================================
# UNIFIED ANALYSIS FUNCTION
#==============================================================================

run_yukon_analysis <- function(year, analysis_type = "age", age_class = NULL, 
                               growth_percentile = 0.85) {
  
  # ============================================================================
  # CONFIG & VALIDATION
  # ============================================================================
  
  if (!(analysis_type %in% c("age", "growth"))) {
    stop("analysis_type must be 'age' or 'growth'")
  }
  
  if (analysis_type == "age" && is.null(age_class)) {
    stop("age_class required for age analysis")
  }
  
  analysis_label <- ifelse(analysis_type == "age", 
                           paste0("Age ", age_class),
                           "Top 20% Growth")
  
  cat(paste("\n=== YEAR", year, "-", analysis_label, "===\n"))
  
  # File paths
  paths <- list(
    yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
    yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
    yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
    yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
    natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
    master_genetics = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Genetic Data/01_Raw/LYTF_2015-2022_Otoliths_Genetics_data.csv",
    escapement_data = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx",
    growth_data = "/Users/benjaminmakhlouf/Research_repos/03_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_Combined.csv"
  )
  
  if (analysis_type == "age") {
    paths$output_dir = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/Maps/Yukon_Annual/AgeClass"
  } else {
    paths$output_dir = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/Maps/Yukon_Annual/Growth"
  }
  
  # Analysis parameters
  params <- list(
    min_stream_order = 4, min_error = 0.0035, sensitivity_threshold = 0.000
  )
  
  # Mapping parameters
  map_params <- list(
    lwd_9 = 3.7, lwd_8 = 5, lwd_7 = 2.0, lwd_6 = 1.5, 
    lwd_5 = 1.4, lwd_4 = 1.0, lwd_3 = 0.5
  )
  
  dir.create(paths$output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ============================================================================
  # LOAD SPATIAL & GENETIC DATA
  # ============================================================================
  
  cat("Loading spatial data...\n")
  edges <- st_read(paths$yukon_edges, quiet = TRUE)
  basin <- st_read(paths$yukon_basin, quiet = TRUE)
  edges <- st_transform(edges, st_crs(basin))
  
  ly_gen <- st_read(paths$yukon_ly_gen, quiet = TRUE)
  my_gen <- st_read(paths$yukon_my_gen, quiet = TRUE)
  
  edges$GenLMU <- 0
  edges$GenLMU[edges$reachid %in% ly_gen$reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my_gen$reachid] <- "middle"
  
  # ============================================================================
  # LOAD NATAL DATA
  # ============================================================================
  
  cat("Loading natal data...\n")
  file_path <- file.path(paths$natal_data_dir, 
                         paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  
  if (!file.exists(file_path)) {
    stop("Natal data file not found: ", file_path)
  }
  
  natal_data_raw <- read_csv(file_path, show_col_types = FALSE) %>%
    filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  
  # ============================================================================
  # ANALYSIS-SPECIFIC DATA LOADING & FILTERING
  # ============================================================================
  
  if (analysis_type == "age") {
    cat("Loading age data...\n")
    
    master_gen <- read.csv(paths$master_genetics) %>%
      filter(sampleYear == year, Genotyped. == "Yes") %>%
      select(Otolith.Number, totalAge) %>%
      distinct(Otolith.Number, .keep_all = TRUE)
    
    natal_data <- natal_data_raw %>%
      left_join(master_gen, by = c("OtoNum" = "Otolith.Number")) %>%
      filter(totalAge == age_class)
    
    if (nrow(natal_data) == 0) {
      stop("No data found for age class: ", age_class)
    }
    
    cat(paste("  Age", age_class, ":", nrow(natal_data), "fish\n"))
    
  } else {
    # GROWTH ANALYSIS
    cat("Loading and processing growth data...\n")
    
    allgrowthdata <- read.csv(paths$growth_data)[, 1:6]
    allgrowthdata$growthfw <- allgrowthdata$Marine_Start - allgrowthdata$Natal_Start
    
    natal_data_raw <- natal_data_raw %>%
      left_join(allgrowthdata %>% select(Fish_id, growthfw), by = "Fish_id")
    
    growth_valid <- natal_data_raw$growthfw[!is.na(natal_data_raw$growthfw)]
    growth_threshold <- quantile(growth_valid, growth_percentile, na.rm = TRUE)
    
    cat(paste("  Growth threshold (", growth_percentile*100, "th percentile):", 
              round(growth_threshold, 2), "\n"))
    
    natal_data <- natal_data_raw %>%
      filter(growthfw >= growth_threshold, !is.na(growthfw))
    
    cat(paste("  Records: before =", nrow(natal_data_raw), 
              "| after =", nrow(natal_data), "\n"))
    
    if (nrow(natal_data) == 0) {
      stop("No data found after growth filtering")
    }
  }
  
  # ============================================================================
  # CALCULATE STRATUM WEIGHTS (seasonal CPUE adjustment)
  # ============================================================================
  
  cat("Calculating stratum weights...\n")
  
  unique_days <- sort(unique(natal_data_raw$Date))
  ndays <- length(unique_days)
  strata_size <- ceiling(ndays / 5)
  
  day_strata <- tibble(
    Date = unique_days,
    strata = rep(1:5, each = strata_size, length.out = ndays)
  )
  
  strata_summary <- natal_data_raw %>%
    distinct(Date, dailyCPUEprop, OtoPropDaily) %>%
    left_join(day_strata, by = "Date") %>%
    group_by(strata) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE),
              oto_sum = sum(OtoPropDaily, na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = cpue_sum / oto_sum) %>%
    select(strata, weight)
  
  day_strata_with_weights <- day_strata %>%
    left_join(strata_summary, by = "strata")
  
  natal_data <- natal_data %>%
    left_join(day_strata_with_weights %>% select(Date, strata, weight), by = "Date")
  
  # ============================================================================
  # LOAD ESCAPEMENT DATA
  # ============================================================================
  
  cat("Loading escapement data...\n")
  
  escapement <- read_excel(paths$escapement_data) %>%
    filter(River == "Yukon", Year == year) %>%
    pull(Total_Run)
  
  if (length(escapement) == 0) {
    stop("Escapement data not found for Yukon, year ", year)
  }
  escapement <- as.numeric(escapement)
  
  # ============================================================================
  # BAYESIAN ASSIGNMENT
  # ============================================================================
  
  cat("Performing Bayesian assignment...\n")
  
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  stream_order_prior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  pid_prior <- edges$PriorSl2
  presence_prior <- ifelse((edges$Str_Order %in% c(8,9)) & edges$SPAWNING_C == 0, 0, 1)
  
  ly_sites <- which(edges$GenLMU == "lower")
  my_sites <- which(edges$GenLMU == "middle")
  
  n_reaches <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_reaches, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    gen_prior <- rep(0, n_reaches)
    gen_prior[ly_sites] <- as.numeric(natal_data$Lower[i])
    gen_prior[my_sites] <- as.numeric(natal_data$Middle[i])
    
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      pid_prior * stream_order_prior * gen_prior * presence_prior
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    
    assignment_matrix[,i] <- assign_rescaled * natal_data$weight[i]
  }
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  
  # ============================================================================
  # PROCESS RESULTS TO NORMALIZED & INDIVIDUAL COUNTS
  # ============================================================================
  
  cat("Processing results...\n")
  
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
  # CREATE MAP
  # ============================================================================
  
  cat("Creating map...\n")
  
  palette <- colorRampPalette(brewer.pal(9, "OrRd"))(10)
  
  colcode <- rep("gray90", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'grey95'
    colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.4] <- palette[2]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.7] <- palette[5]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette[7]
    #colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.8] <- "gray95"
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette [8]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 0.95] <- palette[9]
    colcode[basin_assign_norm > 0.95 & basin_assign_norm <= 1.0] <- palette[10]
  
  
  
  
  stream_order <- edges$Str_Order
  colcode[stream_order < params$min_stream_order] <- "gray50"
  
  linewidths <- rep(0.5, length(stream_order))
  linewidths[stream_order == 9] <- map_params$lwd_9
  linewidths[stream_order == 8] <- map_params$lwd_8
  linewidths[stream_order == 7] <- map_params$lwd_7
  linewidths[stream_order == 6] <- map_params$lwd_6
  linewidths[stream_order == 5] <- map_params$lwd_5
  linewidths[stream_order == 4] <- map_params$lwd_4
  linewidths[stream_order == 3] <- 0
  linewidths[stream_order <= 2] <- 0
  
  linewidths[basin_assign_norm > 0.8] <- linewidths[basin_assign_norm > 0.8] * 1.5
  
  
  # Create filename based on analysis type
  if (analysis_type == "age") {
    map_filename <- file.path(paths$output_dir, 
                              paste0(year, "_Age", age_class, "_Yukon.png"))
    main_title <- paste0("Age ", age_class, " Production - Year ", year, "\nYukon River")
  } else {
    map_filename <- file.path(paths$output_dir, 
                              paste0(year, "_Top20Growth_Yukon.png"))
    main_title <- paste0("Salmon Production - Year ", year, "\nYukon River (Top 20% Growth)")
  }
  
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(8, 4, 4, 2), bg = "white")
  
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = main_title, bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  legend("topleft", legend = c("0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"), 
         col = palette[c(5,7,9,10)], lwd = 5,
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat(paste("✓ Map saved:", basename(map_filename), "\n"))
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  cat("=== COMPLETE ===\n\n")
  
  return(list(
    year = year,
    analysis_type = analysis_type,
    n_fish = n_fish,
    escapement = escapement,
    total_production = sum(basin_assign_sum),
    map_file = map_filename
  ))
}

#==============================================================================
# EXECUTION EXAMPLES
#==============================================================================

# AGE CLASS ANALYSIS
for (year in c(2015, 2016, 2021)) {
  for (age in c(1.3, 1.4)) {
    tryCatch({
      run_yukon_analysis(year, analysis_type = "age", age_class = age)
    }, error = function(e) {
      cat("ERROR - Year", year, "Age", age, ":", e$message, "\n")
    })
  }
}

# # GROWTH ANALYSIS
# for (year in c(2015, 2016, 2021)) {
#   tryCatch({
#     run_yukon_analysis(year, analysis_type = "growth", growth_percentile = 0.75)
#   }, error = function(e) {
#     cat("ERROR - Year", year, ":", e$message, "\n")
#   })
# }