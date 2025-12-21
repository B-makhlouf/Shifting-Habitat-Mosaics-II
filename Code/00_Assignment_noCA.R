################################################################################
# CONSOLIDATED SALMON ASSIGNMENT - SINGLE FUNCTION
# Kusko, Yukon, and Nushagak support with flexible filtering
################################################################################

library(sf); library(dplyr); library(readr); library(readxl)

run_annual_analysis <- function(year, 
                                watershed,
                                filter_type = "none",
                                cpue_lower = NULL,
                                cpue_upper = NULL,
                                date_start = NULL,
                                date_end = NULL,
                                verbose = TRUE) {
  
  # ============================================================================
  # CONFIGURATION
  # ============================================================================
  
  PATHS <- list(
    kusko_edges = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp",
    kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
    yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
    yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
    yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
    yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
    nushagak_edges = "/Users/benjaminmakhlouf/Spatial Data/NushagakUSGS.shp",
    nushagak_basin = "/Users/benjaminmakhlouf/Spatial Data/Nushagak_basin.shp",
    natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
    output_kusko = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko",
    output_yukon = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon",
    output_nushagak = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Nushagak"
  )
  
  PARAMS <- list(
    Kusko = list(min_stream_order = 3, min_error = 0.00003, sensitivity_threshold = 0.00, max_error = NULL),
    Yukon = list(min_stream_order = 4, min_error = 0.003, sensitivity_threshold = 0.000, max_error = NULL),
    Nushagak = list(min_stream_order = 3, min_error = 0.0000, sensitivity_threshold = 0.0, max_error = NULL, has_genetic_data = FALSE)
  )
  
  # ============================================================================
  # VALIDATION & INITIALIZATION
  # ============================================================================
  
  if (!(watershed %in% c("Kusko", "Yukon", "Nushagak"))) {
    stop("Watershed must be 'Kusko', 'Yukon', or 'Nushagak'")
  }
  
  if (verbose) cat(paste("\n=== Processing", watershed, year, "===\n"))
  
  params <- PARAMS[[watershed]]
  
  # ============================================================================
  # APPLY FILTERS (helper function inline)
  # ============================================================================
  
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
  
  # ============================================================================
  # LOAD SPATIAL DATA
  # ============================================================================
  
  if (watershed == "Kusko") {
    edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  } else if (watershed == "Yukon") {
    edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  } else {
    edges <- st_read(PATHS$nushagak_edges, quiet = TRUE)
    basin <- st_read(PATHS$nushagak_basin, quiet = TRUE)
  }
  
  edges <- st_transform(edges, st_crs(basin))
  
  if (verbose) {
    cat(paste("  Loaded", nrow(edges), "total stream segments (all stream orders)\n"))
    meets_threshold <- edges$Str_Order >= params$min_stream_order
    cat(paste("  Segments meeting min stream order threshold:", sum(meets_threshold), "\n"))
  }
  
  # ============================================================================
  # LOAD & CLEAN NATAL DATA
  # ============================================================================
  
  natal_data_raw <- read_csv(file.path(PATHS$natal_data_dir, 
                                       paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv")), 
                             show_col_types = FALSE)
  
  if (watershed == "Yukon") {
    natal_data_clean <- filter(natal_data_raw, !is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  } else {
    natal_data_clean <- filter(natal_data_raw, !is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  
  # ============================================================================
  # APPLY FILTERING
  # ============================================================================
  
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
  
  # ============================================================================
  # CALCULATE ERROR
  # ============================================================================
  
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  
  if (!is.null(params$max_error)) {
    pid_isose_mod <- ifelse(pid_isose_mod > params$max_error, params$max_error, pid_isose_mod)
  }
  
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # ============================================================================
  # SETUP PRIORS
  # ============================================================================
  
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
    pid_prior <- edges$UniPh2oNoE
    PresencePrior <- ifelse((edges$Str_Order %in% c(6)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, edges$Spawner_IP)
  }
  
  # ============================================================================
  # BAYESIAN ASSIGNMENT
  # ============================================================================
  
  if (verbose) cat("  Performing Bayesian assignment...\n")
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    if (watershed == "Kusko") {
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior  * PresencePrior #* NewHabitatPrior
      
    } else if (watershed == "Yukon") {
      gen_prior <- rep(0, length(pid_iso))
      gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
      gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * gen_prior *PresencePrior #NewHabitatPrior *
      
    } else {
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * NewHabitatPrior * PresencePrior
    }
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # ============================================================================
  # PROCESS RESULTS
  # ============================================================================
  
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
  
  # ============================================================================
  # EXPORT TO CSV
  # ============================================================================
  
  output_dir <- if (watershed == "Kusko") PATHS$output_kusko else 
    if (watershed == "Yukon") PATHS$output_yukon else 
      PATHS$output_nushagak
  
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
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
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