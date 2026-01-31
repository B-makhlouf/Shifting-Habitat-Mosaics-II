################################################################################
# SALMON ANALYSIS - SIMPLIFIED VERSION
# Three functions: run_kusko_analysis(), run_yukon_analysis(), create_map()
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
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

PATHS <- list(
  kusko_edges = "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_Reachbase_complete2.shp",
  kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp",
  yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
  yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
  yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
  natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
  runsize_data = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx",
  output_kusko = "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Outputs/ProductionData",
  output_yukon = "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Outputs/ProductionData"
)

BASE_KUSKO_DIR <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Figures/Maps/Kusko_Annual"
BASE_YUKON_DIR <- "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Figures/Maps/Yukon_Annual"

# ==============================================================================
# HELPER: APPLY FILTERS TO NATAL DATA
# ==============================================================================

apply_filters <- function(natal_data, filter_type, cpue_lower, cpue_upper, date_start, date_end) {
  
  filtered_data <- natal_data
  original_n <- nrow(natal_data)
  original_cpue <- sum(natal_data$COratio, na.rm = TRUE)
  filter_description <- "No filter"
  
  # CPUE 50% cutoff filter
  if (filter_type == "cpue_50_cutoff") {
    daily_cpue <- filtered_data %>%
      group_by(DOY) %>%
      summarise(daily_total = sum(COratio, na.rm = TRUE), .groups = 'drop') %>%
      arrange(DOY) %>%
      mutate(cumsum_proportion = cumsum(daily_total) / sum(daily_total))
    
    cutoff_doy <- max(daily_cpue$DOY[daily_cpue$cumsum_proportion <= 0.5])
    filtered_data <- filtered_data %>% filter(DOY <= cutoff_doy)
    filter_description <- paste0("Up to 50% cumulative CPUE (DOY <= ", cutoff_doy, ")")
  }
  
  # CPUE percentile filter
  if (filter_type %in% c("cpue_percentile", "both")) {
    cpue_lower <- ifelse(is.null(cpue_lower), 0, cpue_lower)
    cpue_upper <- ifelse(is.null(cpue_upper), 100, cpue_upper)
    
    daily_cpue <- filtered_data %>%
      group_by(DOY) %>%
      summarise(mean_cpue = mean(dailyCPUEprop, na.rm = TRUE), .groups = 'drop') %>%
      mutate(cpue_percentile = rank(mean_cpue) / n() * 100)
    
    target_doys <- daily_cpue$DOY[daily_cpue$cpue_percentile >= cpue_lower & 
                                    daily_cpue$cpue_percentile <= cpue_upper]
    filtered_data <- filtered_data %>% filter(DOY %in% target_doys)
    filter_description <- paste0("CPUE percentile: ", cpue_lower, "-", cpue_upper, "%")
  }
  
  # Date range filter
  if (filter_type %in% c("date_range", "both")) {
    date_parts <- c()
    if (!is.null(date_start)) {
      filtered_data <- filtered_data %>% filter(DOY >= date_start)
      date_parts <- c(date_parts, paste0("DOY >= ", date_start))
    }
    if (!is.null(date_end)) {
      filtered_data <- filtered_data %>% filter(DOY <= date_end)
      date_parts <- c(date_parts, paste0("DOY <= ", date_end))
    }
    if (filter_type == "both") {
      filter_description <- paste(filter_description, "&", paste(date_parts, collapse = " & "))
    } else {
      filter_description <- paste(date_parts, collapse = " & ")
    }
  }
  
  # Store metadata as attributes
  attr(filtered_data, "original_n") <- original_n
  attr(filtered_data, "filtered_n") <- nrow(filtered_data)
  attr(filtered_data, "percent_retained") <- round(nrow(filtered_data) / original_n * 100, 1)
  attr(filtered_data, "cpue_retained") <- round(sum(filtered_data$COratio, na.rm = TRUE) / original_cpue * 100, 1)
  attr(filtered_data, "filter_description") <- filter_description
  
  return(filtered_data)
}

# ==============================================================================
# FUNCTION 1: KUSKOKWIM ANALYSIS
# ==============================================================================

run_kusko_analysis <- function(year,
                               filter_type = "none",
                               cpue_lower = NULL,
                               cpue_upper = NULL,
                               date_start = NULL,
                               date_end = NULL,
                               verbose = TRUE) {
  
  # Parameters
  min_stream_order <- 3
  min_error <- 0.00057
  max_error <- 0.00089
  sensitivity_threshold <- 0.7
  
  if (verbose) cat(paste("\n=== Processing Kusko", year, "===\n"))
  
  # Load spatial data
  edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
  basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
  edges <- st_transform(edges, st_crs(basin))
  
  if (verbose) cat(paste("  Loaded", nrow(edges), "stream segments\n"))
  
  # Load and filter natal data
  natal_data_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(year, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  )
  natal_data_clean <- filter(natal_data_raw, !is.na(natal_iso), !is.na(dailyCPUEprop))
  natal_data <- apply_filters(natal_data_clean, filter_type, cpue_lower, cpue_upper, date_start, date_end)
  
  if (verbose) {
    cat(paste("  Initial observations:", nrow(natal_data_clean), "\n"))
    cat(paste("  Filter:", attr(natal_data, "filter_description"), "\n"))
    cat(paste("  Retained:", attr(natal_data, "filtered_n"), "(", attr(natal_data, "percent_retained"), "%)\n"))
  }
  
  if (nrow(natal_data) == 0) stop("No data remaining after filtering!")
  
  # Calculate error
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- pmax(pmin(pid_isose, max_error), min_error)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # Setup priors
  StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
  PresencePrior <- ifelse((edges$Str_Order %in% c(6,7)) & edges$SPAWNING_C == 0, 0, 1)
  NewHabitatPrior <- ifelse(edges$Avg_Slop_1 > 2.5, 0, 1)
  pid_prior <- edges$UniPh2oNoE
  
  # Bayesian assignment
  if (verbose) cat("  Performing Bayesian assignment...\n")
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      StreamOrderPrior * PresencePrior * pid_prior * NewHabitatPrior
    
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
    runsize <- as.numeric(runsizedat$Total_Run[runsizedat$River == "Kusko" & runsizedat$Year == year])
    if (filter_type == "cpue_50_cutoff") runsize <- runsize / 2
    basin_assign_individuals <- basin_assign_rescale * runsize
  } else {
    basin_assign_rescale <- basin_assign_norm <- basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  if (verbose) {
    cat(paste("  Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
  }
  
  # Export CSV
  dir.create(PATHS$output_kusko, recursive = TRUE, showWarnings = FALSE)
  
  filename_base <- switch(filter_type,
                          "cpue_50_cutoff" = paste0("CPUE50pct_", year, "_Kusko_Assignment_Results"),
                          "cpue_percentile" = paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_", year, "_Kusko_Assignment_Results"),
                          "date_range" = paste0("DOY", date_start, "-", date_end, "_", year, "_Kusko_Assignment_Results"),
                          "both" = paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_DOY", date_start, "-", date_end, "_", year, "_Kusko_Assignment_Results"),
                          paste0(year, "_Kusko_Assignment_Results")
  )
  
  output_data <- data.frame(
    reachid = st_drop_geometry(edges)$reachid,
    Str_Order = st_drop_geometry(edges)$Str_Order,
    iso_pred = st_drop_geometry(edges)$iso_pred,
    assignment_sum = basin_assign_sum,
    assignment_rescale = basin_assign_rescale,
    assignment_norm = basin_assign_norm,
    assignment_individuals = basin_assign_individuals
  )
  
  filepath <- file.path(PATHS$output_kusko, paste0(filename_base, ".csv"))
  write_csv(output_data, filepath)
  if (verbose) cat(paste("  ✓ Exported:", filepath, "\n"))
  
  return(list(
    edges = edges,
    basin = basin,
    results = output_data,
    natal_data = natal_data,
    filter_metadata = list(
      filter_type = filter_type,
      cpue_lower = cpue_lower,
      cpue_upper = cpue_upper,
      date_start = date_start,
      date_end = date_end
    )
  ))
}

# ==============================================================================
# FUNCTION 2: YUKON ANALYSIS
# ==============================================================================

run_yukon_analysis <- function(year,
                               filter_type = "none",
                               cpue_lower = NULL,
                               cpue_upper = NULL,
                               date_start = NULL,
                               date_end = NULL,
                               verbose = TRUE) {
  
  # Parameters
  min_stream_order <- 4
  min_error <- 0.0035
  sensitivity_threshold <- 0.0
  
  if (verbose) cat(paste("\n=== Processing Yukon", year, "===\n"))
  
  # Load spatial data
  edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
  basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
  edges <- st_transform(edges, st_crs(basin))
  
  # Load genetic region data
  ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
  my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
  
  edges$GenLMU <- "none"
  edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
  
  LYsites <- which(edges$GenLMU == "lower")
  MYsites <- which(edges$GenLMU == "middle")
  
  if (verbose) cat(paste("  Loaded", nrow(edges), "stream segments\n"))
  
  # Load and filter natal data
  natal_data_raw <- read_csv(
    file.path(PATHS$natal_data_dir, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  )
  natal_data_clean <- filter(natal_data_raw, !is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
  natal_data <- apply_filters(natal_data_clean, filter_type, cpue_lower, cpue_upper, date_start, date_end)
  
  if (verbose) {
    cat(paste("  Initial observations:", nrow(natal_data_clean), "\n"))
    cat(paste("  Filter:", attr(natal_data, "filter_description"), "\n"))
    cat(paste("  Retained:", attr(natal_data, "filtered_n"), "(", attr(natal_data, "percent_retained"), "%)\n"))
  }
  
  if (nrow(natal_data) == 0) stop("No data remaining after filtering!")
  
  # Calculate error
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # Setup priors
  StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
  PresencePrior <- ifelse((edges$Str_Order %in% c(7,8,9)) & edges$SPAWNING_C == 0, 0, 1)
  
  # Bayesian assignment
  if (verbose) cat("  Performing Bayesian assignment...\n")
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(0, nrow = n_basins, ncol = n_fish)
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    gen_prior <- rep(0, length(pid_iso))
    gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
    gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])
    
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      StreamOrderPrior * gen_prior * PresencePrior
    
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
    if (filter_type == "cpue_50_cutoff") runsize <- runsize / 2
    basin_assign_individuals <- basin_assign_rescale * runsize
  } else {
    basin_assign_rescale <- basin_assign_norm <- basin_assign_individuals <- rep(0, length(basin_assign_sum))
  }
  
  if (verbose) {
    cat(paste("  Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(edges), "\n"))
  }
  
  # Export CSV
  dir.create(PATHS$output_yukon, recursive = TRUE, showWarnings = FALSE)
  
  filename_base <- switch(filter_type,
                          "cpue_50_cutoff" = paste0("CPUE50pct_", year, "_Yukon_Assignment_Results"),
                          "cpue_percentile" = paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_", year, "_Yukon_Assignment_Results"),
                          "date_range" = paste0("DOY", date_start, "-", date_end, "_", year, "_Yukon_Assignment_Results"),
                          "both" = paste0("CPUE", cpue_lower, "-", cpue_upper, "pct_DOY", date_start, "-", date_end, "_", year, "_Yukon_Assignment_Results"),
                          paste0(year, "_Yukon_Assignment_Results")
  )
  
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
  
  filepath <- file.path(PATHS$output_yukon, paste0(filename_base, ".csv"))
  write_csv(output_data, filepath)
  if (verbose) cat(paste("  ✓ Exported:", filepath, "\n"))
  
  return(list(
    edges = edges,
    basin = basin,
    results = output_data,
    natal_data = natal_data,
    filter_metadata = list(
      filter_type = filter_type,
      cpue_lower = cpue_lower,
      cpue_upper = cpue_upper,
      date_start = date_start,
      date_end = date_end
    )
  ))
}

# ==============================================================================
# FUNCTION 3: CREATE MAP (unified color scheme, watershed-specific line widths)
# ==============================================================================

create_map <- function(analysis_results,
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
  basin_assign_norm <- analysis_results$results$assignment_norm
  
  # --------------------------------------------------------------------------
  # UNIFIED COLOR SCHEME (same for both watersheds)
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
  # WATERSHED-SPECIFIC LINE WIDTHS
  # --------------------------------------------------------------------------
  
  stream_order <- edges$Str_Order
  stream_order[is.na(stream_order)] <- 1
  
  if (watershed == "Yukon") {
    linewidths <- ifelse(stream_order >= 9, 3.7,
                         ifelse(stream_order >= 8, 5,
                                ifelse(stream_order >= 7, 2.0,
                                       ifelse(stream_order >= 6, 1.5,
                                              ifelse(stream_order >= 5, 1.4,
                                                     ifelse(stream_order >= 4, 1.0, 0))))))
  } else {
    linewidths <- ifelse(stream_order >= 9, 5,
                         ifelse(stream_order >= 8, 6,
                                ifelse(stream_order >= 7, 5,
                                       ifelse(stream_order >= 6, 3.0,
                                              ifelse(stream_order >= 5, 2.7,
                                                     ifelse(stream_order >= 4, 2.7,
                                                            ifelse(stream_order >= 3, 1.2, 0)))))))
  }
  
  # Emphasize high production areas
  linewidths[basin_assign_norm > 0.8] <- linewidths[basin_assign_norm > 0.8] * 1.5
  
  # --------------------------------------------------------------------------
  # OUTPUT PATH
  # --------------------------------------------------------------------------
  
  scenario_dir <- switch(filter_type,
                         "cpue_50_cutoff" = "Half_Year",
                         "cpue_percentile" = paste0("CPUE_", cpue_lower, "-", cpue_upper, "pct"),
                         "date_range" = paste0("DOY_", date_start, "-", date_end),
                         "both" = paste0("CPUE_", cpue_lower, "-", cpue_upper, "pct_DOY_", date_start, "-", date_end),
                         "Full_Year"
  )
  
  output_dir <- file.path(base_output_dir, "Production", scenario_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  map_filename <- file.path(output_dir, paste0(year, "_", watershed, "_Annual_Production.png"))
  
  # --------------------------------------------------------------------------
  # CREATE PNG
  # --------------------------------------------------------------------------
  
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = paste0("Annual Production\nYear: ", year, " River: ", watershed), bg = "white")
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
cat("  - run_kusko_analysis(year, filter_type, ...)\n")
cat("  - run_yukon_analysis(year, filter_type, ...)\n")
cat("  - create_map(results, base_output_dir, year, watershed, filter_type, ...)\n\n")

# Kuskokwim full year
for (year in c(2017, 2018, 2019, 2020, 2021, 2022)) {
  tryCatch({
    results <- run_kusko_analysis(year)
    create_map(results, BASE_KUSKO_DIR, year, "Kusko")
  }, error = function(e) cat("ERROR Kusko", year, ":", e$message, "\n"))
}

# Yukon full year
for (year in c(2015, 2016, 2018, 2021)) {
  tryCatch({
    results <- run_yukon_analysis(year)
    create_map(results, BASE_YUKON_DIR, year, "Yukon")
  }, error = function(e) cat("ERROR Yukon", year, ":", e$message, "\n"))
}