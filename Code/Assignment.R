################################################################################
# CONSOLIDATED SALMON ASSIGNMENT ANALYSIS
################################################################################

library(sf); library(dplyr); library(readr)

#------------------------------------------------------------------------------
# CONFIGURATION
#------------------------------------------------------------------------------
PATHS <- list(
  kusko_edges = "/Users/benjaminmakhlouf/Spatial Data/KuskoUSGS_HUC.shp",
  kusko_basin = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  yukon_edges = "/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp",
  yukon_basin = "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp",
  yukon_ly_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
  yukon_my_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
  yukon_uy_gen = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp",
  natal_data_dir = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE",
  output_kusko = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko",
  output_yukon = "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon"
)

PARAMS <- list(
  Kusko = list(min_stream_order = 3, min_error = 0.0006, sensitivity_threshold = 0.6),
  Yukon = list(min_stream_order = 4, min_error = 0.003, sensitivity_threshold = 0.6)
)

#------------------------------------------------------------------------------
# MAIN FUNCTION
#------------------------------------------------------------------------------
run_annual_analysis <- function(year, watershed) {
  cat(paste("\n=== Processing", watershed, year, "===\n"))
  
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
  natal_data <- read_csv(file.path(PATHS$natal_data_dir, paste0(year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv")), show_col_types = FALSE)
  natal_data <- if (watershed == "Yukon") {
    filter(natal_data, !is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop), DOY >= 160, DOY <= 183)
  } else {
    filter(natal_data, !is.na(natal_iso), !is.na(dailyCPUEprop))
  }
  cat(paste("  Loaded", nrow(natal_data), "fish,", nrow(edges), "segments\n"))
  
  # 3. CALCULATE ERROR
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < params$min_error, params$min_error, pid_isose)
  error <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
  
  # 4. SETUP PRIORS
  StreamOrderPrior <- ifelse(edges$Str_Order >= params$min_stream_order, 1, 0)
  
  if (watershed == "Kusko") {
    pid_prior <- edges$UniPh2oNoE
    PresencePrior <- ifelse((edges$Str_Order %in% c(6,7,8)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
  } else {
    pid_prior <- edges$PriorSl2
    PresencePrior <- ifelse((edges$Str_Order %in% c(7,8,9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior <- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    ly.gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    my.gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
    uy.gen <- st_read(PATHS$yukon_uy_gen, quiet = TRUE)
    
    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
    edges$GenLMU[edges$reachid %in% uy.gen$reachid] <- "upper"
    
    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
    UYsites <- which(edges$GenLMU == "upper")
  }
  
  # 5. BAYESIAN ASSIGNMENT
  cat("  Performing Bayesian assignment...\n")
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
      gen_prior[UYsites] <- as.numeric(natal_data$Upper[i])
      
      assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
        pid_prior * StreamOrderPrior * gen_prior #* PresencePrior * NewHabitatPrior
    }
    
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < params$sensitivity_threshold] <- 0
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # 6. PROCESS RESULTS
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum, na.rm = TRUE)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
  
  cat(paste("  Total production:", round(sum(basin_assign_sum), 2), "\n"))
  
  # 7. EXPORT TO CSV
  output_dir <- if (watershed == "Kusko") PATHS$output_kusko else PATHS$output_yukon
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
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
  
  filepath <- file.path(output_dir, paste0(year, "_", watershed, "_Assignment_Results.csv"))
  write_csv(output_data, filepath)
  
  cat(paste("  ✓ Exported:", filepath, "\n"))
  cat(paste("  ✓ Segments with assignment > 0:", sum(basin_assign_sum > 0), "/", nrow(output_data), "\n"))
  
  return(list(edges = edges, basin = basin, results = output_data, natal_data = natal_data))
}


