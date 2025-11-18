################################################################################
# YUKON ANNUAL TRIBUTARY MAPS
# Standalone script to create annual production maps for Yukon watershed
# No external functions - complete self-contained analysis
################################################################################

# Load required libraries
library(sf)
library(dplyr)
library(RColorBrewer)
library(grid)

################################################################################
# CONFIGURATION - UPDATE PATHS FOR YOUR SYSTEM
################################################################################

# File paths - UPDATE THESE FOR YOUR SYSTEM
YUKON_EDGES <- "/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp"
YUKON_BASIN <- "/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp"
DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Maps/Yukon_Annual"

# Yukon genetic data paths (required for Yukon assignments)
YUKON_LY_GEN <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"
YUKON_MY_GEN <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp"
YUKON_UY_GEN <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp"

# Analysis parameters
YEARS <- c(2015,2016,2017,2018,2019,2021)
MIN_STREAM_ORDER <- 4
MIN_ERROR <- 0.003
SENSITIVITY_THRESHOLD <- 0.7

# Create output directory
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# LOAD SPATIAL DATA ONCE
################################################################################

cat("Loading Yukon spatial data...\n")

# Load spatial data
edges <- st_read(YUKON_EDGES, quiet = TRUE)
basin <- st_read(YUKON_BASIN, quiet = TRUE)

# Transform CRS and filter by stream order
edges <- st_transform(edges, st_crs(basin))
edges <- edges[edges$Str_Order >= MIN_STREAM_ORDER, ]

# Extract isoscape values (same for all years)
pid_iso <- edges$iso_pred
pid_isose <- edges$isose_pred

# Calculate error values (same for all years)
pid_isose_mod <- ifelse(pid_isose < MIN_ERROR, MIN_ERROR, pid_isose)
within_site <- 0.0003133684 / 1.96
analyt <- 0.00011 / 2
error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)

# Set up priors (same for all years)
StreamOrderPrior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
pid_prior <- edges$PriorSl2
#PresencePrior <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)

# Load Yukon genetic groups
ly.gen <- st_read(YUKON_LY_GEN, quiet = TRUE)
my.gen <- st_read(YUKON_MY_GEN, quiet = TRUE)
uy.gen <- st_read(YUKON_UY_GEN, quiet = TRUE)

# Create genetic management unit assignments
edges$GenLMU <- 0
edges$GenLMU[edges$reachid %in% ly.gen$reachid] <- "lower"
edges$GenLMU[edges$reachid %in% my.gen$reachid] <- "middle"
edges$GenLMU[edges$reachid %in% uy.gen$reachid] <- "upper"

# Find sites for each genetic group
LYsites <- which(edges$GenLMU == "lower")
MYsites <- which(edges$GenLMU == "middle")
UYsites <- which(edges$GenLMU == "upper")

################################################################################
# MAIN ANALYSIS LOOP
################################################################################

for (year in YEARS) {
  cat("\n=== Processing", year, "===\n")
  
  # Check if data file exists
  data_file <- file.path(DATA_DIR, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  if (!file.exists(data_file)) {
    cat("Data file not found:", basename(data_file), "- skipping\n")
    next
  }
  
  # Load natal data (Yukon requires Lower, Middle, Upper genetic columns)
  natal_data <- read.csv(data_file)
  natal_data <- natal_data[!is.na(natal_data$Lower) & 
                             !is.na(natal_data$natal_iso) & 
                             !is.na(natal_data$dailyCPUEprop), ]
  
  cat("Loaded", nrow(natal_data), "fish observations\n")
  
  if (nrow(natal_data) == 0) {
    cat("No valid data - skipping\n")
    next
  }
  
  #----------------------------------------------------------------------------
  # BAYESIAN ASSIGNMENT
  #----------------------------------------------------------------------------
  
  n_basins <- nrow(edges)
  n_fish <- nrow(natal_data)
  assignment_matrix <- matrix(NA, nrow = n_basins, ncol = n_fish)
  
  cat("Performing Bayesian assignment for", n_fish, "fish...\n")
  
  for (i in 1:n_fish) {
    fish_iso <- natal_data$natal_iso[i]
    
    # Set up genetic priors for this fish
    gen_prior <- rep(0, length = length(pid_iso))
    gen_prior[LYsites] <- as.numeric(natal_data$Lower[i])
    gen_prior[MYsites] <- as.numeric(natal_data$Middle[i])
    gen_prior[UYsites] <- as.numeric(natal_data$Upper[i])
    
    # Calculate assignment probabilities (Yukon includes genetic priors)
    assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(fish_iso - pid_iso)^2/(2*error^2)) * 
      pid_prior * StreamOrderPrior * gen_prior # PresencePrior
    
    # Normalize and threshold
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < SENSITIVITY_THRESHOLD] <- 0
    
    # Weight by CPUE
    assignment_matrix[,i] <- assign_rescaled * as.numeric(natal_data$COratio[i])
  }
  
  # Sum across all fish
  basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
  basin_assign_norm <- basin_assign_sum / max(basin_assign_sum, na.rm = TRUE)
  
  
  #----------------------------------------------------------------------------
  # CREATE MAP
  #----------------------------------------------------------------------------
  
  cat("Creating tributary map...\n")
  
  map_filename <- file.path(OUTPUT_DIR, paste0("Yukon_Annual_", year, ".png"))
  
  # Open PNG file
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  # Color coding (Yukon style - 0.2 intervals)
  palette_colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)
  colcode <- rep("gray60", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'white'
  colcode[basin_assign_norm > 0 & basin_assign_norm <= 0.2] <- palette_colors[2]
  colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.4] <- palette_colors[4]
  colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.6] <- palette_colors[5]
  colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette_colors[7]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_colors[8]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_colors[9]
  colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette_colors[10]
  
  # Line widths (Yukon style - more conservative)
  stream_order <- edges$Str_Order
  linewidths <- rep(1, length(stream_order))
  linewidths <- ifelse(stream_order == 9, 3.7, linewidths)
  linewidths <- ifelse(stream_order == 8, 2.5, linewidths)
  linewidths <- ifelse(stream_order == 7, 1.7, linewidths)
  linewidths <- ifelse(stream_order == 6, 1.5, linewidths)
  linewidths <- ifelse(stream_order == 5, 1, linewidths)
  linewidths <- ifelse(stream_order == 4, 1, linewidths)
  
  # Plot
  par(mar = c(8, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = paste("Yukon Annual Production", year), bg = "white")
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add legend (Yukon style - custom bins)
  legend("topleft", 
         legend = c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.7", "0.7-0.8", 
                    "0.8-0.9", "0.9-1.0"), 
         col = c(palette_colors[1], palette_colors[4], palette_colors[5], 
                 palette_colors[7], palette_colors[8], palette_colors[9], 
                 palette_colors[10]), 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n",
         bg = "white")
  
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  
  cat("Saved:", basename(map_filename), "\n")
  
  #----------------------------------------------------------------------------
  # EXPORT PRODUCTION DATA
  #----------------------------------------------------------------------------
  
  # Create tributary production data
  coords <- st_coordinates(st_centroid(edges))
  tributary_data <- data.frame(
    year = year,
    tributary_id = 1:length(basin_assign_sum),
    longitude = coords[,1],
    latitude = coords[,2],
    stream_order = edges$Str_Order,
    genetic_group = edges$GenLMU,
    raw_production = basin_assign_sum,
    normalized_production = basin_assign_norm,
    production_proportion = basin_assign_sum / sum(basin_assign_sum),
    stringsAsFactors = FALSE
  )
  
  # Sort by production
  tributary_data <- tributary_data[order(tributary_data$raw_production, decreasing = TRUE), ]
  
  # Save CSV
  csv_filename <- file.path(OUTPUT_DIR, paste0("Yukon_Production_", year, ".csv"))
  write.csv(tributary_data, csv_filename, row.names = FALSE)
  
  cat("Exported:", basename(csv_filename), "\n")
}

################################################################################
# SUMMARY
################################################################################

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("Files created:\n")
for (year in YEARS) {
  map_file <- file.path(OUTPUT_DIR, paste0("Yukon_Annual_", year, ".png"))
  csv_file <- file.path(OUTPUT_DIR, paste0("Yukon_Production_", year, ".csv"))
  if (file.exists(map_file)) cat("  ✓", basename(map_file), "\n")
  if (file.exists(csv_file)) cat("  ✓", basename(csv_file), "\n")
}
cat("Done!\n")