# DOY (Day of Year) Quartile Analysis Script
# This script divides data into quartiles based on day of year

library(sf)             # For spatial data handling
library(dplyr)          # For data manipulation
library(ggplot2)        # For plotting
library(RColorBrewer)   # For color palettes
library(scales)         # For rescaling values
library(here)           # For path management
library(grid)           # For viewport and grid graphics
library(gridExtra)      # For arranging multiple plots
library(tidyverse)
library(tidyr)
library(viridis)        # For improved color scales

# Source the utility functions
source(here("Code/Map_Utils.R"))

DOY_Quartile_Analysis <- function(year, sensitivity_threshold, min_error, 
                                  min_stream_order = 3, 
                                  HUC = 8, 
                                  return_values = FALSE) {
  
  # Generate identifier for output files
  identifier <- paste(year, watershed, sep = "_")
  
  # Load shapefiles based on watershed type
  if (watershed == "Kusko") {
    edges <- st_read("/Users/benjaminmakhlouf/Spatial Data/USGS Added/KuskoUSGS.shp")
    basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  } else if (watershed == "Yukon") {
    edges <- st_read("/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp") 
    basin <- st_read("/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp")
  }
  
  edges <- st_transform(edges, st_crs(basin))
  edges <- edges[edges$Str_Order >= min_stream_order,] # Filter the stream network
  
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC", HUC, "_Trimmed.shp"))
  Huc <- st_transform(Huc, st_crs(basin))
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/", 
                                   year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv"))
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso) & !is.na(Natal_Origins$dailyCPUEprop),] # Clean data
  
  # Split data into DOY quartiles
  full_doy_range <- range(Natal_Origins$DOY, na.rm = TRUE)
  doy_breaks <- seq(full_doy_range[1], full_doy_range[2], length.out = 5)  # 5 points makes 4 intervals
  
  # Create 4 quartile subsets based on DOY ranges
  quartile_subsets <- list()
  for (i in 1:4) {
    quartile_subsets[[i]] <- Natal_Origins %>% 
      filter(DOY >= doy_breaks[i] & DOY < doy_breaks[i+1])
  }
  
  # Create labels for the quartiles
  subset_labels <- sapply(1:4, function(i) {
    sprintf("DOY Q%d: %d-%d", i, ceiling(doy_breaks[i]), floor(doy_breaks[i+1]))
  })
  
  # Create visualization of the DOY splits
  doy_plot <- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) +
    geom_line(color = "black", linewidth = 1) +
    geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), fill = "grey70", alpha = 0.5) +
    geom_vline(xintercept = doy_breaks, linetype = "dashed", color = "red") +
    labs(title = "Run Timing Split by DOY Quartiles",
         x = "Day of Year",
         y = "Daily CPUE Proportion") +
    theme_minimal()
  
  # Save the visualization
  dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
  ggsave(paste0(here("Basin Maps/Quartile_Splits/"), identifier, "_DOY_quartiles.pdf"), 
         doy_plot, width = 8, height = 5)
  
  # Initialize common extraction variables
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  
  # Set up watershed-specific variables
  if (watershed == "Kusko") {
    pid_prior <- edges$UniPh2oNoE
    StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    PresencePrior <- ifelse((edges$Str_Order %in% c(6, 7, 8)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior<- ifelse(edges$Spawner_IP == 0, 0, 1)
    
  } else if (watershed == "Yukon") {
    pid_prior <- edges$PriorSl2
    StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    
    # Load Yukon-specific genetic groups
    ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
    ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries
    my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
    my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries
    uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
    uy.gen_reachid <- uy.gen$reachid # reach ids of the upper Yukon tributaries
    
    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen_reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen_reachid] <- "middle"
    edges$GenLMU[edges$reachid %in% uy.gen_reachid] <- "upper"
    LYsites <- which(edges$GenLMU == "lower") # Create a vector of the INDICES associated with each genetic region
    MYsites <- which(edges$GenLMU == "middle")
    UYsites <- which(edges$GenLMU == "upper")
    
    PresencePrior <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior<- ifelse(edges$Spawner_IP == 0, 0, 1)
  }
  
  # Calculate error values (same for both watersheds)
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  
  # Return values storage
  if (return_values) {
    all_results <- list()
  }
  
  # Create output directories
  dir.create(here("Basin Maps/DOY_Quartile/HUC"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/DOY_Quartile/Tribs"), showWarnings = FALSE, recursive = TRUE)
  
  # Process each quartile subset
  for (q in 1:length(quartile_subsets)) {
    current_subset <- quartile_subsets[[q]]
    
    # Skip empty subsets
    if (nrow(current_subset) == 0) {
      message(paste("Skipping", subset_labels[q], "because it contains no data"))
      next
    }
    
    # Create unique ID for this subset
    subset_id <- paste0(watershed, "_", year, "_DOY_Q", q)
    message(paste("Processing", subset_labels[q], "with", nrow(current_subset), "data points"))
    
    # Initialize assignment matrix for this subset
    assignment_matrix <- matrix(NA, nrow = length(pid_iso), ncol = nrow(current_subset))
    
    # Process each data point in the subset
    for (i in 1:nrow(current_subset)) {
      iso_o <- as.numeric(current_subset$natal_iso[i])
      
      if (watershed == "Kusko") {
        # Kusko assignment
        assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * 
          pid_prior * StreamOrderPrior * PresencePrior * NewHabitatPrior
        
      } else if (watershed == "Yukon") {
        # Yukon assignment with genetic priors
        gen.prior <- rep(0, length = length(pid_iso))
        if("Lower" %in% names(current_subset)) {
          gen.prior[LYsites] <- as.numeric(current_subset$Lower[i])
          gen.prior[MYsites] <- as.numeric(current_subset$Middle[i])
          gen.prior[UYsites] <- as.numeric(current_subset$Upper[i])
        }
        
        assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * 
          pid_prior * StreamOrderPrior * PresencePrior * NewHabitatPrior * gen.prior
      }
      
      # Normalize and threshold
      assign_norm <- assign / sum(assign)
      assign_rescaled <- assign_norm / max(assign_norm)
      assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
      
      # Weight by CPUE
      assignment_matrix[,i] <- assign_rescaled * as.numeric(current_subset$COratio[i])
    }
    
    # Calculate basin-scale values
    basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
    basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum)
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale)
    
    # Create improved histogram using create_cpue_histogram utility function
    gg_hist <- create_cpue_histogram(Natal_Origins, current_subset)
    
    # Process HUC data using process_huc_data utility function
    final_result <- process_huc_data(edges, basin, Huc, basin_assign_rescale, HUC)
    
    # Define filepath for HUC map
    huc_filepath <- file.path(here("Basin Maps/DOY_Quartile/HUC"), 
                              paste0(subset_id, "_HUC", HUC, "_.pdf"))
    
    # Create HUC map using the utility function
    create_huc_map(
      final_result = final_result,
      basin_assign_norm = basin_assign_norm,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      HUC = HUC,
      subset_label = subset_labels[q],
      output_filepath = huc_filepath
    )
    
    # Define filepath for tributary map
    trib_filepath <- file.path(here("Basin Maps/DOY_Quartile/Tribs"), 
                               paste0(subset_id, "_.pdf"))
    
    # Create tributary map using the utility function
    create_tributary_map(
      basin = basin,
      edges = edges,
      basin_assign_norm = basin_assign_norm,
      StreamOrderPrior = StreamOrderPrior,
      pid_prior = pid_prior,
      gg_hist = gg_hist,
      year = year,
      watershed = watershed,
      sensitivity_threshold = sensitivity_threshold,
      min_stream_order = min_stream_order,
      min_error = min_error,
      subset_label = subset_labels[q],
      output_filepath = trib_filepath
    )
    
    # Store results if needed
    if (return_values) {
      all_results[[q]] <- list(
        subset = current_subset,
        label = subset_labels[q],
        basin_assign_rescale = basin_assign_rescale,
        basin_assign_norm = basin_assign_norm,
        huc_result = final_result
      )
    }
  }
  
  if (return_values) {
    return(all_results)
  } else {
    return(invisible(NULL))
  }
}

# Function to create CPUE histogram
create_cpue_histogram <- function(full_dataset, current_subset) {
  # Create a custom color for highlighting the subset
  highlight_color <- "tomato"
  background_color <- "gray70"
  
  # Create the improved histogram
  ggplot() + 
    # First plot the full dataset as background with reduced opacity
    geom_line(data = full_dataset, aes(x = DOY, y = dailyCPUEprop), 
              color = "gray40", linewidth = 1, alpha = 0.5) +
    geom_ribbon(data = full_dataset, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = background_color, alpha = 0.3) +
    
    # Then overlay the current subset with higher opacity and different color
    geom_line(data = current_subset, aes(x = DOY, y = dailyCPUEprop), 
              color = "black", linewidth = 2) +
    geom_ribbon(data = current_subset, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = highlight_color, alpha = 0.7) +
    
    # Add vertical lines to show the subset boundaries
    geom_vline(xintercept = c(
      min(current_subset$DOY), 
      max(current_subset$DOY)
    ), linetype = "dashed", color = "darkred") +
    
    # Add labels and theme
    labs(
      title = "CPUE Distribution",
      subtitle = "Current subset highlighted in red",
      x = "Day of Year", 
      y = "Daily CPUE Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    )
}