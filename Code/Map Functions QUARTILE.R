# Enhanced Quartile Mapping Function with Improved Visualization
# This function combines the best parts of ALL_Map_func_Quartile and the improved HUC polygon processing

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

ALL_Map_func_Quartile <- function(year, sensitivity_threshold, min_error, 
                                           min_stream_order = 3, 
                                           filter_by = c("DOY_Q", "CPUE_Q", "TOTAL_CPUE_Q", "CUSTOM"), 
                                           HUC = 8, 
                                           custom_breaks = NULL,
                                           return_values = FALSE) {
  
  # Generate identifier for output files
  identifier <- paste(year, watershed, sep = "_")
  
  ############# 
  ###### Load Shapefiles 
  #############
  
  if (watershed == "Kusko") {
    # Load Kuskokwim specific data
    edges <- st_read("/Users/benjaminmakhlouf/Spatial Data/USGS Added/KuskoUSGS.shp")
    basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  } else if (watershed == "Yukon") {
    # Load Yukon specific data
    edges <- st_read("/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp") 
    basin <- st_read("/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp")
  }
  
  edges <- st_transform(edges, st_crs(basin))
  edges <- edges[edges$Str_Order >= min_stream_order,] # Filter the stream network
  
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC", HUC, "_Trimmed.shp"))
  Huc <- st_transform(Huc, st_crs(basin))
  
  #############
  ##### Natal Origin Data
  #############
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/", year, "_", watershed, "_Natal_Origins_Genetics_CPUE.csv"))
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso) & !is.na(Natal_Origins$dailyCPUEprop),] # Clean data
  
  ### Split data into quartiles based on the filter type 
  
  # Initialize filter variables
  quartile_subsets <- list()
  subset_labels <- character(0)
  
  ## DOY (Day of Year) Processing
  if (filter_by == "DOY_Q") {
    # Split DOY into 4 equal time chunks (quartiles)
    full_doy_range <- range(Natal_Origins$DOY, na.rm = TRUE)
    doy_breaks <- seq(full_doy_range[1], full_doy_range[2], length.out = 5)  # 5 points makes 4 intervals
    
    # Create 4 quartile subsets based on DOY ranges
    quartile_subsets <- lapply(1:4, function(i) {
      Natal_Origins %>% 
        filter(DOY >= doy_breaks[i] & DOY < doy_breaks[i+1])
    })
    
    subset_labels <- sapply(1:4, function(i) {
      sprintf("DOY Q%d: %d-%d", i, ceiling(doy_breaks[i]), floor(doy_breaks[i+1]))
    })
    
    # Create visualization of the splits
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
    
  } else if (filter_by == "CPUE_Q") {
    # Calculate cumulative CPUE proportions to split by run timing
    Natal_Origins <- Natal_Origins %>% 
      arrange(DOY) %>% 
      mutate(
        CumCPUE = cumsum(dailyCPUEprop),
        TotalCPUE = sum(dailyCPUEprop),
        CumProp = CumCPUE / TotalCPUE  # Cumulative proportion of run
      )
    
    # Find DOY values at 25%, 50%, and 75% of total run
    cpue_breaks <- sapply(c(0.25, 0.5, 0.75), function(p) {
      Natal_Origins$DOY[which.min(abs(Natal_Origins$CumProp - p))]
    })
    
    # Create 4 quartile subsets based on CPUE proportions
    quartile_subsets <- list(
      # Q1: Start to 25% of run
      Natal_Origins %>% filter(DOY <= cpue_breaks[1]),
      
      # Q2: 25% to 50% of run
      Natal_Origins %>% filter(DOY > cpue_breaks[1] & DOY <= cpue_breaks[2]),
      
      # Q3: 50% to 75% of run
      Natal_Origins %>% filter(DOY > cpue_breaks[2] & DOY <= cpue_breaks[3]),
      
      # Q4: 75% to end of run
      Natal_Origins %>% filter(DOY > cpue_breaks[3])
    )
    
    subset_labels <- c(
      sprintf("CPUE Q1: Early Run (DOY ≤ %d)", floor(cpue_breaks[1])),
      sprintf("CPUE Q2: Early-Mid Run (DOY %d-%d)", ceiling(cpue_breaks[1]), floor(cpue_breaks[2])),
      sprintf("CPUE Q3: Mid-Late Run (DOY %d-%d)", ceiling(cpue_breaks[2]), floor(cpue_breaks[3])),
      sprintf("CPUE Q4: Late Run (DOY > %d)", ceiling(cpue_breaks[3]))
    )
    
    # Create visualization of the splits
    cpue_plot <- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) +
      geom_line(color = "black", linewidth = 1) +
      geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), fill = "grey70", alpha = 0.5) +
      geom_vline(xintercept = c(min(Natal_Origins$DOY), cpue_breaks, max(Natal_Origins$DOY)), 
                 linetype = "dashed", color = "red") +
      labs(title = "Run Timing Split by Cumulative CPUE Quartiles",
           x = "Day of Year",
           y = "Daily CPUE Proportion") +
      theme_minimal()
    
    # Save the visualization
    dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
    ggsave(paste0(here("Basin Maps/Quartile_Splits/"), identifier, "_CPUE_quartiles.pdf"), 
           cpue_plot, width = 8, height = 5)
  } else if (filter_by == "TOTAL_CPUE_Q") {
    # Sort by CPUE value and calculate total
    total_cpue <- sum(Natal_Origins$dailyCPUEprop)
    
    # Order by CPUE value (not cumulative)
    ordered_data <- Natal_Origins %>% 
      arrange(desc(dailyCPUEprop))
    
    # Calculate running sum of CPUE
    ordered_data$cumulative <- cumsum(ordered_data$dailyCPUEprop)
    ordered_data$cum_proportion <- ordered_data$cumulative / total_cpue
    
    # Find cutpoints for quartiles of total contribution
    q1_cutoff <- min(which(ordered_data$cum_proportion >= 0.25))
    q2_cutoff <- min(which(ordered_data$cum_proportion >= 0.50))
    q3_cutoff <- min(which(ordered_data$cum_proportion >= 0.75))
    
    # Extract the CPUE values at these cutoffs
    cpue_cutoffs <- c(
      ordered_data$dailyCPUEprop[q3_cutoff],  # Q1: Top 25% contributors
      ordered_data$dailyCPUEprop[q2_cutoff],  # Q2: Next 25% contributors
      ordered_data$dailyCPUEprop[q1_cutoff]   # Q3: Next 25% contributors
    )
    
    # Create the quartile subsets
    quartile_subsets[[1]] <- Natal_Origins %>% filter(dailyCPUEprop >= cpue_cutoffs[1])
    quartile_subsets[[2]] <- Natal_Origins %>% filter(dailyCPUEprop < cpue_cutoffs[1] & dailyCPUEprop >= cpue_cutoffs[2])
    quartile_subsets[[3]] <- Natal_Origins %>% filter(dailyCPUEprop < cpue_cutoffs[2] & dailyCPUEprop >= cpue_cutoffs[3])
    quartile_subsets[[4]] <- Natal_Origins %>% filter(dailyCPUEprop < cpue_cutoffs[3])
    
    subset_labels <- c(
      sprintf("Top 25%% CPUE (≥%.4f)", cpue_cutoffs[1]),
      sprintf("50-75%% CPUE (%.4f-%.4f)", cpue_cutoffs[2], cpue_cutoffs[1]),
      sprintf("25-50%% CPUE (%.4f-%.4f)", cpue_cutoffs[3], cpue_cutoffs[2]),
      sprintf("Bottom 25%% CPUE (<%.4f)", cpue_cutoffs[3])
    )
    
    # Create visualization
    cpue_dist_plot <- ggplot(Natal_Origins, aes(x = dailyCPUEprop)) +
      geom_histogram(bins = 30, fill = "grey70", color = "black") +
      geom_vline(xintercept = cpue_cutoffs, linetype = "dashed", color = "red") +
      labs(title = "Distribution of CPUE Values with Quartile Cutoffs",
           x = "Daily CPUE Proportion",
           y = "Count") +
      theme_minimal()
    
    # Save the plot
    dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
    ggsave(paste0(here("Basin Maps/Quartile_Splits/"), identifier, "_TOTAL_CPUE_quartiles.pdf"), 
           cpue_dist_plot, width = 8, height = 5)
  } else if (filter_by == "CUSTOM" && !is.null(custom_breaks)) {
    # Sort breaks to ensure they're in ascending order
    custom_breaks <- sort(custom_breaks)
    
    # Make sure breaks include min and max values
    if (min(custom_breaks) > min(Natal_Origins$DOY)) {
      custom_breaks <- c(min(Natal_Origins$DOY), custom_breaks)
    }
    if (max(custom_breaks) < max(Natal_Origins$DOY)) {
      custom_breaks <- c(custom_breaks, max(Natal_Origins$DOY))
    }
    
    # Create subsets based on custom breaks
    for (i in 1:(length(custom_breaks)-1)) {
      quartile_subsets[[i]] <- Natal_Origins %>% 
        filter(DOY >= custom_breaks[i] & DOY < custom_breaks[i+1])
      subset_labels[i] <- sprintf("Period %d: %d-%d", i, ceiling(custom_breaks[i]), floor(custom_breaks[i+1]))
    }
    
    # Create visualization of the custom splits
    custom_plot <- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) +
      geom_line(color = "black", linewidth = 1) +
      geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), fill = "grey70", alpha = 0.5) +
      geom_vline(xintercept = custom_breaks, linetype = "dashed", color = "red") +
      labs(title = "Run Timing with Custom Splits",
           x = "Day of Year",
           y = "Daily CPUE Proportion") +
      theme_minimal()
    
    # Save the plot
    dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
    ggsave(paste0(here("Basin Maps/Quartile_Splits/"), identifier, "_Custom_splits.pdf"), 
           custom_plot, width = 8, height = 5)
  } else {
    # Default case - no filtering
    quartile_subsets <- list(Natal_Origins)
    subset_labels <- c("Full Dataset")
  }
  
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
  dir.create(here("Basin Maps/Quartile_Maps/HUC"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/Quartile_Maps/Tribs"), showWarnings = FALSE, recursive = TRUE)
  
  # Process each quartile subset
  for (q in 1:length(quartile_subsets)) {
    current_subset <- quartile_subsets[[q]]
    
    # Skip empty subsets
    if (nrow(current_subset) == 0) {
      message(paste("Skipping", subset_labels[q], "because it contains no data"))
      next
    }
    
    # Create unique ID for this subset
    subset_id <- paste0(watershed, "_", year, "_", filter_by, q)
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
    
    # Create improved histogram that shows the subset in context of full data
    # First get the full dataset for context
    full_dataset <- Natal_Origins
    
    # Create a custom color for highlighting the subset
    highlight_color <- "tomato" # Can be changed to any color that stands out
    background_color <- "gray70"
    
    # Create the improved histogram
    gg_hist <- ggplot() + 
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
      
      # Add vertical lines to show the subset boundaries if using a range-based filter
      {if(filter_by == "DOY_Q") {
        geom_vline(xintercept = c(
          min(current_subset$DOY), 
          max(current_subset$DOY)
        ), linetype = "dashed", color = "darkred")
      } else if(filter_by == "CPUE_Q") {
        geom_vline(xintercept = c(
          min(current_subset$DOY), 
          max(current_subset$DOY)
        ), linetype = "dashed", color = "darkred")
      } else {
        # For other filter types, no vertical lines needed
        geom_blank()
      }} +
      
      # Add labels and theme
      labs(
        title = subset_labels[q],
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
    
    #========================================================================
    # HUC MAP - With improved processing from paste-2.txt
    #========================================================================
    
    huc_col <- paste0("HUC", HUC)  # Example: "HUC8" or "HUC10"
    name_col <- "Name"  # Assuming the HUC polygons contain a NAME column
    
    edges <- st_transform(edges, st_crs(Huc)) # Set projection to be the same 
    edges$basin_assign_rescale <- basin_assign_rescale  # Add rescaled values to the shapefile 
    basin <- st_transform(basin, st_crs(Huc))
    
    # Improved HUC filtering - use a more strict intersection criteria
    # First identify which HUCs are within the Basin with a minimum overlap threshold
    basin_buffer <- st_buffer(basin, dist = 0)  # Create a clean boundary without buffer
    hucs_in_basin <- Huc[st_intersects(Huc, basin_buffer, sparse = FALSE)[,1], ]
    
    # Calculate the area of intersection for each HUC with the basin
    intersection_areas <- st_intersection(hucs_in_basin, basin_buffer) %>%
      mutate(area = st_area(.)) %>%
      st_drop_geometry() %>%
      group_by(!!sym(huc_col)) %>%
      summarize(int_area = sum(area))
    
    # Get the original areas of the HUCs
    hucs_areas <- hucs_in_basin %>%
      mutate(total_area = st_area(.)) %>%
      st_drop_geometry() %>%
      select(!!sym(huc_col), total_area)
    
    # Calculate percentage of each HUC that intersects with the basin
    overlap_percentage <- intersection_areas %>%
      left_join(hucs_areas, by = huc_col) %>%
      mutate(pct_overlap = as.numeric(int_area / total_area))
    
    # Filter to only include HUCs with significant overlap (e.g., >10%)
    significant_hucs <- overlap_percentage %>%
      filter(pct_overlap > 0.1) %>%
      pull(!!sym(huc_col))
    
    # Perform spatial join between edges and HUCs
    Combined_edges_HUC <- st_join(edges, Huc, join = st_intersects) 
    edges$stream_length_m <- as.numeric(st_length(edges)) #Calculate stream length 
    
    # Summarize production by HUC polygon
    summary_huc <- Combined_edges_HUC %>%
      group_by(!!sym(huc_col)) %>%
      summarise(
        total_production = sum(basin_assign_rescale, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(production_proportion = total_production / sum(total_production, na.rm = TRUE))
    
    # Summarize stream length by HUC
    stream_length_by_huc <- edges %>%
      st_join(Huc, join = st_intersects) %>%
      st_drop_geometry() %>%
      group_by(!!sym(huc_col)) %>%
      summarise(total_stream_length = sum(stream_length_m, na.rm = TRUE))
    
    # Merge production and stream length summaries with HUC polygons
    final_result <- Huc %>%
      # Filter to only include HUCs with significant overlap with the basin
      filter(!!sym(huc_col) %in% significant_hucs) %>%
      left_join(st_drop_geometry(summary_huc), by = huc_col) %>%
      left_join(stream_length_by_huc, by = huc_col) %>%
      replace_na(list(
        total_production = 0, 
        production_proportion = 0, 
        total_stream_length = 0
      )) %>%
      mutate(
        production_per_meter = ifelse(total_stream_length > 0,
                                      total_production / total_stream_length,
                                      NA_real_),
        production_per_meter_norm = production_per_meter / max(production_per_meter, na.rm = TRUE)
      )
    
    # Create bar plot of production proportion by HUC - ENHANCED VERSION
    # Filter to only include HUCs with non-zero production
    non_zero_hucs <- final_result %>%
      filter(production_per_meter_norm > 0) %>%
      arrange(desc(production_per_meter_norm))
    
    # Get HUC names (or IDs if names aren't available)
    huc_names <- if(name_col %in% names(non_zero_hucs)) {
      non_zero_hucs[[name_col]]
    } else {
      non_zero_hucs[[huc_col]]
    }
    
    # Create more informative bar graph with ordered HUCs by production value
    bargraph <- ggplot(final_result, 
                       aes(x = !!sym(name_col),  # Use NAME directly (alphabetical order)
                           y = production_per_meter_norm)) +
      geom_col(aes(fill = production_per_meter_norm), alpha = 0.9) +
      # Use same YlOrRd color scale as the map for consistency
      scale_fill_gradientn(
        colors = (brewer.pal(9, "YlOrRd")),
        name = "Production per km",
        limits = c(0, 1)
      ) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(title = paste("Production per km by", "HUC", HUC, "(Alphabetical)"),
           x = "",
           y = "Production per km (normalized)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(5, 25, 5, 5, "mm")  # top, right, bottom, left margins
      )
    
    # Modify for HUC10 plots if there are too many
    if (HUC == 10 && nrow(non_zero_hucs) > 15) {
      # For HUC10, only show top 15 by production
      top_hucs <- non_zero_hucs %>%
        top_n(15, production_per_meter_norm)
      
      bargraph <- ggplot(top_hucs, 
                         aes(x = reorder(!!sym(ifelse(name_col %in% names(top_hucs), name_col, huc_col)), 
                                         production_per_meter_norm),
                             y = production_per_meter_norm)) +
        geom_col(aes(fill = production_per_meter_norm), alpha = 0.9) +
        scale_fill_gradientn(
          colors = rev(brewer.pal(9, "YlOrRd")),
          name = "Production per km"
        ) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0),
                           labels = scales::percent_format(accuracy = 1)) +
        labs(title = paste("Top 15 HUC10 by Production per km"),
             x = "",
             y = "Production per km (normalized)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.text.y = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.margin = margin(5, 25, 5, 5, "mm")  # top, right, bottom, left margins
        )
    }
    
    # Create improved main map plot with YlOrRd color scale
    main_plot <- ggplot() +
      geom_sf(
        data = final_result,
        aes(fill = production_per_meter_norm),
        color = "white",
        size = 0.1
      ) +
      # Using YlOrRd color palette as requested
      scale_fill_gradientn(
        colors = (brewer.pal(9, "YlOrRd")),
        name = "Relative production\nper river km",
        na.value = "grey95",
        limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1),
        guide = guide_colorbar(
          barwidth = 1,
          barheight = 15,
          frame.colour = "grey40",
          ticks.colour = "grey40",
          show.limits = TRUE
        )
      ) +
      coord_sf(datum = NA) +
      labs(
        title = paste("Subset:", subset_labels[q]),
        subtitle = paste("Year", year, "- Watershed:", watershed, "- Sensitivity:", sensitivity_threshold)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey30"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold", color = "grey30"),
        legend.text = element_text(color = "grey30"),
        panel.background = element_rect(fill = "grey98", color = NA),
        plot.margin = margin(5, 5, 5, 5, "mm")
      )
    
    # Define filename and save
    huc_filename <- paste0(subset_id, "_HUC", HUC, "_.pdf")
    huc_filepath <- file.path(here("Basin Maps/Quartile_Maps/HUC"), huc_filename)
    
    # Save plots to a PDF with the bar chart included and proper layout
    pdf(file = huc_filepath, width = 12, height = 8)
    
    # Set up the plotting layout with proper spacing
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(1, 2, widths = unit(c(0.6, 0.4), "npc"))))
    
    # Plot main map in left panel
    print(main_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    
    # Plot bar chart in right panel 
    print(bargraph, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    
    # Add histogram on top of the main map - moved higher up to avoid overlap
    print(gg_hist, vp = viewport(x = 0.3, y = 0.85, width = 0.5, height = 0.25))
    
    dev.off()
    
    #========================================================================
    # TRIBUTARY MAP - Enhanced version from paste-2.txt
    #========================================================================
    
    # Define the filename for the tributary map
    trib_filename <- paste0(subset_id, "_.pdf")
    trib_filepath <- file.path(here("Basin Maps/Quartile_Maps/Tribs"), trib_filename)
    
    # Open PDF for tributary map with increased height
    pdf(file = trib_filepath, width = 9, height = 8)  # Increased from height = 6 to height = 8
    
    # Use the YlOrRd palette from RColorBrewer with 9 colors
    pallete <- brewer.pal(9, "YlOrRd")
    
    # Interpolate the 9-color palette to create 10 colors for better gradation
    pallete_expanded <- colorRampPalette(pallete)(10)
    
    # Color coding with bins at every 0.1 for more granular visualization
    colcode <- rep("gray60", length(basin_assign_norm))
    colcode[basin_assign_norm == 0] <- 'white'
    colcode[basin_assign_norm > 0 & basin_assign_norm <= 0.1] <- pallete_expanded[1]
    colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- pallete_expanded[2]
    colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- pallete_expanded[3]
    colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- pallete_expanded[4]
    colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- pallete_expanded[5]
    colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- pallete_expanded[6]
    colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- pallete_expanded[7]
    colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- pallete_expanded[8]
    colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- pallete_expanded[9]
    colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- pallete_expanded[10]
    colcode[which(StreamOrderPrior == 0)] <- 'gray60'
    colcode[which(pid_prior == 0)] <- 'gray60'
    
    # Enhanced line widths for better visualization
    stream_order_lwd <- edges$Str_Order
    
    # Set base linewidths according to stream order
    linewidths <- rep(1, length(stream_order_lwd))
    linewidths <- ifelse(stream_order_lwd == 9, 5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 8, 4, linewidths)
    linewidths <- ifelse(stream_order_lwd == 7, 3, linewidths)
    linewidths <- ifelse(stream_order_lwd == 6, 2, linewidths)
    linewidths <- ifelse(stream_order_lwd == 5, 1.8, linewidths)
    linewidths <- ifelse(stream_order_lwd == 4, 1.5, linewidths)
    linewidths <- ifelse(stream_order_lwd == 3, 1, linewidths)
    
    # Add a multiplier for segments with high probability
    high_prob_multiplier <- rep(1, length(basin_assign_norm))
    high_prob_multiplier[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- 1.5  # 50% wider
    high_prob_multiplier[basin_assign_norm > 0.9] <- 1.9  # 90% wider
    
    # Apply the multiplier to the linewidths
    linewidths <- linewidths * high_prob_multiplier
    
    # Generate title
    plot_title <- paste("Subset:", subset_labels[q], 
                        "\nYear:", year, 
                        "River:", watershed,
                        "Threshold:", sensitivity_threshold, 
                        "Min Stream Order:", min_stream_order)
    
    # Use par to adjust plot margins - create more room at the bottom
    par(mar = c(8, 4, 4, 2))  # Increase bottom margin from default 4 to 8
    
    # Plotting
    plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title)
    plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
    
    # Add improved legend to upper left with more detailed breakdown
    legend("topleft", 
           legend = c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                      "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"), 
           col = pallete_expanded, 
           lwd = 5, 
           title = "Relative posterior density", 
           bty = "n")
    
    # Place histogram at the bottom of the plot
    vp_hist <- viewport(x = 0.5, y = 0.05, width = 0.7, height = 0.2, just = c("center", "bottom"))
    print(gg_hist, vp = vp_hist)
    
    dev.off()
    
    # Reset par to default for subsequent plots
    par(mar = c(5, 4, 4, 2) + 0.1)
    
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

#===============================================================================
# UTILITY FUNCTION FOR RUNNING THE ENHANCED QUARTILE MAPPING
#===============================================================================

run_enhanced_quartile_mapping <- function(years, watersheds, 
                                          filter_types = c("DOY_Q", "CPUE_Q", "TOTAL_CPUE_Q"),
                                          sensitivity_threshold = NULL, 
                                          min_error = NULL, 
                                          min_stream_order = 3, 
                                          HUC = 8, 
                                          custom_breaks = NULL) {
  
  # Create output directories if they don't exist
  dir.create(here("Basin Maps/Quartile_Maps"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/Quartile_Maps/HUC"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/Quartile_Maps/Tribs"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("Basin Maps/Quartile_Splits"), showWarnings = FALSE, recursive = TRUE)
  
  # Process each combination of year, watershed, and filter type
  results <- list()
  
  for (year in years) {
    for (watershed in watersheds) {
      # Set watershed-specific parameters if not provided
      if (is.null(sensitivity_threshold)) {
        if (watershed == "Kusko") {
          sens_thresh <- 0.7
        } else if (watershed == "Yukon") {
          sens_thresh <- 0.0001
        }
      } else {
        sens_thresh <- sensitivity_threshold
      }
      
      if (is.null(min_error)) {
        if (watershed == "Kusko") {
          min_err <- 0.0006
        } else if (watershed == "Yukon") {
          min_err <- 0.003
        }
      } else {
        min_err <- min_error
      }
      
      # Set the global watershed variable needed by the mapping function
      assign("watershed", watershed, envir = .GlobalEnv)
      
      # Process each filter type
      for (filter_type in filter_types) {
        message(paste("Processing", year, watershed, filter_type))
        
        tryCatch({
          # Run the mapping function
          if (filter_type == "CUSTOM" && !is.null(custom_breaks)) {
            result <- ALL_Map_func_Quartile_Enhanced(year, sens_thresh, min_err, min_stream_order, 
                                                     filter_type, HUC, custom_breaks)
          } else {
            result <- ALL_Map_func_Quartile_Enhanced(year, sens_thresh, min_err, min_stream_order, 
                                                     filter_type, HUC)
          }
          
          # Store results
          key <- paste(year, watershed, filter_type, sep = "_")
          results[[key]] <- result
          message(paste("Successfully processed", key))
          
        }, error = function(e) {
          message(paste("ERROR processing", year, watershed, filter_type, ":", e$message))
        })
      }
    }
  }
  
  return(results)
}

# Example usage:
# run_enhanced_quartile_mapping(
#   years = c("2015", "2016"),
#   watersheds = c("Kusko", "Yukon"),
#   filter_types = c("DOY_Q", "CPUE_Q"),
#   min_stream_order = 3,
#   HUC = 8
# )