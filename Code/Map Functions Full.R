# Load necessary libraries
library(sf)
library(here)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(classInt)
library(tidyr)
library(viridis)
library(tidyverse)

############## Function to create maps, includes a watershed specific function to 
#################### assign probability values to each watershed. 

All_Map <- function(year, sensitivity_threshold, min_error, min_stream_order, HUC = 8, return_values = FALSE) {
 
  if(watershed == "Kusko") {
    KK_assign(year, sensitivity_threshold, min_error, min_stream_order, HUC)
  } else if (watershed == "Yukon") {
    YK_assign(year, sensitivity_threshold, min_error, min_stream_order, HUC)
  }
  
  # Calculate basin scale values
  basin_assign_sum <- apply(assignment_matrix, 1, sum)
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale)
  
  ######################## HUC POLYGONS 
  
  huc_col <- paste0("HUC", HUC)  # Example: "HUC8" or "HUC10"
  name_col <- "Name"  # Assuming the HUC polygons contain a NAME column
  
  edges <- st_transform(edges, st_crs(Huc)) # Set projection to be the same 

  edges$basin_assign_rescale <- basin_assign_rescale  # Add rescaled values to the shapefile 
  
  basin <- st_transform(basin, st_crs(Huc))
  
  # First identify which HUCs are within the Basin
  hucs_in_basin <- Huc %>%
    st_filter(basin, .predicate = st_intersects) %>%
    pull(!!sym(huc_col))
  
  Combined_edges_HUC <- st_join(edges, Huc, join = st_intersects) # Perform a spatial join
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
    # Filter to only include HUCs that are within the Basin
    filter(!!sym(huc_col) %in% hucs_in_basin) %>%
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
  
  # Return values if specified
  if (return_values) {
    return(list(
      huc_data = final_result,
      stream_data = edges
    ))
  }
  
  
  # Create bar plot of production proportion by HUC
  bargraph <- ggplot(final_result, 
                     aes(x = !!sym(name_col),  # Use NAME directly (no reordering)
                         y = production_per_meter_norm)) +
    geom_col(fill = "grey60", alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(title = paste("Relative Production per km of river", HUC),
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.3),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_blank()
    )
  
  # Modify for HUC10 plots, too many!
  if (HUC == 10) {
    bargraph <- bargraph + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  
  # Create main map plot with scaled values
  main_plot <- ggplot() +
    geom_sf(
      data = final_result,
      aes(fill = production_per_meter_norm),
      color = "white",
      size = 0.1
    ) +
    scale_fill_gradientn(
      colors = (brewer.pal(9, "Reds")),
      name = "Relative production per river KM",
      na.value = "grey95",
      limits = c(0, 1),  # Explicitly set limits to [0, 1]
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
      title = paste("Year", year, "natal origin dist."),
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey30"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold", color = "grey30"),
      legend.text = element_text(color = "grey30"),
      panel.background = element_rect(fill = "grey98", color = NA)
    )
  
  ######### Define filename and save 
  filename <- paste0(identifier, "_HUC", HUC, "_", sensitivity_threshold, 
                     "_StrOrd", min_stream_order, "_.pdf")
  filepath <- file.path(here("Basin Maps/Full_year_all_individuals/HUC"), filename)
  
  # Save plots to a multi-page PDF
  pdf(file = filepath, width = 12, height = 8)
  print(main_plot)
  
  #print(bargraph, vp = viewport(x = 0.25, y = 0.85, width = 0.4, height = 0.3, just = c("center", "top")))
  dev.off()
  
  ################ 
  ################# MAP 
  #################
  
  if (watershed == "Kusko"){
  edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  } else if (watershed == "Yukon"){
  edges <- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
  }

  edges <- edges[edges$Str_Order >= min_stream_order,]
  
  # Modify the PDF creation part of your All_Map function in Map Functions Full.R
  
  # Define the filename and path for the PDF output
  filename <- paste0(identifier, "_", sensitivity_threshold,"_StrOrd",min_stream_order,"_.pdf")
  filepath <- file.path(here("Basin Maps/Full_year_all_individuals/Tribs", filename))
  
  # Increase the height of the PDF to make more room
  pdf(file = filepath, width = 9, height = 8)  # Increased from height = 6 to height = 8
  # Use the YlOrRd palette from RColorBrewer with 9 colors
  pallete <- brewer.pal(9, "YlOrRd")
  
  # For 10 bins (0.0-0.1, 0.1-0.2, etc.), we need to interpolate to get 10 colors
  # Interpolate the 9-color palette to create 10 colors
  pallete_expanded <- colorRampPalette(pallete)(10)
  
  # Color coding with bins at every 0.1
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
  
  # First, scale linewidths by stream order as before
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
  
  # Now enhance linewidths for high probability regions
  # Add a multiplier for segments with high probability
  high_prob_multiplier <- rep(1, length(basin_assign_norm))
  high_prob_multiplier[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- 1.5  # 30% wider
  high_prob_multiplier[basin_assign_norm > 0.9] <- 1.9  # 50% wider
  
  # Apply the multiplier to the linewidths
  linewidths <- linewidths * high_prob_multiplier
  
  # Generate title
  plot_title <- paste("Year:", year, 
                      "River:", watershed,
                      "Threshold:", sensitivity_threshold, 
                      "Min Stream Order:", min_stream_order, 
                      "Min Error:", min_error)
  
  # Use par to adjust plot margins - create more room at the bottom
  # Arguments to par(mar = c(bottom, left, top, right))
  par(mar = c(8, 4, 4, 2))  # Increase bottom margin from default 4 to 8
  
  # Plotting
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Move legend to upper left
  legend("topleft", 
         legend = c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", 
                    "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"), 
         col = pallete_expanded, 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n")
  
  # Place histogram further down
  vp_hist <- viewport(x = 0.5, y = 0.05, width = 0.7, height = 0.2, just = c("center", "bottom"))
  print(gg_hist, vp = vp_hist)
  
  dev.off()
  
  # Reset par to default if needed for subsequent plots
  par(mar = c(5, 4, 4, 2) + 0.1)  # Reset to default
  
  
  if (!return_values) {
    return(invisible(NULL))
  }
  
  
}


#################################################################################
### Individual assignment functions 

####################################### 
############# Kusko 
#######################################


#Test params 
year <- 2017
sensitivity_threshold <- 0.7
min_error <- 0.5
min_stream_order <- 3




KK_assign <- function(year, sensitivity_threshold, min_error, min_stream_order, HUC = 8) {
  
  # Generate identifier for output files
  identifier <- paste(year, "Kusko", sep = "_")
  
  # Load spatial data
  edges <- st_read("/Users/benjaminmakhlouf/Spatial Data/USGS Added/KuskoUSGS.shp")
  basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  edges <- st_transform(edges, st_crs(basin)) # Make sure all shapefiles are projected to the same CRS
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Kusko_Natal_Origins_Genetics_CPUE.csv"))
  
  # Remove any rows with NA in natal_iso or dailyCPUEprop
  Natal_Origins_clean <- Natal_Origins[!is.na(Natal_Origins$natal_iso) & !is.na(Natal_Origins$dailyCPUEprop), ]
  
  # Filter the shapefile to only StreamOrders equal to or above the min_stream_order
  edges <- edges[edges$Str_Order >= min_stream_order, ]
  
  # Plot CPUE by DOY
  gg_hist <- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "gray20", linewidth = 2) +
    theme_minimal() +
    ggtitle("Prop. CPUE by DOY")
  
  if (nrow(Natal_Origins) > 0) {
    gg_hist <- gg_hist +
      geom_ribbon(data = Natal_Origins, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                  fill = "gray", alpha = 0.7, inherit.aes = FALSE)
  }
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_prior <- edges$UniPh2oNoE
  
  # Constrain all pid_isose values above a given minimum error value
  pid_isose_mod <- pmax(pid_isose, min_error)
  
  # Variance Generating Processes
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  within_pop <- within_site - analyt
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  
  # Create empty matrices for assignments
  assignment_matrix <- matrix(NA, nrow = length(edges$iso_pred), ncol = nrow(Natal_Origins_clean))
  
  # Perform assignments
  for (i in 1:nrow(Natal_Origins_clean)) {
    iso_o <- as.numeric(Natal_Origins_clean[i, "natal_iso"])
    StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    PresencePrior <- ifelse((edges$Str_Order %in% c(6, 7, 8)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior<- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    # Bayesian assignment
    assign <- (1 / sqrt(2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2)) * pid_prior   * NewHabitatPrior *PresencePrior #* StreamOrderPrior
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # Rescale again so that the new values range from 0-1
    assign_rescaled <- assign_rescaled / max(assign_rescaled)
    
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins_clean[i, "COratio"])
    
    assignment_matrix[, i] <- assign_rescaled_wt
  }
  
  assignment_matrix <<- assignment_matrix
  edges<<- edges
  StreamOrderPrior<<- StreamOrderPrior
  pid_prior<<- pid_prior
  identifier<<- identifier
  basin<<- basin
  Huc<<- Huc
  
}


#######################################################
########### Yukon 
#######################################################


YK_assign <- function(year, sensitivity_threshold, min_error, min_stream_order, HUC = 8) {
  
  
  edges<- st_read("/Users/benjaminmakhlouf/Spatial Data/USGS Added/YukonUSGS.shp") 
  basin<<- st_read("/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp")
  identifier <<- paste(year, "Yukon", sep = "_")
  Natal_Origins_ALL <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  Huc <<- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  
  
  #be sure to remove rows with NA in "Lower"
  Natal_Origins <- Natal_Origins_ALL[!is.na(Natal_Origins_ALL$Lower), ]
  edges <- edges[edges$Str_Order >= min_stream_order,]
  
  gg_hist <<- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "gray20", linewidth = 2) +
    theme_grey() +
    ggtitle("Prop. CPUE by DOY")+
    geom_ribbon(data = Natal_Origins, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                fill = "#FF6201", alpha = 0.7, inherit.aes = FALSE)
  
  #Shapefile with the tributaries from the each genetic grouping
  ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
  ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries
  my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
  my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries
  uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
  uy.gen_reachid <- uy.gen$reachid #reach ids of the upper Yukon tributaries
  
  edges$GenLMU <- 0
  edges$GenLMU[edges$reachid %in% ly.gen_reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my.gen_reachid] <- "middle"
  edges$GenLMU[edges$reachid %in% uy.gen_reachid] <- "upper"
  LYsites <- which(edges$GenLMU == "lower") # Create a vector of the INDICES associated with each genetic region
  MYsites <- which(edges$GenLMU == "middle")
  UYsites <- which(edges$GenLMU == "upper")
  
  ## ----- Extract isoscape prediction + error values -----------------------------
  
  pid_iso <- edges$iso_pred # Sr8786 value
  pid_isose <- edges$isose_pred # Error
  pid_prior <<- edges$PriorSl2 #Habitat prior ( RCA slope)
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  
  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 
  
  ###----- CREATE EMPTY MATRICES -------------------------------------------------
  assignment_matrix <- matrix(NA, nrow = length(edges$iso_pred), ncol = nrow(Natal_Origins))
  
  #############################
  ###### ASSIGNMENTS HERE ##### 
  #############################
  ## loop for assingments
  
  for (i in 1:nrow(Natal_Origins)) {
    
    iso_o <- Natal_Origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
    gen.prior <- rep(0, length = length(pid_iso))
    gen.prior[LYsites] <- Natal_Origins$Lower[i]%>% as.numeric()
    gen.prior[MYsites] <- Natal_Origins$Middle[i] %>% as.numeric()
    gen.prior[UYsites] <- Natal_Origins$Upper[i] %>% as.numeric()
    
    StreamOrderPrior <<- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    
    PresencePrior <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
    NewHabitatPrior<- ifelse(edges$Spawner_IP == 0, 0, 1)
    
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * gen.prior * StreamOrderPrior * PresencePrior #* NewHabitatPrior 
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
    
    # If the rescaled value is less than the threshold, then set the same index in assign_norm to 0, otherwise 1 
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # Multiply by the CPUE weight 
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins[i, "COratio"])
    
    #assign_rescaled_wt<- assign_rescaled
    
    assignment_matrix[, i] <- assign_rescaled_wt
    
  }
  
  assignment_matrix<<- assignment_matrix
  edges<<- edges
  identifier<<- identifier
}
