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
library(patchwork)

############## Kuskokwim Basin Assignment and Map ##########################################################

## Test parameters 
year <- 2017
sensitivity_threshold <- 0.5
min_error <- 0.0006
min_stream_order <- 3
filter_conditions<- NULL
HUC<- 8

individual_poly_maps_func <- function(year, sensitivity_threshold, min_error, min_stream_order = 3, HUC = 8) {
  
  # Generate identifier for output files
  identifier <- paste(year, "Kusko", sep = "_")
  
  # Load spatial data
  kusk_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  
  # Transform all to same CRS
  kusk_edges <- st_transform(kusk_edges, st_crs(basin))
  Huc <- st_transform(Huc, st_crs(basin))
  
  if (grepl("longlat", st_crs(kusk_edges)$proj4string)) {
    stop("CRS is in degrees! Please reproject to a projected coordinate system (e.g., UTM or Albers).")
  }
  
  # Filter HUC polygons to only those within the basin
  Huc <- st_intersection(Huc, st_union(basin))
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Kusko_Natal_Origins_Genetics_CPUE.csv"))
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso),]
  
  #Filter the shapefile to only StreamOrders equal to or above the min_stream_order
  kusk_edges <- kusk_edges[kusk_edges$Str_Order >= min_stream_order,]
  
  # Line plot of daily CPUE proportion by DOY
  gg_hist <- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "gray20", linewidth = 2) +
    theme_minimal() +
    ggtitle("Prop. CPUE by DOY")
  
  # Highlight the relevant portion of the histogram (based on filter conditions)
  if (nrow(Natal_Origins) > 0) {
    gg_hist <- gg_hist +
      geom_ribbon(data = Natal_Origins, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                  fill = "gray", alpha = 0.7, inherit.aes = FALSE)
  }
  
  # Extract isoscape prediction and error values
  pid_iso <- kusk_edges$iso_pred
  pid_isose <- kusk_edges$isose_pred
  pid_prior <- kusk_edges$UniPh2oNoE
  
  # Constrain all pid_isose values above a given minimum and maximum error value
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  
  # Variance Generating Processes
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  within_pop <- within_site - analyt
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  
  # Perform assignments for each fish
  for (i in 1:nrow(Natal_Origins)) {
    iso_o <- as.numeric(Natal_Origins[i, "natal_iso"])
    StreamOrderPrior <- ifelse(kusk_edges$Str_Order >= min_stream_order, 1, 0)
    id <- Natal_Origins[i, "Fish_id"]
    
    # Bayesian assignment
    assign <- (1 / sqrt(2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2)) * pid_prior * StreamOrderPrior
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    kusk_edges$ind_assignment_dens <- assign_norm
    kusk_edges$assign_rescaled <- assign_rescaled 
    
    # Create dynamic column names
    huc_col <- paste0("HUC", HUC)
    name_col <- "NAME"  # Assuming both HUC8 and HUC10 have a NAME column
    
    # Assign Streams to HUC Polygons (only those within basin)
    Combined_edges_HUC <- st_join(kusk_edges, Huc, join = st_intersects)
    
    # Aggregate production by HUC
    summary_huc <- Combined_edges_HUC %>%
      group_by(!!sym(huc_col)) %>%
      summarise(
        total_production = sum(ind_assignment_dens, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join to HUC polygons (already filtered to basin)
    final_result <- Huc %>%
      left_join(st_drop_geometry(summary_huc), by = huc_col) %>%
      replace_na(list(total_production = 0)) %>%
      mutate(
        scaled_value = scales::rescale(total_production, to = c(0, 1), na.rm = TRUE)
      )
    
    # --- Map 1: Highlight single HUC with highest value ---
    max_huc <- final_result %>% 
      slice_max(total_production, n = 1)
    
    map1 <- ggplot(final_result) +
      geom_sf(fill = "grey90", color = "white") +
      geom_sf(data = max_huc, aes(fill = total_production), color = "black", linewidth = 0.8) +
      scale_fill_distiller(
        palette = "Reds", 
        direction = 1,
        name = "Production"
      ) +
      ggtitle("HUC with Highest Production") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_sf_text(data = max_huc, aes(label = !!sym(name_col)), 
                   color = "black", size = 3, fontface = "bold")
    
    # --- Map 2: Top 10% binned values ---
    top_10_cutoff <- quantile(kusk_edges$ind_assignment_dens, 0.99, na.rm = TRUE)
    
    top10_segment_counts <- kusk_edges %>% 
      filter(ind_assignment_dens >= top_10_cutoff) %>% 
      st_join(Huc, join = st_intersects) %>% 
      group_by(!!sym(huc_col)) %>% 
      summarise(
        n_segments = n(),
        total_production = sum(ind_assignment_dens, na.rm = TRUE)
      )
    
    final_top10_map <- Huc %>% 
      left_join(st_drop_geometry(top10_segment_counts), by = huc_col) %>%
      replace_na(list(n_segments = 0))
    
    map2 <- ggplot(final_top10_map) +
      geom_sf(aes(fill = n_segments), color = "white", lwd = 0.2) +
      scale_fill_distiller(
        palette = "Reds",
        direction = 1,
        name = "# of Top 1% Segments"
      ) +
      ggtitle("Number of Top 1% Stream Segments per HUC") +
      theme_minimal() +
      geom_sf_text(
        aes(label = ifelse(n_segments > 0, n_segments, "")),
        color = "black", 
        size = 3,
        check_overlap = TRUE
      )
    
    # --- Map 3: Full distribution ---
    map3 <- ggplot(final_result) +
      geom_sf(aes(fill = total_production)) +
      scale_fill_distiller(
        palette = "RdBu",
        direction = -1,
        name = "Total Production"
      ) +
      ggtitle("Full Production Distribution") +
      theme_minimal()
    
    # ---- Map of the edge lines colored by the normalized production values ----
    map4 <- ggplot(kusk_edges) +
      geom_sf(aes(color = assign_rescaled), size = 0.2) +
      scale_color_distiller(
        palette = "RdBu",
        direction = -1,
        name = "Production"
      ) +
      ggtitle("Edge Lines Colored by Production") +
      theme_minimal()
    
    # --- Combine all maps ---
    combined_maps <- (map1 + map2) / (map3 + map4) +
      plot_annotation(tag_levels = 'A')
    
    # Save with ID included 
    filename <- paste0("Basin Maps/Individual/ind_chlor_", id , ".png")
    ggsave(filename, combined_maps, width = 20, height = 10, units = "in")
  }
}

individual_poly_maps_func(2017,.001,.0006)
  
  
#   
#   # Calculate basin scale values
#   basin_assign_sum <- apply(assignment_matrix, 1, sum)
#   basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum)
#   basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale)
#   
#   ############## HUC Polygon assignments 
#   
#   # Ensure the CRS of both datasets match and are projected (in meters)
#   
#   # Assign basin_assign_rescale values
#   kusk_edges$basin_assign_rescale <- basin_assign_rescale
#   
#   # Create dynamic column names
#   huc_col <- paste0("HUC", HUC)
#   name_col <- "NAME"  # Assuming both HUC8 and HUC10 have a NAME column
#   
#   ### Step 1: Assign Streams to HUC Polygons
#   # Spatially join streams to HUC polygons using st_intersects
#   Combined_edges_HUC <- st_join(kusk_edges, Huc, join = st_intersects)
#   
#   ### Step 2: Aggregate production by HUC
#   summary_huc <- Combined_edges_HUC %>%
#     group_by(!!sym(huc_col)) %>%
#     summarise(
#       total_production = sum(basin_assign_rescale, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     filter(total_production > 0)  # Remove HUCs with no production
#   
#   # Calculate the proportion of total production in each HUC
#   summary_huc <- summary_huc %>%
#     mutate(production_proportion = total_production / sum(total_production, na.rm = TRUE))
#   
#   # Calculate stream length in meters for each edge (numeric conversion)
#   kusk_edges$stream_length_m <- as.numeric(st_length(kusk_edges))
#   
#   # Aggregate stream length by HUC
#   stream_length_by_huc <- kusk_edges %>%
#     st_join(Huc, join = st_intersects) %>%
#     st_drop_geometry() %>%
#     group_by(!!sym(huc_col)) %>%
#     summarise(total_stream_length = sum(stream_length_m, na.rm = TRUE))
#   
#   # Join all data together
#   final_result <- Huc %>%
#     left_join(st_drop_geometry(summary_huc), by = huc_col) %>%
#     left_join(stream_length_by_huc, by = huc_col) %>%
#     replace_na(list(total_production = 0, production_proportion = 0, total_stream_length = 0)) %>%
#     filter(total_production > 0) %>%
#     mutate(
#       # Calculate production per meter of stream
#       production_per_meter = ifelse(total_stream_length > 0,
#                                     total_production / total_stream_length,
#                                     NA_real_),
#       # Create scaled values (normalized 0-1)
#       scaled_value = scales::rescale(production_per_meter, to = c(0, 1), na.rm = TRUE)
#     )
#   
#   ## Make a barplot of the production proportion by HUC
#   bargraph <- ggplot(final_result, aes(x = reorder(!!sym(name_col), scaled_value), 
#                                        y = scaled_value)) +
#     geom_col(fill = "grey60", alpha = .8) +
#     coord_flip() +
#     scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
#     labs(title = paste("Production Proportion by HUC", HUC),
#          x = "",
#          y = "") +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(hjust = 0.3),
#       axis.text.y = element_text(size = 8),
#       panel.grid.major.y = element_blank()
#     )
#   
#   if (HUC == 10) {
#     bargraph <- bargraph + 
#       theme(axis.text.y = element_blank(),
#             axis.ticks.y = element_blank())
#   }
#   
#   # Define the filename and path for the PDF output with HUC level
#   filename <- paste0(identifier, "_HUC", HUC, "_", sensitivity_threshold, "_StrOrd", min_stream_order, "_.pdf")
#   filepath <- file.path(here("Basin Maps/Full_year_all_individuals/HUC", filename))
#   
#   # Create main map plot with scaled values
#   main_plot <- ggplot() +
#     geom_sf(
#       data = final_result,  # Using original data without binning
#       aes(fill = scaled_value),  # Continuous fill
#       color = "white",
#       size = 0.1
#     ) +
#     scale_fill_gradientn(
#       colors = rev(brewer.pal(9, "RdYlBu")), 
#       name = "Production Density\n(0-1 scaled)",
#       na.value = "grey95",
#       limits = c(0, 1),
#       labels = scales::number_format(accuracy = 0.1),  # Shows 0.0, 0.1, 0.2...1.0
#       guide = guide_colorbar(
#         barwidth = 1,
#         barheight = 15,
#         frame.colour = "grey40",
#         ticks.colour = "grey40",
#         show.limits = TRUE  # Shows min/max values
#       )
#     ) +
#     coord_sf(datum = NA) +
#     theme_minimal() +
#     labs(
#       title = paste("Year", year, "Production Density"),
#       subtitle = "Continuous 0-1 scaling"
#     ) +
#     theme(
#       plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey30"),
#       plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
#       legend.position = "right",
#       legend.title = element_text(size = 10, face = "bold", color = "grey30"),
#       legend.text = element_text(color = "grey30"),
#       panel.background = element_rect(fill = "grey98", color = NA)
#     )
#   
#   # Save to PDF
#   pdf(file = filepath, width = 12, height = 8)
#   grid.newpage()
#   print(main_plot, vp = viewport(width = 1, height = 1))
#   print(bargraph, 
#         vp = viewport(x = 0.25, y = 0.85, 
#                       width = 0.4, height = 0.3,
#                       just = c("center", "top")))
#   print(gg_hist, 
#         vp = viewport(x = 0.82, y = 0.015, 
#                       width = 0.35, height = 0.24,
#                       just = c("right", "bottom")))
#   dev.off()
#   
#   ################ 
#   ################# MAP 
#   #################
#   
#   # Re read in kusk_edges
#   kusk_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
#   kusk_edges <- kusk_edges[kusk_edges$Str_Order >= min_stream_order,]
#   
#   # Define the filename and path for the PDF output
#   filename <- paste0(identifier, "_", sensitivity_threshold,"_StrOrd",min_stream_order,"_.pdf")
#   filepath <- file.path(here("Basin Maps/Full_year_all_individuals/Tribs", filename))
#   
#   # Open a PDF for the plot
#   pdf(file = filepath, width = 9, height = 6)
#   pallete <- brewer.pal(n = 9, name = "YlOrRd")
#   
#   # Define custom colors based on conditions
#   colcode <- rep("gray60", length(basin_assign_norm)) # Default color for edges
#   colcode[basin_assign_norm == 0] <- 'white'
#   colcode[basin_assign_norm >= 0.9] <- pallete[9]
#   colcode[basin_assign_norm >= 0.8 & basin_assign_norm < 0.9] <- pallete[7]
#   colcode[basin_assign_norm >= 0.7 & basin_assign_norm < 0.8] <- pallete[5]
#   colcode[basin_assign_norm > 0.5 & basin_assign_norm < 0.7] <- pallete[4]
#   colcode[basin_assign_norm <= 0.5 & basin_assign_norm > 0] <- 'cornsilk2'
#   colcode[which(StreamOrderPrior == 0)] <- 'gray60'
#   colcode[which(pid_prior == 0)] <- 'gray60'
#   stream_order_lwd <- kusk_edges$Str_Order
#   
#   #Scale linewidths to basin assign norm
#   linewidths <- ifelse(basin_assign_norm >= 0.9, 2.3, 0.4)
#   linewidths <- ifelse(basin_assign_norm >= 0.8 & basin_assign_norm < 0.9, 1.8, linewidths)
#   linewidths <- ifelse(basin_assign_norm >= 0.7 & basin_assign_norm < 0.8, 1.0, linewidths)
#   linewidths <- ifelse(stream_order_lwd == 7, 2.8, linewidths)
#   linewidths <- ifelse(stream_order_lwd == 6, 1.6, linewidths)
#   linewidths <- ifelse(stream_order_lwd == 5, 1.4, linewidths)
#   
#   # Generate title with parameters
#   plot_title <- paste("Year:", year, 
#                       "River: Kusko", 
#                       "Threshold:", sensitivity_threshold, 
#                       "Min Stream Order:", min_stream_order, 
#                       "Min Error:", min_error)
#   
#   # Plot the basin and edges
#   plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title)
#   plot(st_geometry(kusk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
#   
#   # Add histogram in same position as HUC figure (bottom right)
#   print(gg_hist, 
#         vp = viewport(x = 0.82, y = 0.015, 
#                       width = 0.35, height = 0.24,
#                       just = c("right", "bottom")))
#   
#   # Add legend in same position as bar plot (top left)
#   legend(x = grconvertX(0.1, from = "npc"), 
#          y = grconvertY(0.99, from = "npc"),
#          legend = c(">= 0.9", "0.8-0.9", "0.7-0.8", "Low (<0.7)"),
#          col = c(pallete[9], pallete[7], pallete[5], "cornsilk2"), 
#          lwd = 6,                  # Thicker lines (was 5)
#          title = "Relative posterior density", 
#          bty = "n",
#          cex = 1.2,                # Larger text (was 0.8)
#          title.cex = 1.3,          # Larger title
#          seg.len = 2,              # Longer line segments
#          xpd = NA)
#   
#   # Close the PDF device
#   dev.off()
#   
# }
# 
# 
# 
# ############## Yukon Basin Assignment and Map ##############################################################
# 
# # Test parameters
# # year <- 2015
# # sensitivity_threshold <- 0.5
# # min_error <- 0.003
# # min_stream_order <- 4
# 
# 
# YK_Map_func <- function(year, sensitivity_threshold, min_error, min_stream_order ) {
#   yuk_edges<- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp") #Shapefiles 
#   basin<- st_read("/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp")
#   identifier <- paste(year, "Yukon", sep = "_")
#   Natal_Origins_ALL <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Yukon_Natal_Origins_Genetics_CPUE.csv"))
#   
#   #be sure to remove rows with NA in "Lower"
#   Natal_Origins <- Natal_Origins_ALL[!is.na(Natal_Origins_ALL$Lower), ]
#   yuk_edges <- yuk_edges[yuk_edges$Str_Order >= min_stream_order,]
#   
#   ## Filter the natal origins by some criteria 
#   if (!is.null(filter_conditions)) {
#     # Assuming filter_conditions is a list of conditions (for example: list(year = 2021, DOY = 1:50))
#     Natal_Origins_filtered <- Natal_Origins %>% filter(!!!filter_conditions)
#   } else {
#     Natal_Origins_filtered <- Natal_Origins
#   }
#   
#   gg_hist <- ggplot(Natal_Origins_ALL, aes(x = DOY, y = dailyCPUEprop)) + 
#     geom_line(color = "gray20", linewidth = 2) +
#     theme_grey() +
#     ggtitle("Prop. CPUE by DOY")
#   
#   # Highlight the relevant portion of the histogram (based on filter conditions)
#   if (nrow(Natal_Origins_filtered) > 0) {
#     gg_hist <- gg_hist +
#       geom_ribbon(data = Natal_Origins_filtered, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
#                   fill = "#FF6201", alpha = 0.7, inherit.aes = FALSE)
#   }
#   
#   #Shapefile with the tributaries from the each genetic grouping
#   ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
#   ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries
#   my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
#   my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries
#   uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
#   uy.gen_reachid <- uy.gen$reachid #reach ids of the upper Yukon tributaries
#   
#   yuk_edges$GenLMU <- 0
#   yuk_edges$GenLMU[yuk_edges$reachid %in% ly.gen_reachid] <- "lower"
#   yuk_edges$GenLMU[yuk_edges$reachid %in% my.gen_reachid] <- "middle"
#   yuk_edges$GenLMU[yuk_edges$reachid %in% uy.gen_reachid] <- "upper"
#   LYsites <- which(yuk_edges$GenLMU == "lower") # Create a vector of the INDICES associated with each genetic region
#   MYsites <- which(yuk_edges$GenLMU == "middle")
#   UYsites <- which(yuk_edges$GenLMU == "upper")
#   
#   ## ----- Extract isoscape prediction + error values -----------------------------
#   
#   pid_iso <- yuk_edges$iso_pred # Sr8786 value
#   pid_isose <- yuk_edges$isose_pred # Error
#   pid_prior <- yuk_edges$PriorSl2 #Habitat prior ( RCA slope)
#   pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
#   
#   ###----- Variance Generating Processes ------------------------------------------
#   within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
#   analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
#   within_pop <- within_site - analyt # Population error
#   error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 
#   
#   ###----- CREATE EMPTY MATRICES -------------------------------------------------
#   assignment_matrix <- matrix(NA, nrow = length(yuk_edges$iso_pred), ncol = nrow(Natal_Origins))
#   
#   #############################
#   ###### ASSIGNMENTS HERE ##### 
#   #############################
#   ## loop for assingments
#   
#   for (i in 1:nrow(Natal_Origins)) {
#     
#     iso_o <- Natal_Origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
#     gen.prior <- rep(0, length = length(pid_iso))
#     gen.prior[LYsites] <- Natal_Origins$Lower[i]%>% as.numeric()
#     gen.prior[MYsites] <- Natal_Origins$Middle[i] %>% as.numeric()
#     gen.prior[UYsites] <- Natal_Origins$Upper[i] %>% as.numeric()
#     
#     StreamOrderPrior <- ifelse(yuk_edges$Str_Order >= min_stream_order, 1, 0)
#     
#     #####. BAYES RULE ASSIGNMENT. ##################
#     
#     assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * gen.prior * StreamOrderPrior
#     
#     # normalize so all values sum to 1 (probability distribution)
#     assign_norm <- assign / sum(assign) 
#     
#     #rescale so that all values are between 0 and 1 
#     assign_rescaled <- assign_norm / max(assign_norm) 
#     
#     # If the rescaled value is less than the threshold, then set the same index in assign_norm to 0, otherwise 1 
#     assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
#     
#     # Multiply by the CPUE weight 
#     assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins[i, "COratio"])
#     
#     #assign_rescaled_wt<- assign_rescaled
#     
#     assignment_matrix[, i] <- assign_rescaled_wt
#     
#   }
#   
#   
#   # #Locate the column with NA 
#   # na_col <- which(colSums(is.na(assignment_matrix)) > 0)
#   # 
#   ###------- BASIN SCALE VALUES ----------------------------------------
#   
#   basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
#   basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
#   basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 
#   
#   # Define the filename and path for the PDF output
#   filename <- paste0(identifier, "_", sensitivity_threshold,"_StrOrd",min_stream_order,"_.pdf")
#   filepath <- file.path(here("Basin Maps/Full_year_all_individuals", filename))
#   
#   # Open a PDF for the plot
#   pdf(file = filepath, width = 9, height = 8)
#   
#   pallete <- brewer.pal(9, "YlOrRd")
#   
#   # Manually select colors for each bin
#   # Lightest color for bottom bin and darker colors for higher bins
#   custom_colors <- c(pallete[1],  # Lightest color for the lowest bin
#                      pallete[4],
#                      pallete[5],
#                      pallete[6],  # Next light color
#                      pallete[7],  # Middle color
#                      pallete[8],  # Darker color
#                      pallete[9])  # Darkest color for the highest bin
#   
#   # Define the number of classes
#   nclr <- length(custom_colors)  # Should be 5 based on the selected colors
#   
#   # Define breaks
#   breaks_combined <- c(0, 0.2, 0.4, 0.6,.7, 0.8,.9, 1)
#   
#   # Classify data with custom colors
#   class <- classIntervals(basin_assign_norm, nclr, style = "fixed", fixedBreaks = breaks_combined)
#   
#   # Assign colors to the classified data
#   colcode <- findColours(class, custom_colors, digits = 2)
#   
#   # Plot the color palette to verify selection
#   #barplot(rep(1, length(custom_colors)), col = custom_colors, border = NA, main = "Custom Color Palette")
#   
#   colcode[which(StreamOrderPrior == 0)] <- 'gray60'
#   colcode[which(pid_prior == 0)] <- 'gray60'
#   
#   stream_order_lwd <- yuk_edges$Str_Order
#   
#   #Scale linewidths to basin assign norm, with a minimum of .1 and a maximum of 1.5 
#   linewidths <- ifelse(basin_assign_norm >= 0.8, 1, 0.1)
#   linewidths <- ifelse(basin_assign_norm >= 0.6 & basin_assign_norm < 0.8, .7, linewidths)
#   linewidths <- ifelse(basin_assign_norm >= 0.4 & basin_assign_norm < 0.6, .7, linewidths)
#   linewidths <- ifelse(basin_assign_norm >= 0.2 & basin_assign_norm < 0.4, .5, linewidths)
#   linewidths <- ifelse(basin_assign_norm > 0 & basin_assign_norm < 0.1, .1, linewidths)
#   
#   # make linewidths of stream order 7 big 
#   linewidths[stream_order_lwd == 9] <- 1.8
#   linewidths[stream_order_lwd == 8] <- 1.6
#   linewidths[stream_order_lwd == 7] <- 1.4
#   linewidths[stream_order_lwd == 6] <- 1.3
#   
#   
#   # Generate title with parameters
#   plot_title <- paste("Year:", year, 
#                       "River: Yukon", 
#                       "Threshold:", sensitivity_threshold, 
#                       "Min Stream Order:", min_stream_order, 
#                       "Min Error:", min_error)
#   
#   # Plot the basin and edges
#   plot(st_geometry(basin), col = 'gray60', border = 'gray30', main = plot_title)
#   plot(st_geometry(yuk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
#   
#   legend_labels <- paste0(breaks_combined[-length(breaks_combined)], 
#                           " - ", 
#                           breaks_combined[-1])
#   
#   
#   vp_hist <- viewport(x = 0.28, y = 0.3, width = 0.43, height = 0.3, just = c("left", "top"))
#   print(gg_hist, vp = vp_hist)
#   
#   # Ensure colors match the bins correctly
#   legend("topleft", 
#          legend = paste0(rev(breaks_combined[-length(breaks_combined)]), 
#                          " - ", 
#                          rev(breaks_combined[-1])), 
#          col = rev(custom_colors),  # Reverse the order of the colors
#          lwd = 5, 
#          title = "Relative Posterior Density", 
#          bty = "n")
#   
#   # Close the PDF device
#   dev.off()
# }
# 

individual_poly_maps_func <- function(year, sensitivity_threshold, min_error, min_stream_order = 3, HUC = 8) {
  
  # Generate identifier for output files
  identifier <- paste(year, "Kusko", sep = "_")
  
  # Load spatial data
  kusk_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  
  # Transform all to same CRS
  kusk_edges <- st_transform(kusk_edges, st_crs(basin))
  Huc <- st_transform(Huc, st_crs(basin))
  
  if (grepl("longlat", st_crs(kusk_edges)$proj4string)) {
    stop("CRS is in degrees! Please reproject to a projected coordinate system (e.g., UTM or Albers).")
  }
  
  # Filter HUC polygons to only those within the basin
  Huc <- st_intersection(Huc, st_union(basin))
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Kusko_Natal_Origins_Genetics_CPUE.csv"))
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso),]
  
  #Filter the shapefile to only StreamOrders equal to or above the min_stream_order
  kusk_edges <- kusk_edges[kusk_edges$Str_Order >= min_stream_order,]
  
  # Create dynamic column names
  huc_col <- paste0("HUC", HUC)
  name_col <- "NAME"  # Assuming both HUC8 and HUC10 have a NAME column
  
  # Perform assignments for each fish
  for (i in 1:nrow(Natal_Origins)) {
    iso_o <- as.numeric(Natal_Origins[i, "natal_iso"])
    StreamOrderPrior <- ifelse(kusk_edges$Str_Order >= min_stream_order, 1, 0)
    id <- Natal_Origins[i, "Fish_id"]
    
    # Bayesian assignment
    pid_iso <- kusk_edges$iso_pred
    pid_isose <- kusk_edges$isose_pred
    pid_prior <- kusk_edges$UniPh2oNoE
    
    # Constrain all pid_isose values above a given minimum and maximum error value
    pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
    
    # Variance Generating Processes
    within_site <- 0.0003133684 / 1.96
    analyt <- 0.00011 / 2
    within_pop <- within_site - analyt
    error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
    
    assign <- (1 / sqrt(2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2)) * pid_prior * StreamOrderPrior
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    kusk_edges$ind_assignment_dens <- assign_norm
    kusk_edges$assign_rescaled <- assign_rescaled 
    
    # --- Map 1: Top 1% segments ---
    top_1_cutoff <- quantile(kusk_edges$ind_assignment_dens, 0.99, na.rm = TRUE)
    
    top1_segment_counts <- kusk_edges %>% 
      filter(ind_assignment_dens >= top_1_cutoff) %>% 
      st_join(Huc, join = st_intersects) %>% 
      group_by(!!sym(huc_col)) %>% 
      summarise(
        n_segments = n(),
        total_production = sum(ind_assignment_dens, na.rm = TRUE)
      )
    
    final_top1_map <- Huc %>% 
      left_join(st_drop_geometry(top1_segment_counts), by = huc_col) %>%
      replace_na(list(n_segments = 0))
    
    map1 <- ggplot(final_top1_map) +
      geom_sf(aes(fill = n_segments), color = "white", lwd = 0.2) +
      scale_fill_distiller(
        palette = "Reds",
        direction = 1,
        name = "# of Segments"
      ) +
      ggtitle("Number of Top 1% Stream Segments") +
      theme_minimal() +
      geom_sf_text(
        aes(label = ifelse(n_segments > 0, n_segments, "")),
        color = "black", 
        size = 3,
        check_overlap = TRUE
      )
    
    # --- Map 2: Top 5% segments ---
    top_5_cutoff <- quantile(kusk_edges$ind_assignment_dens, 0.95, na.rm = TRUE)
    
    top5_segment_counts <- kusk_edges %>% 
      filter(ind_assignment_dens >= top_5_cutoff) %>% 
      st_join(Huc, join = st_intersects) %>% 
      group_by(!!sym(huc_col)) %>% 
      summarise(
        n_segments = n(),
        total_production = sum(ind_assignment_dens, na.rm = TRUE)
      )
    
    final_top5_map <- Huc %>% 
      left_join(st_drop_geometry(top5_segment_counts), by = huc_col) %>%
      replace_na(list(n_segments = 0))
    
    map2 <- ggplot(final_top5_map) +
      geom_sf(aes(fill = n_segments), color = "white", lwd = 0.2) +
      scale_fill_distiller(
        palette = "Reds",
        direction = 1,
        name = "# of Segments"
      ) +
      ggtitle("Number of Top 5% Stream Segments") +
      theme_minimal() +
      geom_sf_text(
        aes(label = ifelse(n_segments > 0, n_segments, "")),
        color = "black", 
        size = 3,
        check_overlap = TRUE
      )
    
    # --- Map 3: Top 10% segments ---
    top_10_cutoff <- quantile(kusk_edges$ind_assignment_dens, 0.90, na.rm = TRUE)
    
    top10_segment_counts <- kusk_edges %>% 
      filter(ind_assignment_dens >= top_10_cutoff) %>% 
      st_join(Huc, join = st_intersects) %>% 
      group_by(!!sym(huc_col)) %>% 
      summarise(
        n_segments = n(),
        total_production = sum(ind_assignment_dens, na.rm = TRUE)
      )
    
    final_top10_map <- Huc %>% 
      left_join(st_drop_geometry(top10_segment_counts), by = huc_col) %>%
      replace_na(list(n_segments = 0))
    
    map3 <- ggplot(final_top10_map) +
      geom_sf(aes(fill = n_segments), color = "white", lwd = 0.2) +
      scale_fill_distiller(
        palette = "Reds",
        direction = 1,
        name = "# of Segments"
      ) +
      ggtitle("Number of Top 10% Stream Segments") +
      theme_minimal() +
      geom_sf_text(
        aes(label = ifelse(n_segments > 0, n_segments, "")),
        color = "black", 
        size = 3,
        check_overlap = TRUE
      )
    
    # ---- Map 4: Edge lines colored by normalized production values ----
    map4 <- ggplot(kusk_edges) +
      geom_sf(aes(color = assign_rescaled), size = 0.2) +
      scale_color_distiller(
        palette = "RdBu",
        direction = -1,
        name = "Production"
      ) +
      ggtitle("Stream Segments by Production") +
      theme_minimal()
    
    # --- Combine all maps ---
    combined_maps <- (map1 + map2) / (map3 + map4) +
      plot_annotation(tag_levels = 'A')
    
    # Save with ID included 
    filename <- paste0("Basin Maps/Individual/ind_chlor_", id , ".png")
    ggsave(filename, combined_maps, width = 20, height = 10, units = "in")
  }
}

individual_poly_maps_func(2017, 0.0001, 0.0006)
