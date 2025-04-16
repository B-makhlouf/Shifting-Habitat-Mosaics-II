

#### 
#### 
### 
#### HUC MAP FUNCTION 

HUC_MAP<- function(rescaled_values, identifier, edges, basin, HUC = 8){
  
  huc_col <- paste0("HUC", HUC)  # Example: "HUC8" or "HUC10"
  name_col <- "Name"  # Assuming the HUC polygons contain a NAME column
  edges <- st_transform(edges, st_crs(Huc)) # Set projection to be the same 
  edges$basin_assign_rescale <- rescaled_values # Add rescaled values to the shapefile 
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
  filepath <- file.path(here("Basin Maps/Quartile/HUC"), filename)
  
  # Save plots to a multi-page PDF
  pdf(file = filepath, width = 12, height = 8)
  print(main_plot)
  
  #print(bargraph, vp = viewport(x = 0.25, y = 0.85, width = 0.4, height = 0.3, just = c("center", "top")))
  dev.off()
  
} 



### 
### 
### 
### 
# TRIB MAP FUNCTION


TRIB_MAP<- function(basin_assign_norm, identifier, edges, basin){
  
  if (watershed == "Kusko"){
    edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  } else if (watershed == "Yukon"){
    edges <- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
  }

  
  basin_assign_norm<- basin_assign_norm
  
  
  edges <- edges[edges$Str_Order >= min_stream_order,]
  
  # Define the filename and path for the PDF output
  filename <- paste0(identifier, "_", sensitivity_threshold,"_StrOrd",min_stream_order,"_.pdf")
  filepath <- file.path(here("Basin Maps/Quartile/Tribs", filename))
  
  # Open PDF with original dimensions
  pdf(file = filepath, width = 9, height = 6)
  pallete <- brewer.pal(n = 9, name = "YlOrRd")
  
  # Color coding
  colcode <- rep("gray60", length(basin_assign_norm))
  colcode[basin_assign_norm == 0] <- 'white'
  colcode[basin_assign_norm >= 0.9] <- pallete[9]
  colcode[basin_assign_norm >= 0.8 & basin_assign_norm < 0.9] <- pallete[7]
  colcode[basin_assign_norm >= 0.7 & basin_assign_norm < 0.8] <- pallete[5]
  colcode[basin_assign_norm >= 0.6 & basin_assign_norm < 0.7] <- pallete[3]
  colcode[basin_assign_norm <= 0.6 & basin_assign_norm > 0] <- 'cornsilk2'
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  
  
  # Line widths
  stream_order_lwd <- edges$Str_Order
  
  #set all linewidths to 1 initially 
  linewidths <- rep(1, length(stream_order_lwd))
  linewidths <- ifelse(stream_order_lwd == 9, 3.6, linewidths)
  linewidths <- ifelse(stream_order_lwd == 8, 3.2, linewidths)
  linewidths <- ifelse(stream_order_lwd == 7, 2.8, linewidths)
  linewidths <- ifelse(stream_order_lwd == 6, 2.6, linewidths)
  linewidths <- ifelse(stream_order_lwd == 5, 2.4, linewidths)
  linewidths <- ifelse(stream_order_lwd == 4, 2.2, linewidths)
  linewidths <- ifelse(stream_order_lwd == 3, 2.0, linewidths)
  
  
  # Generate title
  plot_title <- paste("Year:", year, 
                      "River:", watershed,
                      "Threshold:", sensitivity_threshold, 
                      "Min Stream Order:", min_stream_order, 
                      "Min Error:", min_error)
  
  # Plotting
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add histogram
  #vp_hist <- viewport(x = 0.1, y = 0.89, width = 0.43, height = 0.3, just = c("left", "top"))
  #print(gg_hist, vp = vp_hist)
  
  # Legend
  legend("bottomright", 
         legend = c(">= 0.9", "0.8 - 0.9", "0.7 - 0.8", "0.6 - 0.7", "Low (< 0.7)"), 
         col = c(pallete[9], pallete[7], pallete[5], pallete[3], "cornsilk2"), 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n")
  
  dev.off()
}

