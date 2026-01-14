library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(tidyr)

# Loop through years
for(yr in 2017:2021) {
  
  # Load static spatial data
  shp <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_SlpDistkm.shp") %>%
    st_drop_geometry()
  
  # Check column names
  print(paste("Processing year:", yr))
  print(colnames(shp))
  
  # Load assignment data
  df <- read.csv(
    paste0("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko/",
           yr, "_Kusko_Assignment_Results.csv")
  )
  
  # Create data frame
  dat <- data.frame(
    slope   = shp$Avg_Slop_1,
    dist_up = shp$FlowLen_do,
    prod    = df$assignment_individuals
  ) 
  
  dat <- dat %>%
    filter(!is.na(prod), slope <= 2.5)%>%
    filter(slope > 0)
  
  # Bin slope and distance
  dat <- dat %>%
    mutate(
      slope_bin = cut(slope, breaks = 40),
      dist_bin  = cut(dist_up, breaks = 40)
    )
  
  # Aggregate production by bins
  dat_sum <- dat %>%
    group_by(dist_bin, slope_bin) %>%
    summarize(prod = sum(prod, na.rm = TRUE), .groups = "drop")
  
  # Create complete grid of all possible bin combinations
  all_dist_bins <- unique(dat_sum$dist_bin)
  all_slope_bins <- unique(dat_sum$slope_bin)
  complete_grid <- expand.grid(
    dist_bin = all_dist_bins,
    slope_bin = all_slope_bins,
    stringsAsFactors = FALSE
  )
  
  # Left join to fill in missing combinations
  dat_plot <- complete_grid %>%
    left_join(dat_sum, by = c("dist_bin", "slope_bin")) %>%
    replace_na(list(prod = 0))
  
  # Check final data
  print(paste("Rows in plot data:", nrow(dat_plot)))
  
  # Build color palette
  palette_base <- brewer.pal(9, "YlOrRd")
  palette_expanded <- colorRampPalette(palette_base)(10)
  
  # Compute production range
  prod_nonzero <- dat_plot$prod[dat_plot$prod > 0]
  if(length(prod_nonzero) == 0) {
    dat_plot <- dat_plot %>%
      mutate(prod_color = "white")
  } else {
    prod_min <- min(prod_nonzero)
    prod_max <- max(prod_nonzero)
    
    # Normalize to 0-1
    prod_norm <- (dat_plot$prod - prod_min) / (prod_max - prod_min)
    
    # Assign colors
    colcode <- rep("white", nrow(dat_plot))
    colcode[prod_norm > 0.2 & prod_norm <= 0.4] <- palette_expanded[3]
    colcode[prod_norm > 0.4 & prod_norm <= 0.6] <- palette_expanded[6]
    colcode[prod_norm > 0.6 & prod_norm <= 0.8] <- palette_expanded[7]
    colcode[prod_norm > 0.8 & prod_norm <= 0.9] <- palette_expanded[8]
    colcode[prod_norm > 0.9 & prod_norm <= 1.0] <- palette_expanded[10]
    
    dat_plot <- dat_plot %>%
      mutate(prod_color = colcode)
  }
  
  # Plot
  p <- ggplot(dat_plot, aes(x = dist_bin, y = slope_bin, fill = prod_color)) +
    geom_tile(color = NA) +
    scale_fill_identity() +
    labs(
      title = paste0("Total Production by Distance–Slope Space (", yr, ")"),
      x = "Distance Upstream (km, binned)",
      y = "Channel Slope (binned)",
      fill = "Production\n(normalized)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  print(p)
  
  # Save to disk
  ggsave(filename = paste0("Prod_DistSlope_", yr, ".png"), plot = p, width = 7, height = 6)
  
  print(paste("Saved: Prod_DistSlope_", yr, ".png", sep = ""))
  
}