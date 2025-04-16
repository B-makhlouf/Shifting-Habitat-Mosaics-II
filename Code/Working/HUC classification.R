install.packages("sf")
library(sf)
library(ggplot2)
library(tidyverse)
install.packages("viridis")
library(viridis)

Huc6<- st_read("/Users/benjaminmakhlouf/Spatial Data/HUC6_Trimmed.shp")
Huc8<- st_read("/Users/benjaminmakhlouf/Downloads/drive-download-20250225T194704Z-001/HUC8_Trimmed.shp")

# Kusk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")

year<- 2017
sensitivity_threshold<- 0.5
quartile<- "ALL"
min_error<- 0.0006
min_stream_order<- 3



HUC_KK_Map_func <- function(year, sensitivity_threshold, quartile = "ALL", min_error, min_stream_order = 3) {
  
  # Generate identifier for output files
  identifier <- paste(year, "Kusko", sep = "_")
  
  # Load spatial data
  kusk_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  
  # Load natal origins data
  natal_origins_path <- paste0("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Natal_Sr/QCd/ALL_DATA_", year, "_Kusko_Natal_Origins.csv")
  Natal_Origins <- read.csv(natal_origins_path)
  
  # Read in Huc6 and Huc8
  Huc6<- st_read("/Users/benjaminmakhlouf/Spatial Data/HUC6_Trimmed.shp")
  Huc8<- st_read("/Users/benjaminmakhlouf/Downloads/drive-download-20250225T194704Z-001/HUC8_Trimmed.shp")

  
  Huc6 <- st_as_sf(Huc6)
  Huc8 <- st_as_sf(Huc8)
  
  Huc8 <- st_transform(Huc8, st_crs(kusk_edges))
  Huc6 <- st_transform(Huc6, st_crs(kusk_edges))
  
  
  
  # Remove any rows with NA in natal_iso
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso),]
  
  # Filter based on quartile if specified
  if (quartile != "ALL") {
    if (quartile %in% c("Q1", "Q2", "Q3", "Q4")) {
      Natal_Origins <- Natal_Origins[Natal_Origins$Quartile == quartile, ]
    } else if (quartile == "H1") {
      Natal_Origins <- Natal_Origins[Natal_Origins$Quartile %in% c("Q1", "Q2"), ]
    } else if (quartile == "H2") {
      Natal_Origins <- Natal_Origins[Natal_Origins$Quartile %in% c("Q3", "Q4"), ]
    } else {
      stop("Invalid quartile value. Please use 'Q1', 'Q2', 'Q3', 'Q4', 'H1', 'H2', or 'ALL'.")
    }
  }
  
  #Filter the shapefile to only StreamOrders equal to or above the min_stream_order
  kusk_edges <- kusk_edges[kusk_edges$Str_Order >= min_stream_order,]
  
  
  # Extract isoscape prediction and error values
  pid_iso <- kusk_edges$iso_pred
  pid_isose <- kusk_edges$isose_pred
  pid_prior <- kusk_edges$UniPh2oNoE
  
  # Constrain all pid_isose values above a given minimum and maximum error value
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  #pid_isose_mod <- ifelse(pid_isose_mod > 0.0009, 0.0009, pid_isose_mod)
  #pid_isose_mod<- .0007
  
  
  # Variance Generating Processes
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  within_pop <- within_site - analyt
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  
  # Create empty matrices for assignments
  assignment_matrix <- matrix(NA, nrow = length(kusk_edges$iso_pred), ncol = nrow(Natal_Origins))
  
  # Perform assignments
  for (i in 1:nrow(Natal_Origins)) {
    iso_o <- as.numeric(Natal_Origins[i, "natal_iso"])
    StreamOrderPrior <- as.numeric(kusk_edges$Str_Order >= min_stream_order)
    
    # Bayesian assignment
    assign <- (1 / sqrt(2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2)) * pid_prior * StreamOrderPrior
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins[i, "Strat"])
    
    assignment_matrix[, i] <- assign_rescaled_wt
  }
  
  # Calculate basin scale values
  basin_assign_sum <- apply(assignment_matrix, 1, sum)
  basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum)
  basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale)
  
  
  ##############################################################################
  
  
  ### HUC #
  ## Add basin_assign_norm to kusk_edges
  
  kusk_edges$basin_assign_rescale <- basin_assign_rescale
  kusk_edges_huc8 <- st_join(kusk_edges, Huc8, join = st_intersects)
  
  summary_huc8 <- kusk_edges_huc8 %>%
    group_by(HUC8) %>%
    summarise(
      sum_basin_assign_norm = sum(basin_assign_norm, na.rm = TRUE),
      sum_basin_assign_rescale = sum(basin_assign_rescale, na.rm = TRUE),
      .groups = "drop"  # This removes the grouping
    )
  
  summary_huc8$rescale_rescaled <- summary_huc8$sum_basin_assign_norm / max(summary_huc8$sum_basin_assign_norm)
  Huc8_with_summary <- st_join(Huc8, summary_huc8, by = "HUC8")
  Huc8_with_summary <- Huc8_with_summary[, -which(names(Huc8_with_summary) == "HUC8.y")]

  ggplot() +
    geom_sf(data = Huc8_with_summary, aes(fill = sum_basin_assign_norm)) +
    scale_fill_viridis_c(
      limits = c(min(Huc8_with_summary$sum_basin_assign_norm, na.rm = TRUE),
                 max(Huc8_with_summary$sum_basin_assign_norm, na.rm = TRUE)),
      option = "C"  # You can try different options like "A", "B", "C", "D" for different color scales
    ) +
    theme_minimal() +
    labs(title = "HUC8 Polygons with Basin Assign Norm Values")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Define the filename and path for the PDF output
  filename <- paste0(identifier, "_", sensitivity_threshold,"_StrOrd",min_stream_order,"_",quartile, ".pdf")
  filepath <- file.path(here("HUC Maps", filename))
  
  # Open a PDF for the plot
  pdf(file = filepath, width = 9, height = 6)
  
  library(RColorBrewer)
  pallete<-brewer.pal(n = 9, name = "YlOrRd")
  
  
  # Define custom colors based on conditions
  colcode <- rep("gray60", length(basin_assign_norm)) # Default color for edges
  colcode[basin_assign_norm == 0] <- 'white'
  colcode[basin_assign_norm >= 0.9] <- pallete[9]
  colcode[basin_assign_norm >= 0.8 & basin_assign_norm < 0.9] <- pallete[7]
  colcode[basin_assign_norm >= 0.7 & basin_assign_norm < 0.8] <- pallete[5]
  colcode[basin_assign_norm > 0.5 & basin_assign_norm < 0.7] <- 'cornsilk2'
  colcode[basin_assign_norm <= 0.5 & basin_assign_norm > 0] <- 'cornsilk2'
  
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  
  stream_order_lwd <- kusk_edges$Str_Order
  
  # linewidths <- ifelse(stream_order_lwd == 1, 0.1,  # Thin lines for stream order 1
  #                      ifelse(stream_order_lwd == 2, .4,   # Medium lines for stream order 2
  #                             ifelse(stream_order_lwd == 3, .6,   # Thicker lines for stream order 3
  #                                    ifelse(stream_order_lwd == 4, .8,
  #                                           ifelse(stream_order_lwd == 5, 1.0, 
  #                                                  ifelse(stream_order_lwd == 6, 1.4,
  #                                                         ifelse(stream_order_lwd == 7, 2.5, 1.8)))))))  # Thickest lines for stream order 4
  #                                     
  
  #Scale linewidths to basin assign norm, with a minimum of .1 and a maximum of 1.5 
  linewidths <- ifelse(basin_assign_norm >= 0.9, 2.3, 0.4)
  linewidths <- ifelse(basin_assign_norm >= 0.8 & basin_assign_norm < 0.9, 1.8, linewidths)
  linewidths <- ifelse(basin_assign_norm >= 0.7 & basin_assign_norm < 0.8, 1.0, linewidths)
  
  
  
  # Generate title with parameters
  plot_title <- paste("Year:", year, 
                      "River: Kusko", 
                      "Threshold:", sensitivity_threshold, 
                      "Min Stream Order:", min_stream_order, 
                      "Min Error:", min_error)
  
  # Plot the basin and edges
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', main = plot_title)
  plot(st_geometry(kusk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # Add color legend
  legend("bottomright", 
         legend = c(">= 0.9", "0.8 - 0.9", "0.7 - 0.8", "Low (< 0.7)"), 
         col = c(pallete[9], pallete[7], pallete[5], "cornsilk2"), 
         lwd = 5, 
         title = "Relative posterior density", 
         bty = "n")
  
  
  # Close the PDF device
  dev.off()
}


