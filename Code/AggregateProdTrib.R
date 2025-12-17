# Load libraries
library(dplyr)
library(readr)
library(sf)
library(RColorBrewer)
library(glue)

#------------------------------------------------------------------------------
# File paths
#------------------------------------------------------------------------------
tribcollect_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/Kusko_UpstreamReaches_ByStreamOrder.csv"
prod_data_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko/2017_Kusko_Assignment_Results.csv"
edges_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp"
basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp"

# Output directory
output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Kusko_Annual/TribAggregated"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
# Read data
#------------------------------------------------------------------------------
tribcollect <- read_csv(tribcollect_path, show_col_types = FALSE)
prod_data   <- read_csv(prod_data_path, show_col_types = FALSE)
edges <- st_read(edges_path, quiet = TRUE)
basin <- st_read(basin_path, quiet = TRUE)


#------------------------------------------------------------------------------
# Attach tributary_group_id to each reach in production data
#------------------------------------------------------------------------------
prod_with_trib <- prod_data %>%
  left_join(
    tribcollect %>%
      select(
        TribID, reachid
      ),
    by = c("reachid" = "reachid")
  )


#------------------------------------------------------------------------------
# Aggregate production at the tributary level
#------------------------------------------------------------------------------
trib_production <- prod_with_trib %>%
  group_by(TribID) %>%
  summarise(
    trib_total_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
    trib_total_assignment_individuals = sum(assignment_individuals, na.rm = TRUE),
    n_reaches = n_distinct(reachid),
    .groups = "drop"
  )


## is there any with tributary_group_id == NA?
trib_production_na <- trib_production %>%
  filter(is.na(TribID))

# Remove the NA 
trib_production <- trib_production %>%
  filter(!is.na(TribID))

#------------------------------------------------------------------------------
# Assign tributary-level production totals back to each reach
#------------------------------------------------------------------------------
prod_data_trib_level <- prod_with_trib %>%
  left_join(
    trib_production,
    by = "TribID"
  )

# Sum all of the 7th order tributary production values 

production<- prod_data_trib_level %>%
  filter(Str_Order == 7)%>%
  #sum assignment_rescale
  summarise(
    total_trib7_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
    total_trib7_assignment_individuals = sum(assignment_individuals, na.rm = TRUE)
  )

# Assign this value as trib_assign_Rescale for all 7th order trib columns 

seventhorder_rescale<- production$total_trib7_assignment_rescale
seventhorder_individuals<- production$total_trib7_assignment_individuals

prod_data_trib_level <- prod_data_trib_level %>%
  mutate(
    trib_total_assignment_rescale = ifelse(
      Str_Order == 7,
      seventhorder_rescale,
      trib_total_assignment_rescale
    ),
    trib_total_assignment_individuals = ifelse(
      Str_Order == 7,
      seventhorder_individuals,
      trib_total_assignment_individuals
    )
  )







################################################################################
# CREATE TRIBUTARY-AGGREGATED MAP
################################################################################

# Pull out the assignment values 

edges <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp")
basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
basin_assign_norm <- prod_data_trib_level$trib_total_assignment_rescale

# normalize to range from 0-1
basin_assign_norm <- (basin_assign_norm - min(basin_assign_norm, na.rm = TRUE)) / 
  (max(basin_assign_norm, na.rm = TRUE) - min(basin_assign_norm, na.rm = TRUE))


palette <- brewer.pal(9, "YlOrRd")
palette_expanded <- colorRampPalette(palette)(10)
colcode <- rep("gray90", length(basin_assign_norm))

colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette_expanded[1]
colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette_expanded[2]
colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette_expanded[3]
colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette_expanded[4]
colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette_expanded[5]
colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette_expanded[6]
colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette_expanded[7]
colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[8]
colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[9]
colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette_expanded[10]

legend_labels <- c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
legend_colors <- c(palette_expanded[2], palette_expanded[4], palette_expanded[5], 
                   palette_expanded[7], palette_expanded[8], palette_expanded[9], 
                   palette_expanded[10])

stream_order <- edges$Str_Order
stream_order[is.na(stream_order)] <- 1

linewidths <- ifelse(stream_order >= 9, 5,
                     ifelse(stream_order >= 8, 6,
                            ifelse(stream_order >= 7, 5,
                                   ifelse(stream_order >= 6, 3,
                                          ifelse(stream_order >= 5, 2.5,
                                                 ifelse(stream_order >= 4, 2,
                                                        ifelse(stream_order >= 3, 1.5, 1.0)))))))

output_dir<- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Kusko_Annual/TribAggregated"
map_filename <- file.path(output_dir, paste0("Kusko2017_tribaggregatedtrial.png"))
png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")

par(mar = c(8, 4, 4, 2), bg = "white")
plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
     main = paste0("TESTTribagg"), bg = "white")


plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)

# 8. ADD LEGEND
legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5, 
       title = "Relative posterior density", bty = "n", bg = "white")

dev.off()



