library(sf)
library(tidyverse)
library(here)

################################################################################
# YUKON - UPSTREAM REACHES BY STREAM ORDER
################################################################################

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------

yuk_edges <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/YukonUSGS_noCA_reachbase.shp")
yuk_basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

YukonNodes <- read.csv(here("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/yukon_noderelationships.csv"), header = TRUE, stringsAsFactors = FALSE)
YukonNetwork <- YukonNodes %>% rename(child_s = fromnode, parent_s = tonode)

#------------------------------------------------------------------------------
# FUNCTION: Find all upstream reaches for a given reach ID
#------------------------------------------------------------------------------

FindUpstreamReachID <- function(ReachID) {
  TribStartRID <- yuk_edges$rid[which(yuk_edges$reachid == ReachID)]
  
  TRIBindex <- c()
  StartChild <- YukonNetwork$child_s[which(YukonNetwork$rid == TribStartRID)]
  TRIBindex <- c(TRIBindex, StartChild)
  
  ChildList <- YukonNetwork$child_s[which(YukonNetwork$parent_s == StartChild)]
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- YukonNetwork$child_s[which(YukonNetwork$parent_s %in% ChildList)]
  }
  
  TribSegments <- yuk_edges$reachid[match(YukonNetwork$rid[match(TRIBindex, YukonNetwork$child_s)], yuk_edges$rid)]
  return(TribSegments)
}

#------------------------------------------------------------------------------
# MAIN: Find upstream reaches by stream order (YUKON)
#------------------------------------------------------------------------------

# Create data frame to store relationships
yukon_upstream_by_streamorder <- data.frame(
  reachbase = integer(),
  original_reachid = integer(),
  stream_order = integer(),
  upstream_reachid = integer(),
  tributary_group_id = character(),
  stringsAsFactors = FALSE
)

# Get unique reachbase values
reachbases <- sort(unique(yuk_edges$Reachbase))
# Remove 0 
reachbases <- reachbases[reachbases != 0]

for (rb in reachbases) {
  
  # Get all reaches with this reachbase value
  reaches_of_reachbase <- yuk_edges$reachid[yuk_edges$Reachbase == rb]
  
  # Process each reach in this reachbase
  for (reach in reaches_of_reachbase) {
    
    # Get stream order of current reach
    current_stream_order <- yuk_edges$Str_Order[yuk_edges$reachid == reach]
    
    # Find all upstream reaches
    upstream <- FindUpstreamReachID(reach)
    
    if (length(upstream) > 0) {
      # Filter upstream reaches to only those with same stream order
      upstream_same_order <- upstream[yuk_edges$Str_Order[match(upstream, yuk_edges$reachid)] == current_stream_order]
      
      # Create tributary group ID
      tributary_group_id <- paste(rb, reach, current_stream_order, sep = "_")
      
      # Add to data frame
      if (length(upstream_same_order) > 0) {
        for (up_reach in upstream_same_order) {
          yukon_upstream_by_streamorder <- rbind(yukon_upstream_by_streamorder, 
                                                 data.frame(reachbase = rb, 
                                                            original_reachid = reach, 
                                                            stream_order = current_stream_order,
                                                            upstream_reachid = up_reach, 
                                                            tributary_group_id = tributary_group_id,
                                                            stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  cat("Completed reachbase", rb, "for Yukon\n")
}

# Export data frame
yukon_export_path <- paste0(
  "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/",
  "Yukon_UpstreamReaches_ByStreamOrder.csv"
)

write_csv(yukon_upstream_by_streamorder, yukon_export_path)

cat("\nYukon upstream reaches by stream order exported to:", yukon_export_path, "\n")
cat("Total records:", nrow(yukon_upstream_by_streamorder), "\n")
cat("Unique tributary groups:", n_distinct(yukon_upstream_by_streamorder$tributary_group_id), "\n\n")

################################################################################
# KUSKOKWIM - UPSTREAM REACHES BY STREAM ORDER
################################################################################

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------

kusk_edges <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase.shp")
kusk_basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")

KuskoNodes <- read.csv(here("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/kusko_noderelationships.csv"), header = TRUE, stringsAsFactors = FALSE)
KuskoNetwork <- KuskoNodes %>% rename(child_s = fromnode, parent_s = tonode)

#------------------------------------------------------------------------------
# FUNCTION: Find all upstream reaches for a given reach ID (KUSKOKWIM)
#------------------------------------------------------------------------------

FindUpstreamReachID_Kusk <- function(ReachID) {
  TribStartRID <- kusk_edges$rid[which(kusk_edges$reachid == ReachID)]
  
  TRIBindex <- c()
  StartChild <- KuskoNetwork$child_s[which(KuskoNetwork$rid == TribStartRID)]
  TRIBindex <- c(TRIBindex, StartChild)
  
  ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s == StartChild)]
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s %in% ChildList)]
  }
  
  TribSegments <- kusk_edges$reachid[match(KuskoNetwork$rid[match(TRIBindex, KuskoNetwork$child_s)], kusk_edges$rid)]
  return(TribSegments)
}

#------------------------------------------------------------------------------
# MAIN: Find upstream reaches by stream order (KUSKOKWIM)
#------------------------------------------------------------------------------

# Create data frame to store relationships
kusko_upstream_by_streamorder <- data.frame(
  reachbase = integer(),
  original_reachid = integer(),
  stream_order = integer(),
  upstream_reachid = integer(),
  tributary_group_id = character(),
  stringsAsFactors = FALSE
)

# Get unique reachbase values
kusko_reachbases <- sort(unique(kusk_edges$Reachbase))
# Remove 0 
kusko_reachbases <- kusko_reachbases[kusko_reachbases != 0]

for (rb in kusko_reachbases) {
  
  # Get all reaches with this reachbase value
  reaches_of_reachbase <- kusk_edges$reachid[kusk_edges$Reachbase == rb]
  
  # Process each reach in this reachbase
  for (reach in reaches_of_reachbase) {
    
    # Get stream order of current reach (note: Kuskokwim uses 'Strahler' instead of 'Str_Order')
    current_stream_order <- kusk_edges$Strahler[kusk_edges$reachid == reach]
    
    # Find all upstream reaches
    upstream <- FindUpstreamReachID_Kusk(reach)
    
    if (length(upstream) > 0) {
      # Filter upstream reaches to only those with same stream order
      upstream_same_order <- upstream[kusk_edges$Strahler[match(upstream, kusk_edges$reachid)] == current_stream_order]
      
      # Create tributary group ID
      tributary_group_id <- paste(rb, reach, current_stream_order, sep = "_")
      
      # Add to data frame
      if (length(upstream_same_order) > 0) {
        for (up_reach in upstream_same_order) {
          kusko_upstream_by_streamorder <- rbind(kusko_upstream_by_streamorder, 
                                                 data.frame(reachbase = rb, 
                                                            original_reachid = reach, 
                                                            stream_order = current_stream_order,
                                                            upstream_reachid = up_reach, 
                                                            tributary_group_id = tributary_group_id,
                                                            stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  cat("Completed reachbase", rb, "for Kuskokwim\n")
}

# Export data frame
kusko_export_path <- paste0(
  "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/",
  "Kusko_UpstreamReaches_ByStreamOrder.csv"
)

write_csv(kusko_upstream_by_streamorder, kusko_export_path)

cat("\nKuskokwim upstream reaches by stream order exported to:", kusko_export_path, "\n")
cat("Total records:", nrow(kusko_upstream_by_streamorder), "\n")
cat("Unique tributary groups:", n_distinct(kusko_upstream_by_streamorder$tributary_group_id), "\n")




#------------------------------------------------------------------------------
# FUNCTION: Map a single tributary group for visual inspection
#------------------------------------------------------------------------------

MapTributaryGroup <- function(tributary_group_id, data_df, edges_sf, basin_sf, basin_name) {
  
  # Get all reaches for this tributary group
  reaches_in_group <- data_df$upstream_reachid[data_df$tributary_group_id == tributary_group_id]
  original_reach <- unique(data_df$original_reachid[data_df$tributary_group_id == tributary_group_id])
  stream_order <- unique(data_df$stream_order[data_df$tributary_group_id == tributary_group_id])
  
  if (length(reaches_in_group) == 0) {
    cat("No reaches found for tributary group:", tributary_group_id, "\n")
    return()
  }
  
  # Create color vector
  colcode <- rep('gray60', nrow(edges_sf))
  colcode[edges_sf$reachid %in% reaches_in_group] <- 'red'
  colcode[edges_sf$reachid == original_reach] <- 'blue'
  
  # Create plot
  quartz()
  par(mfrow = c(1, 1), mar = c(4, 4, 4, 2))
  
  plot(st_geometry(basin_sf), col = "gray90", border = NA, 
       main = paste(basin_name, "- Tributary Group", tributary_group_id, 
                    "\n(Stream Order", stream_order, "| Original Reach:", original_reach, ")"))
  plot(st_geometry(edges_sf), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = 1)
  
  cat("Mapped tributary group:", tributary_group_id, "\n")
  cat("Original reach (blue):", original_reach, "\n")
  cat("Number of upstream reaches (red):", length(reaches_in_group), "\n")
}








# #------------------------------------------------------------------------------
# # TEST EXAMPLES
# #------------------------------------------------------------------------------
# 
# MapTributaryGroup("7_12892_8", yukon_upstream_by_streamorder, 
#                     yuk_edges, yuk_basin, "Yukon")
# 
# # Example: Pick a tributary group from Kuskokwim data
# if (nrow(kusko_upstream_by_streamorder) > 0) {
#   example_kusko_group <- unique(kusko_upstream_by_streamorder$tributary_group_id)[1]
#   cat("\n=== Testing Kuskokwim Tributary Group ===\n")
#   MapTributaryGroup(example_kusko_group, kusko_upstream_by_streamorder, 
#                     kusk_edges, kusk_basin, "Kuskokwim")
# }