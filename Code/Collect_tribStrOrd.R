library(sf)
library(tidyverse)
library(here)

################################################################################
# YUKON - UPSTREAM REACHES BY STREAM ORDER
################################################################################

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------

yuk_edges <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/YukonReachbaseComplete.shp")
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
    
    # Create tributary group ID (same for all records from this reach)
    tributary_group_id <- paste(rb, reach, current_stream_order, sep = "_")
    
    # Find all upstream reaches
    upstream <- FindUpstreamReachID(reach)
    
    if (length(upstream) > 0) {
      # Filter upstream reaches to only those with same stream order
      upstream_same_order <- upstream[yuk_edges$Str_Order[match(upstream, yuk_edges$reachid)] == current_stream_order]
      
      # Remove the original reach itself if it's in the list
      upstream_same_order <- upstream_same_order[upstream_same_order != reach]
      
      # Add records for each upstream reach WITH same stream order
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
      } else {
        # NO upstream reaches with same stream order - add a self-referential record
        yukon_upstream_by_streamorder <- rbind(yukon_upstream_by_streamorder, 
                                               data.frame(reachbase = rb, 
                                                          original_reachid = reach, 
                                                          stream_order = current_stream_order,
                                                          upstream_reachid = reach,
                                                          tributary_group_id = tributary_group_id,
                                                          stringsAsFactors = FALSE))
      }
    } else {
      # No upstream reaches at all - add a self-referential record
      yukon_upstream_by_streamorder <- rbind(yukon_upstream_by_streamorder, 
                                             data.frame(reachbase = rb, 
                                                        original_reachid = reach, 
                                                        stream_order = current_stream_order,
                                                        upstream_reachid = reach,
                                                        tributary_group_id = tributary_group_id,
                                                        stringsAsFactors = FALSE))
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
# KUSKOKWIM – UPSTREAM REACHES BY STREAM ORDER
################################################################################

library(sf)
library(dplyr)
library(here)

#------------------------------------------------------------------------------
# LOAD SPATIAL DATA
#------------------------------------------------------------------------------

kusk_edges <- st_read(
  "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp",
  quiet = TRUE
)

kusk_basin <- st_read(
  "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp",
  quiet = TRUE
)

#------------------------------------------------------------------------------
# LOAD NETWORK RELATIONSHIPS
#------------------------------------------------------------------------------

KuskoNodes <- read.csv(
  here("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/kusko_noderelationships.csv"),
  stringsAsFactors = FALSE
)

KuskoNetwork <- KuskoNodes %>%
  rename(
    child_s  = fromnode,
    parent_s = tonode
  )

#------------------------------------------------------------------------------
# FUNCTION: FIND ALL UPSTREAM REACH IDS FOR A GIVEN REACH (KUSKOKWIM)
#------------------------------------------------------------------------------

FindUpstreamReachID_Kusk <- function(ReachID) {
  
  # Resolve reachid → rid
  TribStartRID <- kusk_edges$rid[kusk_edges$reachid == ReachID]
  
  if (length(TribStartRID) != 1) {
    stop(paste("ReachID", ReachID, "does not resolve to a unique rid"))
  }
  
  # Initialize traversal
  TRIBindex <- KuskoNetwork$child_s[KuskoNetwork$rid == TribStartRID]
  
  ChildList <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% TRIBindex]
  
  while (length(ChildList) > 0) {
    TRIBindex  <- c(TRIBindex, ChildList)
    ChildList <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% ChildList]
  }
  
  # Convert node indices back to reach IDs
  upstream_rids <- KuskoNetwork$rid[match(TRIBindex, KuskoNetwork$child_s)]
  
  upstream_reachids <- kusk_edges$reachid[
    match(upstream_rids, kusk_edges$rid)
  ]
  
  return(upstream_reachids)
}

#------------------------------------------------------------------------------
# MAIN: GROUP UPSTREAM REACHES BY STREAM ORDER
#------------------------------------------------------------------------------

# Valid reachbases (exclude 0)
kusko_reachbases <- sort(unique(kusk_edges$Reachbase))
kusko_reachbases <- kusko_reachbases[kusko_reachbases != 0]

# Output table
kusk_upstream_by_streamorder <- data.frame(
  reachid      = kusk_edges$reachid,
  Stream_Order = kusk_edges$Str_Order,
  TribID       = NA_character_,
  stringsAsFactors = FALSE
)

# Loop over reachbases
for (rb in kusko_reachbases) {
  
  reaches_of_reachbase <- kusk_edges$reachid[
    kusk_edges$Reachbase == rb
  ]
  
  for (reach in reaches_of_reachbase) {
    
    current_stream_order <- kusk_edges$Str_Order[
      kusk_edges$reachid == reach
    ]
    
    tributary_group_id <- paste(
      rb,
      reach,
      current_stream_order,
      sep = "_"
    )
    
    upstream <- FindUpstreamReachID_Kusk(reach)
    
    if (length(upstream) == 0) next
    
    # Keep only upstream reaches with same stream order
    upstream_same_order <- upstream[
      kusk_edges$Str_Order[
        match(upstream, kusk_edges$reachid)
      ] == current_stream_order
    ]
    
    if (length(upstream_same_order) == 0) next
    
    idx <- kusk_upstream_by_streamorder$reachid %in% upstream_same_order
    
    # NOTE: This overwrites TribID if a reach appears in multiple groups
    kusk_upstream_by_streamorder$TribID[idx] <- tributary_group_id
  }
}



# Export data frame
kusko_export_path <- paste0(
  "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/",
  "Kusko_UpstreamReaches_ByStreamOrder.csv"
)

write_csv(kusk_upstream_by_streamorder, kusko_export_path)


################################################################################
# VALIDATION SUMMARY
# 
# #------------------------------------------------------------------------------
# # FUNCTION: Map a single tributary group for visual inspection
# #------------------------------------------------------------------------------
# 
# MapTributaryGroup <- function(tributary_group_id, data_df, edges_sf, basin_sf, basin_name) {
#   
#   # Get all reaches for this tributary group
#   reaches_in_group <- data_df$upstream_reachid[data_df$tributary_group_id == tributary_group_id]
#   original_reach <- unique(data_df$original_reachid[data_df$tributary_group_id == tributary_group_id])
#   stream_order <- unique(data_df$stream_order[data_df$tributary_group_id == tributary_group_id])
#   
#   if (length(reaches_in_group) == 0) {
#     cat("No reaches found for tributary group:", tributary_group_id, "\n")
#     return()
#   }
#   
#   # Create color vector
#   colcode <- rep('gray60', nrow(edges_sf))
#   colcode[edges_sf$reachid %in% reaches_in_group] <- 'red'
#   colcode[edges_sf$reachid == original_reach] <- 'blue'
#   
#   # Create plot
#   quartz()
#   par(mfrow = c(1, 1), mar = c(4, 4, 4, 2))
#   
#   plot(st_geometry(basin_sf), col = "gray90", border = NA, 
#        main = paste(basin_name, "- Tributary Group", tributary_group_id, 
#                     "\n(Stream Order", stream_order, "| Original Reach:", original_reach, ")"))
#   plot(st_geometry(edges_sf), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = 1)
#   
#   cat("Mapped tributary group:", tributary_group_id, "\n")
#   cat("Original reach (blue):", original_reach, "\n")
#   cat("Number of upstream reaches (red):", length(reaches_in_group), "\n")
# }
# 
# cat("Script complete! Both Yukon and Kuskokwim now include isolated reaches with self-referential IDs.\n")