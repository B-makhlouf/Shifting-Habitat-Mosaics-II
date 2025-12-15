library(sf)
library(tidyverse)
library(here)

################################################################################
# YUKON - EXISTING CODE (NO CHANGES)
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
# MAIN: Map upstream reaches by reachbase (YUKON)
#------------------------------------------------------------------------------

output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/UpstreamReachesbyStrOrd/Yukon"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Create data frame to store relationships
upstream_relationships <- data.frame(
  original_reachid = integer(),
  upstream_reachid = integer(),
  reachbase = integer()
)

# Get unique reachbase values
reachbases <- sort(unique(yuk_edges$Reachbase))
# Remove 0 
reachbases <- reachbases[reachbases != 0]

for (rb in reachbases) {
  
  # Get all reaches with this reachbase value
  reaches_of_reachbase <- yuk_edges$reachid[yuk_edges$Reachbase == rb]
  
  # Find all upstream reaches for each reach of this reachbase
  all_upstream <- c()
  for (reach in reaches_of_reachbase) {
    upstream <- FindUpstreamReachID(reach)
    all_upstream <- c(all_upstream, upstream)
    
    # Add to data frame
    if (length(upstream) > 0) {
      for (up_reach in upstream) {
        upstream_relationships <- rbind(upstream_relationships, 
                                        data.frame(original_reachid = reach, 
                                                   upstream_reachid = up_reach, 
                                                   reachbase = rb))
      }
    }
  }
  
  # Remove duplicates for mapping
  all_upstream <- unique(all_upstream)
  
  # Create color vector
  colcode <- rep('gray60', nrow(yuk_edges))
  colcode[yuk_edges$reachid %in% all_upstream] <- 'red'
  
  # Create linewidth vector by stream order
  stream_order <- yuk_edges$Str_Order
  linewidths <- ifelse(stream_order >= 9, 3.7,
                       ifelse(stream_order >= 8, 2.5,
                              ifelse(stream_order >= 7, 2.3,
                                     ifelse(stream_order >= 6, 2.0,
                                            ifelse(stream_order >= 5, 1.8,
                                                   ifelse(stream_order >= 4, 0.8, 
                                                          ifelse(stream_order >= 3, 0.7, 0.2)))))))
  
  # Save map
  filename <- file.path(output_dir, paste0("Yukon_UpstreamReaches_Reachbase_", rb, ".png"))
  png(filename, width = 10, height = 8, units = "in", res = 300)
  
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(yuk_basin), col = "gray90", border = NA, main = paste("Yukon Reachbase", rb, "- Upstream Reaches"))
  plot(st_geometry(yuk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  dev.off()
}

# Export data frame
yukon_export_path <- file.path(output_dir, "Yukon_UpstreamReaches_Relationships.csv")
write_csv(upstream_relationships, yukon_export_path)

cat("Yukon maps saved to:", output_dir, "\n")
cat("Yukon relationships exported to:", yukon_export_path, "\n")

################################################################################
# KUSKOKWIM - NEW CODE
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
# MAIN: Map upstream reaches by reachbase (KUSKOKWIM)
#------------------------------------------------------------------------------

# Create output subdirectory for Kuskokwim
kusko_output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/UpstreamReachesbyStrOrd/Kuskokwim"
dir.create(kusko_output_dir, recursive = TRUE, showWarnings = FALSE)

# Create data frame to store relationships
kusko_upstream_relationships <- data.frame(
  original_reachid = integer(),
  upstream_reachid = integer(),
  reachbase = integer()
)

# Get unique reachbase values
kusko_reachbases <- sort(unique(kusk_edges$Reachbase))
# Remove 0 
kusko_reachbases <- kusko_reachbases[kusko_reachbases != 0]

for (rb in kusko_reachbases) {
  
  # Get all reaches with this reachbase value
  reaches_of_reachbase <- kusk_edges$reachid[kusk_edges$Reachbase == rb]
  
  # Find all upstream reaches for each reach of this reachbase
  all_upstream <- c()
  for (reach in reaches_of_reachbase) {
    upstream <- FindUpstreamReachID_Kusk(reach)
    all_upstream <- c(all_upstream, upstream)
    
    # Add to data frame
    if (length(upstream) > 0) {
      for (up_reach in upstream) {
        kusko_upstream_relationships <- rbind(kusko_upstream_relationships, 
                                              data.frame(original_reachid = reach, 
                                                         upstream_reachid = up_reach, 
                                                         reachbase = rb))
      }
    }
  }
  
  # Remove duplicates for mapping
  all_upstream <- unique(all_upstream)
  
  # Create color vector
  colcode <- rep('gray60', nrow(kusk_edges))
  colcode[kusk_edges$reachid %in% all_upstream] <- 'red'
  
  # Create linewidth vector by stream order
  stream_order <- kusk_edges$Strahler
  linewidths <- ifelse(stream_order >= 9, 5,
                       ifelse(stream_order >= 8, 4,
                              ifelse(stream_order >= 7, 3,
                                     ifelse(stream_order >= 6, 2,
                                            ifelse(stream_order >= 5, 1.8,
                                                   ifelse(stream_order >= 4, 1.5, 
                                                          ifelse(stream_order >= 3, 1, 0.5)))))))
  
  # Save map
  filename <- file.path(kusko_output_dir, paste0("Kusko_UpstreamReaches_Reachbase_", rb, ".png"))
  png(filename, width = 10, height = 8, units = "in", res = 300)
  
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(kusk_basin), col = "gray90", border = NA, main = paste("Kuskokwim Reachbase", rb, "- Upstream Reaches"))
  plot(st_geometry(kusk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  dev.off()
}

# Export data frame
kusko_export_path <- file.path(kusko_output_dir, "Kusko_UpstreamReaches_Relationships.csv")
write_csv(kusko_upstream_relationships, kusko_export_path)

cat("\nKuskokwim maps saved to:", kusko_output_dir, "\n")
cat("Kuskokwim relationships exported to:", kusko_export_path, "\n")

