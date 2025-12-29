################################################################################
# UPSTREAM REACHES MAPPING AND VALIDATION
# Separate FindUpstreamReachID functions for Yukon and Kusko
# Sequential code for both watersheds - easy line-by-line walkthrough
################################################################################

library(sf)
library(tidyverse)
library(here)

################################################################################
# YUKON - FindUpstreamReachID FUNCTION
################################################################################

#' Find all upstream reaches for a focal reach (YUKON)
#'
#' Traverses the Yukon network from a focal reach ID and returns all upstream reaches.
#' Uses network relationships (parent-child node structure) to walk upstream.
#'
#' @param ReachID Numeric ID of focal reach
#' @param yuk_edges sf object with Yukon reach geometries and attributes
#' @param YukonNetwork Data frame with parent-child node relationships (child_s, parent_s, rid)
#'
#' @return Vector of upstream reach IDs
FindUpstreamReachID <- function(ReachID, yuk_edges, YukonNetwork) {
  
  # Find the reach ID to network ID (rid) conversion for focal reach
  TribStartRID <- yuk_edges$rid[which(yuk_edges$reachid == ReachID)]
  
  # Start traversal: find children of focal reach
  TRIBindex <- c()
  StartChild <- YukonNetwork$child_s[which(YukonNetwork$rid == TribStartRID)]
  TRIBindex <- c(TRIBindex, StartChild)
  
  # Keep walking upstream by finding children of current nodes
  ChildList <- YukonNetwork$child_s[which(YukonNetwork$parent_s == StartChild)]
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- YukonNetwork$child_s[which(YukonNetwork$parent_s %in% ChildList)]
  }
  
  # Convert back from network nodes to reach IDs
  TribSegments <- yuk_edges$reachid[match(
    YukonNetwork$rid[match(TRIBindex, YukonNetwork$child_s)], 
    yuk_edges$rid
  )]
  
  return(TribSegments)
}

################################################################################
# KUSKOKWIM - FindUpstreamReachID_Kusk FUNCTION
################################################################################

#' Find all upstream reaches for a focal reach (KUSKOKWIM)
#'
#' Traverses the Kuskokwim network from a focal reach ID and returns all upstream reaches.
#' Uses network relationships (parent-child node structure) to walk upstream.
#' Note: Kuskokwim uses different shapefile column names than Yukon.
#'
#' @param ReachID Numeric ID of focal reach
#' @param kusk_edges sf object with Kuskokwim reach geometries and attributes
#' @param KuskoNetwork Data frame with parent-child node relationships (child_s, parent_s, rid)
#'
#' @return Vector of upstream reach IDs
FindUpstreamReachID_Kusk <- function(ReachID, kusk_edges, KuskoNetwork) {
  
  # Find the reach ID to network ID (rid) conversion for focal reach
  TribStartRID <- kusk_edges$rid[which(kusk_edges$reachid == ReachID)]
  
  # Start traversal: find children of focal reach
  TRIBindex <- c()
  StartChild <- KuskoNetwork$child_s[which(KuskoNetwork$rid == TribStartRID)]
  TRIBindex <- c(TRIBindex, StartChild)
  
  # Keep walking upstream by finding children of current nodes
  ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s == StartChild)]
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s %in% ChildList)]
  }
  
  # Convert back from network nodes to reach IDs
  TribSegments <- kusk_edges$reachid[match(
    KuskoNetwork$rid[match(TRIBindex, KuskoNetwork$child_s)], 
    kusk_edges$rid
  )]
  
  return(TribSegments)
}

################################################################################
# YUKON ANALYSIS
################################################################################

cat("\n=== PROCESSING YUKON ===\n")

# Load spatial data
yuk_edges <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/KuskoReachbaseCompleteRedone.shp", quiet = TRUE)
yuk_basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp", quiet = TRUE)

# Load network relationships and rename columns
YukonNodes <- read.csv(
  here("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/yukon_noderelationships.csv"),
  header = TRUE, stringsAsFactors = FALSE
)
YukonNetwork <- YukonNodes %>% rename(child_s = fromnode, parent_s = tonode)

# Create output directories
yukon_output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/UpstreamReachesbyStrOrd/Yukon"
dir.create(yukon_output_dir, recursive = TRUE, showWarnings = FALSE)

# Initialize data frame to store all upstream reach relationships
upstream_relationships <- data.frame(
  original_reachid = integer(),
  upstream_reachid = integer(),
  reachbase = integer()
)

# Initialize validation summary
validation_summary <- data.frame(
  reachbase = integer(),
  n_unique_reachid = integer(),
  n_unique_groups = integer(),
  validation_passed = logical(),
  stringsAsFactors = FALSE
)

# Get valid reachbases (exclude 0 and 3)
reachbases <- sort(unique(yuk_edges$Reachbase))
reachbases <- reachbases[!(reachbases %in% c(0, 3))]

# Loop through each reachbase
for (rb in reachbases) {
  
  cat("Processing Yukon Reachbase", rb, "\n")
  
  # Get all reaches with this reachbase value
  reaches_of_reachbase <- yuk_edges$reachid[yuk_edges$Reachbase == rb]
  
  # Find all upstream reaches for visualization
  all_upstream <- c()
  groups_created <- c()
  
  # For each focal reach, find its upstream reaches
  for (reach in reaches_of_reachbase) {
    upstream <- FindUpstreamReachID(reach, yuk_edges, YukonNetwork)
    if (length(upstream) > 0) {
      all_upstream <- c(all_upstream, upstream)
      # Add each upstream reach relationship to the data frame
      for (up_reach in upstream) {
        upstream_relationships <- rbind(upstream_relationships,
                                        data.frame(original_reachid = reach,
                                                   upstream_reachid = up_reach,
                                                   reachbase = rb))
      }
    }
    # Track groups created for this reachbase
    groups_created <- c(groups_created, reach)
  }
  
  # Remove duplicates for mapping
  all_upstream <- unique(all_upstream)
  
  # Validation: count unique reachid values and groups
  n_unique_reachid <- n_distinct(reaches_of_reachbase)
  n_unique_groups <- n_distinct(groups_created)
  validation_passed <- (n_unique_reachid == n_unique_groups)
  
  # Create validation summary record
  validation_summary <- rbind(validation_summary,
                              data.frame(
                                reachbase = rb,
                                n_unique_reachid = n_unique_reachid,
                                n_unique_groups = n_unique_groups,
                                validation_passed = validation_passed
                              ))
  
  # Create map showing upstream reaches (red) for this reachbase
  colcode <- rep('gray60', nrow(yuk_edges))
  colcode[yuk_edges$reachid %in% all_upstream] <- 'red'
  
  # Set line widths based on stream order (Yukon - uses Str_Order column)
  stream_order <- yuk_edges$Str_Order
  linewidths <- ifelse(stream_order >= 9, 3.7,
                       ifelse(stream_order >= 8, 2.5,
                              ifelse(stream_order >= 7, 2.3,
                                     ifelse(stream_order >= 6, 2.0,
                                            ifelse(stream_order >= 5, 1.8,
                                                   ifelse(stream_order >= 4, 0.8,
                                                          ifelse(stream_order >= 3, 0.7, 0.2)))))))
  
  # Save map
  filename <- file.path(yukon_output_dir, 
                        paste0("Yukon_UpstreamReaches_Reachbase_", rb, ".png"))
  png(filename, width = 10, height = 8, units = "in", res = 300)
  
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(yuk_basin), col = "gray90", border = NA,
       main = paste("Yukon Reachbase", rb, "- Upstream Reaches"))
  plot(st_geometry(yuk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  dev.off()
}

# Print Yukon validation summary
cat("\n=== YUKON VALIDATION SUMMARY ===\n")
print(validation_summary)

if (all(validation_summary$validation_passed)) {
  cat("✓ All reachbases passed validation\n\n")
} else {
  cat("✗ Some reachbases failed validation\n\n")
}

# Export Yukon upstream relationships to CSV
yukon_csv_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/TribGroupProdByYear/SameGroupStrOrdYukon_UpstreamReaches_Relationships.csv"
write_csv(upstream_relationships, yukon_csv_path)
cat("Yukon relationships exported to:", yukon_csv_path, "\n")

################################################################################
# KUSKOKWIM ANALYSIS
################################################################################

cat("\n=== PROCESSING KUSKOKWIM ===\n")

# Load spatial data
kusk_edges <- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/KuskoReachbaseCompleteRedone.shp", quiet = TRUE)
kusk_basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp", quiet = TRUE)

# Load network relationships and rename columns
KuskoNodes <- read.csv(
  here("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/kusko_noderelationships.csv"),
  header = TRUE, stringsAsFactors = FALSE
)
KuskoNetwork <- KuskoNodes %>% rename(child_s = fromnode, parent_s = tonode)

# Create output directories
kusko_output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/UpstreamReachesbyStrOrd/Kusko"
dir.create(kusko_output_dir, recursive = TRUE, showWarnings = FALSE)

# Initialize data frame to store all upstream reach relationships
upstream_relationships <- data.frame(
  original_reachid = integer(),
  upstream_reachid = integer(),
  reachbase = integer()
)

# Initialize validation summary
validation_summary <- data.frame(
  reachbase = integer(),
  n_unique_reachid = integer(),
  n_unique_groups = integer(),
  validation_passed = logical(),
  stringsAsFactors = FALSE
)

# Get valid reachbases (exclude 0 and 3)
reachbases <- sort(unique(kusk_edges$Reachbase))
reachbases <- reachbases[!(reachbases %in% c(0, 3))]

# Loop through each reachbase
for (rb in reachbases) {
  
  cat("Processing Kusko Reachbase", rb, "\n")
  
  # Get all reaches with this reachbase value
  reaches_of_reachbase <- kusk_edges$reachid[kusk_edges$Reachbase == rb]
  
  # Find all upstream reaches for visualization
  all_upstream <- c()
  groups_created <- c()
  
  # For each focal reach, find its upstream reaches
  for (reach in reaches_of_reachbase) {
    upstream <- FindUpstreamReachID_Kusk(reach, kusk_edges, KuskoNetwork)
    if (length(upstream) > 0) {
      all_upstream <- c(all_upstream, upstream)
      # Add each upstream reach relationship to the data frame
      for (up_reach in upstream) {
        upstream_relationships <- rbind(upstream_relationships,
                                        data.frame(original_reachid = reach,
                                                   upstream_reachid = up_reach,
                                                   reachbase = rb))
      }
    }
    # Track groups created for this reachbase
    groups_created <- c(groups_created, reach)
  }
  
  # Remove duplicates for mapping
  all_upstream <- unique(all_upstream)
  
  # Validation: count unique reachid values and groups
  n_unique_reachid <- n_distinct(reaches_of_reachbase)
  n_unique_groups <- n_distinct(groups_created)
  validation_passed <- (n_unique_reachid == n_unique_groups)
  
  # Create validation summary record
  validation_summary <- rbind(validation_summary,
                              data.frame(
                                reachbase = rb,
                                n_unique_reachid = n_unique_reachid,
                                n_unique_groups = n_unique_groups,
                                validation_passed = validation_passed
                              ))
  
  # Create map showing upstream reaches (red) for this reachbase
  colcode <- rep('gray60', nrow(kusk_edges))
  colcode[kusk_edges$reachid %in% all_upstream] <- 'red'
  
  # Set line widths based on stream order (Kuskokwim - uses Strahler column)
  stream_order <- kusk_edges$Strahler
  linewidths <- ifelse(stream_order >= 9, 5,
                       ifelse(stream_order >= 8, 4,
                              ifelse(stream_order >= 7, 3,
                                     ifelse(stream_order >= 6, 2,
                                            ifelse(stream_order >= 5, 1.8,
                                                   ifelse(stream_order >= 4, 1.5,
                                                          ifelse(stream_order >= 3, 1, 0.5)))))))
  
  # Save map
  filename <- file.path(kusko_output_dir, 
                        paste0("Kusko_UpstreamReaches_Reachbase_", rb, ".png"))
  png(filename, width = 10, height = 8, units = "in", res = 300)
  
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(kusk_basin), col = "gray90", border = NA,
       main = paste("Kuskokwim Reachbase", rb, "- Upstream Reaches"))
  plot(st_geometry(kusk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  dev.off()
}

# Print Kuskokwim validation summary
cat("\n=== KUSKOKWIM VALIDATION SUMMARY ===\n")
print(validation_summary)

if (all(validation_summary$validation_passed)) {
  cat("✓ All reachbases passed validation\n\n")
} else {
  cat("✗ Some reachbases failed validation\n\n")
}

# Export Kuskokwim upstream relationships to CSV
kusko_csv_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameGroupStrOrdKusko_UpstreamReaches_Relationships.csv"
write_csv(upstream_relationships, kusko_csv_path)
cat("Kusko relationships exported to:", kusko_csv_path, "\n")

cat("\n=== COMPLETE ===\n")
cat("Yukon maps saved to:", yukon_output_dir, "\n")
cat("Kusko maps saved to:", kusko_output_dir, "\n")