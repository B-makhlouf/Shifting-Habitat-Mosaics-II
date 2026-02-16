################################################################################
# ASSIGN sameTribID TO UPSTREAM REACHES WITH SAME STREAM ORDER
# — KUSKOKWIM ONLY, REACHBASES 4, 5, 6
#
# Workflow (mirrors up_grp script):
# 1. For each segment where Reachbase ∈ {4,5,6}, collect upstream reaches
#    with the SAME Strahler order and assign a common sameTrbID
# 2. Spatial join sameTrbID back to Kusko_edges3.shp → save as Kusko_sametrib.shp
################################################################################

library(sf)
library(dplyr)
library(here)

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------
kusk_edges <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_upstream2.shp"),
  quiet = TRUE
)

kusk_edges_working <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  quiet = TRUE
)

KuskoNodes <- read.csv(
  here("Data", "UpstreamReaches", "kusko_noderelationships.csv"),
  stringsAsFactors = FALSE
)

KuskoNetwork <- KuskoNodes %>%
  rename(child_s = fromnode, parent_s = tonode)

#------------------------------------------------------------------------------
# FUNCTION: FIND ALL UPSTREAM REACH IDS FOR A GIVEN REACH (KUSKOKWIM)
#------------------------------------------------------------------------------
FindUpstreamReachID_Kusk <- function(ReachID) {
  TribStartRID <- kusk_edges$rid[kusk_edges$reachid == ReachID]
  if (length(TribStartRID) != 1) {
    stop(paste("ReachID", ReachID, "does not resolve to a unique rid"))
  }
  TRIBindex <- KuskoNetwork$child_s[KuskoNetwork$rid == TribStartRID]
  ChildList  <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% TRIBindex]
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- KuskoNetwork$child_s[KuskoNetwork$parent_s %in% ChildList]
  }
  upstream_rids     <- KuskoNetwork$rid[match(TRIBindex, KuskoNetwork$child_s)]
  upstream_reachids <- kusk_edges$reachid[match(upstream_rids, kusk_edges$rid)]
  return(upstream_reachids)
}

#------------------------------------------------------------------------------
# STEP 1: ASSIGN sameTrbID FOR REACHBASES 4, 5, 6
#------------------------------------------------------------------------------
# Get stream order as a vector for fast lookups
stream_orders <- kusk_edges$Strahler

# Initialize sameTrbID column
kusk_edges$sameTrbID <- NA_integer_

# Only process reachbases 4, 5, 6
target_reachbases <- c(3,4, 5, 6)

# Counter for unique sameTrbID
trib_counter <- 0L

for (rb in target_reachbases) {
  
  rb_reaches <- kusk_edges$reachid[kusk_edges$Reachbase == rb & !is.na(kusk_edges$Reachbase)]
  cat("Reachbase", rb, ":", length(rb_reaches), "mouth segments\n")
  
  for (reach in rb_reaches) {
    
    # Stream order of this reachbase segment
    reach_idx   <- which(kusk_edges$reachid == reach)
    current_so  <- stream_orders[reach_idx[1]]
    
    # Find all upstream reaches
    upstream <- FindUpstreamReachID_Kusk(reach)
    
    # Filter upstream to only those with the SAME stream order
    if (length(upstream) > 0) {
      up_idx <- match(upstream, kusk_edges$reachid)
      up_idx <- up_idx[!is.na(up_idx)]
      same_order_mask <- stream_orders[up_idx] == current_so
      upstream_same   <- upstream[same_order_mask]
    } else {
      upstream_same <- integer(0)
    }
    
    # Combine: the reachbase segment itself + same-order upstream reaches
    all_in_trib <- unique(c(reach, upstream_same))
    
    # Increment counter and assign (only to unclaimed reaches)
    trib_counter <- trib_counter + 1L
    target_rows  <- kusk_edges$reachid %in% all_in_trib & is.na(kusk_edges$sameTrbID)
    kusk_edges$sameTrbID[target_rows] <- trib_counter
    
    if (trib_counter %% 100 == 0) {
      cat("  Assigned sameTrbID", trib_counter, "\n")
    }
  }
  
  cat("  Completed reachbase", rb, "\n")
}

#------------------------------------------------------------------------------
# STEP 1b: ASSIGN sameTrbID FOR MAINSTEM (Strahler 7 and 8)
#------------------------------------------------------------------------------
trib_counter <- trib_counter + 1L
mainstem_rows <- kusk_edges$Strahler %in% c(7, 8) & is.na(kusk_edges$sameTrbID)
kusk_edges$sameTrbID[mainstem_rows] <- trib_counter
cat("Mainstem (Strahler 7 & 8) assigned sameTrbID", trib_counter,
    ":", sum(mainstem_rows), "reaches\n")

cat("\nTotal sameTrbID groups assigned:", trib_counter, "\n")
cat("Reaches with a sameTrbID:",
    sum(!is.na(kusk_edges$sameTrbID)), "/", nrow(kusk_edges), "\n")

#------------------------------------------------------------------------------
# STEP 2: SPATIAL JOIN — Transfer sameTrbID back to Kusko_edges3.shp
#------------------------------------------------------------------------------
# Ensure matching CRS
kusk_edges_working <- st_transform(kusk_edges_working, st_crs(kusk_edges))
# Keep only sameTrbID + geometry from kusk_edges
group_sf <- kusk_edges %>%
  select(sameTrbID)

# Drop any existing sameTrbID column from working shapefile before joining
kusk_edges_working <- kusk_edges_working %>%
  select(-any_of("sameTrbID"))

kusk_edges_sametrib <- kusk_edges_working %>%
  st_join(group_sf, join = st_equals, left = TRUE)

# Clean up duplicate columns if any
st_cols <- grep("^sameTrbID", names(kusk_edges_sametrib), value = TRUE)
if (length(st_cols) > 1) {
  kusk_edges_sametrib <- kusk_edges_sametrib %>%
    mutate(sameTrbID = coalesce(sameTrbID.y, sameTrbID.x)) %>%
    select(-any_of(c("sameTrbID.x", "sameTrbID.y")))
}

#------------------------------------------------------------------------------
# STEP 2b: SAVE
#------------------------------------------------------------------------------
out_path <- here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_sametrib.shp")

st_write(
  kusk_edges_sametrib,
  out_path,
  quiet      = TRUE,
  delete_dsn = TRUE
)

cat("Saved:", out_path, "\n")
