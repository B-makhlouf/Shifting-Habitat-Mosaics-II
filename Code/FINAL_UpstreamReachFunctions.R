################################################################################
# GROUP UPSTREAM REACHES BY MAINSTEM-TOUCHING SEGMENTS
#
# Workflow:
# 1. Spatial join TouchingMs from Kusko_edges.shp → Kusko_upstream.shp
# 2. For each segment where TouchingMs == 1, collect all upstream reaches
# 3. Assign a up_grp ID to each upstream reach
# 4. Spatial join up_grp back to Kusko_edges.shp for downstream use
################################################################################

library(sf)
library(dplyr)
library(here)

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------
kusk_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"),
                      quiet = TRUE)




kusk_basin <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_basin.shp"),
                      quiet = TRUE)

# kusk_edges_working <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"),
#                               quiet = TRUE)

KuskoNodes <- read.csv(
  here("Data","UpstreamReaches","kusko_noderelationships.csv"),
  stringsAsFactors = FALSE
)

KuskoNetwork <- KuskoNodes %>%
  rename(child_s = fromnode, parent_s = tonode)





#------------------------------------------------------------------------------
# FUNCTION: FIND ALL UPSTREAM REACH IDS FOR A GIVEN REACH (KUSKOKWIM)
#------------------------------------------------------------------------------
FindUpstreamReachID_Kusk <- function(ReachID) {
  
  TribStartRID <- kusk_edges$up_rid[kusk_edges$up_reachid == ReachID]
  
  
  
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
  upstream_reachids <- kusk_edges$up_reachid[match(upstream_rids, kusk_edges$up_rid)]
  return(upstream_reachids)
}


# ============================================================
# DIAGNOSTIC: MAP UPSTREAM REACHES FOR A GIVEN REACHID
# ============================================================

MapUpstream_Kusk <- function(ReachID) {

  # Check reachid exists
  if (!ReachID %in% kusk_edges$reachid) {
    stop(paste("ReachID", ReachID, "not found in kusk_edges"))
  }

  # Get upstream reaches
  upstream_ids <- FindUpstreamReachID_Kusk(ReachID)
  cat("ReachID:         ", ReachID, "\n")
  cat("Upstream reaches:", length(upstream_ids), "\n")

  # Build color vector
  colcode <- rep("gray80", nrow(kusk_edges))
  colcode[kusk_edges$reachid %in% upstream_ids] <- "red"      # upstream
  colcode[kusk_edges$reachid == ReachID]        <- "blue"     # seed reach

  # Line widths
  lwds <- rep(0.5, nrow(kusk_edges))
  lwds[kusk_edges$reachid %in% upstream_ids] <- 1.5
  lwds[kusk_edges$reachid == ReachID]        <- 3

  # Plot
  plot(st_geometry(kusk_basin),
       col    = "gray95",
       border = "gray40",
       main   = paste0("Upstream of ReachID: ", ReachID,
                       "\n(blue = seed, red = upstream, n = ", length(upstream_ids), ")"))
  plot(st_geometry(kusk_edges),
       col = colcode, lwd = lwds, add = TRUE)
}

# ============================================================
# USAGE — pass any reachid to inspect
# ============================================================

# Pick a random reachid to test

MapUpstream_Kusk(12100)

# Or pass a specific one
# MapUpstream_Kusk(12345)








library(sf)
library(dplyr)
library(here)

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------
yuk_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Yukon_edges.shp"),
                     quiet = TRUE)

yuk_basin <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Yukon_basin.shp"),
                     quiet = TRUE)


YukonNodes <- read.csv(
  here("Data","UpstreamReaches","yukon_noderelationships.csv"),
  stringsAsFactors = FALSE
)

YukonNetwork <- YukonNodes %>%
  rename(child_s = fromnode, parent_s = tonode)

#------------------------------------------------------------------------------
# FUNCTION: FIND ALL UPSTREAM REACH IDS FOR A GIVEN REACH (YUKON)
#------------------------------------------------------------------------------
FindUpstreamReachID_Yuk <- function(ReachID) {
  TribStartRID <- yuk_edges$up_rid[yuk_edges$reachid == ReachID]
  
  if (length(TribStartRID) != 1) {
    stop(paste("ReachID", ReachID, "does not resolve to a unique rid"))
  }
  
  TRIBindex <- YukonNetwork$child_s[YukonNetwork$rid == TribStartRID]
  
  ChildList  <- YukonNetwork$child_s[YukonNetwork$parent_s %in% TRIBindex]
  
  while (length(ChildList) > 0) {
    TRIBindex <- c(TRIBindex, ChildList)
    ChildList <- YukonNetwork$child_s[YukonNetwork$parent_s %in% ChildList]
  }
  
  upstream_rids     <- YukonNetwork$rid[match(TRIBindex, YukonNetwork$child_s)]
  
  upstream_reachids <- yuk_edges$reachid[match(upstream_rids, yuk_edges$up_rid)]
  return(upstream_reachids)
}


################################################################################
# DIAGNOSTIC: TEST UPSTREAM FUNCTION + EXPORT PNG MAP
# Usage: Set WATERSHED and TEST_REACHID, then source this block
################################################################################

# --- CONFIGURATION ---
WATERSHED    <- "Yukon"   # "Kusko" or "Yukon"
TEST_REACHID <- 16319    # Replace with your test reach ID

# --- RUN & PLOT ---
if (WATERSHED == "Kusko") {
  upstream_ids <- FindUpstreamReachID_Kusk(TEST_REACHID)
  edges_plot   <- kusk_edges
  basin_plot   <- kusk_basin
  reach_col    <- "up_reachid"   # column name used in Kusko edges
} else {
  upstream_ids <- FindUpstreamReachID_Yuk(TEST_REACHID)
  edges_plot   <- yuk_edges
  basin_plot   <- yuk_basin
  reach_col    <- "reachid"      # column name used in Yukon edges
}

cat("Test ReachID:    ", TEST_REACHID, "\n")
cat("Upstream reaches:", length(upstream_ids), "\n")

# Build color + line width vectors
colcode <- rep("gray80", nrow(edges_plot))
colcode[edges_plot[[reach_col]] %in% upstream_ids] <- "red"
colcode[edges_plot[[reach_col]] == TEST_REACHID]   <- "blue"

lwds <- rep(0.5, nrow(edges_plot))
lwds[edges_plot[[reach_col]] %in% upstream_ids] <- 1.5
lwds[edges_plot[[reach_col]] == TEST_REACHID]   <- 3

# Export PNG
out_png <- here(paste0("Upstream_Test_", WATERSHED, "_", TEST_REACHID, ".png"))
png(out_png, width = 2400, height = 1800, res = 200)

plot(st_geometry(basin_plot),
     col    = "gray95",
     border = "gray40",
     main   = paste0(WATERSHED, " — Upstream of ReachID: ", TEST_REACHID,
                     "\n(blue = seed reach, red = upstream, n = ",
                     length(upstream_ids), ")"))
plot(st_geometry(edges_plot),
     col = colcode, lwd = lwds, add = TRUE)

dev.off()
cat("PNG saved to:", out_png, "\n")

