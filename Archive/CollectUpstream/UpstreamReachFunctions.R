################################################################################
# UpstreamReachFunctions.R
#
# Find all UPSTREAM reaches for a given reach, and map them (upstream = red),
# for the Kuskokwim and Yukon networks in THIS repo.
#
# WHY THIS REPLACES THE OLD NODE-TABLE WALK
# -----------------------------------------
# The old workflow walked Data/UpstreamReaches/{kusko,yukon}_noderelationships.csv
# (rid, fromnode, tonode), joining to the edge shapefile by `rid`. That node
# table no longer matches the current edge shapefiles
# (Kusko_edges_geomorphAdded.shp / Yukon_edges_geomorphAdded.shp): the shapefile
# `rid`/`reachid` were renumbered, so the old `rid` is a permutation of the new
# reaches (verified: 0/500 adjacency match against the new geometry). Joining the
# old CSV therefore attaches the wrong topology to the wrong line.
#
# The fix: topology is rebuilt DIRECTLY from each edge shapefile's geometry and
# stored as a sidecar table keyed to THAT shapefile's own `reachid`:
#
#   Data/UpstreamReaches/kusko_upstream_topology.csv
#   Data/UpstreamReaches/yukon_upstream_topology.csv
#     reachid       this reach
#     down_reachid  the reach immediately downstream (-1 at the basin outlet)
#     strahler      Str_Order
#     n_upstream    number of reaches upstream of this reach (precomputed check)
#     is_outlet     1 for the basin-outlet reach
#     component     connected-component id (0 = main network)
#
# Upstream-of(R) = every reach whose downstream path passes through R.
# Validated: from the outlet the walk reaches the whole network, every lowest-
# order reach has 0 upstream, upstream-set size rises monotonically with stream
# order, and rendered maps show contiguous catchments draining to the seed.
#
# To regenerate these tables (e.g. after editing a shapefile), run
#   Code/CollectUpstream/build_upstream_topology.R
################################################################################

library(sf)
library(here)

# ---- LOAD -------------------------------------------------------------------
SHP_DIR <- here("Data", "Spatial Data", "AnalysisShapefiles")
TOPO_DIR <- here("Data", "UpstreamReaches")

kusk_edges <- st_read(file.path(SHP_DIR, "Kusko_edges_geomorphAdded.shp"), quiet = TRUE)
kusk_basin <- st_read(file.path(SHP_DIR, "Kusko_basin.shp"),               quiet = TRUE)
yuk_edges  <- st_read(file.path(SHP_DIR, "Yukon_edges_geomorphAdded.shp"), quiet = TRUE)
yuk_basin  <- st_read(file.path(SHP_DIR, "Yukon_basin.shp"),               quiet = TRUE)

kusk_topo <- read.csv(file.path(TOPO_DIR, "kusko_upstream_topology.csv"), stringsAsFactors = FALSE)
yuk_topo  <- read.csv(file.path(TOPO_DIR, "yukon_upstream_topology.csv"), stringsAsFactors = FALSE)

kusk_edges$reachid <- as.integer(round(kusk_edges$reachid))
yuk_edges$reachid  <- as.integer(round(yuk_edges$reachid))

# children index: for each reach, the reaches that flow directly into it
kusk_children <- split(kusk_topo$reachid, kusk_topo$down_reachid)
yuk_children  <- split(yuk_topo$reachid,  yuk_topo$down_reachid)

# ---- GENERIC WALK -----------------------------------------------------------
.find_upstream <- function(ReachID, topo, children, include_self = FALSE) {
  if (!ReachID %in% topo$reachid) stop(paste("ReachID", ReachID, "not found"))
  result <- integer(0)
  frontier <- children[[as.character(ReachID)]]
  while (length(frontier) > 0) {
    result   <- c(result, frontier)
    frontier <- unlist(children[as.character(frontier)], use.names = FALSE)
  }
  result <- unique(result)
  if (include_self) result <- unique(c(ReachID, result))
  result
}

FindUpstreamReachID_Kusk <- function(ReachID, include_self = FALSE)
  .find_upstream(ReachID, kusk_topo, kusk_children, include_self)

FindUpstreamReachID_Yuk <- function(ReachID, include_self = FALSE)
  .find_upstream(ReachID, yuk_topo, yuk_children, include_self)

# ---- GENERIC MAP (upstream = red, seed = blue) ------------------------------
.map_upstream <- function(ReachID, edges, basin, topo, children, basin_name,
                          save_png = TRUE) {
  if (!ReachID %in% edges$reachid) stop(paste("ReachID", ReachID, "not in edges"))
  upstream_ids <- .find_upstream(ReachID, topo, children)
  cat(basin_name, "ReachID:", ReachID, "| upstream reaches:", length(upstream_ids), "\n")

  colcode <- rep("gray80", nrow(edges))
  colcode[edges$reachid %in% upstream_ids] <- "red"
  colcode[edges$reachid == ReachID]        <- "blue"
  lwds <- rep(0.4, nrow(edges))
  lwds[edges$reachid %in% upstream_ids] <- 1.2
  lwds[edges$reachid == ReachID]        <- 3

  draw <- function() {
    plot(st_geometry(basin), col = "gray95", border = "gray40",
         main = paste0(basin_name, " - Upstream of ReachID: ", ReachID,
                       "\n(blue = seed, red = upstream, n = ", length(upstream_ids), ")"))
    plot(st_geometry(edges), col = colcode, lwd = lwds, add = TRUE)
  }
  if (save_png) {
    out_dir <- here("Figures", "Maps", "UpstreamTests")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    out_png <- file.path(out_dir, paste0("Upstream_", basin_name, "_", ReachID, ".png"))
    png(out_png, width = 2400, height = 1800, res = 200); draw(); dev.off()
    cat("PNG saved to:", out_png, "\n")
    invisible(out_png)
  } else { draw(); invisible(upstream_ids) }
}

MapUpstream_Kusk <- function(ReachID, save_png = TRUE)
  .map_upstream(ReachID, kusk_edges, kusk_basin, kusk_topo, kusk_children, "Kusko", save_png)

MapUpstream_Yuk <- function(ReachID, save_png = TRUE)
  .map_upstream(ReachID, yuk_edges, yuk_basin, yuk_topo, yuk_children, "Yukon", save_png)

# ============================================================
# USAGE
# ============================================================
# MapUpstream_Kusk(7127)                 # saves PNG to Figures/Maps/UpstreamTests/
# MapUpstream_Yuk(1176, save_png = FALSE)   # draw to screen
# ups <- FindUpstreamReachID_Kusk(7127)     # vector of upstream reachids
