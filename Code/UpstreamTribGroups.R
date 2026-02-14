################################################################################
# GROUP UPSTREAM REACHES BY MAINSTEM-TOUCHING SEGMENTS
# 
# Workflow:
#   1. Spatial join TouchingMs from Kusko_edges.shp → Kusko_upstream.shp
#   2. For each segment where TouchingMs == 1, collect all upstream reaches
#   3. Assign a upstream_group ID to each upstream reach
#   4. Spatial join upstream_group back to Kusko_edges.shp for downstream use
################################################################################

library(sf)
library(dplyr)
library(here)

#------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------

kusk_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_upstream.shp"),
                      quiet = TRUE)
kusk_basin <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_basin.shp"),
                      quiet = TRUE)
kusk_edges_working <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"),
                              quiet = TRUE)

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
# STEP 1: SPATIAL JOIN — Transfer TouchingMs to kusk_edges (upstream network)
#------------------------------------------------------------------------------

# Ensure matching CRS
kusk_edges_working <- st_transform(kusk_edges_working, st_crs(kusk_edges))

# Join TouchingMs from working shapefile to upstream shapefile
kusk_edges <- kusk_edges %>%
  st_join(
    kusk_edges_working %>% select(TouchingMs),
    join = st_equals,
    left = TRUE
  )

# Identify mainstem-touching mouth segments
mouth_segments <- kusk_edges %>%
  filter(TouchingMs == 1) %>%
  pull(reachid)

cat("Found", length(mouth_segments), "mainstem-touching mouth segments\n")

#------------------------------------------------------------------------------
# STEP 2: COLLECT UPSTREAM REACHES FOR EACH MOUTH & ASSIGN GROUP IDS
#------------------------------------------------------------------------------

# Initialize upstream_group column
kusk_edges$upstream_group <- NA_integer_

for (i in seq_along(mouth_segments)) {
  
  mouth_id <- mouth_segments[i]
  upstream  <- FindUpstreamReachID_Kusk(mouth_id)
  all_in_group <- unique(c(mouth_id, upstream))
  
  # Assign group — only to reaches not yet claimed
  unclaimed <- kusk_edges$reachid %in% all_in_group & is.na(kusk_edges$upstream_group)
  kusk_edges$upstream_group[unclaimed] <- i
  
  if (i %% 50 == 0) cat("  Processed", i, "/", length(mouth_segments), "mouths\n")
}

cat("Processed all", length(mouth_segments), "mouths\n")
cat("Reaches assigned to a group:", sum(!is.na(kusk_edges$upstream_group)),
    "/", nrow(kusk_edges), "\n")
cat("Unique tributary groups:", length(unique(na.omit(kusk_edges$upstream_group))), "\n")

#------------------------------------------------------------------------------
# STEP 3: SPATIAL JOIN — Transfer upstream_group back to Kusko_edges.shp
#------------------------------------------------------------------------------

kusk_edges_grouped <- kusk_edges_working %>%
  st_join(
    kusk_edges %>% select(upstream_group),
    join = st_equals,
    left = TRUE
  )

#------------------------------------------------------------------------------
# STEP 3b: SAVE — Overwrite Kusko_edges.shp with new upstream_group attribute
#------------------------------------------------------------------------------

st_write(
  kusk_edges_grouped,
  here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"),
  delete_dsn = TRUE,
  quiet = TRUE
)
cat("Saved Kusko_edges.shp with upstream_group attribute\n")

#------------------------------------------------------------------------------
# STEP 4: INTERACTIVE LEAFLET MAP — Click to identify groups
#------------------------------------------------------------------------------

library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

# Transform to WGS84 for leaflet — use the WORKING shapefile with groups joined back
kusk_leaf <- kusk_edges_grouped %>% st_transform(4326)
basin_leaf <- kusk_basin %>% st_transform(4326)

# Assign colors: one per upstream_group, gray for unassigned
n_groups <- length(unique(na.omit(kusk_leaf$upstream_group)))
palette  <- colorFactor(
  palette = sample(colors(distinct = TRUE), n_groups),
  domain  = na.omit(unique(kusk_leaf$upstream_group))
)

kusk_leaf$color <- ifelse(
  is.na(kusk_leaf$upstream_group),
  "#999999",
  palette(kusk_leaf$upstream_group)
)

# Flag mouth segments — check if TouchingMs exists on working shapefile, otherwise use mouth_segments list
kusk_leaf$is_mouth <- kusk_leaf$reachid %in% mouth_segments

# Build popup labels
kusk_leaf$label <- paste0(
  "<b>ReachID:</b> ", kusk_leaf$reachid, "<br>",
  "<b>upstream_group:</b> ", ifelse(is.na(kusk_leaf$upstream_group), "None", kusk_leaf$upstream_group), "<br>",
  "<b>Mouth segment:</b> ", ifelse(kusk_leaf$is_mouth, "YES", "no")
)

# Create map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Basin outline
  addPolygons(data = basin_leaf, fillColor = "transparent",
              color = "black", weight = 1, opacity = 0.4) %>%
  
  # Unassigned reaches (gray, thin)
  addPolylines(data = kusk_leaf %>% filter(is.na(upstream_group)),
               color = "#CCCCCC", weight = 1, opacity = 0.4,
               popup = ~label, group = "Unassigned") %>%
  
  # Grouped reaches (colored by upstream_group)
  addPolylines(data = kusk_leaf %>% filter(!is.na(upstream_group)),
               color = ~color, weight = 2, opacity = 0.8,
               popup = ~label, group = "Tributary Groups") %>%
  
  # Mouth segments highlighted
  addPolylines(data = kusk_leaf %>% filter(is_mouth),
               color = "black", weight = 4, opacity = 1,
               popup = ~label, group = "Mouth Segments") %>%
  
  # Layer control
  addLayersControl(
    overlayGroups = c("Tributary Groups", "Mouth Segments", "Unassigned"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Display
m

# Optionally save to HTML
saveWidget(m, here("UpstreamGroup_Map.html"), selfcontained = TRUE)
cat("Interactive map saved to:", here("UpstreamGroup_Map.html"), "\n")