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

# 
# # ============================================================
# # DIAGNOSTIC: MAP UPSTREAM REACHES FOR A GIVEN REACHID
# # ============================================================
# 
# MapUpstream_Kusk <- function(ReachID) {
#   
#   # Check reachid exists
#   if (!ReachID %in% kusk_edges$reachid) {
#     stop(paste("ReachID", ReachID, "not found in kusk_edges"))
#   }
#   
#   # Get upstream reaches
#   upstream_ids <- FindUpstreamReachID_Kusk(ReachID)
#   cat("ReachID:         ", ReachID, "\n")
#   cat("Upstream reaches:", length(upstream_ids), "\n")
#   
#   # Build color vector
#   colcode <- rep("gray80", nrow(kusk_edges))
#   colcode[kusk_edges$reachid %in% upstream_ids] <- "red"      # upstream
#   colcode[kusk_edges$reachid == ReachID]        <- "blue"     # seed reach
#   
#   # Line widths
#   lwds <- rep(0.5, nrow(kusk_edges))
#   lwds[kusk_edges$reachid %in% upstream_ids] <- 1.5
#   lwds[kusk_edges$reachid == ReachID]        <- 3
#   
#   # Plot
#   plot(st_geometry(kusk_basin),
#        col    = "gray95",
#        border = "gray40",
#        main   = paste0("Upstream of ReachID: ", ReachID,
#                        "\n(blue = seed, red = upstream, n = ", length(upstream_ids), ")"))
#   plot(st_geometry(kusk_edges),
#        col = colcode, lwd = lwds, add = TRUE)
# }
# 
# # ============================================================
# # USAGE — pass any reachid to inspect
# # ============================================================
# 
# # Pick a random reachid to test
# 
# MapUpstream_Kusk(12344)
# 
# # Or pass a specific one
# # MapUpstream_Kusk(12345)
# 
# 
# 
# 
# 
# 
# 
# #------------------------------------------------------------------------------
# # STEP 1: SPATIAL JOIN — Transfer TouchingMs to kusk_edges (upstream network)
# #------------------------------------------------------------------------------
# # Ensure matching CRS
# kusk_edges_working <- st_transform(kusk_edges_working, st_crs(kusk_edges))
# 
# # Join TouchingMs from working shapefile to upstream shapefile
# kusk_edges <- kusk_edges %>%
#   st_join(
#     kusk_edges_working %>% select(TouchingMs),
#     join   = st_equals,
#     left   = TRUE
#   )
# 
# # Identify mainstem-touching mouth segments
# mouth_segments <- kusk_edges %>%
#   filter(TouchingMs == 1) %>%
#   pull(reachid)
# 
# cat("Found", length(mouth_segments), "mainstem-touching mouth segments\n")
# 
# #------------------------------------------------------------------------------
# # STEP 2: COLLECT UPSTREAM REACHES FOR EACH MOUTH & ASSIGN GROUP IDS
# #------------------------------------------------------------------------------
# # Initialize up_grp column
# kusk_edges$up_grp <- NA_integer_
# 
# for (i in seq_along(mouth_segments)) {
#   mouth_id   <- mouth_segments[i]
#   upstream   <- FindUpstreamReachID_Kusk(mouth_id)
#   all_in_group <- unique(c(mouth_id, upstream))
#   
#   # Assign group — only to reaches not yet claimed
#   unclaimed <- kusk_edges$reachid %in% all_in_group & is.na(kusk_edges$up_grp)
#   kusk_edges$up_grp[unclaimed] <- i
#   
#   if (i %% 50 == 0) cat("  Processed", i, "/", length(mouth_segments), "mouths\n")
# }
# 
# cat("Processed all", length(mouth_segments), "mouths\n")
# cat("Reaches assigned to a group:",
#     sum(!is.na(kusk_edges$up_grp)), "/", nrow(kusk_edges), "\n")
# cat("Unique tributary groups:",
#     length(unique(na.omit(kusk_edges$up_grp))), "\n")
# 
# #------------------------------------------------------------------------------
# # STEP 3: SPATIAL JOIN — Transfer up_grp back to Kusko_edges.shp
# #------------------------------------------------------------------------------
# # Keep only up_grp + geometry from kusk_edges to avoid column clashes
# group_sf <- kusk_edges %>%
#   select(up_grp)
# 
# # Drop any existing up_grp column from working shapefile before joining
# kusk_edges_working <- kusk_edges_working %>%
#   select(-any_of("up_grp"))
# 
# kusk_edges_grouped <- kusk_edges_working %>%
#   st_join(group_sf, join = st_equals, left = TRUE)
# 
# # If the join created duplicates (up_grp.x / .y), clean them up
# ug_cols <- grep("^up_grp", names(kusk_edges_grouped), value = TRUE)
# if (length(ug_cols) > 1) {
#   # Keep the one from the join (typically .y), drop the other
#   kusk_edges_grouped <- kusk_edges_grouped %>%
#     mutate(up_grp = coalesce(up_grp.y, up_grp.x)) %>%
#     select(-any_of(c("up_grp.x", "up_grp.y")))
# }
# 
# 
# kusk_edges_working$up
# #------------------------------------------------------------------------------
# # STEP 3b: SAVE
# #------------------------------------------------------------------------------
# out_path <- here("Data","Spatial Data","AnalysisShapefiles","Kusko_new3.shp")
# 
# st_write(
#   kusk_edges_working, 
#   out_path,
#   quiet = TRUE
# )
# 
# cat("Saved:", out_path, "\n")
# 
# #------------------------------------------------------------------------------
# # STEP 4: INTERACTIVE LEAFLET MAP — Click to identify groups
# #------------------------------------------------------------------------------
# library(leaflet)
# library(RColorBrewer)
# library(htmlwidgets)
# 
# # Transform to WGS84 for leaflet
# kusk_leaf   <- kusk_edges_grouped %>% st_transform(4326)
# basin_leaf  <- kusk_basin %>% st_transform(4326)
# 
# # Assign colors: one per up_grp, gray for unassigned
# n_groups <- length(unique(na.omit(kusk_leaf$up_grp)))
# palette  <- colorFactor(
#   palette = sample(colors(distinct = TRUE), n_groups),
#   domain  = na.omit(unique(kusk_leaf$up_grp))
# )
# 
# kusk_leaf$color <- ifelse(
#   is.na(kusk_leaf$up_grp),
#   "#999999",
#   palette(kusk_leaf$up_grp)
# )
# 
# # Flag mouth segments
# kusk_leaf$is_mouth <- kusk_leaf$reachid %in% mouth_segments
# 
# # Build popup labels
# kusk_leaf$label <- paste0(
#   "ReachID: ",    kusk_leaf$reachid, "<br>",
#   "up_grp: ",     ifelse(is.na(kusk_leaf$up_grp), "None",
#                          kusk_leaf$up_grp), "<br>",
#   "Mouth segment: ", ifelse(kusk_leaf$is_mouth, "YES", "no")
# )
# 
# # Create map
# m <- leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   # Basin outline
#   addPolygons(data = basin_leaf,
#               fillColor = "transparent", color = "black",
#               weight = 1, opacity = 0.4) %>%
#   # Unassigned reaches (gray, thin)
#   addPolylines(data  = kusk_leaf %>% filter(is.na(up_grp)),
#                color = "#CCCCCC", weight = 1, opacity = 0.4,
#                popup = ~label, group = "Unassigned") %>%
#   # Grouped reaches (colored by up_grp)
#   addPolylines(data  = kusk_leaf %>% filter(!is.na(up_grp)),
#                color = ~color, weight = 2, opacity = 0.8,
#                popup = ~label, group = "Tributary Groups") %>%
#   # Mouth segments highlighted
#   addPolylines(data  = kusk_leaf %>% filter(is_mouth),
#                color = "black", weight = 4, opacity = 1,
#                popup = ~label, group = "Mouth Segments") %>%
#   # Layer control
#   addLayersControl(
#     overlayGroups = c("Tributary Groups", "Mouth Segments", "Unassigned"),
#     options       = layersControlOptions(collapsed = FALSE)
#   )
# 
# # Display
# m
# 
# # Save to HTML
# saveWidget(m, here("UpstreamGroup_Map.html"), selfcontained = TRUE)
# cat("Interactive map saved to:", here("UpstreamGroup_Map.html"), "\n")
# 
# 
# 
# ################################################################################
# # GROUP UPSTREAM REACHES BY MAINSTEM-TOUCHING SEGMENTS — YUKON
# #
# # Workflow (mirrors Kuskokwim script):
# # 1. Spatial join TouchingMs from Yukon_edges3.shp → Yukon_edges_up.shp
# # 2. For each segment where TouchingMs == 1, collect all upstream reaches
# # 3. Assign a up_grp ID to each upstream reach
# # 4. Spatial join up_grp back to Yukon_edges3.shp → save as Yukon_new.shp
# ################################################################################
# 
# library(sf)
# library(dplyr)
# library(here)
# 
# #------------------------------------------------------------------------------
# # LOAD DATA
# #------------------------------------------------------------------------------
# yuk_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Yukon_edges_up.shp"),
#                      quiet = TRUE)
# 
# yuk_basin <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Yukon_basin.shp"),
#                      quiet = TRUE)
# 
# # Working shapefile with TouchingMs column (equivalent of Kusko_edges3.shp)
# # UPDATE THIS FILENAME if yours is named differently
# yuk_edges_working <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Yukon_edges.shp"),
#                              quiet = TRUE)
# 
# YukonNodes <- read.csv(
#   here("Data","UpstreamReaches","yukon_noderelationships.csv"),
#   stringsAsFactors = FALSE
# )
# 
# YukonNetwork <- YukonNodes %>%
#   rename(child_s = fromnode, parent_s = tonode)
# 
# #------------------------------------------------------------------------------
# # FUNCTION: FIND ALL UPSTREAM REACH IDS FOR A GIVEN REACH (YUKON)
# #------------------------------------------------------------------------------
# FindUpstreamReachID_Yuk <- function(ReachID) {
#   TribStartRID <- yuk_edges$rid[yuk_edges$reachid == ReachID]
#   if (length(TribStartRID) != 1) {
#     stop(paste("ReachID", ReachID, "does not resolve to a unique rid"))
#   }
#   TRIBindex <- YukonNetwork$child_s[YukonNetwork$rid == TribStartRID]
#   ChildList  <- YukonNetwork$child_s[YukonNetwork$parent_s %in% TRIBindex]
#   while (length(ChildList) > 0) {
#     TRIBindex <- c(TRIBindex, ChildList)
#     ChildList <- YukonNetwork$child_s[YukonNetwork$parent_s %in% ChildList]
#   }
#   upstream_rids     <- YukonNetwork$rid[match(TRIBindex, YukonNetwork$child_s)]
#   upstream_reachids <- yuk_edges$reachid[match(upstream_rids, yuk_edges$rid)]
#   return(upstream_reachids)
# }
# 
# #------------------------------------------------------------------------------
# # STEP 1: SPATIAL JOIN — Transfer TouchingMs to yuk_edges (upstream network)
# #------------------------------------------------------------------------------
# # Ensure matching CRS
# yuk_edges_working <- st_transform(yuk_edges_working, st_crs(yuk_edges))
# 
# # Join TouchingMs from working shapefile to upstream shapefile
# yuk_edges <- yuk_edges %>%
#   st_join(
#     yuk_edges_working %>% select(TouchingMs),
#     join   = st_equals,
#     left   = TRUE
#   )
# 
# # Identify mainstem-touching mouth segments
# mouth_segments <- yuk_edges %>%
#   filter(TouchingMs == 1) %>%
#   pull(reachid)
# 
# cat("Found", length(mouth_segments), "mainstem-touching mouth segments\n")
# 
# #------------------------------------------------------------------------------
# # STEP 2: COLLECT UPSTREAM REACHES FOR EACH MOUTH & ASSIGN GROUP IDS
# #------------------------------------------------------------------------------
# # Initialize up_grp column
# yuk_edges$up_grp <- NA_integer_
# 
# for (i in seq_along(mouth_segments)) {
#   mouth_id   <- mouth_segments[i]
#   upstream   <- FindUpstreamReachID_Yuk(mouth_id)
#   all_in_group <- unique(c(mouth_id, upstream))
#   
#   # Assign group — only to reaches not yet claimed
#   unclaimed <- yuk_edges$reachid %in% all_in_group & is.na(yuk_edges$up_grp)
#   yuk_edges$up_grp[unclaimed] <- i
#   
#   if (i %% 50 == 0) cat("  Processed", i, "/", length(mouth_segments), "mouths\n")
# }
# 
# cat("Processed all", length(mouth_segments), "mouths\n")
# cat("Reaches assigned to a group:",
#     sum(!is.na(yuk_edges$up_grp)), "/", nrow(yuk_edges), "\n")
# cat("Unique tributary groups:",
#     length(unique(na.omit(yuk_edges$up_grp))), "\n")
# 
# #------------------------------------------------------------------------------
# # STEP 3: SPATIAL JOIN — Transfer up_grp back to Yukon_edges3.shp
# #------------------------------------------------------------------------------
# # Keep only up_grp + geometry from yuk_edges to avoid column clashes
# group_sf <- yuk_edges %>%
#   select(up_grp)
# 
# # Drop any existing up_grp column from working shapefile before joining
# yuk_edges_working <- yuk_edges_working %>%
#   select(-any_of("up_grp"))
# 
# yuk_edges_grouped <- yuk_edges_working %>%
#   st_join(group_sf, join = st_equals, left = TRUE)
# 
# # If the join created duplicates (up_grp.x / .y), clean them up
# ug_cols <- grep("^up_grp", names(yuk_edges_grouped), value = TRUE)
# if (length(ug_cols) > 1) {
#   yuk_edges_grouped <- yuk_edges_grouped %>%
#     mutate(up_grp = coalesce(up_grp.y, up_grp.x)) %>%
#     select(-any_of(c("up_grp.x", "up_grp.y")))
# }
# 
# #------------------------------------------------------------------------------
# # STEP 3b: SAVE
# #------------------------------------------------------------------------------
# out_path <- here("Data","Spatial Data","AnalysisShapefiles","Yukon_new.shp")
# 
# st_write(
#   st_zm(yuk_edges_grouped, drop = TRUE, what = "ZM"),
#   out_path,
#   quiet = TRUE
# )
# 
# cat("Saved:", out_path, "\n")
# 
# #------------------------------------------------------------------------------
# # STEP 4: INTERACTIVE LEAFLET MAP — Click to identify groups
# #------------------------------------------------------------------------------
# library(leaflet)
# library(RColorBrewer)
# library(htmlwidgets)
# 
# # Transform to WGS84 for leaflet
# yuk_leaf   <- yuk_edges_grouped %>% st_transform(4326)
# basin_leaf <- yuk_basin %>% st_transform(4326)
# 
# # Assign colors: one per up_grp, gray for unassigned
# n_groups <- length(unique(na.omit(yuk_leaf$up_grp)))
# palette  <- colorFactor(
#   palette = sample(colors(distinct = TRUE), n_groups),
#   domain  = na.omit(unique(yuk_leaf$up_grp))
# )
# 
# yuk_leaf$color <- ifelse(
#   is.na(yuk_leaf$up_grp),
#   "#999999",
#   palette(yuk_leaf$up_grp)
# )
# 
# # Flag mouth segments
# yuk_leaf$is_mouth <- yuk_leaf$reachid %in% mouth_segments
# 
# # Build popup labels
# yuk_leaf$label <- paste0(
#   "ReachID: ",    yuk_leaf$reachid, "<br>",
#   "up_grp: ",     ifelse(is.na(yuk_leaf$up_grp), "None",
#                          yuk_leaf$up_grp), "<br>",
#   "Mouth segment: ", ifelse(yuk_leaf$is_mouth, "YES", "no")
# )
# 
# # Create map
# m <- leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   # Basin outline
#   addPolygons(data = basin_leaf,
#               fillColor = "transparent", color = "black",
#               weight = 1, opacity = 0.4) %>%
#   # Unassigned reaches (gray, thin)
#   addPolylines(data  = yuk_leaf %>% filter(is.na(up_grp)),
#                color = "#CCCCCC", weight = 1, opacity = 0.4,
#                popup = ~label, group = "Unassigned") %>%
#   # Grouped reaches (colored by up_grp)
#   addPolylines(data  = yuk_leaf %>% filter(!is.na(up_grp)),
#                color = ~color, weight = 2, opacity = 0.8,
#                popup = ~label, group = "Tributary Groups") %>%
#   # Mouth segments highlighted
#   addPolylines(data  = yuk_leaf %>% filter(is_mouth),
#                color = "black", weight = 4, opacity = 1,
#                popup = ~label, group = "Mouth Segments") %>%
#   # Layer control
#   addLayersControl(
#     overlayGroups = c("Tributary Groups", "Mouth Segments", "Unassigned"),
#     options       = layersControlOptions(collapsed = FALSE)
#   )
# 
# # Display
# m
# 
# # Save to HTML
# saveWidget(m, here("YukonUpstreamGroup_Map.html"), selfcontained = TRUE)
# cat("Interactive map saved to:", here("YukonUpstreamGroup_Map.html"), "\n")