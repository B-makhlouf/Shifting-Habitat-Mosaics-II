library(sf)
library(dplyr)
library(readr)

#### Yukon 

yuk_basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

YukonNodes <- read.csv(here("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/yukon_noderelationships.csv"), header = TRUE, stringsAsFactors = FALSE)
YukonNetwork <- YukonNodes %>% rename(child_s = fromnode, parent_s = tonode)


### Function to find upstream reaches 

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

library(sf)
library(dplyr)
library(tidyr)

#############################
## Load Yukon reach shapefile
#############################

yuk_edges <- st_read(
  "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/YukonReachbaseCompleteRedone.shp",
  quiet = TRUE
)

#############################
## Identify Reachbase values
#############################

Reachbase_levels <- sort(unique(yuk_edges$Reachbase))
Reachbase_levels <- Reachbase_levels[Reachbase_levels > 0]

#############################
## Create full reach × Reachbase table
#############################

Yukon_StreamOrderGroups <- expand.grid(
  ReachID   = yuk_edges$reachid,
  Reachbase = Reachbase_levels,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  arrange(Reachbase, ReachID) %>%
  mutate(GroupID = 0L)

#############################
## Assign tributary groups by Reachbase
#############################

for (rb in Reachbase_levels) {
  
  message("Processing Reachbase = ", rb)
  
  # Tributary mouths at this stream order
  MouthReaches <- yuk_edges$reachid[yuk_edges$Reachbase == rb]
  
  group_counter <- 1L
  
  for (rid in MouthReaches) {
    
    upstream_reaches <- FindUpstreamReachID(rid)
    upstream_reaches <- unique(c(rid, upstream_reaches))
    
    idx <- Yukon_StreamOrderGroups$Reachbase == rb &
      Yukon_StreamOrderGroups$ReachID %in% upstream_reaches &
      Yukon_StreamOrderGroups$GroupID == 0
    
    if (any(idx)) {
      Yukon_StreamOrderGroups$GroupID[idx] <- group_counter
      group_counter <- group_counter + 1L
    }
  }
}

###############################################################################

# Save the CSV as StrOrdGroup_Yukon.csv
write_csv(Yukon_StreamOrderGroups, "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameGroupStrOrd/StrOrdGroup_Yukon.csv")
