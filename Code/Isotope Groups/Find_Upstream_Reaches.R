# Code to define the function for determining upstream reaches
# Adapted from FindUpstreamReaches_Kusko_NEW in code/from tim/FindUpstreamReaches_Kusko_NEW.R
# Written by: Ben Makhlouf 12/2023

library(sf)
library(tidyverse)
library(here)

################################################################################
############## YUKON
################################################################################


#Shapefile
yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc_withprobs.shp")
#Basin
yuk_basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

YukonNodes <- read.csv(here("data/reporting groups/yukon/yukon_noderelationships.csv"),header=T,stringsAsFactors = F)#Node to reach relationships
YukonNetwork<-YukonNodes %>% rename(child_s=fromnode,parent_s=tonode)#rename(child_s=fromnode,parent_s=tonode) #Rename as child and parent nodes 

FindUpstreamReachID <- function(ReachID){
  TribStartRID <- yuk_edges$rid[which(yuk_edges$reachid==ReachID)]
  
  TRIBindex<-c()
  StartParent<-YukonNetwork$parent_s[which(YukonNetwork$rid==TribStartRID)]
  StartChild<-YukonNetwork$child_s[which(YukonNetwork$rid==TribStartRID)]
  TRIBindex<-c(TRIBindex,StartChild)
  ChildList<-YukonNetwork$child_s[which(YukonNetwork$parent_s==StartChild)]
  while(length(ChildList)>0){
    TRIBindex <- c(TRIBindex,ChildList)
    #ParentIndex<-YukonNetwork$parent_s[which(YukonNetwork$parent_s %in% ChildList)]
    ChildList <- YukonNetwork$child_s[which(YukonNetwork$parent_s %in% ChildList)]
  }
  print(length(TRIBindex))
  
  TribSegments<-yuk_edges$reachid[match(YukonNetwork$rid[match(TRIBindex,YukonNetwork$child_s)],yuk_edges$rid)]
  return(TribSegments)
}

# To test to see if it works 
if(T){
  TribReachIDs <- FindUpstreamReachID(14438)
  
  quartz()
  par(mfrow=c(1,1))
  nclr <- 9
  
  plotvar <- rep(0,nrow(yuk_edges))
  plotvar[match(TribReachIDs,yuk_edges$reachid)] <- 1
  colcode <- rep('gray',nrow(yuk_edges))
  colcode[plotvar==0] <- 'gray60'
  colcode[plotvar==1] <- 'red'
  plot(st_geometry(yuk_basin), col = "gray", lwd=0.1)#border=NA)#,  add=TRUE)
  plot(st_geometry(yuk_edges), col = colcode, pch=16, axes = F,add=TRUE, lwd=1)
}

################################################################################
################ Kuskokwim 
################################################################################

#Shapefile
kuskokwim_shapefile <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")

# Load nodes for all tributaries
KuskoNodes <- read.csv(here('data', 'reporting groups', 'kusko', 'kusko_noderelationships.csv'), header = TRUE, stringsAsFactors = FALSE)

# Load Kuskokwim Shapefile
## Must be this version of the shapefile!
kuskokwim_shapefile <- st_read('/Users/benjaminmakhlouf/Downloads/kusko_network_20210717/kusko_edges_20210717.shp')

# Rename fromnode to child_s and tonode to parent_s
KuskoNetwork <- KuskoNodes %>% rename(child_s = fromnode, parent_s = tonode)

# Function to find upstream reach IDs
#' @param ReachID Reach ID value for which upstream reaches need to be determined
#' @return Vector of upstream reach IDs
#' 
FindUpstreamReachID <- function(ReachID) {
  # Select the reach ID from the shapefile that corresponds to the ReachID value passed to the function argument
  TribStartRID <- kuskokwim_shapefile$rid[which(kuskokwim_shapefile$reachid == ReachID)]
  
  # Create an empty list called tributary_indices
  tributary_indices <- c()
  
  # Find the parent reach ID value for the starting tributary
  StartParent <- KuskoNetwork$parent_s[which(KuskoNetwork$rid == TribStartRID)]
  
  # Find the first child reach ID value for the starting tributary
  StartChild <- KuskoNetwork$child_s[which(KuskoNetwork$rid == TribStartRID)]
  
  # Concatenate these together
  tributary_indices <- c(tributary_indices, StartChild)
  
  # Go find all child reach ID values which have a parent value of the specified starting child value
  ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s == StartChild)]
  
  # As long as there are some "child" tributaries
  while (length(ChildList) > 0) {
    # Add the new child values to the existing tributary_indices
    tributary_indices <- c(tributary_indices, ChildList)
    
    # Find the next set of child values
    ChildList <- KuskoNetwork$child_s[which(KuskoNetwork$parent_s %in% ChildList)]
  }
  
  # Find the indices of elements in KuskoNetwork$child_s that match values in tributary_indices
  indices_in_child_s <- match(tributary_indices, KuskoNetwork$child_s)
  
  # Extract the values from KuskoNetwork$rid corresponding to the matched indices
  rid_values <- KuskoNetwork$rid[indices_in_child_s]
  
  # Find the indices of the values obtained in Step 2 within kuskokwim_shapefile$rid
  indices_in_shapefile <- match(rid_values, kuskokwim_shapefile$rid)
  
  # Extract the values from kuskokwim_shapefile$reachid using the indices obtained in Step 3
  upstream_reach_ids <- kuskokwim_shapefile$reachid[indices_in_shapefile]
  
  # Return the resulting vector upstream_reach_ids
  return(upstream_reach_ids)
}
