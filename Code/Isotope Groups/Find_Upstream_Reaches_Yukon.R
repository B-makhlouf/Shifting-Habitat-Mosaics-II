# Code to define the function for determining upstream reaches
# Adapted from FindUpstreamReaches_Kusko_NEW in code/from tim/FindUpstreamReaches_Kusko_NEW.R
# Written by: Ben Makhlouf 12/2023


library(sf)
library(tidyverse)
library(here)

#Shapefile
Yukon_shapefile<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc_withprobs.shp")
#Basin
YukonBasin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")
#Node to reach relationships
YukonNodes <- read.csv(here("data/reporting groups/yukon/yukon_noderelationships.csv"),header=T,stringsAsFactors = F)
#Rename as child and parent nodes 
YukonNetwork<-YukonNodes %>% rename(child_s=fromnode,parent_s=tonode)#rename(child_s=fromnode,parent_s=tonode)



FindUpstreamReachID <- function(ReachID){
  TribStartRID <- Yukon_shapefile$rid[which(Yukon_shapefile$reachid==ReachID)]
  
  TRIBindex<-c()
  StartParent<-YukonNetwork$parent_s[which(YukonNetwork$rid==TribStartRID)]
  StartChild<-YukonNetwork$child_s[which(YukonNetwork$rid==TribStartRID)]
  TRIBindex<-c(TRIBindex,StartChild)
  ChildList<-YukonNetwork$child_s[which(YukonNetwork$parent_s==StartChild)]
  while(length(ChildList)>0){
    TRIBindex <- c(TRIBindex,ChildList)
    ChildList <- YukonNetwork$child_s[which(YukonNetwork$parent_s %in% ChildList)]
  }
  print(length(TRIBindex))
  
  TribSegments<-Yukon_shapefile$reachid[match(YukonNetwork$rid[match(TRIBindex,YukonNetwork$child_s)], Yukon_shapefile$rid)]
  return(TribSegments)
}


#### This can be used to check if the above function worked 
  # Should produce a plot which colors the selected reaches in red. 
#if(T){
#  TribReachIDs <- FindUpstreamReachID(14438)

#  quartz()
#  par(mfrow=c(1,1))
#  nclr <- 9

#  plotvar <- rep(0,nrow(Yukon_shapefile))
#  plotvar[match(TribReachIDs,Yukon_shapefile$reachid)] <- 1
#  colcode <- rep('gray',nrow(Yukon_shapefile))
#  colcode[plotvar==0] <- 'gray60'
#  colcode[plotvar==1] <- 'red'
#  plot(st_geometry(YukonBasin), col = "gray", lwd=0.1)#border=NA)#,  add=TRUE)
#  plot(st_geometry(Yukon_shapefile), col = colcode, pch=16, axes = F,add=TRUE, lwd=1)
#}
     
