# Code to define the function for determining upstream reaches
# Adapted from FindUpstreamReaches_Kusko_NEW in code/from tim/FindUpstreamReaches_Kusko_NEW.R
# Written by: Ben Makhlouf 12/2023


library(sf)
library(tidyverse)
library(here)

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



# TO test to see if it works 
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

