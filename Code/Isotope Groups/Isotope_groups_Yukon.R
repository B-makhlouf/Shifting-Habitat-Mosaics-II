library(here)
library(tidyverse)

if(T){
  YukonGroups<- read_csv(here("data/reporting groups/yukon/Yukon_Tribs_byGroups.csv"))
 # source(here('FindUpstreamReaches_Kusko.R'))
  Yukon_tribs_all<- read_csv("Data/Reporting Groups/Yukon/ReachIDs_tributaries.csv")
  
  YukonTrib_IDs<-lapply(1:nrow(Yukon_tribs_all),FUN=function(x){
    FindUpstreamReachID(Yukon_tribs_all$reachid_trib[x])
  })
  
  names(YukonTrib_IDs) <- Yukon_tribs_all$river
  StreamExtentByTrib <- lapply(YukonTrib_IDs,FUN=function(x){sum(yuk_edges$Shape_Leng[which(yuk_edges$reachid %in% x)])}) %>% unlist()
  YukonTrib_IDs_ordered <- YukonTrib_IDs[order(StreamExtentByTrib)]
  names(YukonTrib_IDs_ordered) <- names(YukonTrib_IDs)[order(StreamExtentByTrib)]
  
  TribRows <- lapply(1:length(YukonTrib_IDs_ordered),FUN=function(i){
    onlythisTrib <- which(!(YukonTrib_IDs_ordered[[i]] %in% unlist(YukonTrib_IDs_ordered[1:(i-1)])))
    YukonTrib_IDs_ordered[[i]][onlythisTrib]
    
    if(length(onlythisTrib)>0){
      INDX<-YukonTrib_IDs_ordered[[i]][onlythisTrib]
    }else{
      INDX<-YukonTrib_IDs_ordered[[i]]
    }
    trib.rows <- which(yuk_edges$reachid %in% INDX)
    return(trib.rows)
  })
  
  names(TribRows) <- names(YukonTrib_IDs_ordered)

  UniG<-unique(YukonGroups$GroupID) %>% na.omit()
  GroupColors <- GroupColors <- c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "yellow", "brown", "darkgreen", "darkblue", "darkred", "darkorange")
  
  PlotCols<-rep('gray60',length(yuk_edges$Prod17ig_n))
  for(G in 1:length(UniG)){
    ind <- YukonGroups$Trib[YukonGroups$GroupID==G] %>% na.omit()
    trib.rows <- unlist(TribRows[ind])
    PlotCols[trib.rows] <- GroupColors[G]
  }
  quartz()
  range(plotvar)
  plot(st_geometry(yuk_basin), col = "gray", lwd=0.1)
  LWD <- rep(0.75,length(plotvar))
  plot(st_geometry(yuk_edges), col = PlotCols, pch=16, axes = F,add=TRUE, lwd=LWD)
  legend("topleft", legend = paste(rep('Group',5),seq(1,5),sep=' '),
         fill = GroupColors, cex=1, ncol=1)
  dev.off()
}










