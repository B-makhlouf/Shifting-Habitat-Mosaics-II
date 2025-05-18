YukonNodes <- read.csv(here('NodeShapefile','ykon_noderelationships.csv'),header=T,stringsAsFactors = F)
YukonNetwork<-YukonNodes %>% rename(child_s=fromnode,parent_s=tonode)#rename(child_s=fromnode,parent_s=tonode)

FindUpstreamReachID <- function(ReachID){
  TribStartRID <- yuk_edges@data$rid[which(yuk_edges@data$reachid==ReachID)]
  
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
  
  TribSegments<-yuk_edges@data$reachid[match(YukonNetwork$rid[match(TRIBindex,YukonNetwork$child_s)],yuk_edges@data$rid)]
  return(TribSegments)
}

if(F){
  TribReachIDs <- FindUpstreamReachID(14438)

  quartz()
  par(mfrow=c(1,1))
  nclr <- 9

  plotvar <- rep(0,nrow(yuk_edges@data))
  plotvar[match(TribReachIDs,yuk_edges@data$reachid)] <- 1
  colcode <- rep('gray',nrow(yuk_edges@data))
  colcode[plotvar==0] <- 'gray60'
  colcode[plotvar==1] <- 'red'
  plot(yuk_basin, col = "gray", lwd=0.1)#border=NA)#,  add=TRUE)
  plot(yuk_edges, col = colcode, pch=16, axes = F,add=TRUE, lwd=1)
}
     