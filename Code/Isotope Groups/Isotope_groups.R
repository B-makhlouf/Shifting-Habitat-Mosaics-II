library(here)
library(tidyverse)
library(RColorBrewer)


#### Attempt to make a map of distinct isotope groups from the Yukon, doesnt work 2/14/24
if(T){
  YukonGroups<- read_csv(here("data/reporting groups/yukon/Yukon_Tribs_byGroups.csv"))
  source(here("Code/Isotope Groups/Find_Upstream_Reaches.R"))
  
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


#### Isotope Groups for the Kuskokwim 


if(T){
  
  kuskGroups <- read.csv(here("Data/reporting groups/kusko/Kusko_Tribs_byGroups.csv")) #Group associated with each trib
  source(here("Code/Isotope Groups/Find_Upstream_Reaches.R")) #Source the code with the function FindUpstreamReachID_Kusk
  
  kuskTrib<-read.csv(here("Data/reporting groups/kusko/KuskoTribs_reachids.csv")) #A bunch of reachids of tributaries
  kuskTrib2<-read.csv(here("Data/reporting groups/kusko/Kusko_ExtraTribs.csv")) # Additional trib data 
  kuskTrib_all <- kuskTrib %>% select(river,reachid) %>% bind_rows(kuskTrib2 %>% rename(river=TribName,reachid=ReachID)) #Combine the above two datasets


  kuskTrib_IDs<-lapply(1:nrow(kuskTrib_all),FUN=function(x){ # Run the function FindUpstreamReachID_Kusk on each reachid
    FindUpstreamReachID_Kusk(kuskTrib_all$reachid[x])
  })
  
  names(kuskTrib_IDs) <- kuskTrib_all$river #Add the trib names to each list of reachids corresponding to that trib
  
  StreamExtentByTrib <- lapply(kuskTrib_IDs,FUN=function(x){sum(kusk_edges$Shape_Leng[which(kusk_edges$reachid %in% x)])}) %>% unlist() #Calculate the streamlength of all of the tribuataries in each list summed
  kuskTrib_IDs_ordered <- kuskTrib_IDs[order(StreamExtentByTrib)] #Order the lists by total streamlength
  names(kuskTrib_IDs_ordered) <- names(kuskTrib_IDs)[order(StreamExtentByTrib)] #rename the lists appropriately 
  
  
  ## Go through each list of reach ids, and find overlap between that and the next list.
  TribRows <- lapply(1:length(kuskTrib_IDs_ordered),FUN=function(i){
    onlythisTrib <- which(!(kuskTrib_IDs_ordered[[i]] %in% unlist(kuskTrib_IDs_ordered[1:(i-1)])))
    kuskTrib_IDs_ordered[[i]][onlythisTrib]
    if(length(onlythisTrib)>0){
      INDX<-kuskTrib_IDs_ordered[[i]][onlythisTrib]
    }else{
      INDX<-kuskTrib_IDs_ordered[[i]]
    }
    trib.rows <- which(kusk_edges$reachid %in% INDX)
    return(trib.rows)
  })
  
  names(TribRows) <- names(kuskTrib_IDs_ordered) #Tribrows now contains unique values for each tributary grouping, no overlap of upstream reaches. 
  UniG<-unique(kuskGroups$GroupID) %>% na.omit() #How many Unique groupIDs are there? 
  GroupColors <- brewer.pal(5,'Set1')
  PlotCols<-rep('gray60',length(kusk_edges$rid))
  
  for(G in 1:length(UniG)){
    ind <- kuskGroups$Trib[kuskGroups$GroupID==G] %>% na.omit()
    trib.rows <- unlist(TribRows[ind])
    PlotCols[trib.rows] <- GroupColors[G]
  }

  pdf(here("Data/reporting groups/kusko",paste0('Kusko_GroupMap','.pdf')),width=9,height=9)
  plot(st_geometry(kusk_basin), col = "gray", lwd=0.1)
  plot(st_geometry(kusk_edges), col = PlotCols, pch=16, axes = F,add=TRUE, lwd=0.5)
  legend("topleft", legend = paste(rep('Group',5),seq(1,5),sep=' '),
         fill = GroupColors, cex=1, ncol=1)
}



























### Reads in the tributary list and "extra tributary" list 
Kusko_extratribs<- read_csv(here("data/reporting groups/kusko/Kusko_ExtraTribs.csv"))
Kusko_tribs<- read_csv(here("data/reporting groups/kusko/KuskoTribs_reachids.csv"))
kuskGroups <- read.csv(here('data/reporting groups/kusko/Kusko_Tribs_byGroups.csv'),header=T,stringsAsFactors = F)

#Combined these two into All_tribs_combined
kuskTrib_all <- bind_rows(
  Kusko_extratribs %>% rename(river = TribName, reachid = ReachID), #renames columns to match between files
  Kusko_tribs %>% select(river, reachid) #selects only the necessary columns
)
rm(Kusko_tribs, Kusko_extratribs) #removes the old dataframes from the environment

# This runs code which defines the Find_Upstream_reaches function
source(here('code/Find_Upstream_Reaches_Kusko.R')) 

# Apply the Find_Upstream_Reaches function to all tributaries.
kuskTrib_IDs<-lapply(1:nrow(kuskTrib_all),FUN=function(x){
  FindUpstreamReachID(kuskTrib_all$reachid[x])
})
names(kuskTrib_IDs) <- kuskTrib_all$river #Add names to each list 

#Calculate the stream exentent of each of these calculated upstream regions
StreamExtentByTrib <- lapply(kuskTrib_IDs,FUN=function(x){sum(kusk_edges$Shape_Leng[which(kusk_edges$reachid %in% x)])}) %>% unlist()
kuskTrib_IDs_ordered <- kuskTrib_IDs[order(StreamExtentByTrib)] #order by stream extent 
names(kuskTrib_IDs_ordered) <- names(kuskTrib_IDs)[order(StreamExtentByTrib)] #add names to each list

#Find upstream tributaries which are unique to each downstream tributary, remove overlap. 
TribRows <- lapply(1:length(kuskTrib_IDs_ordered),FUN=function(i){
  onlythisTrib <- which(!(kuskTrib_IDs_ordered[[i]] %in% unlist(kuskTrib_IDs_ordered[1:(i-1)])))
  
  if(length(onlythisTrib)>0){ # calculate the part that ONLY belongs to that trib
    INDX<-kuskTrib_IDs_ordered[[i]][onlythisTrib]
  }else{
    INDX<-kuskTrib_IDs_ordered[[i]]
  }
  trib.rows <- which(kusk_edges$reachid %in% INDX)
  return(trib.rows)
})
names(TribRows) <- names(kuskTrib_IDs_ordered) #add name  

kuskTable <- matrix(NA,nrow=5,ncol=4) %>% data.frame() # initialize kuskTable
colnames(kuskTable) <- c('GroupID','PropProd_noEek','StrLeng','Prop_StrLeng') # add column names 
StreamOrderPrior<- as.numeric(kusk_edges$Strahler >2) # Define stream order prior 

# For each group... 
for(G in 1:length(unique(kuskGroups$GroupID) %>% na.omit())){
  ind <- kuskGroups$Trib[kuskGroups$GroupID==G] %>% na.omit() #pull out trib names for that group
  trib.rows <- unlist(TribRows[ind]) 
  kuskTable$GroupID[G] <- G # Add group ID number 
  kuskTable$PropProd_noEek[G] <- normalized_basin_values[trib.rows] %>% sum() # Add normalized basin values for all of the tribs 
  kuskTable$StrLeng[G] <- ((kusk_edges$Length[trib.rows] * kusk_edges$UniPrioh2o[trib.rows]*StreamOrderPrior[trib.rows]) %>% sum())/1000 # Calculate the stream length
  
  BasinLength<-sum(kusk_edges$Length*kusk_edges$UniPh2oNoE*StreamOrderPrior)/1000 #sum all possible habitat to get the denominator 
  kuskTable$Prop_StrLeng <- kuskTable$StrLeng/BasinLength #Calculate the proportion of stream length 
  kuskTable$TotalRun_noEek<-kuskTable$PropProd_noEek* run_size / 1000 # calculate the total run # for everywhere but the eek 
  kuskTable$PerDiff_noEek<-((kuskTable$PropProd_noEek-kuskTable$Prop_StrLeng)/kuskTable$Prop_StrLeng) #calculate the difference between the rest and eek 
  
  filename<- paste0(identifier, "_iso_group_results.csv")
  filepath<- file.path(here("results/Reporting group results", filename))
  write_csv(kuskTable, filepath)
  
}







