library(here)
library(tidyverse)

#2015
identifier<- "2015 Yukon Chinook"
normalized_basin_values<- read_csv(here("Data/Production/Yukon/2015 Yukon_basin_norm.csv"))
normalized_basin_values<- as.vector(normalized_basin_values$basin_assign_rescale)
All_tribs_combined<- read_csv("Data/Reporting Groups/Yukon/ReachIDs_tributaries.csv")
run_size<- 219500

#2017
identifier<- "2017 Yukon Chinook"
normalized_basin_values<- read_csv(here("Data/Production/Yukon/2017 Yukon_basin_norm.csv"))
normalized_basin_values<- as.vector(normalized_basin_values$basin_assign_rescale)
All_tribs_combined<- read_csv("Data/Reporting Groups/Yukon/ReachIDs_tributaries.csv")
run_size<- 219500

#2018
identifier<- "2018 Yukon Chinook"
normalized_basin_values<- read_csv(here("Data/Production/Yukon/2018 Yukon_basin_norm.csv"))
normalized_basin_values<- as.vector(normalized_basin_values$basin_assign_rescale)
All_tribs_combined<- read_csv("Data/Reporting Groups/Yukon/ReachIDs_tributaries.csv")
run_size<- 219500


if (T){
  YukonGroups<- read_csv(here("data/reporting groups/yukon/Yukon_Tribs_byGroups.csv"))
  # This runs code which defines the Find_Upstream_reaches function
  source(here('code/Isotope Groups/Find_Upstream_Reaches_Yukon.R')) 
  
  # Apply the Find_Upstream_Reaches function to all tributaries.
  YukonTrib_IDs<-lapply(1:nrow(All_tribs_combined),FUN=function(x){
    FindUpstreamReachID(All_tribs_combined$reachid_trib[x])
  })
  
  names(YukonTrib_IDs) <- All_tribs_combined$river #Add names to each list 
  
  #Calculate the stream exentent of each of these calculated upstream regions
  StreamExtentByTrib <- lapply(YukonTrib_IDs,FUN=function(x){sum(Yukon_shapefile$Shape_Leng[which(Yukon_shapefile$reachid %in% x)])}) %>% unlist()
  YukonTrib_IDs_ordered <- YukonTrib_IDs[order(StreamExtentByTrib)] #order by stream extent 
  names(YukonTrib_IDs_ordered) <- names(YukonTrib_IDs)[order(StreamExtentByTrib)] #add names to each list
  
  #Find upstream tributaries which are unique to each downstream tributary, remove overlap. 
  TribRows <- lapply(1:length(YukonTrib_IDs_ordered),FUN=function(i){
    onlythisTrib <- which(!(YukonTrib_IDs_ordered[[i]] %in% unlist(YukonTrib_IDs_ordered[1:(i-1)])))
    
    if(length(onlythisTrib)>0){ # calculate the part that ONLY belongs to that trib
      INDX<-YukonTrib_IDs_ordered[[i]][onlythisTrib]
    }else{
      INDX<-YukonTrib_IDs_ordered[[i]]
    }
    trib.rows <- which(Yukon_shapefile$reachid %in% INDX)
    return(trib.rows)
  })
  names(TribRows) <- names(YukonTrib_IDs_ordered) #add name  
  
  YukonTable <- matrix(NA,nrow=5,ncol=4) %>% data.frame() # initialize kuskTable
  colnames(YukonTable) <- c('GroupID','PropProd','StrLeng','Prop_StrLeng') # add column names 
  StreamOrderPrior<- as.numeric(Yukon_shapefile$Str_Order > 2) # Define stream order prior 
  
  # For each group... 
  for(G in 1:length(unique(YukonGroups$IsoGroup) %>% na.omit())){
    ind <- YukonGroups$Trib[YukonGroups$IsoGroup==G] %>% na.omit() #pull out trib names for that group
    trib.rows <- unlist(TribRows[ind]) 
    YukonTable$GroupID[G] <- G # Add group ID number 
    YukonTable$PropProd[G] <- normalized_basin_values[trib.rows] %>% sum() # Add normalized basin values for all of the tribs 
    YukonTable$StrLeng[G] <- ((Yukon_shapefile$Length[trib.rows] * Yukon_shapefile$PriorSl2[trib.rows]*StreamOrderPrior[trib.rows]) %>% sum())/1000 # Calculate the stream length
    
    BasinLength<-sum(Yukon_shapefile$Length*Yukon_shapefile$PriorSl2*StreamOrderPrior)/1000 #sum all possible habitat to get the denominator 
    YukonTable$Prop_StrLeng <- YukonTable$StrLeng/BasinLength #Calculate the proportion of stream length 
    YukonTable$TotalRun<-YukonTable$PropProd* run_size / 1000 # calculate the total run # for everywhere but the eek 
  
    filename<- paste0(identifier, "_iso_group_results.csv")
    filepath<- file.path(here("Results/Isotope Group Production", filename))
    write_csv(YukonTable, filepath)
    
    }
}
  
