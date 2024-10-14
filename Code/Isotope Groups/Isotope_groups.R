library(here)
library(tidyverse)
library(RColorBrewer)
library(classInt)
library(sf)

########################################################################################################
########################################################################################################
####################################### Kuskokwim ######################################################

#### Isotope Groups for the Kuskokwim 
clean_Kusko<- st_read("/Users/benjaminmakhlouf/Spatial Data/Shapefiles/AYK Shapefiles/Kusko_cleaned.shp")
clean_Kusko$GroupID<-NA

if(T){
  
  kuskGroups <- read.csv(here("Data/Isotope Groups/Kusko_Tribs_byGroups.csv")) #Group associated with each trib
  source(here("Code/Isotope Groups/Find_Upstream_Reaches.R")) #Source the code with the function FindUpstreamReachID_Kusk
  
  kuskTrib<-read.csv(here("Data/Isotope Groups/KuskoTribs_reachids.csv")) #A bunch of reachids of tributaries
  kuskTrib2<-read.csv(here("Data/Isotope Groups/Kusko_ExtraTribs.csv")) # Additional trib data 
  kuskTrib_all <- kuskTrib %>% select(river,reachid) %>% bind_rows(kuskTrib2 %>% rename(river=TribName,reachid=ReachID)) #Combine the above two datasets
  
  #Remove kuskTrib and kuskTrib2 
  rm(kuskTrib,kuskTrib2)
  
  kuskTrib_IDs<-lapply(1:nrow(kuskTrib_all),FUN=function(x){ # Run the function FindUpstreamReachID_Kusk on each reachid
    FindUpstreamReachID_Kusk(kuskTrib_all$reachid[x])
  })
  
  names(kuskTrib_IDs) <- kuskTrib_all$river #Add the trib names to each list of reachids corresponding to that trib
  
  #Calculate the streamlength of all of the tribuataries in each list summed
  StreamExtentByTrib <- lapply(kuskTrib_IDs,FUN=function(x){sum(kusk_edges$Shape_Leng[which(kusk_edges$reachid %in% x)])}) %>% unlist()
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
  
  
  
  # Initialize an empty data frame to store reach IDs and their corresponding tributaries
  trib_reachid_df <- data.frame()
  
  # Iterate over the ordered list of reach IDs and bind each reachid to its corresponding tributary name
  for(i in seq_along(kuskTrib_IDs_ordered)) {
    # Get the tributary name
    trib_name <- names(kuskTrib_IDs_ordered)[i]
    
    # Get the reach IDs for this tributary
    reach_ids <- kuskTrib_IDs_ordered[[i]]
    
    # Create a temporary data frame with the reach IDs and the corresponding tributary name
    temp_df <- data.frame(reachid = reach_ids, tributary = trib_name)
    
    # Append to the main data frame
    trib_reachid_df <- rbind(trib_reachid_df, temp_df)
  }
  
  # Now add the tributary names to the shapefile by matching the reach Ids
  clean_Kusko$Tribname <- trib_reachid_df$tributary[match(clean_Kusko$reachid, trib_reachid_df$reachid)]
  
  # add another column to the shapefile called "mainstem" and name it NA
  clean_Kusko$mainstem <- NA
  
  # Go through kuskTrib_all and add "river" value to the reachid in the shapefile that matches
  for(i in 1:nrow(kuskTrib_all)){
    clean_Kusko$mainstem[which(clean_Kusko$reachid %in% kuskTrib_all$reachid[i])] <- kuskTrib_all$river[i]
  }
  
  # Export the shapefile with the tributary names
  st_write(clean_Kusko, ("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Kusko_cleaned_with_tribs2.shp"))
}










#################### YUKON 

if(T){
  clean_Yukon<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp")
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
  
  clean_Yukon$tributary_name<-NA
  names(TribRows) <- names(YukonTrib_IDs_ordered)
  
  for (trib_name in names(TribRows)) {
    ind <- unlist(TribRows[[trib_name]])
    clean_Yukon$tributary_name[ind] <- trib_name
  }
  
  iso_by_trib<- list()
  
  for (trib_name in names(TribRows)){
    ind <- unlist(TribRows[[trib_name]])
    iso_by_trib[[trib_name]] <- clean_Yukon$iso_pred[ind]
  }
  
  #Put all into one data frame 
  iso_by_trib_df <- data.frame(tributary_name=rep(names(TribRows),times=sapply(iso_by_trib,length)),
                               isotope=unlist(iso_by_trib))
  
  #order
  tribs_ordered_iso <- iso_by_trib_df %>% group_by(tributary_name) %>% summarise(mean_isotope=mean(isotope)) %>% arrange(mean_isotope)
  
  ordered_iso_by_trib_df$tributary_name <- factor(ordered_iso_by_trib_df$tributary_name, 
                                                  levels = unique(ordered_iso_by_trib_df$tributary_name))
  
  # Create boxplots
  ggplot(ordered_iso_by_trib_df, aes(x = tributary_name, y = isotope)) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # export shapefile using sf
  
  st_write(clean_Yukon, "/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")
  
  
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

