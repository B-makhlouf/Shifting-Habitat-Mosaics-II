library(here)
library(tidyverse)
library(RColorBrewer)
library(classInt)


#### Attempt to make a map of distinct isotope groups from the Yukon, doesnt work 2/14/24
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

########################################################################################################
########################################################################################################
####################################### Kuskokwim ######################################################


#### Isotope Groups for the Kuskokwim 
clean_Kusko<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Kusko_cleaned.shp")
clean_Kusko$GroupID<-NA

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
  
  for (trib_name in names(TribRows)) {
    ind <- unlist(TribRows[[trib_name]])
    clean_Kusko$tributary_name[ind] <- trib_name
  }
  
  #export shapefile using sf
  
  st_write(clean_Kusko, "/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Kusko_w.tribnames.shp")
  
  UniG<-unique(kuskGroups$GroupID) %>% na.omit() #How many Unique groupIDs are there? 
  GroupColors <- brewer.pal(5,'Set1')
  PlotCols<-rep('gray60',length(kusk_edges$rid))
  
  for(G in 1:length(UniG)){
    ind <- kuskGroups$Trib[kuskGroups$GroupID==G] %>% na.omit()
    trib.rows <- unlist(TribRows[ind])
    PlotCols[trib.rows] <- GroupColors[G]
    clean_Kusko$Group[trib.rows] <- G
    
  }

  
  pdf_path <- here("Data/reporting groups/kusko", paste0('Kusko_GroupMap','.pdf'))
  
  pdf(pdf_path, width=9, height=9)
  plot(st_geometry(kusk_basin), col = "gray", lwd=0.1)
  plot(st_geometry(kusk_edges), col = PlotCols, pch=16, axes = F, add=TRUE, lwd=0.5)
  legend("topleft", legend = paste(rep('Group',5), seq(1,5), sep=' '),
         fill = GroupColors, cex=1, ncol=1)
  dev.off()
  
}

#Export shapefile 
st_write(clean_Kusko,("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp"))









### Read in production values for all years 
kusk_prod_2017<- read_csv(here("Data/Production/Kusko/2017 Kusko_basin_norm.csv"))
kusk_prod_2018<- read_csv(here("Data/Production/Kusko/2018 Kusko_basin_norm.csv"))
kusk_prod_2019<- read_csv(here("Data/Production/Kusko/2019 Kusko_basin_norm.csv"))
kusk_prod_2020<- read_csv(here("Data/Production/Kusko/2020 Kusko_basin_norm.csv"))
kusk_prod_2021<- read_csv(here("Data/Production/Kusko/2021 Kusko_basin_norm.csv"))


# add group list from shapefile to each 

kusk_prod_2017$GroupID<-clean_Kusko$GroupID
kusk_prod_2018$GroupID<-clean_Kusko$GroupID
kusk_prod_2019$GroupID<-clean_Kusko$GroupID
kusk_prod_2020$GroupID<-clean_Kusko$GroupID
kusk_prod_2021$GroupID<-clean_Kusko$GroupID


#Sum by GroupID
kusk_prod_2017_sum<-kusk_prod_2017 %>% 
  group_by(GroupID) %>% 
  summarise_all(sum) %>%
  rename(prod = basin_assign_rescale)


kusk_prod_2017 <- kusk_prod_2017 %>%
  left_join(kusk_prod_2017_sum, by = "GroupID") %>%
  #Assign those with a GroupID value of NA, 0 
  mutate(prod = ifelse(is.na(GroupID), 0, prod))


plotvar<- kusk_prod_2017$prod

breaks <- seq(min(plotvar), max(plotvar), length= 5)
nclr <- length(breaks)
filename <- ("2017 Kusko Group Prod_.pdf")
filepath <- file.path(here("Data/reporting groups/kusko", filename))
pdf(file = filepath, width = 9, height = 6)
class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
plotclr <- brewer.pal(nclr, "YlOrRd")
colcode <- findColours(class, plotclr, digits = 2)
colcode[plotvar == 0] <- 'gray80'
plot(st_geometry(kusk_basin), col = 'gray60', border = 'gray48')
plot(st_geometry(kusk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .7 * (exp(plotvar) - 1)))
dev.off()

