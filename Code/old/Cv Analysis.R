
library(tidyverse)
library(here)
library(sf)
library(biscale) #bivariate choropleths
library(cowplot) #combine map and legend bivariate choropleth
library(scatterpie) #proportional symbols pie charts
library(here) #setup
library(sf) #vector data         
library(tidyverse) #data manipulation
library(spData) #datasets



read_total_column <- function(file_path) {
  read_csv(here(file_path)) %>% select(Total)
}

# List of file paths
file_paths <- list(
  "Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv",
  "Outputs/Assignment Matrix/Yukon_2016_0.7_basin_assignments.csv",
  "Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv",
  "Outputs/Assignment Matrix/Yukon_2017_0.7_basin_assignments.csv"
)

# Read in all assignment matrices and combine them
full_yukon_assign <- file_paths %>%
  map_dfc(read_total_column) %>%
  rename_with(~c("yuk2015", "yuk2016", "yuk2017", "yuk2018"))

# Read in the shape file with the Tributary Names
yukon_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")

# Merge the full yukon assign data frame into the yukon_withTribs data frame
yukon_withTribs <- yukon_withTribs %>%
  bind_cols(full_yukon_assign) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c_across(yuk2015:yuk2017), na.rm = TRUE),
    sd_prod = sd(c_across(yuk2015:yuk2017), na.rm = TRUE),
    cv = if_else(is.na(sd_prod / mean_prod * 100), 0, sd_prod / mean_prod * 100)
  ) %>%
  ungroup()

### Filter to one trib group 
Filtered<- yukon_withTribs %>% filter(trbtry_ == "Klondike")

#Map to check 
ggplot(Filtered) +
  geom_sf() + 
  theme_void()


third_order_tributaries <- Filtered %>%
  filter(Str_Ord == 3)
fourth_order_tributaries <- Filtered %>%
  filter(Str_Ord == 4)
fifth_order_tributaries <- Filtered %>%
  filter(Str_Ord == 5)
sixth_order_tributaries <- Filtered %>%
  filter(Str_Ord == 6)


#Map all the 5th order tribs 
ggplot(Filtered) +
  geom_sf() + 
  geom_sf(data = third_order_tributaries, color = "red")+
  geom_sf(data = fourth_order_tributaries, color = "blue")+
  geom_sf(data = fifth_order_tributaries, color = "green")+
  geom_sf(data = sixth_order_tributaries, color = "purple")+ 
  geom_sf(data = touching_tributaries, color = "orange")

touching_indices <- unlist(st_touches(fourth_order_tributaries, fifth_order_tributaries))

# Add tribs with are touching another fourth order trib 
touching


touching_tributaries <- fourth_order_tributaries[touching_indices, ]



source(here("Code/Isotope Groups/Find_Upstream_Reaches.R"))

# Loop through each fourth-order tributary, use the FindUpstream_Reaches function to find the upstream reaches, and store the results in a list

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

# Loop through each fourth-order tributary, use the FindUpstream_Reaches function to find the upstream reaches, and store the results in a list

upstream_reaches_List <- lapply(downstream_fourth_order_REACHID, FindUpstreamReachID)


