library(here)
library(sf)
library(tidyverse)

yuk_polygons<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/trib_polygons.shp")
all_ind_data<- read_csv(here("2015 Yukon .9_tributaries.csv"))

tribs<- yuk_polygons$Trib


for (i in 1:length(unique(all_ind_data$Individual))){
  ind_data<- all_ind_data %>%
    filter(Individual == i) 
  
  ind_data_vector<- rep(0, length(tribs))
  
  for (i in 1:length(tribs)){
    if (tribs[i] %in% ind_data$tribs){
      ind_data_vector[i]<- 1
    }
  }
  
  colors<- ifelse(ind_data_vector == 1, "Red", "Gray60")
  
  plot(st_geometry(yuk_polygons), col = colors, main = paste0("Individual ", i, " Tributary Assignments"))
}

