library(here)
library(tidyverse)
library(sf)

# Read in the shapefile with tributary names
yuk_tribs<- st_read("/Users/benjaminmakhlouf/Desktop/New Shapefiles/yuk_Tribs_named.shp")

#Read in the raw invidual assignment data 
ind_matrix_2015<- read_csv(here("Data/Production/Yukon/ASSIGN_MATRIX2015_full_Yukon_0.9.csv"))

#Pick out one individual (in this case, V2)
ind_trib<- data_frame(tribs = yuk_tribs$trbtry_, 
                      assignments = ind_matrix_2015$V2)

#Pull out tributary names which have a value of 1 
ind_trib_names <- ind_trib %>% 
  filter(assignments == 1) %>% 
  select(tribs)%>%
  #keep only unique values 
  distinct()
