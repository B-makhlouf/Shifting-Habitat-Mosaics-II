# This script is to determine how production patterns vary among individuals by tributary 

#----------------

library(here)
library(tidyverse)
library(sf)

#---------------- 

#Shapefile with tributary names 
#yuk_tribs<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_bigtribs.shp")
yuk_tribs<- st_read(here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp"))
############### Read in individual assignment data 

########## 2015 Yukon .9
ind_2015<- read_csv(here("Data/Production/Yukon/ASSIGN_MATRIX2015_full_Yukon_0.9.csv"))
ind_2015<- ind_2015[,-1]
identifier<- "2015 Yukon .9"

########## 2015 Yukon .95 
ind_2015<- read_csv(here("Data/Production/Yukon/ASSIGN_MATRIX2015_full_Yukon_0.95.csv"))
ind_2015<- ind_2015[,-1]
identifier<- "2015 Yukon .95"

########## 2015 Yukon .97 
ind_2015<- read_csv(here("Data/Production/Yukon/ASSIGN_MATRIX2015_full_Yukon_0.85.csv"))
ind_2015<- ind_2015[,-1]
identifier<- "2015 Yukon .85"

ind_assignment_tributaries<- list() # list to store output

for ( i in 1:ncol(ind_2015)){ #for each collumn... 
  ind_trib<- data_frame(tribs = yuk_tribs$trbtry_, 
                        assignments = ind_2015[,i])
  
  ind_trib_names <- ind_trib %>% 
    filter(assignments > 0) %>% 
    select(tribs)%>%
    distinct()
  
  ind_assignment_tributaries[[i]] <- ind_trib_names
} 

result_df <- bind_rows(ind_assignment_tributaries, .id = "Individual")


# Write to csv
filename<- paste0(identifier, "_tributaries.csv")
write.csv(result_df, filename)


individual_plot <- function(i) {
  ind_Value<- paste0("V", i) 
  ind_assignment <- ind_2015[[ind_Value]]
  ind_assignment <- as.numeric(ind_assignment)
  plotvar <- ind_assignment
  colcode <- rep(0, length(plotvar))
  colcode[plotvar > 0] <- 'red'
  colcode[plotvar == 0] <- 'gray80'
  
  quartz()
  plot.new()
  plot(st_geometry(yuk_tribs), col = colcode) 
}

dev.off()
individual_plot(5)
