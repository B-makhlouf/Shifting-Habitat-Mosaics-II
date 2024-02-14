## This script is to "clean"shapefiles by removing many of the current attributes which are not currently pertinant 

# Read in all shapefiles 
library(sf)
library(here)
library(dplyr)
library(tidyverse)
library(fs)


################### Yukon

Yukon1_raw<- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc_withprobs.shp"))
Yukon2_raw<- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp"))

Yukon1_cleaned <- Yukon1_raw[, c("geometry", "rid", "Shape_Leng", "iso_pred", "isose_pred", "reachid", "PriorSl2", "GenLMU", "Str_Order")]

#export shapefile 
st_write(Yukon1_cleaned, here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp"))


################### Kusko
Kusko_raw<- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp"))
Kusko_cleaned <- Kusko_raw[, c("geometry", "rid", "Shape_Leng", "iso_pred", "isose_pred", "reachid", "Strahler", "UniPh2oNoE","UniPrioh2o" )]

#rename Strahler to Str_Order
Kusko_cleaned <- Kusko_cleaned %>% rename(Str_Order = Strahler)

#export 
st_write(Kusko_cleaned, here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Kusko_cleaned.shp"))

################### Nushagak

Nushagak_raw<- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Nush/nush_edges_20190129_AgeClassAccum_subbasins.shp"))
Nushagak_cleaned <- Nushagak_raw[, c("geometry", "rid", "Shape_Leng", "ISO_pred", "ISO_predSE", "reachid", "StrOrd")]

#rename to match all Kusko names 
Nushagak_cleaned <- Nushagak_cleaned %>% rename(iso_pred = ISO_pred, isose_pred = ISO_predSE, Str_Order = StrOrd)

#export 
st_write(Nushagak_cleaned, here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Nushagak_cleaned.shp"))

#Combine all three cleaned shapefiles into one 
Yukon_cleaned <- st_read(here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp"))
Kusko_cleaned <- st_read(here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Kusko_cleaned.shp"))
Nushagak_cleaned <- st_read(here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Nushagak_cleaned.shp"))

#remove collumns which are not the same between all three 
Yukon_cleaned <- Yukon_cleaned[, c("geometry", "rid", "Shape_Leng", "iso_pred", "isose_pred", "reachid", "Str_Order")]
Kusko_cleaned <- Kusko_cleaned[, c("geometry", "rid", "Shape_Leng", "iso_pred", "isose_pred", "reachid", "Str_Order")]
Nushagak_cleaned <- Nushagak_cleaned[, c("geometry", "rid", "Shape_Leng", "iso_pred", "isose_pred", "reachid", "Str_Order")]

#Add basin name as an attribute 
Yukon_cleaned$basin <- "Yukon"
Kusko_cleaned$basin <- "Kusko"
Nushagak_cleaned$basin <- "Nushagak"

output_file <- here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/all_basins_cleaned.shp")

if (file.exists(output_file)) {
  # If file exists, delete it
  file.remove(output_file)
}

# Combine cleaned shapefiles
all_cleaned <- rbind(Yukon_cleaned, Kusko_cleaned, Nushagak_cleaned)

# Export cleaned shapefiles
st_write(all_cleaned, output_file)
