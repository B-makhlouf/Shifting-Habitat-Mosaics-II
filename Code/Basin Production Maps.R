# Load required libraries
library(tidyverse)
library(here)
library(sf)

# Source in functions 
source(here("Code/Helper Functions/Assignment and Map Functions.R"))

# ### 2015 
# 
# plotvar<-Yukon_map(2015,.7)
# Map_Base("Yukon", plotvar, "Yukon 2015", .7)
# 
# plotvar<-Yukon_map(2015,.7,"Q1")
# Map_Base("Yukon", plotvar, "Yukon 2015 Q1", .7)
# 
# plotvar<-Yukon_map(2015,.7,"Q2")
# Map_Base("Yukon", plotvar, "Yukon 2015 Q2", .7)
# 
# plotvar<-Yukon_map(2015,.7,"Q3")
# Map_Base("Yukon", plotvar, "Yukon 2015 Q3", .7)
# 
# plotvar<-Yukon_map(2015,.7,"Q4")
# Map_Base("Yukon", plotvar, "Yukon 2015 Q4", .7)
# 
# ### 2021
# 
# plotvar<-Yukon_map(2021,.7)
# Map_Base("Yukon", plotvar, "Yukon 2021", .7)
# 
# plotvar<-Yukon_map(2021,.7,"Q1")
# Map_Base("Yukon", plotvar, "Yukon 2021 Q1", .7)
# 
# plotvar<-Yukon_map(2021,.7,"Q2")
# Map_Base("Yukon", plotvar, "Yukon 2021 Q2", .7)
# 
# plotvar<-Yukon_map(2021,.7,"Q3")
# Map_Base("Yukon", plotvar, "Yukon 2021 Q3", .7)
# 
# plotvar<-Yukon_map(2021,.7,"Q4")
# Map_Base("Yukon", plotvar, "Yukon 2021 Q4", .7)
# 

################### Kuskokwim 

#2017
plotvar<- Kusko_map(2017,.7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017", .7)

#2018
plotvar<- Kusko_map(2018,.7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018", .7)

#2019
plotvar<- Kusko_map(2019,.7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019", .7)

#2020
plotvar<- Kusko_map(2020,.7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020", .7)

#2021
plotvar<- Kusko_map(2021,.7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021", .7)


# plotvar<- Kusko_map(2019,.7,"Q1")
# Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q1", .7)
# 
# plotvar<- Kusko_map(2019,.7,"Q2")
# Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q2", .7)
# 
# plotvar<- Kusko_map(2019,.7,"Q3")
# Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q3", .7)
# 
# plotvar<- Kusko_map(2019,.7,"Q4")
# Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q4", .7)


