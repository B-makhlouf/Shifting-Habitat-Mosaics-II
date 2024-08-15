# Load required libraries
library(tidyverse)
library(here)
library(sf)

# Source in functions 
source(here("Code/Helper Functions/Assignment and Map Functions.R"))

plotvar<-Yukon_map(2015,.9)
Map_Base("Yukon", plotvar, "Yukon 2015", .75)

plotvar<-Yukon_map(2021,.9)
Map_Base("Yukon", plotvar, "Yukon 2021", .75)





