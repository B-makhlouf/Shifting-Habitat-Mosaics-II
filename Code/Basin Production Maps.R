
# Load required libraries
library(tidyverse)
library(here)
library(RSQLite)
library(sf)

#Source in functions 
source(here("Code/Helper Functions/Assignment and Map Functions.R"))

# Connect to SQL database
SMH2_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

years <- c(2015, 2016, 2017, 2018, 2019, 2021)
sensitivity <- c(.7, .75, .8, .85, .9, .95)
watershed <- "Yukon"



identifier<- paste(watershed, year, sensitivity, Quartile, sep = "_")

query <- paste(
  "SELECT * FROM production_matrices WHERE Year = ", year, 
  " AND Sensitivity = ", sensitivity, 
  " AND Watershed = '", watershed, 
  "' AND Quartile = '", Quartile, "'", sep = ""
)

Prod <- dbGetQuery(SMH2_db, query)

#Scale Prod Production values to range from 0 to 1 
Plotvar<- Prod$Production
Plotvar<- (Plotvar-min(Plotvar))/(max(Plotvar)-min(Plotvar))

#Make the map
Map_Base(watershed, Plotvar, identifier, sensitivity)



