# Load required libraries
library(tidyverse)
library(here)
library(sf)


# Preprocess data 
source("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Code/Add Genetic Prior.R")
source("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Code/CPUE weights.R")
source(here("Code/Helper Functions/Assignment and Map Functions.R"))


# function to preprocess data 

preprocess_data <- function(year, river) {
  
  # Check the river input and call the appropriate functions
  if (river == "Yukon") {
    add_genetic_prior(year)
    Add_CPUE_weights_Yukon(year)
  } else if (river == "Kusko") {
    Add_CPUE_weights_Kusko(year)
  } else {
    print("Invalid river name. Please specify 'Yukon' or 'Kuskokwim'.")
  }
}

preprocess_data(2015, "Yukon")
preprocess_data(2016, "Yukon")
preprocess_data(2021, "Yukon")



# Kusko 

#2017
plotvar<- Kusko_map(2017, .7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017", .7)


#2018
plotvar<- Kusko_map(2018, .7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018", .7)


#2019
plotvar<- Kusko_map(2019, .7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2019", .7)

#2020
plotvar<- Kusko_map(2020, .7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2020", .7)

#2021
plotvar<- Kusko_map(2021, .7)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2021", .7)


# Kuskokwim 2017 Q1-Q4 
plotvar<- Kusko_map(2017, .7, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q1", .7)

plotvar<- Kusko_map(2017, .7, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q2", .7)

plotvar<- Kusko_map(2017, .7, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q3", .7)

plotvar<- Kusko_map(2017, .7, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q4", .7)

# Kuskokwim 2018 Q1-Q4
plotvar<- Kusko_map(2018, .7, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q1", .7)

plotvar<- Kusko_map(2018, .7, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q2", .7)

plotvar<- Kusko_map(2018, .7, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q3", .7)

plotvar<- Kusko_map(2018, .7, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q4", .7)
