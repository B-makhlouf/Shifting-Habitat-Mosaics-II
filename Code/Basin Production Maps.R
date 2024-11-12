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







#Yukon

#2015
plotvar<- Yukon_map(2021,.001)
Map_Base("Yukon", plotvar, "Yukon 2015 Full run", .001)











################### Kuskokwim 

#2017
plotvar<- Kusko_map(2017,.001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 Full run", .001)

#2017 H1 .001

plotvar<- Kusko_map(2017,.001,"H1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 H1", .001)

#2017 H2 .001

plotvar<- Kusko_map(2017,.001,"H2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 H2", .001)


#2017 Q1 .001

plotvar<- Kusko_map(2017,.001,"Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 Q1", .001)

#2017 Q2 .001

plotvar<- Kusko_map(2017,.001,"Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 Q2", .001)

#2017 Q3 .001

plotvar<- Kusko_map(2017,.001,"Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 Q3", .001)

#2017 Q4 .001

plotvar<- Kusko_map(2017,.001,"Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2017 Q4", .001)


# 2018
plotvar<- Kusko_map(2018,.001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 Full run", .001)

#2018 H1 .001

plotvar<- Kusko_map(2018,.001,"H1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 H1", .001)

#2018 H2 .001

plotvar<- Kusko_map(2018,.001,"H2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 H2", .001)

# 2018 Q1 .001

plotvar<- Kusko_map(2018,.001,"Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 Q1", .001)

# 2018 Q2 .001

plotvar<- Kusko_map(2018,.001,"Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 Q2", .001)

# 2018 Q3 .001

plotvar<- Kusko_map(2018,.001,"Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 Q3", .001)

# 2018 Q4 .001

plotvar<- Kusko_map(2018,.001,"Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2018 Q4", .001)

#2019
plotvar<- Kusko_map(2019,.001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Full run", .001)

#2019 H1 .001

plotvar<- Kusko_map(2019,.001,"H1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 H1", .001)

#2019 H2 .001

plotvar<- Kusko_map(2019,.001,"H2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 H2", .001)


# 2019 Q1 .001

plotvar<- Kusko_map(2019,.001,"Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q1", .001)

# 2019 Q2 .001

plotvar<- Kusko_map(2019,.001,"Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q2", .001)

# 2019 Q3 .001

plotvar<- Kusko_map(2019,.001,"Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q3", .001)

# 2019 Q4 .001

plotvar<- Kusko_map(2019,.001,"Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2019 Q4", .001)

#2020
plotvar<- Kusko_map(2020,.001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 Full run", .001)

#2020 H1 .001

plotvar<- Kusko_map(2020,.001,"H1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 H1", .001)

#2020 H2 .001

plotvar<- Kusko_map(2020,.001,"H2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 H2", .001)

# 2020 Q1 .001

plotvar<- Kusko_map(2020,.001,"Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 Q1", .001)

# 2020 Q2 .001

plotvar<- Kusko_map(2020,.001,"Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 Q2", .001)

# 2020 Q3 .001

plotvar<- Kusko_map(2020,.001,"Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 Q3", .001)

# 2020 Q4 .001

plotvar<- Kusko_map(2020,.001,"Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2020 Q4", .001)

#2021
plotvar<- Kusko_map(2021,.001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 Full run", .001)

#2021 H1 .001

plotvar<- Kusko_map(2021,.001,"H1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 H1", .001)

#2021 H2 .001

plotvar<- Kusko_map(2021,.001,"H2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 H2", .001)

# 2021 Q1 .001

plotvar<- Kusko_map(2021,.001,"Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 Q1", .001)

# 2021 Q2 .001

plotvar<- Kusko_map(2021,.001,"Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 Q2", .001)

# 2021 Q3 .001

plotvar<- Kusko_map(2021,.001,"Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 Q3", .001)

# 2021 Q4 .001

plotvar<- Kusko_map(2021,.001,"Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko 2021 Q4", .001)


# Yukon 

#2015
plotvar<- Yukon_map(2017,.001)


