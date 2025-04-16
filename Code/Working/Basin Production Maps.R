# Load required libraries
library(tidyverse)
library(here)
library(sf)
library(cowplot)
library(fabricatr)
install.packages("fabricatr")


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



# Kusko 
KK_Map_func(2017, .5, , min_error = .0006, min_stream_order = 3)
KK_Map_func(2018, .5, , min_error = .0006, min_stream_order = 3)
KK_Map_func(2019, .5, , min_error = .0006, min_stream_order = 3)
KK_Map_func(2020, .5, , min_error = .0006, min_stream_order = 3)
KK_Map_func(2021, .5, , min_error = .0006, min_stream_order = 3)

## Same thing, but with min_stream_oder = 4 
KK_Map_func(2017, .01, , min_error = .0006, min_stream_order = 4)
KK_Map_func(2018, .01, , min_error = .0006, min_stream_order = 4)
KK_Map_func(2019, .01, , min_error = .0006, min_stream_order = 4)
KK_Map_func(2020, .01, , min_error = .0006, min_stream_order = 4)
KK_Map_func(2021, .01, , min_error = .0006, min_stream_order = 4)

### 2017 Q1-Q4 
KK_Map_func(2017, .5, "Q1", min_error = .0006, min_stream_order = 4)
KK_Map_func(2017, .5, "Q2", min_error = .0006, min_stream_order = 4)
KK_Map_func(2017, .5, "Q3", min_error = .0006, min_stream_order = 4)
KK_Map_func(2017, .5, "Q4", min_error = .0006, min_stream_order = 4)

### 2018 Q1-Q4
KK_Map_func(2018, .5, "Q1", min_error = .0006, min_stream_order = 3)
KK_Map_func(2018, .5, "Q2", min_error = .0006, min_stream_order = 3)
KK_Map_func(2018, .5, "Q3", min_error = .0006, min_stream_order = 3)
KK_Map_func(2018, .5, "Q4", min_error = .0006, min_stream_order = 3)



### Yukon with different lower StrOrd limits 
YK_Map_func(2015, .5, , min_error = .0035, min_stream_order = 6)
YK_Map_func(2015, .5, , min_error = .0035, min_stream_order = 5)
YK_Map_func(2015, .5, , min_error = .0035, min_stream_order = 4)


# Yukon different years 
YK_Map_func(2015, .5, min_error = .0035, min_stream_order = 5)
YK_Map_func(2016, .5, min_error = .0035, min_stream_order = 5)
# YK_Map_func(2017, .5, min_error = .0035, min_stream_order = 5)
# YK_Map_func(2018, .5, min_error = .0035, min_stream_order = 5)
# YK_Map_func(2019, .5, min_error = .0035, min_stream_order = 5)
YK_Map_func(2021, .5, min_error = .0035, min_stream_order = 5)


# Yukon different years with full error 
YK_Map_func(2015, .0001, min_error = .0035, min_stream_order = 5)
YK_Map_func(2016, .0001, min_error = .0035, min_stream_order = 5)
YK_Map_func(2021, .0001, min_error = .0035, min_stream_order = 5)


























#2017
plotvar<- Kusko_map(2017, .001, min_erro = .0006)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017", .001)

#2018
plotvar<- Kusko_map(2018, .001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018", .5)

#2019
plotvar<- Kusko_map(2019, .001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2019", .5)

#2020
plotvar<- Kusko_map(2020, .001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2020", .5)

#2021
plotvar<- Kusko_map(2021, .001)
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2021", .5)


##### YUKON the three years we have, 2015, 2016, 2021 


plotvar<- Yukon_map(2015, .001)
Map_Base("Yukon", plotvar, "Yukon_2015", .5)


plotvar<- Yukon_map(2016, .001)
Map_Base("Yukon", plotvar, "Yukon_2016", .5)


plotvar<- Yukon_map(2021, .001)
Map_Base("Yukon", plotvar, "Yukon_2021", .5)


############## 
# Yukon Q1-Q4 2015-2016

# Yukon 2015 Q1-Q4
plotvar<- Yukon_map(2015, .001, "Q1")
Map_Base("Yukon", plotvar, "Yukon_2015_Q1", .5)

plotvar<- Yukon_map(2015, .001, "Q2")
Map_Base("Yukon", plotvar, "Yukon_2015_Q2", .5)

plotvar<- Yukon_map(2015, .001, "Q3")
Map_Base("Yukon", plotvar, "Yukon_2015_Q3", .5)

plotvar<- Yukon_map(2015, .001, "Q4")
Map_Base("Yukon", plotvar, "Yukon_2015_Q4", .5)


# Yukon 2016 Q1-Q4
plotvar<- Yukon_map(2016, .001, "Q1")
Map_Base("Yukon", plotvar, "Yukon_2016_Q1", .5)

plotvar<- Yukon_map(2016, .001, "Q2")
Map_Base("Yukon", plotvar, "Yukon_2016_Q2", .5)

plotvar<- Yukon_map(2016, .001, "Q3")
Map_Base("Yukon", plotvar, "Yukon_2016_Q3", .5)

plotvar<- Yukon_map(2016, .001, "Q4")
Map_Base("Yukon", plotvar, "Yukon_2016_Q4", .5)


# Yukon 2021 Q1-Q4
plotvar<- Yukon_map(2021, .001, "Q1")
Map_Base("Yukon", plotvar, "Yukon_2021_Q1", .5)

plotvar<- Yukon_map(2021, .001, "Q2")
Map_Base("Yukon", plotvar, "Yukon_2021_Q2", .5)

plotvar<- Yukon_map(2021, .001, "Q3")
Map_Base("Yukon", plotvar, "Yukon_2021_Q3", .5)

plotvar<- Yukon_map(2021, .001, "Q4")
Map_Base("Yukon", plotvar, "Yukon_2021_Q4", .5)

###########################################################################################


# Kuskokwim 2017 Q1-Q4 
plotvar<- Kusko_map(2017, .001, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q1", .7)

plotvar<- Kusko_map(2017, .001, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q2", .7)

plotvar<- Kusko_map(2017, .001, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q3", .7)

plotvar<- Kusko_map(2017, .001, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2017_Q4", .7)

# Kuskokwim 2018 Q1-Q4
plotvar<- Kusko_map(2018, .001, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q1", .1)

plotvar<- Kusko_map(2018, .001, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q2", .7)

plotvar<- Kusko_map(2018, .001, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q3", .7)

plotvar<- Kusko_map(2018, .001, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2018_Q4", .7)

# Kuskokwim 2019 Q1-Q4
plotvar<- Kusko_map(2019, .001, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2019_Q1", .7)

plotvar<- Kusko_map(2019, .001, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2019_Q2", .7)

plotvar<- Kusko_map(2019, .001, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2019_Q3", .7)

plotvar<- Kusko_map(2019, .001, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2019_Q4", .7)

# Kuskokwim 2020 Q1-Q4
plotvar<- Kusko_map(2020, .001, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2020_Q1", .7)

plotvar<- Kusko_map(2020, .001, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2020_Q2", .7)

plotvar<- Kusko_map(2020, .001, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2020_Q3", .7)

plotvar<- Kusko_map(2020, .001, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2020_Q4", .7)

# Kuskokwim 2021 Q1-Q4
plotvar<- Kusko_map(2021, .001, "Q1")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2021_Q1", .7)

plotvar<- Kusko_map(2021, .001, "Q2")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2021_Q2", .7)

plotvar<- Kusko_map(2021, .001, "Q3")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2021_Q3", .7)

plotvar<- Kusko_map(2021, .001, "Q4")
Map_Base_Kusko("Kuskokwim", plotvar, "Kusko_2021_Q4", .7)

###########################################################################################
###########################################################################################

# individual maps 

# Kuskokwim Natal Origins 
year<- 2017
Kusko2017NatalOrigins<-  read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Natal_Sr/QCd/ALL_DATA_", year, "_Kusko_Natal_Origins.csv"))

for (i in 1:length(Kusko2017NatalOrigins$natal_iso)){
  i<- as.numeric(i)
  plotvar<- Individual_Kusko_map(year, .001, i)
  identifier<- Kusko2017NatalOrigins$Fish_id[i]
  Map_Base_Kusko_Individual("Kuskokwim", plotvar, identifier, .001)
}

for (i in 1:length(Kusko2017NatalOrigins$natal_iso)){
  i<- as.numeric(i)
  plotvar<- LOW_ERROR_Individual_Kusko_map(year, .001, i)
  identifier<- paste0("LOW ERROR_",Kusko2017NatalOrigins$Fish_id[i])
  Map_Base_Kusko_Individual("Kuskokwim", plotvar, identifier, .001)
}



year<- 2018



