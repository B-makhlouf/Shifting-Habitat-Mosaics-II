# Source the code/Provenance assignment Yukon.R

source("code/Provenance assignment Yukon.R")
library(tidyverse)

# Make a list from .4 to .9 in increments of .1
sensitivity_thresholds <- seq(.4, .9, .1)
years<- c(2015,2016,2017)
Quartiles<- c("Q1","Q2","Q3","Q4")

#loop through each year, threshold value, and quartile

for (year in years){
  for (threshold in sensitivity_thresholds){
    for (quartile in Quartiles){
      
      # Run the function
      Yukon_map(year, threshold, quartile)
    }
  }
}

for (year in years){
  for (threshold in sensitivity_thresholds){
      # Run the function
      Yukon_map(year, threshold)
    }
  }



## Read in each production year for each threshold 

prod_2015_.4 <- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.4_basin_norm.csv"))
prod_2016_.4 <- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.4_basin_norm.csv"))
prod_2017_.4 <- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.4_basin_norm.csv"))

#intialize an empty data frame with collum names 2015, 2016, and 2017 using dplyr

prod_all_.4 <- data.frame(
  year_2015 = prod_2015_.4$basin_assign_rescale,
  year_2016 = prod_2016_.4$basin_assign_rescale,
  year_2017 = prod_2017_.4$basin_assign_rescale
)

#Calculate the standard deviation for each row

prod_all_.4$sd <- apply(prod_all_.4, 1, sd)

#Calculate the mean of each row

prod_all_.4$mean <- apply(prod_all_.4, 1, mean)

#Calculate the CV for each row 

prod_all_.4$cv <- prod_all_.4$sd/prod_all_.4$mean

#Calculate the mean CV for each year

prod_all_.4$mean_cv <- mean(prod_all_.4$cv)

#Export as a .csv 

write_csv(prod_all_.4, here("Results/Variability/Yukon_Production_all_.4.csv"))

#Repeat for .5

prod_2015_.5 <- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.5_basin_norm.csv"))
prod_2016_.5 <- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.5_basin_norm.csv"))
prod_2017_.5 <- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.5_basin_norm.csv"))

prod_all_.5 <- data.frame(
  year_2015 = prod_2015_.5$basin_assign_rescale,
  year_2016 = prod_2016_.5$basin_assign_rescale,
  year_2017 = prod_2017_.5$basin_assign_rescale
)

prod_all_.5$sd <- apply(prod_all_.5, 1, sd)
prod_all_.5$mean <- apply(prod_all_.5, 1, mean)
prod_all_.5$cv <- prod_all_.5$sd/prod_all_.5$mean
prod_all_.5$mean_cv <- mean(prod_all_.5$cv)

write_csv(prod_all_.5, here("Results/Variability/Yukon_Production_all_.5.csv"))

#Repeat for .6

prod_2015_.6 <- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.6_basin_norm.csv"))
prod_2016_.6 <- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.6_basin_norm.csv"))
prod_2017_.6 <- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.6_basin_norm.csv"))

prod_all_.6 <- data.frame(
  year_2015 = prod_2015_.6$basin_assign_rescale,
  year_2016 = prod_2016_.6$basin_assign_rescale,
  year_2017 = prod_2017_.6$basin_assign_rescale
)

prod_all_.6$sd <- apply(prod_all_.6, 1, sd)
prod_all_.6$mean <- apply(prod_all_.6, 1, mean)
prod_all_.6$cv <- prod_all_.6$sd/prod_all_.6$mean
prod_all_.6$mean_cv <- mean(prod_all_.6$cv)

write_csv(prod_all_.6, here("Results/Variability/Yukon_Production_all_.6.csv"))



