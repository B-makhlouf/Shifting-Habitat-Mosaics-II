# Source the code/Provenance assignment Yukon.R

source("code/Provenance assignment Yukon.R")
library(tidyverse)
library(here)

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


calculate_metrics <- function(threshold) {
  # Read in each production year for the given threshold
  prod_2015 <- read_csv(here(paste0("Data/Production/Yukon/2015_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2016 <- read_csv(here(paste0("Data/Production/Yukon/2016_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2017 <- read_csv(here(paste0("Data/Production/Yukon/2017_full_Yukon_", threshold, "_basin_norm.csv")))
  
  # Initialize an empty data frame
  prod_all <- data.frame(
    year_2015 = prod_2015$basin_assign_rescale,
    year_2016 = prod_2016$basin_assign_rescale,
    year_2017 = prod_2017$basin_assign_rescale
  )
  
  # Calculate the standard deviation, mean, CV for each row
  prod_all$sd <- apply(prod_all, 1, sd)
  prod_all$mean <- apply(prod_all, 1, mean)
  prod_all$cv <- prod_all$sd / prod_all$mean
  
  # Calculate the mean CV for each year without NAs
  prod_all$mean_cv <- apply(prod_all, 1, mean, na.rm = TRUE)
  
  # Calculate change from 2016 to 2017
  prod_all$change_2016_2017 <- prod_all$year_2017 - prod_all$year_2016
  
  # Calculate change from 2015 to 2016 
  prod_all$change_2015_2016 <- prod_all$year_2016 - prod_all$year_2015
  
  # Export as a .csv
  write_csv(prod_all, here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv")))
}

for (threshold in sensitivity_thresholds ){
  calculate_metrics(threshold)
}


# Create a dataframe with the collumns threshold and mean_CV

CV_data <- data.frame(
  threshold = NA,
  mean_cv = NA
)

#Loop through each threshold file and add the mean CV to the dataframe

for (threshold in sensitivity_thresholds){
  CV_data <- CV_data %>% add_row(threshold = threshold, mean_cv = mean(read_csv(here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv"))) %>% select(mean_cv) %>% unlist()))
}

#Plot mean CV vs threshold

ggplot(CV_data, aes(x = threshold, y = mean_cv)) +
  geom_point(color = "coral") +
  geom_line(color = "coral") +
  labs(x = "Threshold", y = "CV")+ 
  ggtitle("CV of relative production among years vs cutoff threshold")+ 
  theme_gray()

# Export to figures
ggsave(here("Results/Figures/Yukon_CV_threshold.png"), width = 6, height = 6, units = "in", dpi = 300)





