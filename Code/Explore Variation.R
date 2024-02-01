
#### This script uses the CV and change function to explore how production has varied at each location over time
library(here)
library(tidyverse)
library(gridExtra)


source(here("Code/Functions/CV_and change_function.R"))

sensitivity_thresholds <- seq(.4, .9, .1)

#Loop through each of the sensitivity threshold values, 
#Calculate the CV among years, mean CV, and change in production between 2015-2016 and 2016-2017
#Export the results to a csv file

for (threshold in sensitivity_thresholds ){
  CV_and_change(threshold)
}

# Calculate and visualize how the mean CV from 2015-2017 changes over time 
CV_data <- data.frame(
  threshold = NA,
  mean_cv = NA, 
  avg_str_length = NA, 
  within_year_CV = NA
)

#Loop through each of the sensitivity thresholds and add threshold, 
#read in the production csv, 
#calculate the mean CV, 
#add it to the new data frame

for (threshold in sensitivity_thresholds){
  file_path <- here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv"))
  cv_file_data <- read_csv(file_path)
  
  CV_data <- CV_data %>% 
    add_row(
      threshold = threshold, 
      mean_cv = mean(cv_file_data$mean),
      avg_str_length = cv_file_data$avg_str_length[1], 
      within_year_CV = cv_file_data$mean_within_year_CV[1]
    )
}


#Plot mean CV vs threshold
p1<-ggplot(CV_data, aes(x = threshold, y = mean_cv)) +
  geom_point(color = "coral") +
  geom_line(color = "coral") +
  labs(x = "Threshold", y = "CV")+ 
  ggtitle("CV of relative production among years vs cutoff threshold")+ 
  theme_gray()

p2<-ggplot(CV_data, aes(x = threshold, y = avg_str_length)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(x = "Threshold", y = "KM Stream length ")+ 
  ggtitle("Average > .7 Stream length (KM)")+ 
  theme_gray()

p3<-ggplot(CV_data, aes(x = threshold, y = within_year_CV)) +
  geom_point(color = "green") +
  geom_line(color = "green") +
  labs(x = "Threshold", y = "CV")+ 
  ggtitle("Within year CV of production estimates")+ 
  theme_gray()


grid.arrange(p1, p2 , p3,  ncol = 1)


# Export to figures
ggsave(here("Results/Figures/CV_and_Stream_length.png"), width = 6, height = 6, units = "in", dpi = 300)






############# Variation within a year, among thresholds ###### 

full_2015_.4<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.4_basin_norm.csv"))  %>% select(normalized)
full_2015_.5<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.5_basin_norm.csv")) %>% select(normalized)
full_2015_.6<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.6_basin_norm.csv")) %>% select(normalized)
full_2015_.7<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.7_basin_norm.csv")) %>% select(normalized)
full_2015_.8<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.8_basin_norm.csv")) %>% select(normalized)
full_2015_.9<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.9_basin_norm.csv")) %>% select(normalized)


#Put all of these into one dataframe 
full_2015 <- data.frame(
  threshold_4 = full_2015_.4,
  threshold_5 = full_2015_.5,
  threshold_6 = full_2015_.6,
  threshold_7 = full_2015_.7,
  threshold_8 = full_2015_.8,
  threshold_9 = full_2015_.9
)

#Put the data into long format

full_2015_long <- full_2015 %>% 
  pivot_longer(cols = everything(), names_to = "threshold", values_to = "production") %>%
  arrange(threshold)

#Create a boxplotfor each threshold
library(ggridges)
ggplot(full_2015_long, aes(x = production, y = threshold ,color = threshold, fill = threshold)) +
  geom_density_ridges(alpha = .15)


library(tidyverse)
library(ggridges)

# Define a function to read and process data for a specific year and threshold
read_and_process_data <- function(year, threshold) {
  file_path <- here::here(sprintf("Data/Production/Yukon/%d_full_Yukon_%.1f_basin_norm.csv", year, threshold))
  data <- read_csv(file_path) %>% select(normalized)
  return(data %>% mutate(year = year, threshold = as.character(threshold)))
}

# Define the thresholds
thresholds <- seq(0.4, 0.9, by = 0.1)

# Create a list to store dataframes for each combination of year and threshold
data_list <- list()

# Loop through years and thresholds to read and process data
for (year in c(2015, 2016, 2017, 2019, 2021)) {  # Add more years as needed
  for (threshold in thresholds) {
    data_list[[paste0("full_", year, "_", threshold)]] <- read_and_process_data(year, threshold)
  }
}

# Combine data into one dataframe
full_data <- bind_rows(data_list)

# Create a boxplot for each threshold
ggplot(full_data, aes(x = normalized, y = as.factor(threshold), fill = as.factor(year))) +
  geom_density_ridges(alpha = .4) +
  labs(title = "Production Distribution by Threshold and Year")

ggplot(full_data, aes(x = as.factor(threshold), y = normalized, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Production Distribution by Threshold and Year", x = "Threshold", y = "Normalized Production")

