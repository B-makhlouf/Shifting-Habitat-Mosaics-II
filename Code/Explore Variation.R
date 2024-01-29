
#### This script uses the CV and change function to explore how production has varied at each location over time
library(here)

source(here("Code/Functions/CV_and change_function.R"))

sensitivity_thresholds <- seq(.4, .9, .1)

#Loop through each of the sensitivity threshold values, 
#Calculate the CV among years, mean CV, and change in production between 2015-2016 and 2016-2017
#Export the results to a csv file

for (threshold in sensitivity_thresholds ){
  calculate_metrics(threshold)
}

# Calculate and visualize how the mean CV from 2015-2017 changes over time 
CV_data <- data.frame(
  threshold = NA,
  mean_cv = NA
)

#Loop through each of the sensitivity thresholds and add threshold, 
#read in the production csv, 
#calculate the mean CV, 
#add it to the new data frame

for (threshold in sensitivity_thresholds){
  CV_data <- CV_data %>% 
    add_row(threshold = threshold, mean_cv = mean(read_csv(here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv"))) 
                                                  %>% select(mean_cv) %>% 
                                                    unlist()))
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