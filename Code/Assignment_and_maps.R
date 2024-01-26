# Source the code/Provenance assignment Yukon.R

source("code/Provenance assignment Yukon.R")

# Make a list from .4 to .9 in increments of .1
sensitivity_thresholds <- seq(.4, .9, .1)
years<- c(2015,2016,2017)

#loop through each year and each threshold value and apply Yukon_map function

for (year in years) {
  for (sensitivity_threshold in sensitivity_thresholds) {
    Yukon_map(year, sensitivity_threshold)
  }
}
