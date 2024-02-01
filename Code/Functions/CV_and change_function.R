#' Calculate Metrics for Yukon Production Data
#'
#' This function reads in production data for each year, calculates various metrics, 
#' and exports the results as a CSV file.
#'
#' @param threshold Numeric value specifying the sensitivity threshold.
#'
#' @return A data frame containing calculated metrics for each year, including standard deviation (sd),
#'         mean, coefficient of variation (cv), mean coefficient of variation (mean_cv),
#'         change from 2016 to 2017 (change_2016_2017), and change from 2015 to 2016 (change_2015_2016).
#'
#' @export
#'
#' @examples
#' calculate_metrics(threshold = 0.5)
threshold <- 0.5

CV_and_change <- function(threshold) {
  
  library(sf)
  
  # Read in each production year for the given threshold
  prod_2015 <- read_csv(here(paste0("Data/Production/Yukon/2015_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2016 <- read_csv(here(paste0("Data/Production/Yukon/2016_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2017 <- read_csv(here(paste0("Data/Production/Yukon/2017_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2019 <- read_csv(here(paste0("Data/Production/Yukon/2019_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2021 <- read_csv(here(paste0("Data/Production/Yukon/2021_full_Yukon_", threshold, "_basin_norm.csv")))
  
  # Combine data frames for all years
  prod_all <- data.frame(
    year_2015_resc = prod_2015$rescaled,
    year_2015_norm = prod_2015$normalized,
    year_2016_resc = prod_2016$rescaled,
    year_2016_norm = prod_2016$normalized,
    year_2017_resc= prod_2017$rescaled,
    year_2017_norm = prod_2017$normalized,
    year_2019_resc = prod_2019$rescaled,
    year_2019_norm = prod_2019$normalized,
    year_2021_resc = prod_2021$rescaled,
    year_2021_norm = prod_2021$normalized
  )
  
  Yukon_shapefile<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
  
  # Calculate metrics
  prod_all$sd <- apply(prod_all, 1, sd)
  prod_all$mean <- apply(prod_all, 1, mean)
  prod_all$cv <- prod_all$sd / prod_all$mean
  prod_all$mean_cv <- apply(prod_all, 1, mean, na.rm = TRUE)
  prod_all$change_2016_2017 <- prod_all$year_2017_resc - prod_all$year_2016_resc
  prod_all$change_2015_2016 <- prod_all$year_2016_resc - prod_all$year_2015_resc
  prod_all$Str_length_2015 <- sum(Yukon_shapefile$Shape_Leng[prod_all$year_2015_norm > .7])/1000
  prod_all$Str_length_2016 <- sum(Yukon_shapefile$Shape_Leng[prod_all$year_2016_norm > .7])/1000
  prod_all$Str_length_2017 <- sum(Yukon_shapefile$Shape_Leng[prod_all$year_2017_norm > .7])/1000
  prod_all$Str_length_2019 <- sum(Yukon_shapefile$Shape_Leng[prod_all$year_2019_norm > .7])/1000
  prod_all$Str_length_2021 <- sum(Yukon_shapefile$Shape_Leng[prod_all$year_2021_norm > .7])/1000
  prod_all$avg_str_length <- mean(c(prod_all$Str_length_2015[1], prod_all$Str_length_2016[1], prod_all$Str_length_2017[1], prod_all$Str_length_2019[1], prod_all$Str_length_2021[1]))
  prod_all$within_year_CV_2015 <- sd(prod_2015$rescaled) / mean(prod_2015$rescaled) * 100
  prod_all$within_year_CV_2016 <- sd(prod_2016$rescaled) / mean(prod_2016$rescaled) * 100
  prod_all$within_year_CV_2017 <- sd(prod_2017$rescaled) / mean(prod_2017$rescaled) * 100
  prod_all$within_year_CV_2019 <- sd(prod_2019$rescaled) / mean(prod_2019$rescaled) * 100
  prod_all$within_year_CV_2021 <- sd(prod_2021$rescaled) / mean(prod_2021$rescaled) * 100
  prod_all$mean_within_year_CV <- mean(c(prod_all$within_year_CV_2015[1], prod_all$within_year_CV_2016[1], prod_all$within_year_CV_2017[1], prod_all$within_year_CV_2019[1], prod_all$within_year_CV_2021[1]))
    
  # Export as a .csv
  write_csv(prod_all, here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv")))
  
}

library(tidyverse)
