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


CV_and_change <- function(threshold) {
  
  # Read in each production year for the given threshold
  prod_2015 <- read_csv(here(paste0("Data/Production/Yukon/2015_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2016 <- read_csv(here(paste0("Data/Production/Yukon/2016_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2017 <- read_csv(here(paste0("Data/Production/Yukon/2017_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2019 <- read_csv(here(paste0("Data/Production/Yukon/2019_full_Yukon_", threshold, "_basin_norm.csv")))
  prod_2021 <- read_csv(here(paste0("Data/Production/Yukon/2021_full_Yukon_", threshold, "_basin_norm.csv")))
  
  # Combine data frames for all years
  prod_all <- data.frame(
    year_2015 = prod_2015$rescaled,
    year_2016 = prod_2016$rescaled,
    year_2017 = prod_2017$rescaled,
    year_2019 = prod_2019$rescaled,
    year_2021 = prod_2021$rescaled
  )
  
  # Calculate metrics
  prod_all$sd <- apply(prod_all, 1, sd)
  prod_all$mean <- apply(prod_all, 1, mean)
  prod_all$cv <- prod_all$sd / prod_all$mean
  prod_all$mean_cv <- apply(prod_all, 1, mean, na.rm = TRUE)
  prod_all$change_2016_2017 <- prod_all$year_2017 - prod_all$year_2016
  prod_all$change_2015_2016 <- prod_all$year_2016 - prod_all$year_2015
  
  # Export as a .csv
  write_csv(prod_all, here(paste0("Results/Variability/Yukon_Production_all_", threshold, ".csv")))
  
}
