






# 
# ### Data 
# # Load Libraries
# library(tidyverse)
# library(here)
# library(zoo)
# library(lubridate)
# 
# # Function for processing Yukon data
# Add_CPUE_weights_Yukon <- function(year) {
#   # File paths for Yukon data
#   natal_file <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origins/Extracted/", year, "_Yukon_Natal_Origins_Genetics.csv")
#   cpue_file <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/CPUE Data/Cleaned/Yukon_CPUE_", year, ".csv")
#   identifier <- paste0(year, "_Yukon")
#   
#   # Load data
#   natal_values <- read_csv(natal_file)
#   CPUE <- read_csv(cpue_file)
#   
#   # Process the data
#   processed_data <- process_CPUE_and_natal_data(natal_values, CPUE, identifier)
#   
#   # Save the processed data
#   file_name <- paste0("ALL_DATA_", identifier, "_Natal_Origins.csv")
#   output_path <- file.path("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE", file_name)
#   write_csv(processed_data, output_path)
#   message("Data processed and saved for Yukon ", year)
# }
# 
# 
# # Function for processing Kuskokwim data
# Add_CPUE_weights_Kusko <- function(year) {
#   # File paths for Kuskokwim data
#   natal_file <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origins/QCd/", year, " Kuskokwim_QCd_Natal_Origins.csv")
#   cpue_file <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/CPUE Data/Cleaned/Kusko_CPUE_", year, ".csv")
#   identifier <- paste0(year, "_Kusko")
#   
#   # Load data
#   natal_values <- read_csv(natal_file)
#   CPUE <- read_csv(cpue_file)
#   
#   # Process the data
#   processed_data <- process_CPUE_and_natal_data(natal_values, CPUE, identifier)
#   
#   # Save the processed data
#   file_name <- paste0("ALL_DATA_", identifier, "_Natal_Origins.csv")
#   output_path <- file.path("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE", file_name)
#   write_csv(processed_data, output_path)
#   message("Data processed and saved for Kuskokwim ", year)
# }
# 
# 
# 
# # Helper function to process CPUE and natal data (used by both Yukon and Kusko functions)
# process_CPUE_and_natal_data <- function(natal_values, CPUE, identifier) {
#   # Remove individuals without genetics data
#   natal_values <- natal_values %>%
#     filter(!is.na("Lower"))
#   
#   # Summarize the number of fish caught per day of the year (DOY)
#   DOY_summary <- natal_values %>%
#     group_by(DOY) %>%
#     summarise(Oto_catch_daily = n())
#   
#   # Calculate the proportion of otoliths caught per DOY
#   DOY_summary <- DOY_summary %>%
#     mutate(OtoPropDaily = Oto_catch_daily / sum(Oto_catch_daily))
#   
#   # Clean up CPUE data and make sure Date is in date format
#   CPUE <- CPUE %>%
#     filter(!is.na(dailyCPUE)) %>%
#     mutate(Date = as.Date(Date), DOY = yday(Date))
#   
#   # Add Quartile information to CPUE
#   CPUE_quartiles <- CPUE %>%
#     mutate(Quartile = case_when(
#       DOY <= yday(as.Date(paste0(year(Date), "-06-07"))) ~ "Q1",
#       DOY <= yday(as.Date(paste0(year(Date), "-06-14"))) ~ "Q2",
#       DOY <= yday(as.Date(paste0(year(Date), "-06-21"))) ~ "Q3",
#       DOY <= yday(as.Date(paste0(year(Date), "-06-30"))) ~ "Q4",
#       DOY > yday(as.Date(paste0(year(Date), "-06-30"))) ~ "Q4", # Extend Q4 to include DOY > June 30
#       DOY < yday(as.Date(paste0(year(Date), "-06-07"))) ~ "Q1"  # Extend Q1 to include DOY < June 7
#     ))
#   
#   # Calculate daily CPUE proportion
#   CPUE <- CPUE %>%
#     group_by(DOY) %>%
#     summarise(dailyCPUEprop = sum(dailyCPUE) / sum(CPUE$dailyCPUE))
#   
#   # Create a data frame with all unique DOY values from CPUE
#   ALL_CPUE_days <- tibble(DOY = unique(CPUE$DOY)) %>%
#     left_join(CPUE, by = "DOY") %>%
#     left_join(DOY_summary, by = "DOY") %>%
#     select(-Oto_catch_daily) %>%
#     mutate(OtoPropDaily = ifelse(is.na(OtoPropDaily), 0, OtoPropDaily))
#   
#   # Calculate the ratio of daily CPUE proportion to daily otolith proportion
#   ALL_CPUE_days <- ALL_CPUE_days %>%
#     mutate(COratio = dailyCPUEprop / OtoPropDaily,
#            COratio = ifelse(is.na(COratio) | is.infinite(COratio), 0, COratio))
#   
#   # Create Strata weights
#   ndaysPerStrat <- 5
#   strat_index <- rep(1:(nrow(CPUE) / ndaysPerStrat), each = ndaysPerStrat)
#   strat_index <- c(strat_index, rep(max(strat_index), nrow(CPUE) - length(strat_index)))
#   
#   strat.wt <- sapply(unique(strat_index), function(i) {
#     CPUEmean <- mean(ALL_CPUE_days$dailyCPUEprop[which(strat_index == i)])
#     otomean <- mean(ALL_CPUE_days$OtoPropDaily[which(strat_index == i)])
#     wt <- ifelse(is.infinite(CPUEmean / otomean) | is.na(CPUEmean / otomean), 0, CPUEmean / otomean)
#     return(wt)
#   })
#   
#   strat <- strat.wt[strat_index]
#   ALL_CPUE_days$Strat <- strat
#   
#   # Join calculated strata and quartile data with natal values
#   natal_values <- natal_values %>%
#     left_join(ALL_CPUE_days, by = "DOY") %>%
#     left_join(CPUE_quartiles %>% select(DOY, Quartile), by = "DOY") %>%
#     filter(!is.na(Date)) %>%
#     mutate(Quartile = ifelse(is.na(Quartile), "Q4", Quartile),
#            Strat = ifelse(is.na(Strat), strat.wt[length(strat.wt)], Strat))
#   
#   return(natal_values)
# }
# 
# Add_CPUE_weights_Yukon(2015)
# Add_CPUE_weights_Yukon(2016)
# Add_CPUE_weights_Yukon(2021)
