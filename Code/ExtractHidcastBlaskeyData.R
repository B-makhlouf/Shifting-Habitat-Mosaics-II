library(ncdf4)
library(dplyr)
library(lubridate)
library(tidyr)

# Function to process NetCDF files and create weekly summaries
process_netcdf_data <- function(data_dir, pattern, variable_name, output_dir, output_prefix, file_type = "discharge") {
  
  cat("\n========================================\n")
  cat("Processing:", variable_name, "\n")
  cat("========================================\n\n")
  
  # Get ALL files for 2015-2021
  nc_files <- list.files(
    path = data_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  cat("Found", length(nc_files), "files to process (2015-2021, all outlets)\n\n")
  
  # Check if files were found
  if (length(nc_files) == 0) {
    cat("ERROR: No files found matching pattern:", pattern, "\n")
    cat("In directory:", data_dir, "\n")
    return(NULL)
  }
  
  # Initialize empty list to store results
  all_data <- list()
  
  # Create progress bar
  pb <- txtProgressBar(min = 0, max = length(nc_files), style = 3)
  
  # Loop through each file
  for (i in 1:length(nc_files)) {
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # Open file
    nc <- nc_open(nc_files[i])
    
    # Extract data based on file type
    data_vals <- ncvar_get(nc, variable_name)
    
    if (file_type == "discharge") {
      # Discharge files structure: [reach, time]
      reach_ids <- ncvar_get(nc, "reachID")
      time_vals <- ncvar_get(nc, "time")
      origin_date <- as.Date("1989-06-01")
      
    } else if (file_type == "temperature") {
      # Temperature files structure: [hru, no_seg, time]
      reach_ids <- ncvar_get(nc, "hru")
      time_vals <- ncvar_get(nc, "time")
      
      # Extract year from filename (e.g., "81000004_2015.nc" -> 2015)
      year <- as.numeric(sub(".*_(\\d{4})\\.nc$", "\\1", basename(nc_files[i])))
      origin_date <- as.Date(paste0(year, "-01-01"))
      
      # Extract downstream segment (no_seg = 2)
      # data_vals is [hru, no_seg, time], we want [hru, time] for no_seg=2
      data_vals <- data_vals[, 2, ]
    }
    
    nc_close(nc)
    
    # Convert time to dates
    dates <- origin_date + time_vals
    
    # Filter to June 1 - July 31 only
    june_july_indices <- which(month(dates) %in% c(6, 7))
    
    if (length(june_july_indices) > 0) {
      dates_filtered <- dates[june_july_indices]
      
      # Filter data for June-July
      # After processing, both file types have [reach/hru, time] structure
      data_filtered <- data_vals[, june_july_indices]
      
      # Create data frame for this file
      for (j in 1:length(reach_ids)) {
        df_temp <- data.frame(
          COMID = reach_ids[j],
          date = dates_filtered,
          value = data_filtered[j, ]
        )
        all_data[[length(all_data) + 1]] <- df_temp
      }
    }
  }
  
  # Close progress bar
  close(pb)
  
  # Combine all data and remove any duplicates
  cat("\n\nCombining all data...\n")
  full_data <- bind_rows(all_data) %>%
    distinct(COMID, date, .keep_all = TRUE)
  
  # Calculate weekly averages
  cat("Calculating weekly averages...\n")
  weekly_data <- full_data %>%
    mutate(
      year = year(date),
      week_start = floor_date(date, "week")
    ) %>%
    group_by(COMID, year, week_start) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    ) %>%
    filter(n_days == 7) %>%  # ONLY KEEP COMPLETE 7-DAY WEEKS
    select(-n_days) %>%
    arrange(COMID, year, week_start)
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save regular format to CSV
  output_file <- file.path(output_dir, paste0(output_prefix, ".csv"))
  write.csv(weekly_data, output_file, row.names = FALSE)
  
  cat("\nRegular format saved to:", output_file, "\n")
  cat("Total rows:", nrow(weekly_data), "\n")
  cat("Unique COMIDs:", length(unique(weekly_data$COMID)), "\n")
  cat("Years covered:", min(weekly_data$year), "-", max(weekly_data$year), "\n")
  
  # Create pivoted version
  cat("\nCreating pivoted version...\n")
  weekly_data_pivot <- weekly_data %>%
    mutate(
      week_num = week(week_start),
      field_name = paste0(substr(output_prefix, 1, 1), "_", year, "_wk", week_num)
    ) %>%
    select(COMID, field_name, mean_value) %>%
    pivot_wider(
      names_from = field_name,
      values_from = mean_value
    )
  
  # Save pivoted data
  output_file_pivot <- file.path(output_dir, paste0(output_prefix, "_Pivoted.csv"))
  write.csv(weekly_data_pivot, output_file_pivot, row.names = FALSE)
  
  cat("Pivoted data saved to:", output_file_pivot, "\n")
  cat("Columns:", ncol(weekly_data_pivot), "\n")
  cat("Rows:", nrow(weekly_data_pivot), "\n")
  
  return(list(regular = weekly_data, pivoted = weekly_data_pivot))
}

# ========================================
# PROCESS DISCHARGE DATA
# ========================================
discharge_results <- process_netcdf_data(
  data_dir = "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Data/Spatial Data/Blaskey_Hindcast_simdata/mizuRoute_Output",
  pattern = "AK_Rivers_.*\\.h\\.(2015|2016|2017|2018|2019|2020|2021).*\\.nc$",
  variable_name = "IRFroutedRunoff",
  output_dir = "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Data/Spatial Data/Blaskey_Hindcast_simdata/RiverDischargeExtracted",
  output_prefix = "WeeklyRiverDischargeExtr",
  file_type = "discharge"
)

# ========================================
# PROCESS TEMPERATURE DATA
# ========================================
temperature_results <- process_netcdf_data(
  data_dir = "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Data/Spatial Data/Blaskey_Hindcast_simdata/Production",
  pattern = "^\\d+_(2015|2016|2017|2018|2019|2020|2021)\\.nc$",
  variable_name = "T_stream",
  output_dir = "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Data/Spatial Data/Blaskey_Hindcast_simdata/RiverTempExtracted",
  output_prefix = "WeeklyRiverTempExtr",
  file_type = "temperature"
)

cat("\n========================================\n")
cat("ALL PROCESSING COMPLETE!\n")
cat("========================================\n")