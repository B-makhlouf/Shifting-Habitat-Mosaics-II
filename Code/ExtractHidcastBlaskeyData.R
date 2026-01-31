library(ncdf4)
library(dplyr)
library(lubridate)

# Set your directory
data_dir <- "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Data/Spatial Data/Blaskey_Hindcast_simdata/mizuRoute_Output"

# Get only 2015-2021 files for outlet 81000004
nc_files <- list.files(
  path = data_dir,
  pattern = "AK_Rivers_81000004\\.h\\.(2015|2016|2017|2018|2019|2020|2021).*\\.nc$",
  full.names = TRUE
)

cat("Found", length(nc_files), "files to process (2015-2021)\n")

# Initialize empty list to store results
all_data <- list()

# Loop through each file
for (i in 1:length(nc_files)) {
  cat("Processing file", i, "of", length(nc_files), "\n")
  
  # Open file
  nc <- nc_open(nc_files[i])
  
  # Extract data
  discharge <- ncvar_get(nc, "IRFroutedRunoff")
  reach_ids <- ncvar_get(nc, "reachID")
  time_vals <- ncvar_get(nc, "time")
  
  nc_close(nc)
  
  # Convert time to dates
  origin_date <- as.Date("1989-06-01")
  dates <- origin_date + time_vals
  
  # Filter to June 1 - July 31 only
  june_july_indices <- which(month(dates) %in% c(6, 7))
  
  if (length(june_july_indices) > 0) {
    dates_filtered <- dates[june_july_indices]
    discharge_filtered <- discharge[, june_july_indices]
    
    # Create data frame for this file
    for (j in 1:length(reach_ids)) {
      df_temp <- data.frame(
        COMID = reach_ids[j],
        date = dates_filtered,
        discharge_m3s = discharge_filtered[j, ]
      )
      all_data[[length(all_data) + 1]] <- df_temp
    }
  }
}

# Combine all data
cat("\nCombining all data...\n")
full_data <- bind_rows(all_data)

# Calculate weekly averages
cat("Calculating weekly averages...\n")
weekly_data <- full_data %>%
  mutate(
    year = year(date),
    week_start = floor_date(date, "week")
  ) %>%
  group_by(COMID, year, week_start) %>%
  summarise(
    mean_discharge_m3s = mean(discharge_m3s, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  arrange(COMID, year, week_start)

# Save to CSV
output_file <- "C:/Users/makhl/Research Repos/Shifting-Habitat-Mosaics-II/Data/weekly_discharge_june_july_2015_2021.csv"
write.csv(weekly_data, output_file, row.names = FALSE)

cat("\nDONE!\n")
cat("Output saved to:", output_file, "\n")
cat("Total rows:", nrow(weekly_data), "\n")
cat("Unique COMIDs:", length(unique(weekly_data$COMID)), "\n")
cat("Years covered:", min(weekly_data$year), "-", max(weekly_data$year), "\n")