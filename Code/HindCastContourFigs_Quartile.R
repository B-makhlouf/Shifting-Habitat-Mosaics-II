# =============================================================================
# SETUP
# =============================================================================
library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(patchwork)
library(RColorBrewer)
library(ggplot2)

# =============================================================================
# READ IN BASE DATA (USED FOR ALL YEARS)
# =============================================================================
# Read shapefile
kusko_shp <- st_read(here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp")) %>%
  st_drop_geometry()

# Read Blaskey Hindcasted Temperature Data
RiverTemp <- read.csv(here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", 
                           "RiverTempExtracted", "WeeklyRiverTempExtr.csv"))

# Read Blaskey Hindcasted Discharge Data
RiverDisch <- read.csv(here("Data","Spatial Data","Blaskey_Hindcast_simdata","RiverDischargeExtracted","WeeklyRiverDischargeExtr.csv"))

# =============================================================================
# 2017
# =============================================================================
# Filter and summarize river temperature
RiverTemp_2017 <- RiverTemp %>%
  filter(year == 2017)

# Filter and summarize river discharge
RiverDisch_2017 <- RiverDisch %>%
  filter(year == 2017)

# Read production data
Prod2017 <- read.csv(here("Outputs", "ProductionData","Quartiles", "2017_Kusko_Quartile_Assignment_Results.csv"))

# Add COMID to this 
Prod2017$COMID <- kusko_shp$COMID

Prod2017_summary <- Prod2017 %>%
  # Select COMID and the assignment_individuals columns
  select(COMID, matches("assignment_individuals")) %>%
  # Reshape from wide to long
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  # Extract quartile from column name (Q1, Q2, Q3, Q4)
  mutate(Quartile = str_extract(Quartile, "Q[1-4]"))

library(tidyverse)

# First, filter RiverTemp_2017 for June weeks only
june_temps <- RiverTemp_2017 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    week_num = row_number(),
    Quartile = paste0("Q", week_num)
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# Now join with Prod2017_summary
Prod2017_joined <- Prod2017_summary %>%
  left_join(june_temps, by = c("COMID", "Quartile"))

Prod2017_weighted_avg <- Prod2017_joined %>%
  group_by(COMID) %>%
  summarise(
    weighted_avg_temp = weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals = sum(assignment_individuals, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    relative_production = (total_individuals - min(total_individuals, na.rm = TRUE)) / 
      (max(total_individuals, na.rm = TRUE) - min(total_individuals, na.rm = TRUE))
  )

CorrectCOMIDOrder <- as.data.frame(kusko_shp$COMID)
colnames(CorrectCOMIDOrder) <- "COMID"

# Join the weighted average data to this so it's in the right order
Prod2017_ordered <- CorrectCOMIDOrder %>%
  left_join(Prod2017_weighted_avg, by = "COMID")


Prod2017_ordered <- CorrectCOMIDOrder %>%
  left_join(Prod2017_weighted_avg, by = "COMID") %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0)
  )


RiverDisch_2017 <- RiverDisch %>%
  filter(year == 2017)

RiverDisch_summer_mean_2017 <- RiverDisch_2017 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

Kusko_shp_temp_2017 <- kusko_shp %>%
  left_join(RiverDisch_summer_mean_2017, by = "COMID")

weighted_avg<- Prod2017_ordered$weighted_avg_temp

# Create dataframe
df2017 <- data.frame(
  COMID = Kusko_shp_temp_2017$COMID,
  mean_summer_temp = weighted_avg,
  mean_summer_disch = Kusko_shp_temp_2017$mean_summer_disch,
  SNAP_temp = kusko_shp$SnapTp2017, 
  Production = Prod2017_ordered$relative_production
)

# =============================================================================
# 2018
# =============================================================================
# Filter river temperature for 2018
RiverTemp_2018 <- RiverTemp %>%
  filter(year == 2018)

# Filter and summarize river discharge
RiverDisch_2018 <- RiverDisch %>%
  filter(year == 2018)

RiverDisch_summer_mean_2018 <- RiverDisch_2018 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join discharge to shapefile
Kusko_shp_temp_2018 <- kusko_shp %>%
  left_join(RiverDisch_summer_mean_2018, by = "COMID")

# Read production data
Prod2018 <- read.csv(here("Outputs", "ProductionData","Quartiles", "2018_Kusko_Quartile_Assignment_Results.csv"))

# Add COMID to production data
Prod2018$COMID <- kusko_shp$COMID

# Reshape production data
Prod2018_summary <- Prod2018 %>%
  select(COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(Quartile = str_extract(Quartile, "Q[1-4]"))

# Filter for June weeks and assign quartiles
june_temps_2018 <- RiverTemp_2018 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    week_num = row_number(),
    Quartile = paste0("Q", week_num)
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# Join production and temperature data
Prod2018_joined <- Prod2018_summary %>%
  left_join(june_temps_2018, by = c("COMID", "Quartile"))

# Calculate weighted average and relative production
Prod2018_weighted_avg <- Prod2018_joined %>%
  group_by(COMID) %>%
  summarise(
    weighted_avg_temp = weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals = sum(assignment_individuals, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    relative_production = (total_individuals - min(total_individuals, na.rm = TRUE)) / 
      (max(total_individuals, na.rm = TRUE) - min(total_individuals, na.rm = TRUE))
  )

# Order by correct COMID sequence
Prod2018_ordered <- CorrectCOMIDOrder %>%
  left_join(Prod2018_weighted_avg, by = "COMID") %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0),
    relative_production = replace_na(relative_production, 0)
  )

# Create dataframe
df2018 <- data.frame(
  COMID = Kusko_shp_temp_2018$COMID,
  mean_summer_temp = Prod2018_ordered$weighted_avg_temp,
  mean_summer_disch = Kusko_shp_temp_2018$mean_summer_disch,
  SNAP_temp = kusko_shp$SnapTp2018, 
  Production = Prod2018_ordered$relative_production
)

# =============================================================================
# 2019
# =============================================================================
# Filter river temperature for 2019
RiverTemp_2019 <- RiverTemp %>%
  filter(year == 2019)

# Filter and summarize river discharge
RiverDisch_2019 <- RiverDisch %>%
  filter(year == 2019)

RiverDisch_summer_mean_2019 <- RiverDisch_2019 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join discharge to shapefile
Kusko_shp_temp_2019 <- kusko_shp %>%
  left_join(RiverDisch_summer_mean_2019, by = "COMID")

# Read production data
Prod2019 <- read.csv(here("Outputs", "ProductionData","Quartiles", "2019_Kusko_Quartile_Assignment_Results.csv"))

# Add COMID to production data
Prod2019$COMID <- kusko_shp$COMID

# Reshape production data
Prod2019_summary <- Prod2019 %>%
  select(COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(Quartile = str_extract(Quartile, "Q[1-4]"))

# Filter for June weeks and assign quartiles
june_temps_2019 <- RiverTemp_2019 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    week_num = row_number(),
    Quartile = paste0("Q", week_num)
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# Join production and temperature data
Prod2019_joined <- Prod2019_summary %>%
  left_join(june_temps_2019, by = c("COMID", "Quartile"))

# Calculate weighted average and relative production
Prod2019_weighted_avg <- Prod2019_joined %>%
  group_by(COMID) %>%
  summarise(
    weighted_avg_temp = weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals = sum(assignment_individuals, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    relative_production = (total_individuals - min(total_individuals, na.rm = TRUE)) / 
      (max(total_individuals, na.rm = TRUE) - min(total_individuals, na.rm = TRUE))
  )

# Order by correct COMID sequence
Prod2019_ordered <- CorrectCOMIDOrder %>%
  left_join(Prod2019_weighted_avg, by = "COMID") %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0),
    relative_production = replace_na(relative_production, 0)
  )

# Create dataframe
df2019 <- data.frame(
  COMID = Kusko_shp_temp_2019$COMID,
  mean_summer_temp = Prod2019_ordered$weighted_avg_temp,
  mean_summer_disch = Kusko_shp_temp_2019$mean_summer_disch,
  SNAP_temp = kusko_shp$SnapTp2019, 
  Production = Prod2019_ordered$relative_production
)

# =============================================================================
# 2020
# =============================================================================
# Filter river temperature for 2020
RiverTemp_2020 <- RiverTemp %>%
  filter(year == 2020)

# Filter and summarize river discharge
RiverDisch_2020 <- RiverDisch %>%
  filter(year == 2020)

RiverDisch_summer_mean_2020 <- RiverDisch_2020 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join discharge to shapefile
Kusko_shp_temp_2020 <- kusko_shp %>%
  left_join(RiverDisch_summer_mean_2020, by = "COMID")

# Read production data
Prod2020 <- read.csv(here("Outputs", "ProductionData","Quartiles", "2020_Kusko_Quartile_Assignment_Results.csv"))

# Add COMID to production data
Prod2020$COMID <- kusko_shp$COMID

# Reshape production data
Prod2020_summary <- Prod2020 %>%
  select(COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(Quartile = str_extract(Quartile, "Q[1-4]"))

# Filter for June weeks and assign quartiles
june_temps_2020 <- RiverTemp_2020 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    week_num = row_number(),
    Quartile = paste0("Q", week_num)
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# Join production and temperature data
Prod2020_joined <- Prod2020_summary %>%
  left_join(june_temps_2020, by = c("COMID", "Quartile"))

# Calculate weighted average and relative production
Prod2020_weighted_avg <- Prod2020_joined %>%
  group_by(COMID) %>%
  summarise(
    weighted_avg_temp = weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals = sum(assignment_individuals, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    relative_production = (total_individuals - min(total_individuals, na.rm = TRUE)) / 
      (max(total_individuals, na.rm = TRUE) - min(total_individuals, na.rm = TRUE))
  )

# Order by correct COMID sequence
Prod2020_ordered <- CorrectCOMIDOrder %>%
  left_join(Prod2020_weighted_avg, by = "COMID") %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0),
    relative_production = replace_na(relative_production, 0)
  )

# Create dataframe
df2020 <- data.frame(
  COMID = Kusko_shp_temp_2020$COMID,
  mean_summer_temp = Prod2020_ordered$weighted_avg_temp,
  mean_summer_disch = Kusko_shp_temp_2020$mean_summer_disch,
  SNAP_temp = kusko_shp$SnapTp2020, 
  Production = Prod2020_ordered$relative_production
)

# =============================================================================
# 2021
# =============================================================================
# Filter river temperature for 2021
RiverTemp_2021 <- RiverTemp %>%
  filter(year == 2021)

# Filter and summarize river discharge
RiverDisch_2021 <- RiverDisch %>%
  filter(year == 2021)

RiverDisch_summer_mean_2021 <- RiverDisch_2021 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join discharge to shapefile
Kusko_shp_temp_2021 <- kusko_shp %>%
  left_join(RiverDisch_summer_mean_2021, by = "COMID")

# Read production data
Prod2021 <- read.csv(here("Outputs", "ProductionData","Quartiles", "2021_Kusko_Quartile_Assignment_Results.csv"))

# Add COMID to production data
Prod2021$COMID <- kusko_shp$COMID

# Reshape production data
Prod2021_summary <- Prod2021 %>%
  select(COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(Quartile = str_extract(Quartile, "Q[1-4]"))

# Filter for June weeks and assign quartiles
june_temps_2021 <- RiverTemp_2021 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    week_num = row_number(),
    Quartile = paste0("Q", week_num)
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# Join production and temperature data
Prod2021_joined <- Prod2021_summary %>%
  left_join(june_temps_2021, by = c("COMID", "Quartile"))

# Calculate weighted average and relative production
Prod2021_weighted_avg <- Prod2021_joined %>%
  group_by(COMID) %>%
  summarise(
    weighted_avg_temp = weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals = sum(assignment_individuals, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    relative_production = (total_individuals - min(total_individuals, na.rm = TRUE)) / 
      (max(total_individuals, na.rm = TRUE) - min(total_individuals, na.rm = TRUE))
  )

# Order by correct COMID sequence
Prod2021_ordered <- CorrectCOMIDOrder %>%
  left_join(Prod2021_weighted_avg, by = "COMID") %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0),
    relative_production = replace_na(relative_production, 0)
  )

# Create dataframe
df2021 <- data.frame(
  COMID = Kusko_shp_temp_2021$COMID,
  mean_summer_temp = Prod2021_ordered$weighted_avg_temp,
  mean_summer_disch = Kusko_shp_temp_2021$mean_summer_disch,
  SNAP_temp = kusko_shp$SnapTp2021, 
  Production = Prod2021_ordered$relative_production
)

# =============================================================================
# DETERMINE GLOBAL AXIS LIMITS FOR TEMPERATURE PLOTS
# =============================================================================
# Combine all filtered data to find global min/max
all_data <- bind_rows(
  df2017 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2017),
  df2018 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2018),
  df2019 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2019),
  df2020 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2020),
  df2021 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2021)
)

# Calculate global limits with some padding for temperature plots
x_limits_temp <- c(floor(min(all_data$SNAP_temp, na.rm = TRUE)), 
                   ceiling(max(all_data$SNAP_temp, na.rm = TRUE)))
y_limits_temp <- c(floor(min(all_data$mean_summer_temp, na.rm = TRUE)), 
                   ceiling(max(all_data$mean_summer_temp, na.rm = TRUE)))

cat("X-axis limits (SNAP temp):", x_limits_temp, "\n")
cat("Y-axis limits (Mean summer temp):", y_limits_temp, "\n")

# =============================================================================
# DETERMINE GLOBAL AXIS LIMITS FOR DISCHARGE PLOTS
# =============================================================================
# Filter data for discharge plots and add log transformation
all_data_disch <- all_data %>% 
  mutate(log_discharge = log10(mean_summer_disch))

# Calculate global limits for discharge plots
x_limits_disch <- c(floor(min(all_data_disch$log_discharge, na.rm = TRUE)), 
                    ceiling(max(all_data_disch$log_discharge, na.rm = TRUE)))
y_limits_disch <- y_limits_temp  # Same y-axis as temperature plots

cat("X-axis limits (Log10 Mean summer discharge):", x_limits_disch, "\n")
cat("Y-axis limits (Mean summer temp):", y_limits_disch, "\n")

# =============================================================================
# CREATE TEMPERATURE PLOTS WITH CONSISTENT AXES (NO LEGENDS)
# =============================================================================
# Create plot for 2017
p2017_temp <- ggplot(df2017 %>% filter(SNAP_temp > 5, Production > 0.7), 
                     aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "SNAP Temperature 2017",
    y = "Mean Summer Temp (Blaskey)",
    title = "2017"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create plot for 2018
p2018_temp <- ggplot(df2018 %>% filter(SNAP_temp > 5, Production > 0.7), 
                     aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "SNAP Temperature 2018",
    y = "Mean Summer Temp (Blaskey)",
    title = "2018"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create plot for 2019
p2019_temp <- ggplot(df2019 %>% filter(SNAP_temp > 5, Production > 0.7), 
                     aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "SNAP Temperature 2019",
    y = "Mean Summer Temp (Blaskey)",
    title = "2019"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create plot for 2020
p2020_temp <- ggplot(df2020 %>% filter(SNAP_temp > 5, Production > 0.7), 
                     aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "SNAP Temperature 2020",
    y = "Mean Summer Temp (Blaskey)",
    title = "2020"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create plot for 2021
p2021_temp <- ggplot(df2021 %>% filter(SNAP_temp > 5, Production > 0.7), 
                     aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "SNAP Temperature 2021",
    y = "Mean Summer Temp (Blaskey)",
    title = "2021"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# =============================================================================
# CREATE DISCHARGE PLOTS WITH CONSISTENT AXES (NO LEGENDS) - LOG TRANSFORMED
# =============================================================================
# Create discharge plot for 2017
p2017_disch <- ggplot(df2017 %>% filter(SNAP_temp > 5, Production > 0.7, mean_summer_disch > 0), 
                      aes(x = log10(mean_summer_disch), y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge 2017",
    y = "Mean Summer Temp (Blaskey)",
    title = "2017"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create discharge plot for 2018
p2018_disch <- ggplot(df2018 %>% filter(SNAP_temp > 5, Production > 0.7, , mean_summer_disch > 0), 
                      aes(x = log10(mean_summer_disch), y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge 2018",
    y = "Mean Summer Temp (Blaskey)",
    title = "2018"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create discharge plot for 2019
p2019_disch <- ggplot(df2019 %>% filter(SNAP_temp > 5, Production > 0.7, mean_summer_disch > 0), 
                      aes(x = log10(mean_summer_disch), y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge 2019",
    y = "Mean Summer Temp (Blaskey)",
    title = "2019"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create discharge plot for 2020
p2020_disch <- ggplot(df2020 %>% filter(SNAP_temp > 5, Production > 0.7, , mean_summer_disch > 0), 
                      aes(x = log10(mean_summer_disch), y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge 2020",
    y = "Mean Summer Temp (Blaskey)",
    title = "2020"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Create discharge plot for 2021
p2021_disch <- ggplot(df2021 %>% filter(SNAP_temp > 5, Production > 0.7,  mean_summer_disch > 0), 
                      aes(x = log10(mean_summer_disch), y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge 2021",
    y = "Mean Summer Temp (Blaskey)",
    title = "2021"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# =============================================================================
# COMBINE TEMPERATURE PLOTS INTO MULTI-PANEL FIGURE (SINGLE ROW)
# =============================================================================
combined_plot_temp <- (p2017_temp | p2018_temp | p2019_temp | p2020_temp | p2021_temp) + 
  plot_annotation(title = "Comparison of SNAP vs Blaskey Hindcast Temperatures - Kuskokwim River",
                  subtitle = "2017-2021")

# Display the temperature plot
combined_plot_temp

# =============================================================================
# COMBINE DISCHARGE PLOTS INTO MULTI-PANEL FIGURE (SINGLE ROW)
# =============================================================================
combined_plot_disch <- (p2017_disch | p2018_disch | p2019_disch | p2020_disch | p2021_disch) + 
  plot_annotation(title = "Log10 Mean Summer Discharge vs Blaskey Hindcast Temperatures - Kuskokwim River",
                  subtitle = "2017-2021")

# Display the discharge plot
combined_plot_disch

# =============================================================================
# EXPORT FIGURES
# =============================================================================
# Create output directory if it doesn't exist
output_dir <- here("Figures", "ContourMaps")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the temperature figure
ggsave(
  filename = file.path(output_dir, "SNAP_vs_Blaskey_Temp_2017-2021.png"),
  plot = combined_plot_temp,
  width = 25,
  height = 5,
  dpi = 300,
  bg = "white"
)

cat("\nTemperature figure saved to:", file.path(output_dir, "SNAP_vs_Blaskey_Temp_2017-2021.png"), "\n")

# Save the discharge figure
ggsave(
  filename = file.path(output_dir, "Log_Discharge_vs_Blaskey_Temp_2017-2021.png"),
  plot = combined_plot_disch,
  width = 25,
  height = 5,
  dpi = 300,
  bg = "white"
)

cat("Discharge figure saved to:", file.path(output_dir, "Log_Discharge_vs_Blaskey_Temp_2017-2021.png"), "\n")


