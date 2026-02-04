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

# Extract COMID from the shapefile (used for all years)
COMID <- kusko_shp$COMID

# =============================================================================
# 2017
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Subset hydrology data to 2017
# -----------------------------------------------------------------------------
RiverTemp_2017 <- RiverTemp %>%
  filter(year == 2017)

RiverDisch_2017 <- RiverDisch %>%
  filter(year == 2017)

# -----------------------------------------------------------------------------
# 2. Read production quartile assignments (2017)
# -----------------------------------------------------------------------------
Prod2017 <- read.csv(
  here(
    "Outputs", "ProductionData", "Quartiles",
    "2017_Kusko_Quartile_Assignment_Results.csv"
  )
)

# -----------------------------------------------------------------------------
# 3. Attach COMID to production data
# -----------------------------------------------------------------------------
Prod2017$COMID <- COMID

# -----------------------------------------------------------------------------
# 4. Reshape production data to long format (quartiles)
# -----------------------------------------------------------------------------
Prod2017_long <- Prod2017 %>%
  select(reachid, COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(
    Quartile = str_extract(Quartile, "Q[1-4]")
  )

# -----------------------------------------------------------------------------
# 5. Assign June temperature weeks to quartiles (Q1–Q4)
# -----------------------------------------------------------------------------
JuneTemps_2017 <- RiverTemp_2017 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    Quartile = paste0("Q", row_number())
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# -----------------------------------------------------------------------------
# 6. Join production and temperature by COMID × Quartile
# -----------------------------------------------------------------------------
Prod2017_joined <- Prod2017_long %>%
  left_join(
    JuneTemps_2017,
    by = c("COMID", "Quartile")
  )

# -----------------------------------------------------------------------------
# 7. Compute production-weighted summer temperature per reachid
# -----------------------------------------------------------------------------
Prod2017_weighted <- Prod2017_joined %>%
  group_by(reachid, COMID) %>%
  summarise(
    weighted_avg_temp =
      weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals =
      sum(assignment_individuals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0)
  )

# -----------------------------------------------------------------------------
# 8. Read in the annual production data
# -----------------------------------------------------------------------------
AnnualProd2017 <- read.csv(here("Outputs","ProductionData","2017_Kusko_Assignment_Results.csv"))

# Add the assignment_norm column to Prod2017_weighted by matching reachid
Prod2017_weighted <- Prod2017_weighted %>%
  left_join(
    AnnualProd2017 %>%
      select(reachid, assignment_norm),
    by = "reachid"
  ) %>%
  mutate(
    assignment_norm = replace_na(assignment_norm, 0)
  )

# -----------------------------------------------------------------------------
# 9. Compute mean summer discharge (2017)
# -----------------------------------------------------------------------------
RiverDisch_summer_mean_2017 <- RiverDisch_2017 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 10. Build final analysis dataframe
# -----------------------------------------------------------------------------
df2017 <- Prod2017_weighted

# Add in mean summer discharge by matching COMID
df2017 <- df2017 %>%
  left_join(
    RiverDisch_summer_mean_2017,
    by = "COMID"
  )

# Add in SNAP data from kusko_shp by matching reachid
df2017 <- df2017 %>%
  left_join(
    kusko_shp %>%
      select(reachid, SnapTp2017, SnapPr2017),
    by = "reachid"
  ) %>%
  rename(
    mean_summer_temp = weighted_avg_temp,
    SNAP_temp = SnapTp2017,
    SNAP_prec = SnapPr2017,
    Production = assignment_norm
  )
# =============================================================================
# 2018
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Subset hydrology data to 2018
# -----------------------------------------------------------------------------
RiverTemp_2018 <- RiverTemp %>%
  filter(year == 2018)

RiverDisch_2018 <- RiverDisch %>%
  filter(year == 2018)

# -----------------------------------------------------------------------------
# 2. Read production quartile assignments (2018)
# -----------------------------------------------------------------------------
Prod2018 <- read.csv(
  here(
    "Outputs", "ProductionData", "Quartiles",
    "2018_Kusko_Quartile_Assignment_Results.csv"
  )
)

# -----------------------------------------------------------------------------
# 3. Attach COMID to production data
# -----------------------------------------------------------------------------
Prod2018$COMID <- COMID

# -----------------------------------------------------------------------------
# 4. Reshape production data to long format (quartiles)
# -----------------------------------------------------------------------------
Prod2018_long <- Prod2018 %>%
  select(reachid, COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(
    Quartile = str_extract(Quartile, "Q[1-4]")
  )

# -----------------------------------------------------------------------------
# 5. Assign June temperature weeks to quartiles (Q1–Q4)
# -----------------------------------------------------------------------------
JuneTemps_2018 <- RiverTemp_2018 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    Quartile = paste0("Q", row_number())
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# -----------------------------------------------------------------------------
# 6. Join production and temperature by COMID × Quartile
# -----------------------------------------------------------------------------
Prod2018_joined <- Prod2018_long %>%
  left_join(
    JuneTemps_2018,
    by = c("COMID", "Quartile")
  )

# -----------------------------------------------------------------------------
# 7. Compute production-weighted summer temperature per reachid
# -----------------------------------------------------------------------------
Prod2018_weighted <- Prod2018_joined %>%
  group_by(reachid, COMID) %>%
  summarise(
    weighted_avg_temp =
      weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals =
      sum(assignment_individuals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0)
  )

# -----------------------------------------------------------------------------
# 8. Read in the annual production data
# -----------------------------------------------------------------------------
AnnualProd2018 <- read.csv(here("Outputs","ProductionData","2018_Kusko_Assignment_Results.csv"))

# Add the assignment_norm column
Prod2018_weighted <- Prod2018_weighted %>%
  left_join(
    AnnualProd2018 %>%
      select(reachid, assignment_norm),
    by = "reachid"
  ) %>%
  mutate(
    assignment_norm = replace_na(assignment_norm, 0)
  )

# -----------------------------------------------------------------------------
# 9. Compute mean summer discharge (2018)
# -----------------------------------------------------------------------------
RiverDisch_summer_mean_2018 <- RiverDisch_2018 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 10. Build final analysis dataframe
# -----------------------------------------------------------------------------
df2018 <- Prod2018_weighted

# Add in mean summer discharge by matching COMID
df2018 <- df2018 %>%
  left_join(
    RiverDisch_summer_mean_2018,
    by = "COMID"
  )

# Add in SNAP data from kusko_shp by matching reachid
df2018 <- df2018 %>%
  left_join(
    kusko_shp %>%
      select(reachid, SnapTp2018, SnapPr2018),
    by = "reachid"
  ) %>%
  rename(
    mean_summer_temp = weighted_avg_temp,
    SNAP_temp = SnapTp2018,
    SNAP_prec = SnapPr2018,
    Production = assignment_norm
  )

# =============================================================================
# 2019
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Subset hydrology data to 2019
# -----------------------------------------------------------------------------
RiverTemp_2019 <- RiverTemp %>%
  filter(year == 2019)

RiverDisch_2019 <- RiverDisch %>%
  filter(year == 2019)

# -----------------------------------------------------------------------------
# 2. Read production quartile assignments (2019)
# -----------------------------------------------------------------------------
Prod2019 <- read.csv(
  here(
    "Outputs", "ProductionData", "Quartiles",
    "2019_Kusko_Quartile_Assignment_Results.csv"
  )
)

# -----------------------------------------------------------------------------
# 3. Attach COMID to production data
# -----------------------------------------------------------------------------
Prod2019$COMID <- COMID

# -----------------------------------------------------------------------------
# 4. Reshape production data to long format (quartiles)
# -----------------------------------------------------------------------------
Prod2019_long <- Prod2019 %>%
  select(reachid, COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(
    Quartile = str_extract(Quartile, "Q[1-4]")
  )

# -----------------------------------------------------------------------------
# 5. Assign June temperature weeks to quartiles (Q1–Q4)
# -----------------------------------------------------------------------------
JuneTemps_2019 <- RiverTemp_2019 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    Quartile = paste0("Q", row_number())
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# -----------------------------------------------------------------------------
# 6. Join production and temperature by COMID × Quartile
# -----------------------------------------------------------------------------
Prod2019_joined <- Prod2019_long %>%
  left_join(
    JuneTemps_2019,
    by = c("COMID", "Quartile")
  )

# -----------------------------------------------------------------------------
# 7. Compute production-weighted summer temperature per reachid
# -----------------------------------------------------------------------------
Prod2019_weighted <- Prod2019_joined %>%
  group_by(reachid, COMID) %>%
  summarise(
    weighted_avg_temp =
      weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals =
      sum(assignment_individuals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0)
  )

# -----------------------------------------------------------------------------
# 8. Read in the annual production data
# -----------------------------------------------------------------------------
AnnualProd2019 <- read.csv(here("Outputs","ProductionData","2019_Kusko_Assignment_Results.csv"))

# Add the assignment_norm column
Prod2019_weighted <- Prod2019_weighted %>%
  left_join(
    AnnualProd2019 %>%
      select(reachid, assignment_norm),
    by = "reachid"
  ) %>%
  mutate(
    assignment_norm = replace_na(assignment_norm, 0)
  )

# -----------------------------------------------------------------------------
# 9. Compute mean summer discharge (2019)
# -----------------------------------------------------------------------------
RiverDisch_summer_mean_2019 <- RiverDisch_2019 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 10. Build final analysis dataframe
# -----------------------------------------------------------------------------
df2019 <- Prod2019_weighted

# Add in mean summer discharge by matching COMID
df2019 <- df2019 %>%
  left_join(
    RiverDisch_summer_mean_2019,
    by = "COMID"
  )

# Add in SNAP data from kusko_shp by matching reachid
df2019 <- df2019 %>%
  left_join(
    kusko_shp %>%
      select(reachid, SnapTp2019, SnapPr2019),
    by = "reachid"
  ) %>%
  rename(
    mean_summer_temp = weighted_avg_temp,
    SNAP_temp = SnapTp2019,
    SNAP_prec = SnapPr2019,
    Production = assignment_norm
  )


# =============================================================================
# 2020
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Subset hydrology data to 2020
# -----------------------------------------------------------------------------
RiverTemp_2020 <- RiverTemp %>%
  filter(year == 2020)

RiverDisch_2020 <- RiverDisch %>%
  filter(year == 2020)

# -----------------------------------------------------------------------------
# 2. Read production quartile assignments (2020)
# -----------------------------------------------------------------------------
Prod2020 <- read.csv(
  here(
    "Outputs", "ProductionData", "Quartiles",
    "2020_Kusko_Quartile_Assignment_Results.csv"
  )
)

# -----------------------------------------------------------------------------
# 3. Attach COMID to production data
# -----------------------------------------------------------------------------
Prod2020$COMID <- COMID

# -----------------------------------------------------------------------------
# 4. Reshape production data to long format (quartiles)
# -----------------------------------------------------------------------------
Prod2020_long <- Prod2020 %>%
  select(reachid, COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(
    Quartile = str_extract(Quartile, "Q[1-4]")
  )

# -----------------------------------------------------------------------------
# 5. Assign June temperature weeks to quartiles (Q1–Q4)
# -----------------------------------------------------------------------------
JuneTemps_2020 <- RiverTemp_2020 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    Quartile = paste0("Q", row_number())
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# -----------------------------------------------------------------------------
# 6. Join production and temperature by COMID × Quartile
# -----------------------------------------------------------------------------
Prod2020_joined <- Prod2020_long %>%
  left_join(
    JuneTemps_2020,
    by = c("COMID", "Quartile")
  )

# -----------------------------------------------------------------------------
# 7. Compute production-weighted summer temperature per reachid
# -----------------------------------------------------------------------------
Prod2020_weighted <- Prod2020_joined %>%
  group_by(reachid, COMID) %>%
  summarise(
    weighted_avg_temp =
      weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals =
      sum(assignment_individuals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0)
  )

# -----------------------------------------------------------------------------
# 8. Read in the annual production data
# -----------------------------------------------------------------------------
AnnualProd2020 <- read.csv(here("Outputs","ProductionData","2020_Kusko_Assignment_Results.csv"))

# Add the assignment_norm column
Prod2020_weighted <- Prod2020_weighted %>%
  left_join(
    AnnualProd2020 %>%
      select(reachid, assignment_norm),
    by = "reachid"
  ) %>%
  mutate(
    assignment_norm = replace_na(assignment_norm, 0)
  )

# -----------------------------------------------------------------------------
# 9. Compute mean summer discharge (2020)
# -----------------------------------------------------------------------------
RiverDisch_summer_mean_2020 <- RiverDisch_2020 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 10. Build final analysis dataframe
# -----------------------------------------------------------------------------
df2020 <- Prod2020_weighted

# Add in mean summer discharge by matching COMID
df2020 <- df2020 %>%
  left_join(
    RiverDisch_summer_mean_2020,
    by = "COMID"
  )

# Add in SNAP data from kusko_shp by matching reachid
df2020 <- df2020 %>%
  left_join(
    kusko_shp %>%
      select(reachid, SnapTp2020, SnapPr2020),
    by = "reachid"
  ) %>%
  rename(
    mean_summer_temp = weighted_avg_temp,
    SNAP_temp = SnapTp2020,
    SNAP_prec = SnapPr2020,
    Production = assignment_norm
  )

# =============================================================================
# 2021
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Subset hydrology data to 2021
# -----------------------------------------------------------------------------
RiverTemp_2021 <- RiverTemp %>%
  filter(year == 2021)

RiverDisch_2021 <- RiverDisch %>%
  filter(year == 2021)

# -----------------------------------------------------------------------------
# 2. Read production quartile assignments (2021)
# -----------------------------------------------------------------------------
Prod2021 <- read.csv(
  here(
    "Outputs", "ProductionData", "Quartiles",
    "2021_Kusko_Quartile_Assignment_Results.csv"
  )
)

# -----------------------------------------------------------------------------
# 3. Attach COMID to production data
# -----------------------------------------------------------------------------
Prod2021$COMID <- COMID

# -----------------------------------------------------------------------------
# 4. Reshape production data to long format (quartiles)
# -----------------------------------------------------------------------------
Prod2021_long <- Prod2021 %>%
  select(reachid, COMID, matches("assignment_individuals")) %>%
  pivot_longer(
    cols = matches("assignment_individuals"),
    names_to = "Quartile",
    values_to = "assignment_individuals"
  ) %>%
  mutate(
    Quartile = str_extract(Quartile, "Q[1-4]")
  )

# -----------------------------------------------------------------------------
# 5. Assign June temperature weeks to quartiles (Q1–Q4)
# -----------------------------------------------------------------------------
JuneTemps_2021 <- RiverTemp_2021 %>%
  filter(month(week_start) == 6) %>%
  group_by(COMID) %>%
  arrange(week_start) %>%
  mutate(
    Quartile = paste0("Q", row_number())
  ) %>%
  ungroup() %>%
  select(COMID, Quartile, mean_value)

# -----------------------------------------------------------------------------
# 6. Join production and temperature by COMID × Quartile
# -----------------------------------------------------------------------------
Prod2021_joined <- Prod2021_long %>%
  left_join(
    JuneTemps_2021,
    by = c("COMID", "Quartile")
  )

# -----------------------------------------------------------------------------
# 7. Compute production-weighted summer temperature per reachid
# -----------------------------------------------------------------------------
Prod2021_weighted <- Prod2021_joined %>%
  group_by(reachid, COMID) %>%
  summarise(
    weighted_avg_temp =
      weighted.mean(mean_value, w = assignment_individuals, na.rm = TRUE),
    total_individuals =
      sum(assignment_individuals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_avg_temp = replace_na(weighted_avg_temp, 0),
    total_individuals = replace_na(total_individuals, 0)
  )

# -----------------------------------------------------------------------------
# 8. Read in the annual production data
# -----------------------------------------------------------------------------
AnnualProd2021 <- read.csv(here("Outputs","ProductionData","2021_Kusko_Assignment_Results.csv"))

# Add the assignment_norm column
Prod2021_weighted <- Prod2021_weighted %>%
  left_join(
    AnnualProd2021 %>%
      select(reachid, assignment_norm),
    by = "reachid"
  ) %>%
  mutate(
    assignment_norm = replace_na(assignment_norm, 0)
  )

# -----------------------------------------------------------------------------
# 9. Compute mean summer discharge (2021)
# -----------------------------------------------------------------------------
RiverDisch_summer_mean_2021 <- RiverDisch_2021 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_disch = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 10. Build final analysis dataframe
# -----------------------------------------------------------------------------
df2021 <- Prod2021_weighted

# Add in mean summer discharge by matching COMID
df2021 <- df2021 %>%
  left_join(
    RiverDisch_summer_mean_2021,
    by = "COMID"
  )

# Add in SNAP data from kusko_shp by matching reachid
df2021 <- df2021 %>%
  left_join(
    kusko_shp %>%
      select(reachid, SnapTp2021, SnapPr2021),
    by = "reachid"
  ) %>%
  rename(
    mean_summer_temp = weighted_avg_temp,
    SNAP_temp = SnapTp2021,
    SNAP_prec = SnapPr2021,
    Production = assignment_norm
  )

# =============================================================================
# CALCULATE GLOBAL AXIS LIMITS
# =============================================================================
# Create filtered datasets for each year
df_2017_filtered <- df2017 %>% filter(Production > .7)
df_2018_filtered <- df2018 %>% filter(Production > .7)
df_2019_filtered <- df2019 %>% filter(Production > .7)
df_2020_filtered <- df2020 %>% filter(Production > .7)
df_2021_filtered <- df2021 %>% filter(Production > .7)

# Combine all filtered data
all_data_temp <- bind_rows(
  df_2017_filtered %>% mutate(year = 2017),
  df_2018_filtered %>% mutate(year = 2018),
  df_2019_filtered %>% mutate(year = 2019),
  df_2020_filtered %>% mutate(year = 2020),
  df_2021_filtered %>% mutate(year = 2021)
)

# Calculate global limits for temperature plots
x_limits_temp <- range(all_data_temp$mean_summer_temp, na.rm = TRUE)
y_limits_temp <- c(10, max(all_data_temp$SNAP_temp, na.rm = TRUE))

cat("Temperature plot X-axis limits (mean_summer_temp):", x_limits_temp, "\n")
cat("Temperature plot Y-axis limits (SNAP_temp):", y_limits_temp, "\n")

# Calculate global limits for discharge plots (only for data with positive discharge and precip)
all_data_disch <- all_data_temp %>% 
  filter(mean_summer_disch > 0, !is.na(SNAP_prec), SNAP_prec > 0) %>%
  mutate(
    log_discharge = log10(mean_summer_disch),
    log_precip = log10(SNAP_prec)
  )

x_limits_disch <- range(all_data_disch$log_discharge, na.rm = TRUE)
y_limits_disch <- range(all_data_disch$log_precip, na.rm = TRUE)

cat("Discharge plot X-axis limits (log10 discharge):", x_limits_disch, "\n")
cat("Discharge plot Y-axis limits (log10 SNAP_prec):", y_limits_disch, "\n")
# =============================================================================
# CREATE TEMPERATURE PLOTS WITH CONSISTENT AXES
# =============================================================================
p2017_temp <- ggplot(df_2017_filtered, aes(x = mean_summer_temp, y = SNAP_temp)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "YlOrRd")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "Mean Summer Temp (Blaskey)",
    y = "SNAP Temperature",
    title = "2017"
  ) +
  theme_bw()

p2018_temp <- ggplot(df_2018_filtered, aes(x = mean_summer_temp, y = SNAP_temp)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "YlOrRd")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "Mean Summer Temp (Blaskey)",
    y = "SNAP Temperature",
    title = "2018"
  ) +
  theme_bw()

p2019_temp <- ggplot(df_2019_filtered, aes(x = mean_summer_temp, y = SNAP_temp)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "YlOrRd")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "Mean Summer Temp (Blaskey)",
    y = "SNAP Temperature",
    title = "2019"
  ) +
  theme_bw()

p2020_temp <- ggplot(df_2020_filtered, aes(x = mean_summer_temp, y = SNAP_temp)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "YlOrRd")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "Mean Summer Temp (Blaskey)",
    y = "SNAP Temperature",
    title = "2020"
  ) +
  theme_bw()

p2021_temp <- ggplot(df_2021_filtered, aes(x = mean_summer_temp, y = SNAP_temp)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "YlOrRd")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_cartesian(xlim = x_limits_temp, ylim = y_limits_temp) +
  labs(
    x = "Mean Summer Temp (Blaskey)",
    y = "SNAP Temperature",
    title = "2021"
  ) +
  theme_bw()

# =============================================================================
# CREATE DISCHARGE PLOTS WITH CONSISTENT AXES
# =============================================================================
p2017_disch <- ggplot(df_2017_filtered %>% filter(mean_summer_disch > 0, SNAP_prec > 0), 
                      aes(x = log10(mean_summer_disch), y = log10(SNAP_prec))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "Blues")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge",
    y = "Log10 SNAP Precipitation",
    title = "2017"
  ) +
  theme_bw()

p2018_disch <- ggplot(df_2018_filtered %>% filter(mean_summer_disch > 0, SNAP_prec > 0), 
                      aes(x = log10(mean_summer_disch), y = log10(SNAP_prec))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "Blues")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge",
    y = "Log10 SNAP Precipitation",
    title = "2018"
  ) +
  theme_bw()

p2019_disch <- ggplot(df_2019_filtered %>% filter(mean_summer_disch > 0, SNAP_prec > 0), 
                      aes(x = log10(mean_summer_disch), y = log10(SNAP_prec))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "Blues")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge",
    y = "Log10 SNAP Precipitation",
    title = "2019"
  ) +
  theme_bw()

p2020_disch <- ggplot(df_2020_filtered %>% filter(mean_summer_disch > 0, SNAP_prec > 0), 
                      aes(x = log10(mean_summer_disch), y = log10(SNAP_prec))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "Blues")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge",
    y = "Log10 SNAP Precipitation",
    title = "2020"
  ) +
  theme_bw()

p2021_disch <- ggplot(df_2021_filtered %>% filter(mean_summer_disch > 0, SNAP_prec > 0), 
                      aes(x = log10(mean_summer_disch), y = log10(SNAP_prec))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = brewer.pal(9, "Blues")[1]) +
  stat_density_2d_filled(bins = 8) +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(xlim = x_limits_disch, ylim = y_limits_disch) +
  labs(
    x = "Log10 Mean Summer Discharge",
    y = "Log10 SNAP Precipitation",
    title = "2021"
  ) +
  theme_bw()

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