# =============================================================================
# SETUP
# =============================================================================
library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(patchwork)
library(RColorBrewer)

# =============================================================================
# READ IN BASE DATA (USED FOR ALL YEARS)
# =============================================================================
# Read shapefile
kusko_shp <- st_read(here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp")) %>%
  st_drop_geometry()

# Read Blaskey Hindcasted Temperature Data
RiverTemp <- read.csv(here("Data", "Spatial Data", "Blaskey_Hindcast_simdata", 
                           "RiverTempExtracted", "WeeklyRiverTempExtr.csv"))

# =============================================================================
# 2017
# =============================================================================
# Filter and summarize river temperature
RiverTemp_2017 <- RiverTemp %>%
  filter(year == 2017)

RiverTemp_summer_mean_2017 <- RiverTemp_2017 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_temp = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join temperature to shapefile
Kusko_shp_temp_2017 <- kusko_shp %>%
  left_join(RiverTemp_summer_mean_2017, by = "COMID")

# Read production data
Prod2017 <- read.csv(here("Outputs", "ProductionData", "2017_Kusko_Assignment_Results.csv"))

# Create dataframe
df2017 <- data.frame(
  COMID = Kusko_shp_temp_2017$COMID,
  mean_summer_temp = Kusko_shp_temp_2017$mean_summer_temp,
  SNAP_temp = Kusko_shp_temp_2017$SnapTp2017, 
  Production = Prod2017$assignment_norm
)

# =============================================================================
# 2018
# =============================================================================
# Filter and summarize river temperature
RiverTemp_2018 <- RiverTemp %>%
  filter(year == 2018)

RiverTemp_summer_mean_2018 <- RiverTemp_2018 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_temp = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join temperature to shapefile
Kusko_shp_temp_2018 <- kusko_shp %>%
  left_join(RiverTemp_summer_mean_2018, by = "COMID")

# Read production data
Prod2018 <- read.csv(here("Outputs", "ProductionData", "2018_Kusko_Assignment_Results.csv"))

# Create dataframe
df2018 <- data.frame(
  COMID = Kusko_shp_temp_2018$COMID,
  mean_summer_temp = Kusko_shp_temp_2018$mean_summer_temp,
  SNAP_temp = Kusko_shp_temp_2018$SnapTp2018, 
  Production = Prod2018$assignment_norm
)

# =============================================================================
# 2019
# =============================================================================
# Filter and summarize river temperature
RiverTemp_2019 <- RiverTemp %>%
  filter(year == 2019)

RiverTemp_summer_mean_2019 <- RiverTemp_2019 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_temp = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join temperature to shapefile
Kusko_shp_temp_2019 <- kusko_shp %>%
  left_join(RiverTemp_summer_mean_2019, by = "COMID")

# Read production data
Prod2019 <- read.csv(here("Outputs", "ProductionData", "2019_Kusko_Assignment_Results.csv"))

# Create dataframe
df2019 <- data.frame(
  COMID = Kusko_shp_temp_2019$COMID,
  mean_summer_temp = Kusko_shp_temp_2019$mean_summer_temp,
  SNAP_temp = Kusko_shp_temp_2019$SnapTp2019, 
  Production = Prod2019$assignment_norm
)

# =============================================================================
# 2020
# =============================================================================
# Filter and summarize river temperature
RiverTemp_2020 <- RiverTemp %>%
  filter(year == 2020)

RiverTemp_summer_mean_2020 <- RiverTemp_2020 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_temp = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join temperature to shapefile
Kusko_shp_temp_2020 <- kusko_shp %>%
  left_join(RiverTemp_summer_mean_2020, by = "COMID")

# Read production data
Prod2020 <- read.csv(here("Outputs", "ProductionData", "2020_Kusko_Assignment_Results.csv"))

# Create dataframe
df2020 <- data.frame(
  COMID = Kusko_shp_temp_2020$COMID,
  mean_summer_temp = Kusko_shp_temp_2020$mean_summer_temp,
  SNAP_temp = Kusko_shp_temp_2020$SnapTp2020, 
  Production = Prod2020$assignment_norm
)

# =============================================================================
# 2021
# =============================================================================
# Filter and summarize river temperature
RiverTemp_2021 <- RiverTemp %>%
  filter(year == 2021)

RiverTemp_summer_mean_2021 <- RiverTemp_2021 %>%
  group_by(COMID) %>%
  summarise(
    mean_summer_temp = mean(mean_value, na.rm = TRUE),
    n_weeks = n(),
    .groups = 'drop'
  )

# Join temperature to shapefile
Kusko_shp_temp_2021 <- kusko_shp %>%
  left_join(RiverTemp_summer_mean_2021, by = "COMID")

# Read production data
Prod2021 <- read.csv(here("Outputs", "ProductionData", "2021_Kusko_Assignment_Results.csv"))

# Create dataframe
df2021 <- data.frame(
  COMID = Kusko_shp_temp_2021$COMID,
  mean_summer_temp = Kusko_shp_temp_2021$mean_summer_temp,
  SNAP_temp = Kusko_shp_temp_2021$SnapTp2021, 
  Production = Prod2021$assignment_norm
)

# =============================================================================
# DETERMINE GLOBAL AXIS LIMITS
# =============================================================================
# Combine all filtered data to find global min/max
all_data <- bind_rows(
  df2017 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2017),
  df2018 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2018),
  df2019 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2019),
  df2020 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2020),
  df2021 %>% filter(SNAP_temp > 5, Production > 0.7) %>% mutate(year = 2021)
)

# Calculate global limits with some padding
x_limits <- c(floor(min(all_data$SNAP_temp, na.rm = TRUE)), 
              ceiling(max(all_data$SNAP_temp, na.rm = TRUE)))
y_limits <- c(floor(min(all_data$mean_summer_temp, na.rm = TRUE)), 
              ceiling(max(all_data$mean_summer_temp, na.rm = TRUE)))

cat("X-axis limits (SNAP temp):", x_limits, "\n")
cat("Y-axis limits (Mean summer temp):", y_limits, "\n")

# =============================================================================
# CREATE PLOTS WITH CONSISTENT AXES
# =============================================================================
# Create plot for 2017
p2017 <- ggplot(df2017 %>% filter(SNAP_temp > 5, Production > 0.7), 
                aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(
    x = "SNAP Temperature 2017",
    y = "Mean Summer Temp (Blaskey)",
    title = "2017",
    size = "Production",
    fill = "Density"
  ) +
  theme_bw()

# Create plot for 2018
p2018 <- ggplot(df2018 %>% filter(SNAP_temp > 5, Production > 0.7), 
                aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(
    x = "SNAP Temperature 2018",
    y = "Mean Summer Temp (Blaskey)",
    title = "2018",
    size = "Production",
    fill = "Density"
  ) +
  theme_bw()

# Create plot for 2019
p2019 <- ggplot(df2019 %>% filter(SNAP_temp > 5, Production > 0.7), 
                aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(
    x = "SNAP Temperature 2019",
    y = "Mean Summer Temp (Blaskey)",
    title = "2019",
    size = "Production",
    fill = "Density"
  ) +
  theme_bw()

# Create plot for 2020
p2020 <- ggplot(df2020 %>% filter(SNAP_temp > 5, Production > 0.7), 
                aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(
    x = "SNAP Temperature 2020",
    y = "Mean Summer Temp (Blaskey)",
    title = "2020",
    size = "Production",
    fill = "Density"
  ) +
  theme_bw()

# Create plot for 2021
p2021 <- ggplot(df2021 %>% filter(SNAP_temp > 5, Production > 0.7), 
                aes(x = SNAP_temp, y = mean_summer_temp)) +
  geom_point(aes(size = Production), alpha = 0.2, color = "gray30") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(
    x = "SNAP Temperature 2021",
    y = "Mean Summer Temp (Blaskey)",
    title = "2021",
    size = "Production",
    fill = "Density"
  ) +
  theme_bw()

# =============================================================================
# COMBINE ALL PLOTS INTO MULTI-PANEL FIGURE
# =============================================================================
# Two rows: 3 on top, 2 on bottom
(p2017 + p2018 + p2019) / (p2020 + p2021 + plot_spacer()) + 
  plot_annotation(title = "Comparison of SNAP vs Blaskey Hindcast Temperatures - Kuskokwim River",
                  subtitle = "2017-2021")