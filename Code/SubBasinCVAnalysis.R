library(readxl)
library(sf)
library(dplyr)
library(here)
library(tidyverse)
library(ggplot2)

# ---------------------------
# 1. Read base input data
# ---------------------------

# Shapefile with line geometries (same across years)
shp <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"))

# Escapement data
escapement <- read_xlsx(here("Data","AYKEscapement.xlsx"))

# Spatial scale polygons
sb5 <- st_read(here("Data","Spatial Data","SubBasinPolygons","Kusko_SubWs5.shp"))
sb6 <- st_read(here("Data","Spatial Data","SubBasinPolygons","Kusko_SubWs6.shp"))
sb7 <- st_read(here("Data","Spatial Data","SubBasinPolygons","Kusko_SubWs7.shp"))

# Ensure same CRS
sb6 <- st_transform(sb6, st_crs(sb5))
sb7 <- st_transform(sb7, st_crs(sb5))

# ---------------------------
# 2. Process Year 2017
# ---------------------------

prod_2017 <- read.csv(here("Outputs","ProductionData","2017_Kusko_Assignment_Results.csv"))

analysisDf_2017 <- st_as_sf(
  prod_2017["assignment_rescale"],
  geometry = st_geometry(shp)
)

analysisDf_2017 <- st_transform(analysisDf_2017, st_crs(sb5))

esc_2017 <- escapement %>%
  filter(Year == 2017, River == "Kusko") %>%
  pull(Escapement)

# Sb5 - 2017
sb5_joined_2017 <- st_join(sb5, analysisDf_2017, join = st_intersects)
sb5_joined_df_2017 <- st_set_geometry(sb5_joined_2017, NULL)

sb5_summary_df_2017 <- sb5_joined_df_2017 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb5_tidy_2017 <- sb5_summary_df_2017 %>%
  mutate(
    num_fish = total_assignment * esc_2017,
    spScale = 5,
    Year = 2017
  ) %>%
  rename(ID = HYBAS_ID)

# Sb6 - 2017
sb6_joined_2017 <- st_join(sb6, analysisDf_2017, join = st_intersects)
sb6_joined_df_2017 <- st_set_geometry(sb6_joined_2017, NULL)

sb6_summary_df_2017 <- sb6_joined_df_2017 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb6_tidy_2017 <- sb6_summary_df_2017 %>%
  mutate(
    num_fish = total_assignment * esc_2017,
    spScale = 6,
    Year = 2017
  ) %>%
  rename(ID = HYBAS_ID)

# Sb7 - 2017
sb7_joined_2017 <- st_join(sb7, analysisDf_2017, join = st_intersects)
sb7_joined_df_2017 <- st_set_geometry(sb7_joined_2017, NULL)

sb7_summary_df_2017 <- sb7_joined_df_2017 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb7_tidy_2017 <- sb7_summary_df_2017 %>%
  mutate(
    num_fish = total_assignment * esc_2017,
    spScale = 7,
    Year = 2017
  ) %>%
  rename(ID = HYBAS_ID)

all_scales_2017 <- bind_rows(sb5_tidy_2017, sb6_tidy_2017, sb7_tidy_2017)

# ---------------------------
# 3. Process Year 2018
# ---------------------------

prod_2018 <- read.csv(here("Outputs","ProductionData","2018_Kusko_Assignment_Results.csv"))

analysisDf_2018 <- st_as_sf(
  prod_2018["assignment_rescale"],
  geometry = st_geometry(shp)
)

analysisDf_2018 <- st_transform(analysisDf_2018, st_crs(sb5))

esc_2018 <- escapement %>%
  filter(Year == 2018, River == "Kusko") %>%
  pull(Escapement)

# Sb5 - 2018
sb5_joined_2018 <- st_join(sb5, analysisDf_2018, join = st_intersects)
sb5_joined_df_2018 <- st_set_geometry(sb5_joined_2018, NULL)

sb5_summary_df_2018 <- sb5_joined_df_2018 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb5_tidy_2018 <- sb5_summary_df_2018 %>%
  mutate(
    num_fish = total_assignment * esc_2018,
    spScale = 5,
    Year = 2018
  ) %>%
  rename(ID = HYBAS_ID)

# Sb6 - 2018
sb6_joined_2018 <- st_join(sb6, analysisDf_2018, join = st_intersects)
sb6_joined_df_2018 <- st_set_geometry(sb6_joined_2018, NULL)

sb6_summary_df_2018 <- sb6_joined_df_2018 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb6_tidy_2018 <- sb6_summary_df_2018 %>%
  mutate(
    num_fish = total_assignment * esc_2018,
    spScale = 6,
    Year = 2018
  ) %>%
  rename(ID = HYBAS_ID)

# Sb7 - 2018
sb7_joined_2018 <- st_join(sb7, analysisDf_2018, join = st_intersects)
sb7_joined_df_2018 <- st_set_geometry(sb7_joined_2018, NULL)

sb7_summary_df_2018 <- sb7_joined_df_2018 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb7_tidy_2018 <- sb7_summary_df_2018 %>%
  mutate(
    num_fish = total_assignment * esc_2018,
    spScale = 7,
    Year = 2018
  ) %>%
  rename(ID = HYBAS_ID)

all_scales_2018 <- bind_rows(sb5_tidy_2018, sb6_tidy_2018, sb7_tidy_2018)

# ---------------------------
# 4. Process Year 2019
# ---------------------------

prod_2019 <- read.csv(here("Outputs","ProductionData","2019_Kusko_Assignment_Results.csv"))

analysisDf_2019 <- st_as_sf(
  prod_2019["assignment_rescale"],
  geometry = st_geometry(shp)
)

analysisDf_2019 <- st_transform(analysisDf_2019, st_crs(sb5))

esc_2019 <- escapement %>%
  filter(Year == 2019, River == "Kusko") %>%
  pull(Escapement)

# Sb5 - 2019
sb5_joined_2019 <- st_join(sb5, analysisDf_2019, join = st_intersects)
sb5_joined_df_2019 <- st_set_geometry(sb5_joined_2019, NULL)

sb5_summary_df_2019 <- sb5_joined_df_2019 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb5_tidy_2019 <- sb5_summary_df_2019 %>%
  mutate(
    num_fish = total_assignment * esc_2019,
    spScale = 5,
    Year = 2019
  ) %>%
  rename(ID = HYBAS_ID)

# Sb6 - 2019
sb6_joined_2019 <- st_join(sb6, analysisDf_2019, join = st_intersects)
sb6_joined_df_2019 <- st_set_geometry(sb6_joined_2019, NULL)

sb6_summary_df_2019 <- sb6_joined_df_2019 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb6_tidy_2019 <- sb6_summary_df_2019 %>%
  mutate(
    num_fish = total_assignment * esc_2019,
    spScale = 6,
    Year = 2019
  ) %>%
  rename(ID = HYBAS_ID)

# Sb7 - 2019
sb7_joined_2019 <- st_join(sb7, analysisDf_2019, join = st_intersects)
sb7_joined_df_2019 <- st_set_geometry(sb7_joined_2019, NULL)

sb7_summary_df_2019 <- sb7_joined_df_2019 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb7_tidy_2019 <- sb7_summary_df_2019 %>%
  mutate(
    num_fish = total_assignment * esc_2019,
    spScale = 7,
    Year = 2019
  ) %>%
  rename(ID = HYBAS_ID)

all_scales_2019 <- bind_rows(sb5_tidy_2019, sb6_tidy_2019, sb7_tidy_2019)

# ---------------------------
# 5. Process Year 2020
# ---------------------------

prod_2020 <- read.csv(here("Outputs","ProductionData","2020_Kusko_Assignment_Results.csv"))

analysisDf_2020 <- st_as_sf(
  prod_2020["assignment_rescale"],
  geometry = st_geometry(shp)
)

analysisDf_2020 <- st_transform(analysisDf_2020, st_crs(sb5))

esc_2020 <- escapement %>%
  filter(Year == 2020, River == "Kusko") %>%
  pull(Escapement)

# Sb5 - 2020
sb5_joined_2020 <- st_join(sb5, analysisDf_2020, join = st_intersects)
sb5_joined_df_2020 <- st_set_geometry(sb5_joined_2020, NULL)

sb5_summary_df_2020 <- sb5_joined_df_2020 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb5_tidy_2020 <- sb5_summary_df_2020 %>%
  mutate(
    num_fish = total_assignment * esc_2020,
    spScale = 5,
    Year = 2020
  ) %>%
  rename(ID = HYBAS_ID)

# Sb6 - 2020
sb6_joined_2020 <- st_join(sb6, analysisDf_2020, join = st_intersects)
sb6_joined_df_2020 <- st_set_geometry(sb6_joined_2020, NULL)

sb6_summary_df_2020 <- sb6_joined_df_2020 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb6_tidy_2020 <- sb6_summary_df_2020 %>%
  mutate(
    num_fish = total_assignment * esc_2020,
    spScale = 6,
    Year = 2020
  ) %>%
  rename(ID = HYBAS_ID)

# Sb7 - 2020
sb7_joined_2020 <- st_join(sb7, analysisDf_2020, join = st_intersects)
sb7_joined_df_2020 <- st_set_geometry(sb7_joined_2020, NULL)

sb7_summary_df_2020 <- sb7_joined_df_2020 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb7_tidy_2020 <- sb7_summary_df_2020 %>%
  mutate(
    num_fish = total_assignment * esc_2020,
    spScale = 7,
    Year = 2020
  ) %>%
  rename(ID = HYBAS_ID)

all_scales_2020 <- bind_rows(sb5_tidy_2020, sb6_tidy_2020, sb7_tidy_2020)

# ---------------------------
# 6. Process Year 2021
# ---------------------------

prod_2021 <- read.csv(here("Outputs","ProductionData","2021_Kusko_Assignment_Results.csv"))

analysisDf_2021 <- st_as_sf(
  prod_2021["assignment_rescale"],
  geometry = st_geometry(shp)
)

analysisDf_2021 <- st_transform(analysisDf_2021, st_crs(sb5))

esc_2021 <- escapement %>%
  filter(Year == 2021, River == "Kusko") %>%
  pull(Escapement)

# Sb5 - 2021
sb5_joined_2021 <- st_join(sb5, analysisDf_2021, join = st_intersects)
sb5_joined_df_2021 <- st_set_geometry(sb5_joined_2021, NULL)

sb5_summary_df_2021 <- sb5_joined_df_2021 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb5_tidy_2021 <- sb5_summary_df_2021 %>%
  mutate(
    num_fish = total_assignment * esc_2021,
    spScale = 5,
    Year = 2021
  ) %>%
  rename(ID = HYBAS_ID)

# Sb6 - 2021
sb6_joined_2021 <- st_join(sb6, analysisDf_2021, join = st_intersects)
sb6_joined_df_2021 <- st_set_geometry(sb6_joined_2021, NULL)

sb6_summary_df_2021 <- sb6_joined_df_2021 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb6_tidy_2021 <- sb6_summary_df_2021 %>%
  mutate(
    num_fish = total_assignment * esc_2021,
    spScale = 6,
    Year = 2021
  ) %>%
  rename(ID = HYBAS_ID)

# Sb7 - 2021
sb7_joined_2021 <- st_join(sb7, analysisDf_2021, join = st_intersects)
sb7_joined_df_2021 <- st_set_geometry(sb7_joined_2021, NULL)

sb7_summary_df_2021 <- sb7_joined_df_2021 %>%
  group_by(HYBAS_ID) %>%
  summarise(total_assignment = sum(assignment_rescale, na.rm = TRUE)) %>%
  ungroup()

sb7_tidy_2021 <- sb7_summary_df_2021 %>%
  mutate(
    num_fish = total_assignment * esc_2021,
    spScale = 7,
    Year = 2021
  ) %>%
  rename(ID = HYBAS_ID)

all_scales_2021 <- bind_rows(sb5_tidy_2021, sb6_tidy_2021, sb7_tidy_2021)

# ---------------------------
# 7. Combine all years
# ---------------------------

all_years_data <- bind_rows(
  all_scales_2017,
  all_scales_2018,
  all_scales_2019,
  all_scales_2020,
  all_scales_2021
)

# ---------------------------
# 8. Calculate CV for each polygon
# ---------------------------

cv_summary <- all_years_data %>%
  group_by(ID, spScale) %>%
  summarise(
    mean_fish = mean(num_fish, na.rm = TRUE),
    sd_fish = sd(num_fish, na.rm = TRUE),
    cv = sd_fish / mean_fish,
    n_years = n(),
    .groups = "drop"
  )

# ---------------------------
# 9. Join CV to spatial data
# ---------------------------

# Scale 5
cv_sb5 <- cv_summary %>%
  filter(spScale == 5)

sb5_cv <- sb5 %>%
  left_join(cv_sb5, by = c("HYBAS_ID" = "ID"))

# Scale 6
cv_sb6 <- cv_summary %>%
  filter(spScale == 6)

sb6_cv <- sb6 %>%
  left_join(cv_sb6, by = c("HYBAS_ID" = "ID"))

# Scale 7
cv_sb7 <- cv_summary %>%
  filter(spScale == 7)

sb7_cv <- sb7 %>%
  left_join(cv_sb7, by = c("HYBAS_ID" = "ID"))

# ---------------------------
# 10. Visualizations
# ---------------------------

# Calculate basin-wide escapement CV
basin_wide <- escapement %>%
  filter(Year %in% 2017:2021, River == "Kusko") %>%
  summarise(
    mean_esc = mean(Escapement, na.rm = TRUE),
    sd_esc = sd(Escapement, na.rm = TRUE),
    cv = sd_esc / mean_esc,
    spScale = "Basin-wide"
  )

# Calculate average CV by spatial scale
cv_by_scale <- cv_summary %>%
  group_by(spScale) %>%
  summarise(
    mean_cv = mean(cv, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(spScale = as.character(spScale))

# Combine for boxplot
cv_for_boxplot <- cv_summary %>%
  mutate(spScale = as.character(spScale)) %>%
  select(spScale, cv)

basin_wide_for_boxplot <- data.frame(
  spScale = "Basin-wide",
  cv = basin_wide$cv
)

cv_all_scales <- bind_rows(cv_for_boxplot, basin_wide_for_boxplot)

# Reorder spatial scale factor
cv_all_scales$spScale <- factor(cv_all_scales$spScale, 
                                levels = c("7", "6", "5", "Basin-wide"))

# Calculate mean and median for annotations
cv_stats_for_plot <- cv_all_scales %>%
  group_by(spScale) %>%
  summarise(
    mean_cv = mean(cv, na.rm = TRUE),
    median_cv = median(cv, na.rm = TRUE)
  )

print("CV Statistics:")
print(cv_stats_for_plot)

# Full boxplot with ordered scales
p_cv_boxplot <- ggplot(cv_all_scales, aes(x = spScale, y = cv)) +
  geom_boxplot(fill = "#F79D5C") +
  geom_point(alpha = 0.3) +
  geom_text(data = cv_stats_for_plot, 
            aes(x = spScale, y = -0.05, 
                label = paste0("Mean: ", round(mean_cv, 3), "\nMedian: ", round(median_cv, 3))),
            size = 3, hjust = 0.5) +
  labs(
    title = "Coefficient of Variation by Spatial Scale",
    x = "Spatial Scale",
    y = "CV"
  ) +
  theme_minimal()

# Limited boxplot (CV < 0.7)
cv_all_scales_limited <- cv_all_scales %>%
  filter(cv < 0.7)

cv_stats_for_plot_limited <- cv_all_scales_limited %>%
  group_by(spScale) %>%
  summarise(
    mean_cv = mean(cv, na.rm = TRUE),
    median_cv = median(cv, na.rm = TRUE),
    n = n()
  )

print("CV Statistics (limited to CV < 0.7):")
print(cv_stats_for_plot_limited)

p_cv_boxplot_limited <- ggplot(cv_all_scales_limited, aes(x = spScale, y = cv)) +
  geom_boxplot(fill = "#F79D5C") +
  geom_point(alpha = 0.3) +
  geom_text(data = cv_stats_for_plot_limited, 
            aes(x = spScale, y = -0.02, 
                label = paste0("Mean: ", round(mean_cv, 3), "\nMedian: ", round(median_cv, 3))),
            size = 3, hjust = 0.5) +
  labs(
    title = "Coefficient of Variation by Spatial Scale (CV < 0.7)",
    x = "Spatial Scale",
    y = "CV"
  ) +
  theme_minimal() +
  ylim(c(-0.05, 0.7))


# Prepare basin-wide z-normalized escapement for time series
basin_esc_timeseries <- escapement %>%
  filter(Year %in% 2017:2021, River == "Kusko") %>%
  mutate(
    mean_esc = mean(Escapement, na.rm = TRUE),
    sd_esc = sd(Escapement, na.rm = TRUE),
    z_esc = (Escapement - mean_esc) / sd_esc
  )

# Time series - Scale 5
data_sb5 <- all_years_data %>%
  filter(spScale == 5) %>%
  group_by(ID) %>%
  mutate(
    mean_fish = mean(num_fish, na.rm = TRUE),
    sd_fish = sd(num_fish, na.rm = TRUE),
    z_fish = (num_fish - mean_fish) / sd_fish
  ) %>%
  ungroup()

p_sb5_ts <- ggplot(data_sb5, aes(x = Year, y = z_fish, group = ID)) +
  geom_line(alpha = 0.6, color = "grey30") +
  geom_line(data = basin_esc_timeseries, aes(x = Year, y = z_esc, group = 1), 
            color = "#F15156", size = 1.5, linetype = "solid") +
  labs(
    title = "Scale 5",
    x = "Year",
    y = "Z-score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Time series - Scale 6
data_sb6 <- all_years_data %>%
  filter(spScale == 6) %>%
  group_by(ID) %>%
  mutate(
    mean_fish = mean(num_fish, na.rm = TRUE),
    sd_fish = sd(num_fish, na.rm = TRUE),
    z_fish = (num_fish - mean_fish) / sd_fish
  ) %>%
  ungroup()

p_sb6_ts <- ggplot(data_sb6, aes(x = Year, y = z_fish, group = ID)) +
  geom_line(alpha = 0.6, color = "grey30") +
  geom_line(data = basin_esc_timeseries, aes(x = Year, y = z_esc, group = 1), 
            color = "#F15156", size = 1.5, linetype = "solid") +
  labs(
    title = "Scale 6",
    x = "Year",
    y = "Z-score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Time series - Scale 7
data_sb7 <- all_years_data %>%
  filter(spScale == 7) %>%
  group_by(ID) %>%
  mutate(
    mean_fish = mean(num_fish, na.rm = TRUE),
    sd_fish = sd(num_fish, na.rm = TRUE),
    z_fish = (num_fish - mean_fish) / sd_fish
  ) %>%
  ungroup()

p_sb7_ts <- ggplot(data_sb7, aes(x = Year, y = z_fish, group = ID)) +
  geom_line(alpha = 0.6, color = "grey30") +
  geom_line(data = basin_esc_timeseries, aes(x = Year, y = z_esc, group = 1), 
            color = "#F15156", size = 1.5, linetype = "solid") +
  labs(
    title = "Scale 7",
    x = "Year",
    y = "Z-score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Combine all three time series into a single 3-panel figure
library(patchwork)

p_combined_ts <- p_sb7_ts / p_sb6_ts / p_sb5_ts +
  plot_annotation(
    title = "Fish Production Over Time by Spatial Scale (Z-normalized)",
    subtitle = "Colored line = Basin-wide escapement"
  )

# Print the plots
print(p_combined_ts)
print(p_cv_boxplot)
print(p_cv_boxplot_limited)
