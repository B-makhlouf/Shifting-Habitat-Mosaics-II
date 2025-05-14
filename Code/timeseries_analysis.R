library(here)
library(tidyverse)

# Load both datasets
huc_prod_all <- read.csv(here("/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Analysis_Results/all_huc_production_values.csv"))
runsize <- read.csv(here("path/to/your/runsize.csv"))  # You'll need to update this path

# Prepare your huc data as before
huc_prod <- huc_prod_all %>%
  select(Name, production_proportion, year, watershed, analysis_type, quartile) %>%
  filter(analysis_type == "DOY") 

# Create the year-quartile timeseries
huc_timeseries <- huc_prod %>%
  mutate(year_quartile = paste0(year,"_", quartile)) %>%
  arrange(Name, year, quartile) %>%
  select(Name, year_quartile, production_proportion, watershed, year, quartile)

# Merge with run size data and calculate actual production values
huc_production_values <- huc_timeseries %>%
  left_join(runsize, by = c("year" = "Year")) %>%  # Assuming the column in runsize is "Year"
  mutate(production_value = production_proportion * Total.Run) %>%
  select(Name, year_quartile, production_proportion, production_value, watershed, year, quartile)

# View the results
head(huc_production_values)

# Now you can plot the actual production values instead of proportions
p1 <- ggplot(huc_production_values, aes(x = year_quartile, y = production_value, group = Name)) +
  geom_line(alpha = 0.7) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Name, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Production Values by Name (Actual Numbers)",
       x = "Year-Quartile",
       y = "Production Value",
       subtitle = "Calculated as production_proportion × total_run_size")

print(p1)


