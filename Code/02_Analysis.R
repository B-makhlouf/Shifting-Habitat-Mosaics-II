library(tidyverse)
library(here)
library(sf)

# Function to read and extract the 'Total' column from CSV files
read_total_column <- function(file_path) {
  read_csv(here(file_path)) %>% select(Total)
}

# List of file paths
file_paths <- list(
  "Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv",
  "Outputs/Assignment Matrix/Yukon_2016_0.7_basin_assignments.csv",
  "Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv",
  "Outputs/Assignment Matrix/Yukon_2017_0.7_basin_assignments.csv"
)

# Read in all assignment matrices and combine them
full_yukon_assign <- file_paths %>%
  map_dfc(read_total_column) %>%
  rename_with(~c("yuk2015", "yuk2016", "yuk2017", "yuk2018"))

# Read in the shape file with the Tributary Names
yukon_withTribs <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")

# Merge the full yukon assign data frame into the yukon_withTribs data frame
yukon_withTribs <- yukon_withTribs %>%
  bind_cols(full_yukon_assign) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c_across(yuk2015:yuk2017), na.rm = TRUE),
    sd_prod = sd(c_across(yuk2015:yuk2017), na.rm = TRUE),
    cv = if_else(is.na(sd_prod / mean_prod * 100), 0, sd_prod / mean_prod * 100)
  ) %>%
  ungroup()



####################
###### CV and production by trib section
####################

#### How is variability distributed by trib section across the riverscape 

# Plot the CV map
CVmap <- ggplot(yukon_withTribs) +
  geom_sf(aes(color = cv)) +
  scale_color_gradient2(low = "white", high = "firebrick") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Coefficient of Variation in Production Across the Yukon Basin", color = "Coefficient of Variation")

# Export as a pdf
ggsave(here("Figures/Maps/CVmap.pdf"), plot = CVmap, width = 10, height = 10, units = "in", dpi = 300)


##### How is mean production distributed by trib section across the riverscape

# Plot the mean production map
meanProdMap <- ggplot(yukon_withTribs) +
  geom_sf(aes(color = mean_prod)) +
  scale_color_gradient2(low = "white", high = "firebrick") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Mean Production Across the Yukon Basin", color = "Mean Production")

# Export as a pdf
ggsave(here("Figures/Maps/meanProdMap.pdf"), plot = meanProdMap, width = 10, height = 10, units = "in", dpi = 300)









?scale_fill_brewer
?scale_color_gradient2






# Start a new data frame, and sum the total production by tributary for each year 
yukon_trib_summary<- yukon_withTribs %>% 
  group_by(trbtry_) %>% 
  summarize(totalprod2015 = sum(yuk2015), totalprod2016 = sum(yuk2016), totalprod2017 = sum(yuk2017))

# Calculate the cv for each tributary
yukon_trib_summary$cv2015<- sd(yukon_trib_summary$totalprod2015)/mean(yukon_trib_summary$totalprod2015)
yukon_trib_summary <- yukon_trib_summary %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(totalprod2015, totalprod2016, totalprod2017), na.rm = TRUE),
    sd_prod = sd(c(totalprod2015, totalprod2016, totalprod2017), na.rm = TRUE),
    cv = sd_prod / mean_prod
  )

# 

#Plot CV by trib, ordered by CV 
ggplot(yukon_trib_summary, aes(x = reorder(trbtry_, cv), y = cv))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Tributary", y = "Coefficient of Variation") 

# Plot Cv vs production
ggplot(yukon_trib_summary, aes(x = mean_prod, y = cv))+
  geom_point()+
  theme_minimal()+
  labs(x = "Mean Production", y = "Coefficient of Variation") 
