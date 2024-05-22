library(tidyverse)
library(here)
library(sf)
library(biscale) #bivariate choropleths
library(cowplot) #combine map and legend bivariate choropleth
library(scatterpie) #proportional symbols pie charts
library(here) #setup
library(sf) #vector data         
library(tidyverse) #data manipulation
library(spData) #datasets

Yukon_basemap<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_alb.shp")



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


#################
# Aggregate by tributary 

# Start a new data frame, and sum the total production by tributary for each year

yukon_trib_summary <- yukon_withTribs %>%
  group_by(trbtry_) %>%
  summarize(
    totalprod2015 = sum(yuk2015, na.rm = TRUE),
    totalprod2016 = sum(yuk2016, na.rm = TRUE),
    totalprod2017 = sum(yuk2017, na.rm = TRUE),
    total_stream_length = sum(Shp_Lng, na.rm = TRUE)  # Summarize the sum of Shp_lng per tributary
  ) %>%
  rowwise() %>%  # Ensure operations are row-wise
  mutate(
    mean_prod = mean(c(totalprod2015, totalprod2016, totalprod2017), na.rm = TRUE),
    sd_prod = sd(c(totalprod2015, totalprod2016, totalprod2017), na.rm = TRUE),
    cv = if_else(is.na(sd_prod / mean_prod * 100), 0, sd_prod / mean_prod * 100), 
    prod_per_stream_length = mean_prod / total_stream_length
  ) %>%
  ungroup()

# Read in polygon shapefile 
trib_polygons<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/trib_polygons.shp")

#Make yukon_trib_summary a df and remove geometry 
yukon_trib_summary_df <- as.data.frame(yukon_trib_summary)
yukon_trib_summary_df <- yukon_trib_summary_df %>% select(-geometry)

#Join the df into the sf object 
trib_polygons <- left_join(trib_polygons, yukon_trib_summary_df, by = c("Trib" = "trbtry_"))

trib_polygons_map <- ggplot() +
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) +  # Add basemap layer
  geom_sf(data = trib_polygons, aes(fill = prod_per_stream_length), color = "white") +
  scale_fill_gradient2(low = "steelblue4", mid = "white", high = "firebrick4", midpoint = mean(trib_polygons$prod_per_stream_length, na.rm = TRUE)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Coefficient of Variation in Production Across the Yukon Basin", fill = "Coefficient of Variation")

print(trib_polygons_map)

##### Bivariate Cloropleth Map of Production per Stream Length and CV

trib_polygons <- trib_polygons %>%
  mutate(
    prod_per_stream_length = ifelse(is.na(prod_per_stream_length), 0, prod_per_stream_length),
    cv = ifelse(is.na(cv), 0, cv)
  )


bivariate_data<- bi_class(
  trib_polygons, 
  x = "prod_per_stream_length",
  y = "cv", 
  style = "quantile", 
  dim = 3
)

bivariate_map<- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) +  # Add basemap layer 
  geom_sf(
    data = bivariate_data, 
    mapping = aes(fill = bi_class), color = "white", 
    size = .1, show.legend = FALSE
  ) + 
  bi_scale_fill(pal = "BlueOr", dim = 3) + 
  bi_theme()

bivch_legend <- bi_legend(
  pal = "BlueOr", 
  dim = 3, 
  xlab = "Higher production per stream length", 
  ylab = "Higher Variability", 
  size = 8 
) 

?bi_pal
ggdraw() + 
  draw_plot(bivariate_map, 0,0,1,1) + 
  draw_plot(bivch_legend, 0,0,.3,.3)

#### Variability by Stream Order resolution


# Find all of the lowest sections of Trib with a given StrOrd starting at 6, (touching a trib with one higher stream order) 
# Find all of the upstream reaches of that trib 
# Group all together with some sort of ID
# Calculate the mean production of that aggregate section 
# Calculate the Sd of that aggregate section 
# Calculate the CV of that aggregate section




##################
#### Demonstrate the Variability lost by aggregating at genetic resolution for Canada 

# Break Canada Tribs into 5th or lower
# Calculate CV for Canada region as a whole 
# Calculate the CV for each tributary grouping 
# Compare 

#################








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
