# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(here)
library(sf)
library(ggplot2)

# Connect to the database 
SMH2_db <- dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

# Define the parameters
years <- c(2015, 2016, 2017, 2018)
watershed <- "Yukon"

#years <- c(2017, 2018, 2019, 2020)
#watershed <- "Kuskokwim"

# Create an empty list to store the results
TotalProd <- list()

# Loop through each year and retrieve the data
for (year in years) {
  # Construct the query
  query <- paste(
    "SELECT Production FROM production_matrices WHERE Watershed = '", watershed, 
    "' AND Year = ", year, 
    " AND Sensitivity = 0.7",
    " AND Quartile = 'Total'", sep = ""
  )
  
  # Fetch data from the database for the specified conditions
  data <- dbGetQuery(SMH2_db, query)
  
  # Store the result in the list, naming the column by the year
  TotalProd[[as.character(year)]] <- data$Production
}

# Convert the list into a data frame, filling shorter columns with NA
TotalProd_df <- bind_cols(TotalProd)

##### Now add a new collumn of a mean of the production values across the years 
TotalProd_df$Mean <- rowMeans(TotalProd_df, na.rm = TRUE)

### Add a new column of sd across the first 4 collumns 
TotalProd_df$SD <- apply(TotalProd_df[,1:4], 1, sd, na.rm = TRUE)

### Calculate the coefficient of variation
TotalProd_df$CV <- TotalProd_df$SD / TotalProd_df$Mean

#### Save as a .csv file for Yukon Prod_CV 
write.csv(TotalProd_df, here("Outputs", "Yukon_Prod_CV.csv"), row.names = FALSE)


#################################################################################
#################################################################################
# Read in the Yukon Shapefile 
shp_Yukon<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")
Yukon_basemap<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_alb.shp")
trib_polygons<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/trib_polygons.shp")

## Add Mean production and CV as attributes to the shapefile 
shp_Yukon$Mean_Production <- TotalProd_df$Mean
shp_Yukon$CV <- TotalProd_df$CV

# Create maps of distribution of mean production over the whole dataset 

meanProdMap <- ggplot(shp_Yukon) +
  geom_sf(aes(color = Mean_Production)) +
  scale_color_gradient2(low = "white", high = "firebrick") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Mean Production Across the Yukon Basin", color = "Mean Production")

# Export as a pdf
ggsave(here("Basin Maps/meanProdMap.pdf"), plot = meanProdMap, width = 10, height = 10, units = "in", dpi = 300)


# Create map of the distribution of variation over the dataset 

# Make any NA's in the CV column white
shp_Yukon$CV[is.na(shp_Yukon$CV)] <- 0

CVmap <- ggplot() +
  geom_sf(data = Yukon_basemap)+ 
  geom_sf(data = shp_Yukon, aes(color = CV)) +
  scale_color_gradient2(low = "dodgerblue4", high = "firebrick") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Coefficient of Variation in Production Across the Yukon Basin", color = "Coefficient of Variation")

ggsave(here("Basin Maps/CVmap.pdf"), plot = CVmap, width = 10, height = 10, units = "in", dpi = 300)


#Create a scatteplot of mean production vs CV 

scatterplot <- ggplot(TotalProd_df, aes(x = Mean, y = CV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mean Production vs Coefficient of Variation", x = "Mean Production", y = "Coefficient of Variation")



#########

TotalProd_df$Trib<- shp_Yukon$trbtry_

yearly_prod_trib <- TotalProd_df %>%
  select(-Mean, -SD, -CV) %>%
  group_by(Trib) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) 

# Add values to the sf shapefile trib polygons 
trib_polygons <- left_join(trib_polygons, yearly_prod_trib, by = c("Trib" = "Trib"))


# Map of 2015 production by tributary

prod_2015 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = `2015`), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2015 Production by Tributary", fill = "Production")

prod_2015

# Map of 2016 production by tributary

prod_2016 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = `2016`), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2016 Production by Tributary", fill = "Production")

prod_2016

# Map of 2017 production by tributary

prod_2017 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = `2017`), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2017 Production by Tributary", fill = "Production")

# Map of 2018 production by tributary

prod_2018 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = `2018`), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2018 Production by Tributary", fill = "Production")

### Display all the maps together
library(gridExtra)
combined<-grid.arrange(prod_2015, prod_2016, prod_2017, prod_2018, ncol = 2)

# Save as a pdf 
ggsave(here("Basin Maps/Production_by_tributary.pdf"), plot = combined, width = 10, height = 10, units = "in", dpi = 300)



##############################################

# Bivariate chloropleth map of production vs variability by tributary grouping






bivariate_data<- bi_class(
  trib_polygons, 
  x = "prod_per_stream_length",
  y = "cv", 
  style = "quantile", 
  dim = 3
)

bivariate_map<- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + # Add basemap layer 
  geom_sf(
    data = bivariate_data, 
    mapping = aes(fill = bi_class), color = "white", 
    size = .1, show.legend = FALSE
  ) + 
  bi_scale_fill(pal = "BlueOr", dim = 3) + 
  bi_theme()+ 
  ggtitle("Mean Production vs Variability by Tributary grouping") 

bivch_legend <- bi_legend(
  pal = "BlueOr", 
  dim = 3, 
  xlab = "Higher production per stream length", 
  ylab = "Higher Variability", 
  size = 8 
) 

ggdraw() + 
  draw_plot(bivariate_map, 0,0,1,1) + 
  draw_plot(bivch_legend, 0,0,.2,.2)



