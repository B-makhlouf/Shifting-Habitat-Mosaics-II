# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(here)
library(sf)
library(ggplot2)
library(matrixStats)
library(gridExtra)

# Connect to the database 
SMH2_db <- dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Desktop/Databases/SHM2.db")

# Define the parameters
years <- c(2015, 2016, 2017, 2018)
watershed <- "Yukon"

# Create an empty list to store the results
TotalProd <- list()

# Loop through each year and retrieve the data
for (year in years) {
  # Construct the query
  query <- paste(
    "SELECT Production, Shp_Lng FROM production_matrices WHERE Watershed = '", watershed, 
    "' AND Year = ", year, 
    " AND Sensitivity = 0.7",
    " AND Quartile = 'Total'", sep = ""
  )
  
  # Fetch data from the database for the specified conditions
  data <- dbGetQuery(SMH2_db, query)
  
  # Store the result in the list, naming the column by the year
  TotalProd[[as.character(year)]] <- data$Production
  
  # Extract Shp_Lng from any year (since it's the same across years)
  if (!exists("Shp_Lng")) {
    Shp_Lng <- data$Shp_Lng
  }
}

# Convert the list into a data frame, filling shorter columns with NA
TotalProd_df <- bind_cols(TotalProd)

# Add the mean of the production values across the years
TotalProd_df$Mean <- rowMeans(TotalProd_df, na.rm = TRUE)

# Add the standard deviation across the first 4 columns
TotalProd_df$SD <- apply(TotalProd_df[, 1:4], 1, sd, na.rm = TRUE)

# Calculate the coefficient of variation
TotalProd_df$CV <- TotalProd_df$SD / TotalProd_df$Mean

# Add the Shp_Lng column to TotalProd_df
TotalProd_df$Shp_Lng <- Shp_Lng

TotalProd_df$ProdPerStrLngth<- TotalProd_df$Mean/TotalProd_df$Shp_Lng

# Save as a .csv file for Yukon Prod_CV
write.csv(TotalProd_df, here("Outputs", "Yukon_Prod_CV.csv"), row.names = FALSE)






#################################################################################

#### Trib maps of production 

#################################################################################
# Read in the Yukon Shapefile 
shp_Yukon<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")
Yukon_basemap<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_alb.shp")
trib_polygons<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/trib_polygons.shp")

## Add Mean production and CV as attributes to the shapefile 
shp_Yukon$Mean_Production <- TotalProd_df$ProdPerStrLngth
shp_Yukon$CV <- TotalProd_df$CV

############ Mean Production per Km stream length

meanProdMap <- ggplot(shp_Yukon) +
  geom_sf(aes(color = Mean_Production)) +
  scale_color_gradient2(low = "white", high = "firebrick") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Mean Production Across the Yukon Basin", color = "Mean Production")
ggsave(here("Basin Maps/meanProdMap.pdf"), plot = meanProdMap, width = 10, height = 10, units = "in", dpi = 300) # Export


############ Total CV 

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



#########################################################################################################################
################### Production by tribuatary polygons ##################################################################

TotalProd_df$Trib <- shp_Yukon$trbtry_# Add the Trib name for each trib

rescale_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

yearly_prod_trib <- TotalProd_df %>%
  select(-Mean, -SD, -CV, -ProdPerStrLngth) %>% # Remove the Mean, SD, and CV columns
  group_by(Trib) %>% # Group by Trib group
  summarise(across(everything(), sum, na.rm = TRUE)) %>% # Sum the production values for each trib grouping
  mutate(across(-c(Trib, Shp_Lng), ~ . / Shp_Lng)) %>% # Divide the production values by the stream length
  mutate(GROUP_mean_prod_StrLength = rowMeans(select(., `2015`, `2016`, `2017`, `2018`), na.rm = TRUE), # Add Mean_prod_StrLength column
         GROUP_SD_prod_StrLength = rowSds(as.matrix(select(., `2015`, `2016`, `2017`, `2018`)), na.rm = TRUE), # Add SD_prod_StrLength column
         GROUP_CV_prod_StrLength = GROUP_SD_prod_StrLength / GROUP_mean_prod_StrLength) %>% # Add CV_prod_StrLength column 
  mutate(across(c(`2015`, `2016`, `2017`, `2018`), rescale_01, .names = "rescaled_{col}")) # Rescale production values

# Rescale all production values to be between 0 and 1



# Add values to the sf shapefile trib polygons 
trib_polygons <- left_join(trib_polygons, yearly_prod_trib, by = c("Trib" = "Trib"))


# Map of 2015 production by tributary
prod_2015 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = rescaled_2015), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2015 Production by Tributary", fill = "Production")


# Map of 2016 production by tributary

prod_2016 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = rescaled_2016), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2016 Production by Tributary", fill = "Production")

# Map of 2017 production by tributary

prod_2017 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = rescaled_2017), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2017 Production by Tributary", fill = "Production")

# Map of 2018 production by tributary

prod_2018 <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + 
  geom_sf(data = trib_polygons, aes(fill = rescaled_2018), color = "white", size = .1) + 
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "2018 Production by Tributary", fill = "Production")

### Display all the maps together
combined<-grid.arrange(prod_2015, prod_2016, prod_2017, prod_2018, ncol = 2)

# Save as a pdf 
ggsave(here("Basin Maps/Production_by_tributary.pdf"), plot = combined, width = 10, height = 10, units = "in", dpi = 300)


##############################################

library(biscale) #bivariate choropleths
library(cowplot) #combine map and legend bivariate choropleth
library(scatterpie) #proportional symbols pie charts
library(here) #setup
library(sf) #vector data         
library(tidyverse) #data manipulation
library(spData) #datasets
# Bivariate chloropleth map of production vs variability by tributary grouping

# Classify the data for bivariate mapping
bivariate_data <- bi_class(
  trib_polygons, 
  x = "mean_prod_StrLength",
  y = "CV_prod_StrLength", 
  style = "quantile", 
  dim = 3
)

# Create the bivariate map
bivariate_map <- ggplot() + 
  geom_sf(data = Yukon_basemap, fill = "lightgrey", color = NA) + # Add basemap layer 
  geom_sf(
    data = bivariate_data, 
    mapping = aes(fill = bi_class), color = "white", 
    size = 0.1, show.legend = FALSE
  ) + 
  bi_scale_fill(pal = "BlueOr", dim = 3) + 
  bi_theme() + 
  ggtitle("Mean Production vs Variability by Tributary Grouping")

# Create the legend for the bivariate map
bivch_legend <- bi_legend(
  pal = "BlueOr", 
  dim = 3, 
  xlab = "Higher production per stream length", 
  ylab = "Higher Variability", 
  size = 8 
)

# Combine the map and the legend using cowplot
final_plot <- ggdraw() + 
  draw_plot(bivariate_map, 0, 0, 1, 1) + 
  draw_plot(bivch_legend, 0.75, 0.1, 0.2, 0.2) # Adjust position and size as needed

# Print the final plot
print(final_plot)



