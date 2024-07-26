# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)

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
shp_Yukon<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp")

## Add Mean production and CV as attributes to the shapefile 
shp_Yukon$Mean_Production <- TotalProd_df$Mean





# Create maps of distribution of mean production over the whole dataset 



# Create map of the distribution of variation over the dataset 



# Create a bicolor chloropleth map of the coefficient of variation of production values

  

