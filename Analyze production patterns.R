# This is a script to analyze patterns of production 
library(sf)
library(tidyverse)
library(here)

# Read in clean shapefiles 
yuk_edges<-st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")

## Read in all the data for the full years of the Yukon 
yuk_2015_all<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.5_basin_norm.csv")) 
yuk_2016_all<- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.5_basin_norm.csv"))
yuk_2017_all<- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.5_basin_norm.csv"))
yuk_2019_all<- read_csv(here("Data/Production/Yukon/2019_full_Yukon_0.5_basin_norm.csv"))
yuk_2021_all<- read_csv(here("Data/Production/Yukon/2021_full_Yukon_0.5_basin_norm.csv"))

#Add stream order values to production estimates based on the indices of edges
yuk_2015_all$stream_order<- yuk_edges$Str_Ord
yuk_2016_all$stream_order<- yuk_edges$Str_Ord
yuk_2017_all$stream_order<- yuk_edges$Str_Ord
yuk_2019_all$stream_order<- yuk_edges$Str_Ord
yuk_2021_all$stream_order<- yuk_edges$Str_Ord

#Add Stream Length values 
yuk_2015_all$stream_length<- yuk_edges$Shp_Lng
yuk_2016_all$stream_length<- yuk_edges$Shp_Lng
yuk_2017_all$stream_length<- yuk_edges$Shp_Lng
yuk_2019_all$stream_length<- yuk_edges$Shp_Lng
yuk_2021_all$stream_length<- yuk_edges$Shp_Lng

#Add Tributary names 
yuk_2015_all$tribs<-yuk_edges$trbtry_
yuk_2016_all$tribs<-yuk_edges$trbtry_
yuk_2017_all$tribs<-yuk_edges$trbtry_
yuk_2019_all$tribs<-yuk_edges$trbtry_
yuk_2021_all$tribs<-yuk_edges$trbtry_

#Add Year 
yuk_2015_all$Year<-2015
yuk_2016_all$Year<-2016
yuk_2017_all$Year<-2017
yuk_2019_all$Year<-2019
yuk_2021_all$Year<-2021

#Combine all the years into one dataframe
yuk_all<-rbind(yuk_2015_all,yuk_2016_all,yuk_2017_all,yuk_2019_all,yuk_2021_all)

#Combine across years 
yuk_across_years<- data_frame(tributary = yuk_2015_all$tribs, 
                              Stream_order = yuk_2015_all$stream_order,
                              stream_length = yuk_2015_all$stream_length,
                              year_2015 = yuk_2015_all$rescaled,
                              year_2016 = yuk_2016_all$rescaled,
                              yuk_2021 = yuk_2021_all$rescaled,
                              avg_mean = (yuk_2015_all$rescaled + yuk_2016_all$rescaled +  yuk_2021_all$rescaled)/5)

#Add sd 
yuk_across_years <- yuk_across_years %>%
  rowwise() %>%
  mutate(sd = sd(c(year_2015, year_2016, yuk_2021)))

# Calculate the coefficient of variation for each row 
yuk_across_years <- yuk_across_years %>%
  rowwise() %>%
  mutate(cv = sd/avg_mean)


# rescale all the production estimates to be between 0 and 1
yuk_across_years$rescaled <- (yuk_across_years$avg_mean - min(yuk_across_years$avg_mean)) / (max(yuk_across_years$avg_mean) - min(yuk_across_years$avg_mean))

###### production estimates 

fishcount2015<- 116000 #cumulative passage estimate
fishcount2016<- 176900 
fishcount2021<- 124874

fish_across_years <- data_frame(
  tributary = yuk_2015_all$tribs,
  Stream_order = yuk_2015_all$stream_order,
  stream_length = yuk_2015_all$stream_length,
  year_2015 = yuk_2015_all$rescaled * fishcount2015, 
  year_2016 = yuk_2016_all$rescaled * fishcount2016,
  year_2021 = yuk_2021_all$rescaled * fishcount2021
)

fish_across_years <- mutate(fish_across_years,
                            avg_mean = (year_2015 + year_2016 + year_2021) / 3)

fish_across_years <- fish_across_years %>%
  rowwise() %>%
  mutate(sd = sd(c(year_2015, year_2016, year_2021)))

fish_across_years <- fish_across_years %>%
  rowwise() %>%
  mutate(cv = sd/avg_mean)

tributary_names<-unique(yuk_across_years$tributary)



by_trib_cv<- data_frame(tributary = NA, 
                        StrOrd = NA, 
                        mean_cv = NA,
                        range_cv = NA,
                        mean_prod = NA, 
                        range_prod = NA) 


# Initialize an empty list to store data frames
trib_data_list <- list()


###############-----------------------------------------------------------------



for (x in 1:length(tributary_names)) {
  
  current_trib <- fish_across_years %>%
    filter(tributary == tributary_names[x])

  
  # Initialize an empty list to store data frames for each i
  trib_data <- list()
  
  for (i in 3:9) {
    
    current_trib_cv_select <- current_trib %>%
      filter(Stream_order <= i)
    
    
    # Create a data frame for the current i value
    by_trib_cv <- data.frame(
      Tributary = tributary_names[x],
      StrOrd = i,
      mean_cv = mean(current_trib_cv_select$cv),
      mean_prod = mean(current_trib_cv_select$avg_mean)
    )
    
    # Store the data frame in the list
    trib_data[[i - 2]] <- by_trib_cv  # Store at index corresponding to i value
    
  }
  
  # Store the list of data frames for the current tributary in the main list
  trib_data_list[[x]] <- trib_data
  
}




names(trib_data_list) <- tributary_names
combined_data <- bind_rows(unlist(trib_data_list, recursive = FALSE))

plot <- ggplot(combined_data, aes(x = StrOrd, y = mean_cv)) +
geom_point() +
  facet_wrap(~ Tributary) +
  labs(x = "Mean Production", y = "Coefficient of Variation", title = "CV vs Mean Production by Tributary")
