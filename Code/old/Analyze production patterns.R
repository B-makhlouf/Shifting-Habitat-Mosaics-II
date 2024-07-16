# This is a script to analyze patterns of production 
library(sf)
library(tidyverse)
library(here)

# Read in clean shapefiles 
yuk_edges<-st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")

## Read in all the data for the full years of the Yukon 
yuk_2015_all<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.75_basin_norm.csv")) 
yuk_2016_all<- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.75_basin_norm.csv"))
yuk_2017_all<- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.75_basin_norm.csv"))
yuk_2019_all<- read_csv(here("Data/Production/Yukon/2019_full_Yukon_0.75_basin_norm.csv"))
yuk_2021_all<- read_csv(here("Data/Production/Yukon/2021_full_Yukon_0.75_basin_norm.csv"))

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


########## Data frame to compare among years 

yuk_across_years<- data_frame(tributary = yuk_2015_all$tribs, 
                              Stream_order = yuk_2015_all$stream_order,
                              stream_length = yuk_2015_all$stream_length,
                              year_2015 = yuk_2015_all$rescaled,
                              year_2016 = yuk_2016_all$rescaled,
                              yuk_2021 = yuk_2021_all$rescaled,
                              avg_mean = (yuk_2015_all$rescaled + yuk_2016_all$rescaled +  yuk_2021_all$rescaled)/5)


yuk_across_years <- yuk_across_years %>% #Standard deviation
  rowwise() %>%
  mutate(sd = sd(c(year_2015, year_2016, yuk_2021)))


yuk_across_years <- yuk_across_years %>% #Coefficient of variation 
  rowwise() %>%
  mutate(cv = sd/avg_mean)


###### production estimates 

fishcount2015<- 116000 #cumulative passage estimate
fishcount2016<- 176900 
fishcount2021<- 124874


######## Same as Yuk_across_years but with fish values 

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

fish_across_years <- fish_across_years %>% #SD 
  rowwise() %>%
  mutate(sd = sd(c(year_2015, year_2016, year_2021)))

fish_across_years <- fish_across_years %>% #CV 
  rowwise() %>%
  mutate(cv = sd/avg_mean)

tributary_names<-unique(yuk_across_years$tributary)


###### Sorting by watershed size 

tributary_length<- yuk_across_years %>%
  group_by(tributary) %>%
  summarize(total_length = sum(stream_length))

tributary_length<- tributary_length[order(-tributary_length$total_length),]

tributary_length<- tributary_length[!is.na(tributary_length$tributary),]

Largest_tribs<- tributary_length[1:5,]



######### Calculate CV vs Stream order 

trib_data_list <- list()
by_trib_cv<- data_frame(tributary = NA, 
                        StrOrd = NA, 
                        mean_cv = NA,
                        range_cv = NA,
                        mean_prod = NA, 
                        range_prod = NA) 

###############-----------------------------------------------------------------

### Calculate for 5 largest tribs and plot 
x<-1
for (x in 1:5) {
  
  current_trib <- fish_across_years %>%
    filter(tributary == Largest_tribs$tributary[x])%>% 
    #Remove NA values 
    na.omit()

 
     trib_data <- list()
  
  for (i in 3:7) {

    current_trib_cv_select <- current_trib %>%
      filter(Stream_order <= i)
    
    by_trib_cv <- data.frame(
      Tributary = Largest_tribs$tributary[x],
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
names(trib_data_list) <- Largest_tribs$tributary

combined_data <- bind_rows(unlist(trib_data_list, recursive = FALSE))

plot <- ggplot(combined_data, aes(x = StrOrd, y = mean_cv)) +
geom_point() +
  facet_wrap(~ Tributary) +
  labs(x = "Mean Production", y = "Coefficient of Variation", title = "CV vs Mean Production by Tributary")




plot



############################################################################################################

# Do the same thing, but for the top 10 tributaries by number of fish

Most_productive <- fish_across_years %>%
  group_by(tributary) %>%
  summarize(total_prod = sum(avg_mean)) %>%
  filter(!is.na(tributary)) %>%
  arrange(desc(total_prod)) 
  #head(10)

trib_data_list <- list()

for (x in 1:length(Most_productive$tributary)) {
  
  current_trib <- fish_across_years %>%
    filter(tributary == Most_productive$tributary[x])

  trib_data <- list()
  
  for (i in 3:7) {

    current_trib_cv_select <- current_trib %>%
      filter(Stream_order <= i)
    
    by_trib_cv <- data.frame(
      Tributary = Most_productive$tributary[x],
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

names(trib_data_list) <- Most_productive$tributary

combined_data <- bind_rows(unlist(trib_data_list, recursive = FALSE))

#plot
plot2 <- ggplot(combined_data, aes(x = StrOrd, y = mean_cv)) +
  geom_point() +
  facet_wrap(~ Tributary) +
  labs(x = "Mean Production", y = "Coefficient of Variation", title = "CV vs Mean Production by Tributary")

plot2


########################### Explore most likely tributary 

# Read in the files with only the most likely value as non0 
most_likely_matrix_2015 <- read_csv(here("Data/Production/Yukon/MAX_MATRIX2015_full_Yukon_0.75.csv"))

#read in the shapefile with tributaries 
trib_shape <- read_sf(here("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp"))

#Create a data frame with all tributary names and a count collumn 
Trib_count_2015<- data_frame(tributary = unique(trib_shape$trbtry_), 
                             count = 0)

for (i in 1:ncol(most_likely_matrix_2015)) {
  # Find the non-zero values in each column
  non_zero_values <- most_likely_matrix_2015[, i][most_likely_matrix_2015[, i] != 0]
  
  # Find the corresponding tributary names
  trib_names <- trib_shape$trbtry_[which(most_likely_matrix_2015[, i] != 0)]

  row_index <- which(Trib_count_2015$tributary == trib_names)
  
  Trib_count_2015$count[row_index] <- Trib_count_2015$count[row_index] + 1
  
  Trib_count_2015$year<- 2015 
}

#Repeat this process for 2016 
most_likely_matrix_2016 <- read_csv(here("Data/Production/Yukon/MAX_MATRIX2016_full_Yukon_0.75.csv"))

# remove the first column 
most_likely_matrix_2016 <- most_likely_matrix_2016[,-1]

Trib_count_2016<- data_frame(tributary = unique(trib_shape$trbtry_), 
                             count = 0)

for (i in 1:ncol(most_likely_matrix_2016)) {
  # Find the non-zero values in each column
  non_zero_values <- most_likely_matrix_2016[, i][most_likely_matrix_2016[, i] != 0]
  
  # Find the corresponding tributary names
  trib_names <- trib_shape$trbtry_[which(most_likely_matrix_2016[, i] != 0)]
  
  row_index <- which(Trib_count_2016$tributary == trib_names)
  
  Trib_count_2016$count[row_index] <- Trib_count_2016$count[row_index] + 1
  
  Trib_count_2016$year<- 2016 
}
# repeat for 2019 

most_likely_matrix_2019 <- read_csv(here("Data/Production/Yukon/MAX_MATRIX2019_full_Yukon_0.75.csv"))

# remove the first column
most_likely_matrix_2019 <- most_likely_matrix_2019[,-1]

Trib_count_2019<- data_frame(tributary = unique(trib_shape$trbtry_), 
                             count = 0)

for (i in 1:ncol(most_likely_matrix_2019)) {
  # Find the non-zero values in each column
  non_zero_values <- most_likely_matrix_2019[, i][most_likely_matrix_2019[, i] != 0]
  
  # Find the corresponding tributary names
  trib_names <- trib_shape$trbtry_[which(most_likely_matrix_2019[, i] != 0)]
  
  row_index <- which(Trib_count_2019$tributary == trib_names)
  
  Trib_count_2019$count[row_index] <- Trib_count_2019$count[row_index] + 1
  Trib_count_2019$year <- 2019
}

# Combine all three data frames 
Trib_count_all <- bind_rows(Trib_count_2015, Trib_count_2016, Trib_count_2019)

# Show count by year, separated by tributary 
ggplot(Trib_count_all, aes(x = year, y = count, color = tributary)) +
  geom_line() +
  labs(x = "Year", y = "Count", title = "Count of Most Likely Tributary by Year")



############################################## 


# Explore variation by individual 

Yuk_ind_2015<- read_csv(here("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Production/Yukon/ASSIGN_MATRIX2015_full_Yukon_0.9.csv"))

#remove the first column
Yuk_ind_2015 <- Yuk_ind_2015[,-1]

#extract the first collumn, into a vector of values 
Yuk_ind_2015_1 <- as.vector(Yuk_ind_2015$V11)

#filter out 0 values 
Yuk_ind_2015_1 <- Yuk_ind_2015_1[Yuk_ind_2015_1 != 0]

#histogram 
hist(Yuk_ind_2015_1, breaks = 100, main = "Yukon 2015 Individual Variation", xlab = "Production")
