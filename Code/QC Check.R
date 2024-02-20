library(tidyverse)
library(here)

#### Check to make sure we're not losing fish 

#2015 full Yukon 

full_2015<- read_csv(here("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Production/Yukon/ASSIGN_MATRIX2015_full_Yukon_0.75.csv"))

#Sum each collumn 
full_2015_sum<- full_2015 %>% 
  summarise_all(sum)

#Check if there are any 0 values with a binary 
full_2015_sum %>% 
  mutate_all(~ifelse(.==0, "No", "Yes"))

#2016 full Yukon

full_2016<- read_csv(here("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Production/Yukon/ASSIGN_MATRIX2016_full_Yukon_0.75.csv"))

#Sum each collumn
full_2016_sum<- full_2016 %>% 
  summarise_all(sum)

#2017 
full_2017<- read_csv(here("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Production/Yukon/ASSIGN_MATRIX2017_full_Yukon_0.75.csv"))

#Sum each collumn
full_2017_sum<- full_2017 %>% 
  summarise_all(sum)


