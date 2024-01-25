library(here)
library(tidyverse)

# This script reads the master CPUE list, adds a year collumn, extracts values by year, and exports them as individual .csvs

CPUE_all <- read_csv(here("Data/To_be_cleaned/CPUE/Yukon CPUE 2010 - 2018.csv"))

# make Date into a date object with format month/day/year
CPUE_all$Date <- as.Date(CPUE_all$Date, format = "%m/%d/%y")

#extract the year value into another column 
CPUE_all$year <- year(CPUE_all$Date)

#Extract by year 
CPUE_2010 <- CPUE_all %>% filter(year == 2010)
CPUE_2015 <- CPUE_all %>% filter(year == 2015)
CPUE_2016 <- CPUE_all %>% filter(year == 2016)
CPUE_2017 <- CPUE_all %>% filter(year == 2017)
CPUE_2018 <- CPUE_all %>% filter(year == 2018)

#Write to a csv file
write_csv(CPUE_2010, here("Data/CPUE/Yukon CPUE 2010.csv"))
write_csv(CPUE_2015, here("Data/CPUE/Yukon CPUE 2015.csv"))
write_csv(CPUE_2016, here("Data/CPUE/Yukon CPUE 2016.csv"))
write_csv(CPUE_2017, here("Data/CPUE/Yukon CPUE 2017.csv"))
write_csv(CPUE_2018, here("Data/CPUE/Yukon CPUE 2018.csv"))

## KUSKO 

CPUE_all<- read_csv(here("Data/To_be_cleaned/CPUE/Kusko CPUE 2017-2018.csv"))

#Extract by year 
CPUE_2017 <- CPUE_all %>% filter(`Sample Year` == 2017)

CPUE_2017<- CPUE_2017 %>%
  rename(Date = `Date Kog.`)

CPUE_2018 <- CPUE_all %>% filter(`Sample Year` == 2018)

#writ to csv
write_csv(CPUE_2017, here("Data/CPUE/Kusko CPUE 2017.csv"))
write_csv(CPUE_2018, here("Data/CPUE/Kusko CPUE 2018.csv"))

