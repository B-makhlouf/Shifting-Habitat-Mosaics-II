library(here)
library(dplyr)

### This script breaks up the master genetics for 2015-2017 into each year. 

genetics_combined<- read_csv(here("Data/To_be_cleaned/Genetics/Yukon_Genetics_2015_16_17.csv"))

genetics_combined <- genetics_combined %>%
  rename( year = `mixture_collection`)

gen2015<- genetics_combined %>% filter(year == 2015)
gen2016<- genetics_combined %>% filter(year == 2016)
gen2017<- genetics_combined %>% filter(year == 2017)

write_csv(gen2015, here("Data/Genetics/Yukon_Genetics_2015.csv"))
write_csv(gen2016, here("Data/Genetics/Yukon_Genetics_2016.csv"))
write_csv(gen2017, here("Data/Genetics/Yukon_Genetics_2017.csv"))
