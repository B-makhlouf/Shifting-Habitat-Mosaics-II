library(here)
library(dplyr)
library(readr)


Yukon2010<- read_csv(here("Data/To_be_cleaned/Natal Origin/2010 Yukon_natal_data.csv"))

#Take the value in the column file.id, and remove _yukon_chinook_
Yukon2010$fish.id <- gsub("_yukon_chin_", "", Yukon2010$file.id)

#Add a space to file.id in the 4th and 7th position
Yukon2010$fish.id <- gsub("yk", "_yk_", Yukon2010$fish.id)
