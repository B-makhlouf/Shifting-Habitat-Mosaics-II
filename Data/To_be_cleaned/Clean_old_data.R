library(here)
library(dplyr)
library(readr)
library(lubridate)

#############
###2010 Yukon 
#############


Yukon2010<- read_csv(here("Data/Natal Origin/2010 Yukon_natal_data.csv"))

#Take the value in the column file.id, and remove _yukon_chinook_
Yukon2010$fish.id <- gsub("_yukon_chin_", "", Yukon2010$file.id)

#Add a space to file.id in the 4th and 7th position
Yukon2010$fish.id <- gsub("yk", "_yk_", Yukon2010$fish.id)

# change the capture date to Julian date
Yukon2010$capture_date <- as.Date(Yukon2010$capture_date, format = "%m/%d/%y")

# Convert capture_date to day of the year (Julian day)
Yukon2010$capture_date_julian<- yday(as.Date(Yukon2010$capture_date, format = "%m/%d/%y"))

#Write to a csv file
write_csv(Yukon2010, here("Data/Natal Origin/2010 Yukon_natal_data.csv"))


#### 
#### 2015 Yukon 
#### 

Yukon2015<- read_csv(here("Data/To_be_cleaned/Natal Origin/2015 Yukon_natal data.csv"))

#Take the value in the column file.id, and remove _yukon_chinook_
Yukon2015$fish.id <- gsub("_yokon_chin_", "", Yukon2015$file.id)

#Add a space to file.id in the 4th and 7th position
Yukon2015$fish.id <- gsub("yk", "_yk_", Yukon2015$fish.id)

# change the capture date to Julian date
Yukon2010$capture_date <- as.Date(Yukon2010$capture_date, format = "%m/%d/%y")

#remove .exp 
Yukon2015$fish.id <- gsub(".exp", "", Yukon2015$fish.id)

# Convert capture_date to day of the year (Julian day)
Yukon2015$capture_date_julian<- yday(as.Date(Yukon2015$capture_date, format = "%m/%d/%y"))

#Write to a csv file
write_csv(Yukon2015, here("Data/Natal Origin/2015 Yukon_natal_data.csv"))

