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










##### 
## 2016 Yukon 
#### 

Yukon2016 <- read_csv(here("Data/To_be_cleaned/Natal Origin/2016 Yukon_natal data.csv"))

#Take the value in the column file.id, and remove _yukon_chinook_
Yukon2016$fish.id <- gsub("_yukon_chin_", "", Yukon2016$file.id)

#Add a space to file.id in the 4th and 7th position
Yukon2016$fish.id <- gsub("yk", "_yk_", Yukon2016$fish.id)

# change the capture date to Julian date
Yukon2016$capture_date <- as.Date(Yukon2016$capture_date, format = "%m/%d/%y")

# Convert capture_date to day of the year (Julian day)
Yukon2016$capture_date_julian<- yday(as.Date(Yukon2016$capture_date, format = "%m/%d/%y"))

#Write to a csv file
write_csv(Yukon2016, here("Data/Natal Origin/2016 Yukon_natal_data.csv"))







#######
# 2017 Yukon 
#########

Yukon2017 <- read_csv(here("Data/To_be_cleaned/Natal Origin/2017 Yukon_natal data.csv"))
Yukon2017_extra<- read_csv(here("Data/To_be_cleaned/Natal Origin/2017 Yukon extra data.csv"), skip = 1)

#bring in the capture date from Yukon2017_extra into Yukon2017 by matching the indices of matching values in otonum and otolith #

Yukon2017$capture_date <- Yukon2017_extra$sampleDate[match(Yukon2017$otoNum, Yukon2017_extra$otoNum)]

#Take the value in the column file.id, and remove _yukon_chinook_
Yukon2017$fish.id <- gsub("_yukon_king_", "", Yukon2017$file.id)

#Add a space to file.id in the 4th and 7th position
Yukon2017$fish.id <- gsub("yk", "_yk_", Yukon2017$fish.id)

#remove .exp
Yukon2017$fish.id <- gsub(".exp", "", Yukon2017$fish.id)

# Convert capture_date to day of the year (Julian day)
Yukon2017$capture_date_julian<- yday(as.Date(Yukon2017$capture_date, format = "%m/%d/%y"))

#Write to a csv file
write_csv(Yukon2017, here("Data/Natal Origin/2017 Yukon_natal_data.csv"))




##############
### 2018 Yukon 
##############

Yukon2018 <- read_csv(here("Data/To_be_cleaned/Natal Origin/2018 Yukon_natal_data.csv"))
Yukon2018_extra <- read_csv(here("Data/To_be_cleaned/Natal Origin/2018 Yukon extra data.csv"))

#bring in the capture date from Yukon2018_extra into Yukon2018 by matching the indices of matching values in otonum and otolith #
Yukon2018$capture_date <- Yukon2018_extra$sampleDate[match(Yukon2018$otoNum, Yukon2018_extra$otoNum)]

#subsitute - for _ in fish.id 
Yukon2018$fish.id <- gsub("-", "_", Yukon2018$fish.id)

#Convert capture_date to day of the year (Julian day)
Yukon2018$capture_date_julian<- yday(as.Date(Yukon2018$capture_date, format = "%m/%d/%y"))

#write to csv
write_csv(Yukon2018, here("Data/Natal Origin/2018 Yukon_natal_data.csv"))



############### KUSKO


##### 2017 Kusko 
Kusko2017 <- read_csv(here("Data/To_be_cleaned/Natal Origin/2017 Kusko_natal_data.csv"))
Kusko2017_extra <- read_csv(here("Data/To_be_cleaned/Natal Origin/2017 Kusko_extra_data(1).csv"))

#Fix fish id 
Kusko2017$fish.id <- gsub("_kusko_king_", "", Kusko2017$fish.id)
Kusko2017$fish.id <- gsub("kk", "_kk_", Kusko2017$fish.id)
Kusko2017$fish.id <- gsub("redo", "", Kusko2017$fish.id)
Kusko2017$fish.id <- gsub("a", "", Kusko2017$fish.id)

#populate otoNUM with the last three digits of fish.id
Kusko2017$otoNum <- substr(Kusko2017$fish.id, 9, 11) %>% as.numeric()

#populate capture date by pulling the sample date in which UWID and otoNUM match
Kusko2017$capture_date <- Kusko2017_extra$Sample_Date[match(Kusko2017$otoNum, Kusko2017_extra$UW_ID)]

#Convert Capture date to Julian date
Kusko2017$capture_date_julian<- yday(as.Date(Kusko2017$capture_date, format = "%m/%d/%y"))

#write to csv
write_csv(Kusko2017, here("Data/Natal Origin/2017 Kusko_natal_data.csv"))






##### 2018 Kusko

Kusko2018 <- read_csv(here("Data/To_be_cleaned/Natal Origin/2018 Kusko_natal_data.csv"))
Kusko2018_extra <- read_csv(here("Data/To_be_cleaned/Natal Origin/2018 Kusko_extra_data.csv"))

#Fish fish id
Kusko2018$fish.id <- gsub("-", "_", Kusko2018$fish.id)
Kusko2018$fish.id <- gsub("_redo", "", Kusko2018$fish.id)

#populate otoNUM with the last three digits of fish.id
Kusko2018$otoNum <- substr(Kusko2018$fish.id, 9, 11) %>% as.numeric()

#populate capture date by pulling the sample date in which UWID and otoNUM match
Kusko2018$capture_date <- Kusko2018_extra$date[match(Kusko2018$otoNum, Kusko2018_extra$UW_OtoID)]

#Convert Capture date to Julian date
Kusko2018$capture_date_julian<- yday(as.Date(Kusko2018$capture_date, format = "%m/%d/%y"))

#write to csv
write_csv(Kusko2018, here("Data/Natal Origin/2018 Kusko_natal_data.csv"))
