##----- CPUE ----- 

# This script takes in CPUE values and calculates weights for each fish in natal 
# origin based on the day of the year that they were caught and the proportional CPUE for that time strata. '

#---------------- 

library(lubridate)
library(tidyverse)
library(here)
library(lubridate)

### Data 

#### Yukon ######## 

#2010 Yukon 
natal_values<- read_csv(here("Data/Natal Origin/2010 Yukon_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Yukon CPUE 2010.csv"))
identifier <- "2010_Yukon"

#2015 Yukon 
natal_values<- read_csv(here("Data/Natal Origin/2015_Yukon_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Yukon CPUE 2015.csv"))
identifier <- "2015_Yukon"

#2016 Yukon
natal_values<- read_csv(here("Data/Natal Origin/2016_Yukon_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Yukon CPUE 2016.csv"))
identifier <- "2016_Yukon"

#2017 Yukon 
natal_values<- read_csv(here("Data/Natal Origin/2017_Yukon_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Yukon CPUE 2017.csv"))
identifier <- "2017_Yukon"

#2018 Yukon 
natal_values<- read_csv(here("Data/Natal Origin/2018_Yukon_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Yukon CPUE 2018.csv"))
identifier <- "2018_Yukon"

#2017 Kusko
natal_values<- read_csv(here("Data/Natal Origin/2017_Kusko_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Kusko CPUE 2017.csv"))
identifier <- "2017_Kusko"

#2018 Kusko 
natal_values<- read_csv(here("Data/Natal Origin/2018_Kusko_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Kusko CPUE 2018.csv"))
identifier <- "2018_Kusko"

#2019 Kusko
natal_values<- read_csv(here("Data/Natal Origin/2019_Kusko_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Kusko CPUE 2019.csv"))
identifier <- "2019_Kusko"

#2020 Kusko
natal_values<- read_csv(here("Data/Natal Origin/2020_Kusko_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Kusko CPUE 2020.csv"))
identifier <- "2020_Kusko"

#2021 Kusko 
natal_values<- read_csv(here("Data/Natal Origin/2021_Kusko_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Kusko CPUE 2021.csv"))
identifier <- "2021_Kusko"

# ------------------------------------------------------------------------------
## Summarizes the # of fish caught on that day of the year.
  

CPUE<- CPUE %>%
  #change the date format to the proper format
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  rename( dailyCPUE = CPUE)

otoCatch <- natal_values %>%
  rename(DOY = capture_date_julian)%>%
  group_by(DOY) %>%
  summarise(Catch = length(fish.id))

CPUE <- CPUE %>%
  mutate(DOY = format(as.POSIXct(Date), '%j')) %>% #create DOY collumn which makes date into day of year
  mutate(dailyPROP = dailyCPUE/sum(dailyCPUE)) #calculate the daily proportion of the total catch

CPUE$otoCATCH <- 0 #Create an otocatch collumn in CPUE

matching_indices <- match(otoCatch$DOY, CPUE$DOY) %>% na.omit() # Match DOY values between otoCatch and CPUE data frames

CPUE$otoCATCH[matching_indices] <- otoCatch$Catch # Update the otoCATCH column in CPUE where matches are found

CPUE <- CPUE %>%
 #filter(otoCATCH > 0) %>% #keep only those with at least 1 fish caught
 mutate(otoPROP = otoCATCH / sum(otoCATCH), #calculate the otolith proportion
 oto_cumPROP = cumsum(otoPROP), #calculate the cumulative proportion    
 COratio = dailyPROP / otoPROP) #calculate the ratio of daily catch to otolith catch    

#------ Creating strata for CPUE weighting -------------------------------------

NdaysPerStrat <- 5 #how is this decided? 
strat <- rep(NA, nrow(CPUE)) 
strat_index <- rep(1:(nrow(CPUE)/NdaysPerStrat), each = NdaysPerStrat)
strat[is.na(strat)] <- strat_index[is.na(strat)]
strat[is.na(strat)] <- max(strat, na.rm = TRUE)  # Assign any stragglers to the last stratum

#Empty vector for strata wieghts 
strat.wt <- rep(NA, length(unique(strat)))

# Calculate strata weights
for (i in 1:length(strat.wt)) {
  CPUEmean <- mean(CPUE$dailyPROP[which(strat == i)])  # Mean dailyPROP for the stratum
  otomean <- mean(CPUE$otoPROP[which(strat == i)])  # Mean otoPROP for the stratum
  strat.wt[i] <- CPUEmean / otomean  # Calculate stratum weight
}

#make any NA values 0 
strat.wt[is.na(strat.wt)] <- 0

# Normalize the strata weight
strat.wt <- strat.wt / sum(strat.wt)  

# Calculate catch by strata variable
CatchByStrat <- CPUE %>%
  mutate(Strat = strat) %>% #create a strate collumn
  group_by(Strat) %>% #group by strata
  summarise(Nfish = sum(otoCATCH)) #summarize the number of fish caught in each strata

weighting_vector <- rep(NA, nrow(natal_values))

for (i in 1:nrow(natal_values)) {
  DOY<-natal_values$capture_date_julian[i]
  f.strata <- as.numeric(strat[which(CPUE$DOY == DOY)])
  ind_strata_weight<- strat.wt[f.strata]
  
  if (length(ind_strata_weight) > 0) {
    weighting_vector[i] <- ind_strata_weight
  } else {
    weighting_vector[i] <- min(strat.wt, na.rm = TRUE)
  }
}

weighting_df<- as.data.frame(weighting_vector)

##### Exported 
filename<- paste0(identifier, "_CPUE weights.csv")
filepath<- file.path(here("data", "CPUE", "CPUE_weights", filename))
write_csv(weighting_df, filepath)

# Export CPUE as a .csv
filename<- paste0(identifier, "_expanded_CPUE.csv")
filepath<- file.path(here("data", "CPUE", filename))
write_csv(CPUE, filepath)

## Read in all the expanded CPUE files and put them all in one dataframe 
# Add "year" collumn to each dataframe

#2015
exp_CPUE_2015<- read_csv(here("Data/CPUE/2015_Yukon_expanded_CPUE.csv"))
exp_CPUE_2015$year<- 2015

#2016
exp_CPUE_2016<- read_csv(here("Data/CPUE/2016_Yukon_expanded_CPUE.csv"))
exp_CPUE_2016$year<- 2016

#2017
exp_CPUE_2017<- read_csv(here("Data/CPUE/2017_Yukon_expanded_CPUE.csv"))
exp_CPUE_2017$year<- 2017

#2018
exp_CPUE_2018<- read_csv(here("Data/CPUE/2018_Yukon_expanded_CPUE.csv"))
exp_CPUE_2018$year<- 2018

#2019 Yukon
exp_CPUE_2019<- read_csv(here("Data/CPUE/2019_Yukon_expanded_CPUE.csv"))
exp_CPUE_2019$year<- 2019
#make sure data is a date, form month day year
exp_CPUE_2019$Date<- as.Date(exp_CPUE_2019$Date, format = "%m/%d/%Y")

#2021 Yukon
exp_CPUE_2021<- read_csv(here("Data/CPUE/2021_Yukon_expanded_CPUE.csv"))
exp_CPUE_2021$year<- 2021
#make sure data is a date, form year month day 
exp_CPUE_2021$Date<- as.Date(exp_CPUE_2021$Date, format = "%m/%d/%Y")

#Combine all the CPUE dataframes into one
all_exp_CPUE<- rbind(exp_CPUE_2015, exp_CPUE_2016, exp_CPUE_2017, exp_CPUE_2018, exp_CPUE_2019, exp_CPUE_2021)


#Export the combined CPUE dataframe
filename<- "Yukon_all_exp_CPUE.csv"
filepath<- file.path(here("data", "CPUE", filename))
write_csv(all_exp_CPUE, filepath)


all_exp_CPUE$Date<- as.Date(all_exp_CPUE$Date, format = "%Y-%m-%d") #convert date to date format

#plot a line chart of CPUE by date, with a separate line for each year

ggplot(all_exp_CPUE, aes(x = DOY, y = dailyCPUE, color = as.factor(year))) + 
  geom_line() +
  labs(title = "CPUE by Year", x = "Day of Year", y = "CPUE")

CPUE_otocatch<- ggplot(all_exp_CPUE, aes(x = DOY, y = dailyCPUE, color = as.factor(year))) + 
  geom_line(size = 2) +
  geom_line(aes(y = (oto_cumPROP * 100)), size = 1, color = "black")+
  labs(title = "CPUE by Year", x = "Day of Year", y = "CPUE") +
  facet_wrap(~ year, nrow = 2, ncol = 3)

ggsave(filename = "CPUE_otocatch_Kusko.pdf", plot = CPUE_otocatch, path = here("Figures","Graphs"), dpi = 300)


## Do the same process with Kusko 

#Read in the Kusko CPUE data

exp_CPUE_2017<- read_csv(here("Data/CPUE/2017_Kusko_expanded_CPUE.csv"))
exp_CPUE_2017$year<- 2017
#remove cumulative CPUE 
exp_CPUE_2017<- exp_CPUE_2017 %>% select(- `Cumulative CPUE`)

exp_CPUE_2018<- read_csv(here("Data/CPUE/2018_Kusko_expanded_CPUE.csv"))
exp_CPUE_2018$year<- 2018
#remove cumulative CPUE
exp_CPUE_2018<- exp_CPUE_2018 %>% select(- `Cumulative CPUE`)

exp_CPUE_2019<- read_csv(here("Data/CPUE/2019_Kusko_expanded_CPUE.csv"))
exp_CPUE_2019$year<- 2019

exp_CPUE_2020<- read_csv(here("Data/CPUE/2020_Kusko_expanded_CPUE.csv"))
exp_CPUE_2020$year<- 2020

exp_CPUE_2021<- read_csv(here("Data/CPUE/2021_Kusko_expanded_CPUE.csv"))
exp_CPUE_2021$year<- 2021

all_exp_CPUE<- rbind(exp_CPUE_2017, exp_CPUE_2018, exp_CPUE_2019, exp_CPUE_2020, exp_CPUE_2021)

