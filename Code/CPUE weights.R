##----- CPUE ----- DO NOT HAVE YET --------------------------------------------------------------
# Create otoCatch, which groups all the values in natal_values by the 
# day of the year that they were caught, summarizes the number of 
# fish caught on that day of the year 
library(lubridate)
library(tidyverse)
library(here)

### Data 

#### Yukon ######## 

## 2019 Yukon 

natal_values<- read_csv(here("data/natal data/2019 Yukon_natal_data.csv"))
metadata <- read_csv(here("data/metadata/2019 Yukon metadata.csv"))
CPUE <- read_csv(here("data/CPUE/2019 Yukon CPUE.csv"))
identifier<- "2019 Yukon"

## 2021 Yukon

natal_values<- read_csv(here("data/natal data/2021 Yukon_natal_data.csv"))
metadata <- read_csv(here("data/metadata/2021 Yukon metadata.csv"))
CPUE <- read_csv(here("data/CPUE/2021 Yukon CPUE.csv"))
identifier<- "2021 Yukon"

############################

#### Kusko ########

## 2019 Kusko 
natal_values<- read_csv(here("data/natal data/2019 Kusko_natal_data.csv"))
metadata <- read_csv(here("data/CPUE/2019 Kusko CPUE.csv"))
CPUE <- read_csv(here("data/CPUE/2019 Kusko CPUE.csv"))
identifier<- "2019 Kusko"

## 2020 Kusko 
natal_values<- read_csv(here("data/natal data/2020 Kusko_natal_data.csv"))
metadata <- NA
CPUE <- read_csv(here("data/CPUE/2020 Kusko CPUE.csv"))
identifier<- "2020 Kusko"

## 2021 Kusko 
natal_values<- read_csv(here("data/natal data/2021 Kusko_natal_data.csv"))
metadata <- read_csv(here("data/metadata/2021 Yukon metadata.csv"))
CPUE <- read_csv(here("data/CPUE/2021 Kusko CPUE.csv"))
identifier<- "2021 Kusko"

# ------------------------------------------------------------------------------
## Summarizes the # of fish caught on that day of the year.
otoCatch <- natal_values %>%
  rename(DOY = capture_date_julian)%>%
  group_by(DOY) %>%
  summarise(Catch = length(fish.id))

# Create CPUE, which is the daily catch divided by the sum of the daily catch
CPUE$`Date Kog.`<- as.Date(CPUE$`Date Kog.`, format = "%m/%d")
CPUE$`Date Kog.` <- sub("^.{4}", CPUE$`Sample Year`, CPUE$`Date Kog.`)

CPUE <- CPUE %>%
  rename(Date = `Date Kog.`) %>%
  filter(!is.na(CPUE)) %>%
  filter(CPUE > 0)
  
CPUE <- CPUE %>%
  mutate(DOY = format(as.POSIXct(Date), '%j'))%>%
  mutate(dailyPROP = CPUE / sum(CPUE), # How much proportionally is caught ON that day 
         cumPROP = cumsum(dailyPROP)) # cumulative proportion (how much of the total catch has been caught UP TO that day)

#Create an otocatch collumn in CPUE
CPUE$otoCATCH <- 0

# Match DOY values between otoCatch and CPUE data frames
matching_indices <- match(otoCatch$DOY, CPUE$DOY) %>% na.omit()

# Update the otoCATCH column in CPUE where matches are found
CPUE$otoCATCH[matching_indices] <- otoCatch$Catch


CPUE <- CPUE %>%
 filter(otoCATCH > 0) %>%
 mutate(otoPROP = otoCATCH / sum(otoCATCH),
 oto_cumPROP = cumsum(otoPROP),        
 COratio = dailyPROP / otoPROP)        


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

# Normalize the strata weight
strat.wt <- strat.wt / sum(strat.wt)  

# Calculate catch by strata variable
CatchByStrat <- CPUE %>%
  mutate(Strat = strat) %>%
  group_by(Strat) %>%
  summarise(Nfish = sum(otoCATCH))

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



