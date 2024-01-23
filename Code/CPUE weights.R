##----- CPUE ----- 


# Create otoCatch, which groups all the values in natal_values by the 
# day of the year that they were caught, summarizes the number of 
# fish caught on that day of the year 
library(lubridate)
library(tidyverse)
library(here)

### Data 

#### Yukon ######## 

#2010 Yukon 
natal_values<- read_csv(here("Data/Natal Origin/2010 Yukon_natal_data.csv"))
CPUE <- read_csv(here("Data/CPUE/Yukon CPUE 2010.csv"))
identifier <- "2010 Yukon"

# ------------------------------------------------------------------------------
## Summarizes the # of fish caught on that day of the year.

otoCatch <- natal_values %>%
  rename(DOY = capture_date_julian)%>%
  group_by(DOY) %>%
  summarise(Catch = length(fish.id))

CPUE <- CPUE %>%
  mutate(DOY = format(as.POSIXct(Date), '%j')) %>%#create DOY collumn which makes date into day of year
  mutate(dailyPROP = dailyCPUE/sum(dailyCPUE)) #calculate the daily proportion of the total catch

CPUE$otoCATCH <- 0 #Create an otocatch collumn in CPUE

matching_indices <- match(otoCatch$DOY, CPUE$DOY) %>% na.omit() # Match DOY values between otoCatch and CPUE data frames

CPUE$otoCATCH[matching_indices] <- otoCatch$Catch # Update the otoCATCH column in CPUE where matches are found

CPUE <- CPUE %>%
 filter(otoCATCH > 0) %>% #keep only those with at least 1 fish caught
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



