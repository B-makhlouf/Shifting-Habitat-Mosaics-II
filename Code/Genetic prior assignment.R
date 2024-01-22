### Genetics cleaning script 
library(tidyverse)
library(here)

## 2019 YUKON
genetics<- read_csv(here("data/genetics data/2019 Yukon chinook genetics.csv"))
identifier<- " 2019 Yukon"
natal_values<- read_csv(here("data/natal data/2019 Yukon_natal_data.csv"))


##### 2021 YUKON! 
genetics<- read_csv(here("data/genetics data/2021 Yukon chinook genetics.csv"))
identifier<- "2021 Yukon"
natal_values<- read_csv(here("data/natal data/2021 Yukon_natal_data.csv"))


genetics <- genetics %>% 
  drop_na(otolithNum) %>%
  drop_na(`Assignment Probability`)%>%
  drop_na(`Genetic Assignment`)


natal_values$otonum <- substr(natal_values$fish.id, 9, nchar(natal_values$fish.id))

natal_values$otonum <- as.character(natal_values$otonum)
natal_values$otonum <- substr(natal_values$otonum, 1, 3)
natal_values$otonum <- as.numeric(natal_values$otonum)

#genetic_assignments <- data_frame(
#  LowerUS = rep(NA, nrow(natal_values)),
#  Kuyukuk = rep(NA, nrow(natal_values)),
#  Tanana = rep(NA, nrow(natal_values)),
#  UpperUS = rep(NA, nrow(natal_values)),
#  Canada = rep(NA, nrow(natal_values))
#)

genetic_assignments <- data_frame(
  Lower = rep(NA, nrow(natal_values)),
  Middle = rep(NA, nrow(natal_values)),
  Upper = rep(NA, nrow(natal_values))
)

#---------------- 

for (i in 1:nrow(natal_values)) {
  index <- which(genetics$otolithNum == natal_values$otonum[i])
  
  if (length(index) > 0) {
    if (genetics$`Genetic Assignment`[index] == "LowerUS") {
      genetic_assignments$Lower[i] <- genetics$`Assignment Probability`[index]
    } else if (genetics$`Genetic Assignment`[index] == "Kuyukuk") {
      genetic_assignments$Middle[i] <- genetics$`Assignment Probability`[index]
    } else if (genetics$`Genetic Assignment`[index] == "Tanana") {
      genetic_assignments$Middle[i] <- genetics$`Assignment Probability`[index]
    } else if (genetics$`Genetic Assignment`[index] == "UpperUS") {
      genetic_assignments$Middle[i] <- genetics$`Assignment Probability`[index]
    } else if (genetics$`Genetic Assignment`[index] == "Canada") {
      genetic_assignments$Upper[i] <- genetics$`Assignment Probability`[index]
    } else if (is.na(genetics$`Genetic Assignment`[index])) {
      genetic_assignments$Lower[i] <- 0.2
      genetic_assignments$Middle[i] <- 0.2
      genetic_assignments$Upper[i] <- 0.2
    }
  } else {
    genetic_assignments$Lower[i] <- 0.2
    genetic_assignments$Middle[i] <- 0.2
    genetic_assignments$Upper[i] <- 0.2
  }
}

genetic_assignments$Lower <- as.numeric(genetic_assignments$Lower)
genetic_assignments$Middle <- as.numeric(genetic_assignments$Middle)
genetic_assignments$Upper <- as.numeric(genetic_assignments$Upper)

#--------------
row_sums <- rowSums(genetic_assignments, na.rm = TRUE)
dif<- 1-row_sums
dif_each<- dif/2

for (i in 1:nrow(genetic_assignments)) {
  if (all(is.na(genetic_assignments[i,]))) {
    # Replace the entire row with 0.2 in each column
    genetic_assignments[i, ] <- 1/3
  } else {
    # Fill NA values with dif_each[i]
    na_indices <- which(is.na(genetic_assignments[i, ]))
    genetic_assignments[i, na_indices] <- dif_each[i]
  }
}

genetic_assignments$fish.id<- natal_values$fish.id

#genetic_assignments <- genetic_assignments %>%
#  rowwise() %>%
#  mutate(Middle = sum(Kuyukuk, Tanana, UpperUS)) %>%
#  rename(Lower = LowerUS, Upper = Canada) %>%
#  select(fish.id, everything(), -Kuyukuk, -Tanana, -UpperUS)

filename<- paste0(identifier, "_genetics.csv")
filepath<- file.path(here("data","genetics data","Genetic_assign", filename))
write_csv(genetic_assignments, filepath)

