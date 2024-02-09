#load in the isoscape 
library(sf)
library(here)
library(classInt)
library(RColorBrewer)
library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)


year<- 2017 
sensitivity_threshold<- .7 


##Kusko 2017 
natal_origins<- read.csv(here("Data/Natal Origin/2017_Kusko_natal_data.csv"))
CPUE<- read.csv(here("Data/CPUE/CPUE_weights/2017_Kusko_CPUE weights.csv")) 
CPUE<- CPUE %>% unlist() %>% as.numeric()
identifier<- "2017 Kusko"

#Kusko 2018 
natal_values<- read.csv(here("Data/Natal Origin/2017_Kusko_natal_data.csv"))
CPUE_weights<- read.csv(here("Data/CPUE/CPUE_weights/2017_Kusko_CPUE weights.csv")) 
identifier<- "2017 Kusko"

#Kusko 2019 
natal_values<- read.csv(here("Data/Natal Origin/2019_Kusko_natal_data.csv"))
CPUE_weights<- read.csv(here("Data/CPUE/CPUE_weights/2019_Kusko_CPUE weights.csv"))
identifier<- "2019 Kusko"

#Kusko 2020 
natal_values<- read.csv(here("Data/Natal Origin/2020_Kusko_natal_data.csv"))
CPUE_weights<- read.csv(here("Data/CPUE/CPUE_weights/2020_Kusko_CPUE weights.csv"))
identifier<- "2020 Kusko"

#Kusko 2021 
natal_values<- read.csv(here("Data/Natal Origin/2021_Kusko_natal_data.csv"))
CPUE_weights<- read.csv(here("Data/CPUE/CPUE_weights/2021_Kusko_CPUE weights.csv"))
identifier<- "2021 Kusko"





#Isoscape and Basin Shapefile 
kusk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")

########################################################
#### Function to produce maps and production data 
########################################################

Kusko_map <- function(year, sensitivity_threshold) {  
  
  identifier <- paste(year, "Kusko", sep = "_")

  # Read data based on the year
  natal_origins <- read.csv(paste("Data/Natal Origin/", year, "_Kusko_natal_data.csv", sep = ""))
  CPUE <- read.csv(here("Data/CPUE/CPUE_weights/", paste(year, "_Kusko_CPUE weights.csv", sep = "")), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()

  ## ----- Extract isoscape prediction + error values -----------------------------
  
  pid_iso <- kusk_edges$iso_pred # Sr8786 value
  pid_isose <- kusk_edges$isose_pred # Error
  pid_prior <- kusk_edges$UniPh2oNoE#Habitat prior ( RCA slope)

  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose^2 + within_site^2 + analyt^2)  # COMBINED error 
  
  ###----- CREATE EMPTY MATRICES -------------------------------------------------
  output_matrix <- matrix(NA, nrow = length(kusk_edges$iso_pred), ncol = nrow(natal_origins))
  l<-length(natal_origins[, 1])
  f.strata.vec <- rep(NA,l)
  assignment_matrix <- matrix(NA,nrow=length(pid_iso),ncol=l)
  
  #############################
  ###### ASSIGNMENTS HERE ##### 
  #############################
  ## loop for assingments
  i<-1
  for (i in 1:length(natal_origins[, 1])) {
    
    iso_o <- natal_origins[i, "natal_iso_mean"] %>% as.numeric()  # Otolith ratio
    StreamOrderPrior <- as.numeric(kusk_edges$Strahler > 2)
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * StreamOrderPrior
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    
    assign_norm <- assign_norm * CPUE[i] # multiply times the CPUE
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
    
    
    percentile <- quantile(assign_rescaled[!is.na(assign_rescaled) & assign_rescaled != 0], probs = sensitivity_threshold)
    
    # Apply the operation only to non-zero and non-NA values
    assign_rescale_removed <- ifelse(!is.na(assign_rescaled) & assign_rescaled != 0 & assign_rescaled >= percentile, 1, 0)
    
    assignment_matrix[,i] <- assign_rescaled
  }
  
  ###------- BASIN SCALE VALUES ----------------------------------------
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
  basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
  basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 
  
  # Create a data frame, with one for each of the assignment types 
  
  basin_df <- data.frame(normalized = basin_assign_norm, 
                         rescaled = basin_assign_rescale, 
                         raw = basin_assign_sum
  )
  
  filename<- paste0(identifier, "_", sensitivity_threshold, "_basin_norm.csv")
  filepath<- file.path(here("Data", "Production", "Yukon", filename))
  write.csv(basin_df, filepath)
  
  
  ################################################################################
  ##### Mapping using base R 
  ################################################################################
  
  # Save as PDF
  breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9)
  #breaks <- c(0, .4, .6, .7, .8, .9, 1)
  nclr <- length(breaks)
  filename <- paste0(identifier, "_", sensitivity_threshold, "_.pdf")
  filepath <- file.path(here("Figures", "Maps", "Kusko", filename))
  pdf(file = filepath, width = 9, height = 6)
  plotvar <- basin_assign_norm
  class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
  plotclr <- brewer.pal(nclr, "YlOrRd")
  colcode <- findColours(class, plotclr, digits = 2)
  colcode[plotvar == 0] <- 'gray80'
  colcode[which(StreamOrderPrior == 0)] <- 'gray68'
  colcode[which(pid_prior == 0)] <- 'gray60'
  plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
  plot(st_geometry(kusk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .7 * (exp(plotvar) - 1)))
  dev.off()
  
  
  ################################################################################
  ## Write out a time enabled shapefile
  ################################################################################
  
  only_edges<- st_geometry(kusk_edges)
  only_edges<- st_as_sf(only_edges)
  
  #add production data
  only_edges$production <- basin_assign_norm
  
  #add year 
  only_edges$year <- year
  
  #Export shapefile 
  filename<- paste0(identifier, "_", ".shp")
  filepath<- file.path(here("/Users/benjaminmakhlouf/Desktop/Animation_Shapefiles", filename))
  
  st_write(only_edges, filepath)
}


Kusko_map(2017,.7)
Kusko_map(2018,.7)
Kusko_map(2019,.7)
Kusko_map(2020,.7)
Kusko_map(2021,.7)

