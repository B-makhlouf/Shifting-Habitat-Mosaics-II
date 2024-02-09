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

##Kusko 2017 
natal_values<- read.csv(here("Data/Natal Origin/2017_Kusko_natal_data.csv"))
CPUE_weights<- read.csv(here("Data/CPUE/CPUE_weights/2017_Kusko_CPUE weights.csv")) 
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
isoscape<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")

# Extract the predicted isoscape Sr8786 values for each tributary
isoscape_sr<- isoscape$iso_pred 

# Extract the predicted isoscape Sr8786 error values for each tributary
isoscape_sr_err<- isoscape$isose_pred 

# Bump up the lowest error values so that assignments arent artificially inflated in these areas
isoscape_sr_err<- ifelse(isoscape_sr_err < 0.0001, 0.0001, isoscape_sr_err)

### Natal_values now contains relevant data from natal origin extraction as well 
# as metadata file. 

###-----------------------------------------------------------------------------

#Create an empty matrix which will hold all of the assignment values
#Each column corresponds to an individual, 
#Each row corresponds to a tributary in the shape file 

assignment_matrix<- matrix(NA,nrow= length(isoscape_sr), ncol= nrow(natal_values))

###----- Variance Generating Processes ------------------------------------------

within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
within_pop <- within_site - analyt # Population error
error <- sqrt(isoscape_sr_err^2 + within_site^2 + analyt^2)  # COMBINED error 

###----- Priors ----------------------------------------------------------------

stream_order_pr <- as.numeric(isoscape$Strahler > 2 ) #Stream order > 2 
habitat_prior<- isoscape$UniPh2oNoE #Habitat prior, see README for details

###---- CPUE -------------------------------------------------------------------

CPUE<- unlist(as.vector(CPUE_weights))
CPUE<- as.numeric(CPUE)

###---- Assignment loop --------------------------------------------------------

## Beginning of assignment loop 
# This loop goes through, for each fish, each tributary and assigns a probability
# of natal origin based on the Bayes rule equation and priors 

samplenumbs<-length(natal_values[,1])

for (i in 1:samplenumbs){
  ###---- Isotope values ---------------------------------------------------------
  iso_o<- natal_values$natal_iso_mean[i] #measured isotope value from the otolith 
  
  ####### ------------------------------------------------------------------------
  ###### BAYES RULE ASSIGNMENT CODE #############
  ######  ------------------------------------------------------------------------
  
  ## Line below is Bayes Rule 
  assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o- isoscape_sr)^2/(2*error^2))) * habitat_prior * stream_order_pr
 
  
  # normalize so all values sum to 1 (probability distribution)
  assign_norm <- assign / sum(assign) 
  assign_norm <- assign_norm * CPUE[i]
  
  #rescale so that all values are between 0 and 1 
  assign_rescaled <- assign_norm / max(assign_norm) 

  percentile_80 <- quantile(assign_rescaled, probs = sensitivity_threshold)
  assign_rescale_removed <- ifelse(assign_rescaled >= percentile_80, 1, 0)
  
  assignment_matrix[,i] <- assign_rescale_removed
  
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
filepath<- file.path(here("Data", "Production", "Kusko", filename))
write.csv(basin_df, filepath)


################################################################################
##### Mapping using base R 
################################################################################

# Save as PDF
#breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9)
breaks <- c(0, .2, .4, .6, .7, .8, .9, 1)
nclr <- length(breaks)
filename <- paste0(identifier, "_", sensitivity_threshold, "_.pdf")
filepath <- file.path(here("Figures", "Maps", "Kusko", year, filename))
pdf(file = filepath, width = 9, height = 6)
plotvar <- basin_assign_norm
class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
plotclr <- brewer.pal(nclr, "YlOrRd")
colcode <- findColours(class, plotclr, digits = 2)
colcode[plotvar == 0] <- 'gray80'
colcode[which(StreamOrderPrior == 0)] <- 'gray60'
colcode[which(pid_prior == 0)] <- 'gray60'
plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
plot(st_geometry(yuk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .7 * (exp(plotvar) - 1)))
dev.off()


