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

  # Rescaled values are placed into the assignment matrix for that fish 
  # until entire loop is finished 
  assignment_matrix[,i] <- assign_rescaled
  
}

####-------- Basin scale assignment --------------------------------

## Basin scale assignments
basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #normalized probability for each location
basin_assign_norm <- basin_assign_rescale/max(basin_assign_rescale) #scale so entire basin of assignments ranges 0 to 1

## Writes a csv of basin scale assignment, individual and basin level normalized and rescaled
basin_df<- as.data.frame(basin_assign_rescale) #convert to data frame for export 
filename<- paste0(identifier, "_basin_norm.csv")
filepath<- file.path(here("Data", "Production", "Kusko", filename))
write_csv(basin_df, filepath)

################################################################################
##### Mapping using base R 
################################################################################
breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9) #comment in to change the breaks from standardized to scaled from the data. 
nclr <- length(breaks)
filename <- paste0(identifier, ".pdf")
filepath <- file.path(here("Figures", "Maps", "Kusko", filename))
pdf(file = filepath, width = 9, height = 6)
plotvar <- basin_assign_norm
class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
plotclr <- brewer.pal(nclr, "YlOrRd")
colcode <- findColours(class, plotclr, digits = 2)
colcode[plotvar == 0] <- 'gray92'
colcode[which(habitat_prior==0)] <- 'gray60'
colcode[which(stream_order_pr ==0)] <- 'gray60'
plot(st_geometry(basin), col = "gray60", border = "gray60", main = identifier)
plot(st_geometry(isoscape), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar < 0.2, 0.05, .7))
#0.9 * (exp(plotvar) - 1)))
#legend_labels <- c("0", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "1")
#legend("topright", legend = legend_labels, fill = brewer.pal(nclr, "YlOrRd"), title = "Legend")
dev.off()

rm(list = setdiff(ls(), "basin_df"))

