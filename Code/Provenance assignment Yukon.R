# This code assigns geographic likelihood of all fish to tributaries,
  ## and produces a map using base R of production estimates 

## Written by: Ben Makhlouf 12/2023

#Packages 
library(sf)
library(here)
library(classInt)
library(RColorBrewer)
library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(here)
library(readr)
library(stringr)

#Shapefiles
isoscape<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

#Remove all NA values from the natal_values data frame
natal_values<- natal_values %>%
  na.omit()

# Extract the predicted isoscape Sr8786 values for each tributary
isoscape_sr<- isoscape$iso_pred 

# Extract the predicted isoscape Sr8786 error values for each tributary
isoscape_sr_err<- isoscape$isose_pred 

# To avoid relatively small areas with exceptionally low error from dominating assignments, 
  # we set a minimum error level of .0031, based off an examination of the relative distribution
  # of error values across the entire isoscape

isoscape_sr_err<- ifelse(isoscape_sr_err < 0.0031, 0.003, isoscape_sr_err)

#------------ Genetics ---------------------------------------------------------
#Read in the edges for each genetic grouping 

ly.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp")
ly.gen_reachid <- ly.gen$reachid

my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp")
my.gen_reachid <- my.gen$reachid

uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp")
uy.gen_reachid <- uy.gen$reachid

#Create a new attribute in yuk_edges, assign lower, middle, or upper to it 

# Add lower, middle , or upper attribute based on matching reachid values
isoscape$GenLMU <- 0
isoscape$GenLMU[isoscape$reachid %in% ly.gen_reachid] <- "lower"
isoscape$GenLMU[isoscape$reachid %in% my.gen_reachid] <- "middle"
isoscape$GenLMU[isoscape$reachid %in% uy.gen_reachid] <- "upper"

# Create a vector of all sites for each genetic region
LYsites <- which(isoscape$GenLMU == "lower")
MYsites <- which(isoscape$GenLMU == "middle")
UYsites <- which(isoscape$GenLMU == "upper")

# Each index refers to the lower, middle, and upper Yukon likelihood for i fish 
LowerAssign<- genetics$Lower
MiddleAssign<- genetics$Middle
UpperAssign<- genetics$Upper

###------ CPUE data, calculated elsewhere *SEE README -------------------------

CPUE<- unlist(as.vector(CPUE_weights))
CPUE<- as.numeric(CPUE)

# -----------------------------------------------------------------------------

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

stream_order_pr <- as.numeric(isoscape$Str_Order > 3) #Stream order > 2 

#Habitat prior, which... 
# include description here of what the habitat prior turns off.
habitat_prior<- isoscape$PriorSl2

###---- Assignment loop --------------------------------------------------------

## Beginning of assignment loop 
# This loop goes through, for each fish, each tributary and assigns a probability
# of natal origin based on the Bayes rule equation and priors 

samplenumbs<-nrow(natal_values)

for (i in 1:samplenumbs){
  ###---- Isotope values ---------------------------------------------------------
  iso_o<- natal_values$natal_iso_mean[i] #measured isotope value from the otolith 
  
  ###---- Incorporate Genetic Data ---------------------------------------------
  
  gen.prior <- rep(0, length = length(isoscape_sr))
  gen.prior[LYsites] <- LowerAssign [i] %>% as.numeric()
  gen.prior[MYsites] <- MiddleAssign [i] %>% as.numeric()
  gen.prior[UYsites] <- UpperAssign[i] %>% as.numeric()
  
  ####### ------------------------------------------------------------------------
  ###### BAYES RULE ASSIGNMENT CODE #############
  ######  ------------------------------------------------------------------------
  
  ## Line below is Bayes Rule 
  assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o- isoscape_sr)^2/(2*error^2))) * habitat_prior * stream_order_pr * gen.prior # CPUE
  
  # normalize so all values sum to 1 (probability distribution)
  assign_norm <- assign / sum(assign) 
  
  assign_norm <- assign_norm * CPUE[i]
  
  #rescale so that all values are between 0 and 1 
  assign_rescaled <- assign_norm / max(assign_norm) 
  
  # To remove diffuse probability of many, low probability fish in one area, 
  # we will assign any values less than .5 in the assign_rescale to 0. (unlikely)
  # and any values greater than .5 to 1. (likely)
  
  assign_rescaled[assign_rescaled < .7] <- 0 
  assign_rescaled[assign_rescaled >= .7] <- 1
  
  # Rescaled values are placed into the assignment matrix for that fish 
  # until entire loop is finished 
  assignment_matrix[,i] <- assign_rescaled
}

####-------- Basin scale assignment --------------------------------
basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 

basin_df<- as.data.frame(basin_assign_rescale) #convert to data frame for export 
filename<- paste0(identifier, "_basin_norm.csv")
filepath<- file.path(here("data", "production data", "Yukon","normalized basin values", filename))
write_csv(basin_df, filepath)


################################################################################
##### Mapping using base R 
################################################################################

# Rest of your plotting code
#breaks <- seq(min(basin_assign_rescale), max(basin_assign_rescale), length= 9)

breaks <- c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1)
nclr <- length(breaks)
filename <- paste0(identifier, "_.pdf")
filepath <- file.path(here('results',"figures", "production_maps", "Yukon", filename))
pdf(file = filepath, width = 9, height = 6)
plotvar <- basin_assign_norm
class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
plotclr <- brewer.pal(nclr, "YlOrRd")
colcode <- findColours(class, plotclr, digits = 2)
colcode[plotvar == 0] <- 'gray80'
colcode[which(stream_order_pr == 0)] <- 'gray60'
colcode[which(habitat_prior == 0)] <- 'gray60'
plot(st_geometry(basin), col = "gray60", border = "gray48", main = identifier)
plot(st_geometry(isoscape), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .6 * (exp(plotvar) - 1)))
dev.off()


rm(list = setdiff(ls(), "basin_df"))
