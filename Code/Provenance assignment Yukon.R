#BayesRule Assignments and Isoscape mapping
#Original written by : Tim Cline
#Updated by:  Ben Makhlouf 7/18/23 

###------ Packages -------------------------------------------------------------

rm(list=ls())
library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(classInt)
library(RColorBrewer)
library(fabricatr)
library(ggplot2)
library(sf)

###----- Data ------------------------------------------------------------------ 

## 2015 
natal_origins<- read.csv("Data/Natal Origin/2015 Yukon_natal_data.csv")
yuk_gen <- read.csv(here("Data/Genetics/Yukon_Genetics_2015.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
CPUE <- read.csv(here("Data/CPUE/CPUE_weights/2015 Yukon_CPUE weights.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()
identifier <- "2015 Yukon"

###----- Shapefiles ------------------------------------------------------------

#Shapefile with all of the tributaries, predicted isotopic values, and predicted isotopic error values
yuk_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc_withprobs.shp",quiet = TRUE)
yuk_edges<-as_Spatial(yuk_edges)

#Shapefile with Yukon basin (US + Canada)
yuk_basin<-st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp"), quiet = TRUE)
yuk_basin<-as_Spatial(yuk_basin)

#Shapefile with the tributaries from the lower Yukon river basin 
ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries

#Shapefile with the tributaries from the middle Yukon river basin
my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries

#Shapefile with the tributaries from the upper Yukon river basin 
uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
uy.gen_reachid <- uy.gen$reachid #reach ids of the upper Yukon tributaries

#------------------------------------------------------------------------------ 

# Create a new attribute in the yuk_edges shapefile that indicates which genetic region each tributary belongs to 

yuk_edges$GenLMU <- 0
yuk_edges$GenLMU[yuk_edges$reachid %in% ly.gen_reachid] <- "lower"
yuk_edges$GenLMU[yuk_edges$reachid %in% my.gen_reachid] <- "middle"
yuk_edges$GenLMU[yuk_edges$reachid %in% uy.gen_reachid] <- "upper"

# Create a vector of the INDICES associated with each genetic region

LYsites <- which(yuk_edges$GenLMU == "lower")
MYsites <- which(yuk_edges$GenLMU == "middle")
UYsites <- which(yuk_edges$GenLMU == "upper")

#Sum probability values for each genetic region for each fish 

yuk2015_gen <- yuk_gen %>%
  filter( QC_or_RR != "qc") %>% #only 2015 and NOT QC
  group_by(FishID, indiv, repunit) %>% #Group by.. Fish, individual, unit,
  summarise(P = sum(PofZ)) #Each individual's summed posterior probability of being in each genetic region. 

otoTab <- natal_origins #otolith data
geneTab <- yuk2015_gen #genetic data 

# create fishid by extracting the last three characters

fishid<- otoTab$otoNum %>%
  as.numeric()

otoTab$FishID <- fishid #extract fish_id


###----- Build Otogene ---------------------------------------------------------

otogene <- data.frame( 
  FishID = fishid, #all fish_ids
  Date = otoTab$capture_date, #all_dates associated
  iso = otoTab$natal_iso_mean, #otolith natal isotope value
  L = rep(1/3, length(fishid)), #assign equal likelihood of each genetic region
  M = rep(1/3, length(fishid)),
  U = rep(1/3, length(fishid))
)

#go to the genetics table and extract those which have a oto info

fish.g <- geneTab %>% 
  filter(FishID %in% fishid) %>%
  arrange(FishID)

otogene <- otogene %>% 
  arrange(FishID)

## in otogene, bring in the summed probabilities for lower, middle, and upper for each fish 
otogene$L[fishid %in% fish.g$FishID] <- fish.g$P[fish.g$repunit == "lower"]
otogene$M[fishid %in% fish.g$FishID] <- fish.g$P[fish.g$repunit == "middle"]
otogene$U[fishid %in% fish.g$FishID] <- fish.g$P[fish.g$repunit == "upper"]

#bring in DOY (julian date) by matching FsihID in otogene to FishID in otoTab
otogene$DOY <- otoTab$capture_date_julian[match(otogene$FishID, otoTab$FishID)]


## ----- Extract isoscape prediction + error values -----------------------------

pid_iso <- yuk_edges$iso_pred # Sr8786 value
pid_isose <- yuk_edges$isose_pred # Error
pid_prior <- yuk_edges$PriorSl2 #Habitat prior ( RCA slope)
pid_isose_mod <- ifelse(pid_isose < 0.0031, 0.003, pid_isose) #bumps super low error areas up, to avoid excessive bias towards them 

###----- Variance Generating Processes ------------------------------------------
within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
within_pop <- within_site - analyt # Population error
error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 

###----- CREATE EMPTY MATRICES -------------------------------------------------
output_matrix <- matrix(NA, nrow = length(yuk_edges$iso_pred), ncol = nrow(otogene))
l<-length(otogene[, 1])
f.strata.vec <- rep(NA,l)
assignment_matrix <- matrix(NA,nrow=length(pid_iso),ncol=l)

#############################
###### ASSIGNMENTS HERE ##### 
#############################

## loop for assingments
for (i in 1:length(otogene[, 1])) {

  iso_o <- otogene[i, "iso"] %>% as.numeric()  # Otolith ratio
  genP <- otogene[i, c("L", "M", "U")] #genetic posterior for each
  gen.prior <- rep(0, length = length(pid_iso))
  gen.prior[LYsites] <- genP[1] %>% as.numeric()
  gen.prior[MYsites] <- genP[2] %>% as.numeric()
  gen.prior[UYsites] <- genP[3] %>% as.numeric()
  StreamOrderPrior <- as.numeric(yuk_edges$Str_Order > 2)
  
  #####. BAYES RULE ASSIGNMENT. ##################
  
  assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * gen.prior * StreamOrderPrior
  
  # normalize so all values sum to 1 (probability distribution)
  assign_norm <- assign / sum(assign) 
  
  assign_norm <- assign_norm * CPUE[i]
  
  #rescale so that all values are between 0 and 1 
  assign_rescaled <- assign_norm / max(assign_norm) 
  
  ## Remove diffuse probability by negating anything under a threshold
  assign_rescaled[assign_rescaled < .7] <- 0 
  assign_rescaled[assign_rescaled >= .7] <- 1
  
  # Rescaled values are placed into the assignment matrix for that fish 
  assignment_matrix[,i] <- assign_rescaled
  
}



###------- BASIN SCALE POSTERIOR VALUES ----------------------------------------


