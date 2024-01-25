library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(classInt)
library(RColorBrewer)
library(fabricatr)
library(ggplot2)
library(sf)
#Shapefiles
yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

#Shapefile with the tributaries from the lower Yukon river basin 
ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries

#Shapefile with the tributaries from the middle Yukon river basin
my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries

#Shapefile with the tributaries from the upper Yukon river basin 
uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
uy.gen_reachid <- uy.gen$reachid #reach ids of the upper Yukon tributaries


############# Data
natal_origins<- read.csv("Data/Natal Origin/2015 Yukon_natal_data.csv")
yuk_gen <- read.csv(here("Data/Genetics/Yukon_Genetics_2015.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
identifier<- "2015_Yukon"






##################


# Create a new attribute in the yuk_edges shapefile that indicates which genetic region each tributary belongs to 

yuk_edges$GenLMU <- 0
yuk_edges$GenLMU[yuk_edges$reachid %in% ly.gen_reachid] <- "lower"
yuk_edges$GenLMU[yuk_edges$reachid %in% my.gen_reachid] <- "middle"
yuk_edges$GenLMU[yuk_edges$reachid %in% uy.gen_reachid] <- "upper"

# Create a vector of the INDICES associated with each genetic region

LYsites <- which(yuk_edges$GenLMU == "lower")
MYsites <- which(yuk_edges$GenLMU == "middle")
UYsites <- which(yuk_edges$GenLMU == "upper")

yuk2015_gen <- yuk_gen %>%
  filter( QC_or_RR != "qc") %>% #only 2015 and NOT QC
  group_by(FishID, indiv, repunit) %>% #Group by.. Fish, individual, unit,
  summarise(P = sum(PofZ)) #Each individual's summed posterior probability of being in each genetic region. 

geneTab <- yuk2015_gen #genetic data 

#Sum probability values for each genetic region for each fish 
otoTab <- natal_origins #otolith data

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


gen.prior <- data.frame(
  fish.id = fishid,
  Lower = rep(0, length(fishid)),
  Middle = rep(0, length(fishid)),
  Upper = rep(0, length(fishid))
)

# Identify the rows corresponding to lower, middle, and upper in otogene
lower_rows <- otogene$L > 0
middle_rows <- otogene$M > 0
upper_rows <- otogene$U > 0

# Assign genetic posterior to gen.prior using vectorized operations
gen.prior$Lower[lower_rows] <- otogene$L[lower_rows]
gen.prior$Middle[middle_rows] <- otogene$M[middle_rows]
gen.prior$Upper[upper_rows] <- otogene$U[upper_rows]

#export as a .csv

filename <- paste0(identifier, "_genetic_prior_", ".csv")
filepath <- here("Data/Genetics/Genetic Prior/", filename)
write.csv(gen.prior, filepath, row.names = FALSE)
