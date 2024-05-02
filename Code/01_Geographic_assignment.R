library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(classInt)
library(RColorBrewer)
library(fabricatr)
library(ggplot2)
library(sf)

################################################################################
###########################. YUKON ASSIGNMENT CODE #############################
################################################################################
################################################################################


###----- Shapefiles ------------------------------------------------------------
#Shapefiles
#yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_bigtribs.shp")
yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")


# For testing 
if (T){
year<- 2015
sensitivity_threshold <- 0.7
}

########################################################
#### Function to produce maps and production data 
########################################################

Yukon_map <- function(year, sensitivity_threshold) {  
  
  identifier <- paste(year, "Yukon", sep = "_")
  
  # Read data based on the year
  natal_origins <- read.csv(paste("Data/Natal_Sr/", year, "_Yukon_NatalOrigins.csv", sep = ""))
  CPUE <- read.csv(here("Data/CPUE_weights/", paste(year, "_Yukon_CPUE weights.csv", sep = "")), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()
  Genetics <- read.csv(here("Data/Genetic_Prior", paste(year, "_Yukon_genetic_prior_.csv", sep = "")))

  #Shapefile with the tributaries from the each genetic grouping
  ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
  ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries
  my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
  my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries
  uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
  uy.gen_reachid <- uy.gen$reachid #reach ids of the upper Yukon tributaries
  
  yuk_edges$GenLMU <- 0
  yuk_edges$GenLMU[yuk_edges$reachid %in% ly.gen_reachid] <- "lower"
  yuk_edges$GenLMU[yuk_edges$reachid %in% my.gen_reachid] <- "middle"
  yuk_edges$GenLMU[yuk_edges$reachid %in% uy.gen_reachid] <- "upper"
  LYsites <- which(yuk_edges$GenLMU == "lower") # Create a vector of the INDICES associated with each genetic region
  MYsites <- which(yuk_edges$GenLMU == "middle")
  UYsites <- which(yuk_edges$GenLMU == "upper")
  
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
  output_matrix <- matrix(NA, nrow = length(yuk_edges$iso_pred), ncol = nrow(natal_origins))
  l<-length(natal_origins[, 1])
  f.strata.vec <- rep(NA,l)
  assignment_matrix <- matrix(NA,nrow=length(pid_iso),ncol=l)
  
  #############################
  ###### ASSIGNMENTS HERE ##### 
  #############################
  ## loop for assingments

  for (i in 1:length(natal_origins[, 1])) {
  
    iso_o <- natal_origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
    genP <- Genetics[i,] #genetic posterior for each
    gen.prior <- rep(0, length = length(pid_iso))
    gen.prior[LYsites] <- genP["Lower"] %>% as.numeric()
    gen.prior[MYsites] <- genP[2] %>% as.numeric()
    gen.prior[UYsites] <- genP[3] %>% as.numeric()
    StreamOrderPrior <- as.numeric(yuk_edges$Str_Ord > 2)
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * gen.prior * StreamOrderPrior
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    assign_norm <- assign_norm * CPUE[i] # multiply times the CPUE
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
    
    assign_rescale_removed<- ifelse(assign_rescaled >= sensitivity_threshold, assign_rescaled, 0)
    
    assignment_matrix[, i] <- assign_rescale_removed
  
  }
  
  ###------- BASIN SCALE VALUES ----------------------------------------
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
  basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
  basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 

  
  ################################################################################
  ##### Mapping using base R 
  ################################################################################
  
    # Save as PDF
   # breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9)
    breaks <- c(0, .1, .2, .4, .6, .8, .9, 1)
    #breaks <- c(0, .3, .7, 1)
    nclr <- length(breaks)
    filename <- paste0(identifier, "_", sensitivity_threshold, "_.pdf")
    filepath <- file.path(here("Figures", "Maps", filename))
    pdf(file = filepath, width = 9, height = 6)
    plotvar <- basin_assign_norm
    class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
    plotclr <- brewer.pal(nclr, "YlOrRd")
    colcode <- findColours(class, plotclr, digits = 2)
    colcode[plotvar == 0] <- 'gray60'
    colcode[plotvar < .2] <- 'gray80'
    colcode[which(StreamOrderPrior == 0)] <- 'gray60'
    colcode[which(pid_prior == 0)] <- 'gray60'
    plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
    plot(st_geometry(yuk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .6 * (exp(plotvar) - 1)))
    dev.off()
    
    
    ########### Bring in tribs
    
    #library(sf)
    #library(dplyr)
    #library(ggplot2)
    
    # Read shapefile
    #tribnames <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")
    
    # Assuming basin_assign_norm is defined correctly
    #tribnames$assign_rescaled <- basin_assign_norm
    
    # Summarize by tributary
    #trib_summary <- tribnames %>% 
    #  group_by(trbtry_) %>% 
    #  summarize(totalprod = sum(assign_rescaled))
    
    # Rescale totalprod to sum to 1 
    #trib_summary$totalprod <- trib_summary$totalprod / sum(trib_summary$totalprod)
    
    #normalize total prod values to range from 0 - 1
    #trib_summary$totalprod <- trib_summary$totalprod / max(trib_summary$totalprod)
    
    # Add totalprod value to tribnames by matching on trbtry_
    #tribnames <- st_join(tribnames, trib_summary, by = "trbtry_")
    
    #summary(tribnames$totalprod)
    
    # 
    # # Plot using ggplot
    # map <- ggplot(data = tribnames) +
    #   geom_sf(aes(color = totalprod), lwd = .05) +  # Specify color aesthetic
    #   scale_color_gradient(low = "dodgerblue", high = "firebrick") + # Adjust color gradient if needed
    #   theme_void()+
    #   labs(title = "Production by trib basin")  # Add title if desired
    # 
    # tribnames$totalprod
    # # Save as a tif
    # filename <- paste0(identifier, "_", sensitivity_threshold, "_.tif")
    # filepath <- file.path("Figures", "Maps", filename)  # Assuming you don't need 'here' function
    # ggsave(filepath, plot = map, device = "tiff", width = 9, height = 6)
}


################################################################################
###################### Kuskokwim Assignment Code ##################################
################################################################################
################################################################################


#Isoscape and Basin Shapefile 
kusk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")

########################################################
#### Function to produce maps and production data 
########################################################

Kusko_map <- function(year, sensitivity_threshold) {  
  
  identifier <- paste(year, "Kusko", sep = "_")
  
  # Read data based on the year
  natal_origins <- read.csv(paste("Data/Natal_Sr/", year, "_Kusko_NatalOrigins.csv", sep = ""))
  #CPUE <- read.csv(here("Data/CPUE_weights/", paste(year, "_Kusko_CPUE weights.csv", sep = "")), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()
  
  ## ----- Extract isoscape prediction + error values -----------------------------
  pid_iso <- kusk_edges$iso_pred # Sr8786 value
  pid_isose <- kusk_edges$isose_pred # Error
  pid_isose[pid_isose < .0005] <- .0005 #bump up error in really low error places 
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
  
  for (i in 1:length(natal_origins[, 1])) {
    
    iso_o <- natal_origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
    StreamOrderPrior <- as.numeric(kusk_edges$Strahler > 2)
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * StreamOrderPrior
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    
    assign_norm <- assign_norm #* CPUE[i] # multiply times the CPUE
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
    
    assign_rescale_removed<- ifelse(assign_rescaled >= sensitivity_threshold, assign_rescaled, 0)
    assignment_matrix[,i] <- assign_rescale_removed
    
    #assignment_matrix[,i] <- assign_rescaled
    
  }
  
  ###------- BASIN SCALE VALUES ----------------------------------------
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
  basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
  basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 
  
  ################################################################################
  ##### Mapping using base R 
  ################################################################################
  
  # Save as PDF
  #breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9)
  breaks <- c(0, .1, .2, .4, .6, .8, .9, 1)
  nclr <- length(breaks)
  filename <- paste0(identifier, "_", sensitivity_threshold, "_.pdf")
  filepath <- file.path(here("Figures", "Maps", filename))
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
}



##############################################
########## Producing All Maps
##############################################


# List of years with data
years <- c(2015, 2016, 2017)
sensitivity_threshold<- .75

# Yukon Mapping 
for (i in 1:length(years)) {
  year <- years[i]
  Yukon_map(year, sensitivity_threshold)
}

years <- c(2017, 2018)
sensitivity_threshold<- .75

#Kusko mapping
for (i in 1:length(years)) {
  year <- years[i]
  Kusko_map(year, sensitivity_threshold)
}







