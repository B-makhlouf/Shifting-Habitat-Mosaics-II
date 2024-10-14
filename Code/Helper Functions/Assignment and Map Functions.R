library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(classInt)
library(RColorBrewer)
library(fabricatr)
library(ggplot2)
library(sf)
library(readr)
library(sf)
library(classInt)
library(RColorBrewer)
library(here)
################################################################################
###########################. YUKON ASSIGNMENT CODE #############################
################################################################################
################################################################################


###----- Shapefiles ------------------------------------------------------------
#Shapefiles
#yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_bigtribs.shp")
yuk_edges<- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

########################################################
#### Function to produce maps and production data 
########################################################

Yukon_map <- function(year, sensitivity_threshold, quartile = "ALL") {   
  
  identifier <- paste(year, "Yukon", sep = "_")
  yuk_edges<- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp") #Shapefiles 
  Natal_Origins<- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origins/Extracted/ALL_DATA_",year,"_Yukon_Natal_Origins.csv")) #Natal Origins
  
  # Filter based on quartile if specified
  if (quartile != "ALL") {
    if (quartile %in% c("Q1", "Q2", "Q3", "Q4")) {
      Natal_Origins <- Natal_Origins[Natal_Origins$Quartile == quartile, ]
    } else {
      stop("Invalid quartile value. Please use 'Q1', 'Q2', 'Q3', 'Q4', or 'ALL'.")
    }
  }
  
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
  
  # ***make all pid_isose values .003
  
  #pid_isose_mod <- ifelse(pid_isose < 0.0031, 0.003, .003) 
  
  pid_isose_mod <- ifelse(pid_isose < 0.0031, 0.003, pid_isose) #bumps super low error areas up, to avoid excessive bias towards them 
  pid_isose_mod <- ifelse(pid_isose_mod > 0.0042, 0.0042, pid_isose_mod)
  
  
  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 
  
  ###----- CREATE EMPTY MATRICES -------------------------------------------------
  assignment_matrix <- matrix(NA, nrow = length(yuk_edges$iso_pred), ncol = nrow(Natal_Origins))
  #assignment_matrix <- matrix(NA,nrow=length(pid_iso),ncol=nrow(Natal_Origins))
  
  #############################
  ###### ASSIGNMENTS HERE ##### 
  #############################
  ## loop for assingments
  
  for (i in 1:nrow(Natal_Origins)) {
    
    iso_o <- Natal_Origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
    gen.prior <- rep(0, length = length(pid_iso))
    gen.prior[LYsites] <- Natal_Origins$Gen_L[i]%>% as.numeric()
    gen.prior[MYsites] <- Natal_Origins$Gen_M[i] %>% as.numeric()
    gen.prior[UYsites] <- Natal_Origins$Gen_U[i] %>% as.numeric()
    
    StreamOrderPrior <- as.numeric(yuk_edges$Str_Ord > 2)
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * gen.prior * StreamOrderPrior
    
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
  
    # If the rescaled value is less than the threshold, then set the same index in assign_norm to 0, otherwise 1 
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    assign_rescaled[assign_rescaled >= sensitivity_threshold] <- 1
    
    # Multiply by the CPUE weight 
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins[i, "Strat"])
  
    assignment_matrix[, i] <- assign_rescaled_wt
    
  }
  
  
  
  #Locate the column with NA 
  na_col <- which(colSums(is.na(assignment_matrix)) > 0)

  ###------- BASIN SCALE VALUES ----------------------------------------
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
  basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
  basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 
  
  return(basin_assign_norm)
}






################################################################################
################################################################################

# Functions for mapping 

library(sf)
library(classInt)
library(RColorBrewer)

Map_Base <- function(River, plotvar, identifier, sensitivity_threshold) {
  library(sf)
  library(classInt)
  library(RColorBrewer)
  library(here)
  
  # Load the appropriate shapefiles based on the River parameter
  if (River == "Yukon") {
    edges_path <- "/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp"
    basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp"
  } else if (River == "Kuskokwim") {
    edges_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp"
    basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp"
  } else {
    stop("Invalid river specified.")
  }
  
  edges <- st_read(edges_path)
  basin <- st_read(basin_path)
  
  # Calculate StreamOrderPrior and pid_prior based on the River parameter
  if (River == "Yukon") {
    StreamOrderPrior <- as.numeric(edges$Str_Order > 2)
    pid_prior <- edges$PriorSl2
  } else if (River == "Kuskokwim") {
    StreamOrderPrior <- as.numeric(edges$Strahler > 2)
    pid_prior <- edges$UniPh2oNoE
  }
  
  # Define breaks and custom color palette for the plot
  breaks <- c(0, 0.05, 0.1, .15, 0.2, .25, 0.3, .35, 0.4, .45, 0.5, .55, 0.6, .65, 0.7, .75, 0.8, .85, 0.9, .95, 1)
  nclr <- length(breaks)
  filename <- paste0(identifier, "_", sensitivity_threshold, ".pdf")
  filepath <- file.path(here("Basin Maps", filename))
  
  pdf(file = filepath, width = 9, height = 6)
  class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
  
  # Custom palette from light orange to dark red
  custom_palette <- colorRampPalette(c("lightgoldenrod1", "orangered3", "#4B0000"))(nclr)
  colcode <- findColours(class, custom_palette, digits = 2)
  
  # Adjust colors based on conditions
  colcode[plotvar == 0] <- 'gray80'
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  
  # Plot the basin and edges with the appropriate colors and line widths
  plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = 0.7*(exp(plotvar)-1))
  dev.off()
}


################################################################################
########################### Kuskokwim ASSIGNMENT CODE #############################
################################################################################
################################################################################
#2020 kusko example 
year <- 2020
sensitivity_threshold <- 0.7
quartile <- "ALL"

###----- Shapefiles ------------------------------------------------------------

########################################################
#### Function to produce maps and production data 
########################################################

Kusko_map <- function(year, sensitivity_threshold, quartile = "ALL") {   
  
  identifier <- paste(year, "Kusko", sep = "_")
  kusk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Natal_Sr/QCd/ALL_DATA_", year, "_Kusko_Natal_Origins.csv"))
  
  #Remove any with NA in natal_iso
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso),]
  
  # Filter based on quartile if specified
  if (quartile != "ALL") {
    if (quartile %in% c("Q1", "Q2", "Q3", "Q4")) {
      Natal_Origins <- Natal_Origins[Natal_Origins$Quartile == quartile, ]
    } else {
      stop("Invalid quartile value. Please use 'Q1', 'Q2', 'Q3', 'Q4', or 'ALL'.")
    }
  }
  
  ## ----- Extract isoscape prediction + error values -----------------------------
  
  pid_iso <- kusk_edges$iso_pred # Sr8786 value
  pid_isose <- kusk_edges$isose_pred # Error
  pid_prior <- kusk_edges$UniPh2oNoE #Habitat prior ( RCA slope)
  
  #quick histogram of pid_isose values 
 # hist(pid_isose)
  
  # ***make all pid_isose values .003
  
  #pid_isose_mod <- ifelse(pid_isose < 0.0031, 0.003, .003) 
  
  pid_isose_mod <- ifelse(pid_isose < 0.0006, 0.0006, pid_isose) # Bumps super low error areas up
  pid_isose_mod <- ifelse(pid_isose_mod > 0.001, 0.001, pid_isose_mod) # Bumps super high error areas down
  
  hist(pid_isose_mod)
  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 
  
  ###----- CREATE EMPTY MATRICES -------------------------------------------------
  assignment_matrix <- matrix(NA, nrow = length(kusk_edges$iso_pred), ncol = nrow(Natal_Origins))
  #assignment_matrix <- matrix(NA,nrow=length(pid_iso),ncol=nrow(Natal_Origins))
  
  #############################
  ###### ASSIGNMENTS HERE ##### 
  #############################
  ## loop for assingments
  
  for (i in 1:nrow(Natal_Origins)) {
    
    iso_o <- Natal_Origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
   
    StreamOrderPrior <- as.numeric(kusk_edges$Str_Order > 2)
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * StreamOrderPrior
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
    
    # If the rescaled value is less than the threshold, then set the same index in assign_norm to 0, otherwise 1 
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    assign_rescaled[assign_rescaled >= sensitivity_threshold] <- 1
    
    # Multiply by the CPUE weight 
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins[i, "Strat"])
    
    assignment_matrix[, i] <- assign_rescaled_wt
    
  }
  
  #Locate the column with NA 
  na_col <- which(colSums(is.na(assignment_matrix)) > 0)
  
  ###------- BASIN SCALE VALUES ----------------------------------------
  
  basin_assign_sum <- apply(assignment_matrix, 1, sum) #total probability for each location
  basin_assign_rescale <- basin_assign_sum/sum(basin_assign_sum) #rescaled probability for each location
  basin_assign_norm<- basin_assign_rescale/max(basin_assign_rescale) #normalized from 0 to 1 
  
  return(basin_assign_norm)
}






################################################################################
################################################################################

# Functions for mapping Kusko 

Map_Base_Kusko <- function(River, plotvar, identifier, sensitivity_threshold) {
 
  
  # Load the appropriate shapefiles based on the River parameter
  if (River == "Yukon") {
    edges_path <- "/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp"
    basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp"
  } else if (River == "Kuskokwim") {
    edges_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp"
    basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp"
  } else {
    stop("Invalid river specified.")
  }
  
  edges <- st_read(edges_path)
  basin <- st_read(basin_path)
  
  # Calculate StreamOrderPrior and pid_prior based on the River parameter
  if (River == "Yukon") {
    StreamOrderPrior <- as.numeric(edges$Str_Order > 2)
    pid_prior <- edges$PriorSl2
  } else if (River == "Kuskokwim") {
    StreamOrderPrior <- as.numeric(edges$Strahler > 2)
    pid_prior <- edges$UniPh2oNoE
  }
  
  # Define breaks and custom color palette for the plot
  breaks <- c(0, 0.05, 0.1, .15, 0.2, .25, 0.3, .35, 0.4, .45, 0.5, .55, 0.6, .65, 0.7, .75, 0.8, .85, 0.9, .95, 1)
  nclr <- length(breaks)
  filename <- paste0(identifier, "_", sensitivity_threshold, ".pdf")
  filepath <- file.path(here("Basin Maps", filename))
  
  pdf(file = filepath, width = 9, height = 6)
  class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
  
  # Custom palette from light orange to dark red
  custom_palette <- colorRampPalette(c("lightgoldenrod1", "firebrick4"))(nclr)
  colcode <- findColours(class, custom_palette, digits = 2)
  
  # Adjust colors based on conditions
  colcode[plotvar == 0] <- 'gray80'
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  
  # Plot the basin and edges with the appropriate colors and line widths
  plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = 1 *(exp(plotvar)-1))
  dev.off()
}
