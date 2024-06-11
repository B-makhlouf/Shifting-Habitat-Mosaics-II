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
#basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")

########################################################
#### Function to produce maps and production data 
########################################################

Yukon_assign <- function(year, sensitivity_threshold) {  
  
  identifier <- paste(year, "Yukon", sep = "_")
  
  # Read data based on the year
  natal_origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/03_Extracted Data/Natal Origins/Cleaned/Yukon_", year, "_Cleaned_Natal_Origins.csv"))                        
  CPUE <- read.csv(here(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/04_CPUE Data/", year, "_Yukon_CPUE weights.csv")), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()
  Genetics <- read.csv(here(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/02_Genetic Data/03_Genetic Prior/", year, " Yukon_genetic_prior_.csv", sep = "")))
  
  # Shapefile with the tributaries from each genetic grouping
  ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
  ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries
  my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
  my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries
  uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
  uy.gen_reachid <- uy.gen$reachid # reach ids of the upper Yukon tributaries
  
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
  pid_prior <- yuk_edges$PriorSl2 # Habitat prior (RCA slope)
  pid_isose_mod <- ifelse(pid_isose < 0.0031, 0.003, pid_isose) # bumps super low error areas up, to avoid excessive bias towards them 
  
  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 
  ###----- Filter to various quartiles 
  

  
  # Find the 4 Quartiles of the natal origin data based on Days_into_run
  natal_origins$quartile <- cut(natal_origins$Days_into_run, breaks = quantile(natal_origins$Days_into_run, probs = c(0, 0.25, 0.5, 0.75, 1)), labels = c("Q1", "Q2", "Q3", "Q4"))
  # give the first value Q1
  natal_origins$quartile[is.na(natal_origins$quartile)] <- "Q1"
  
  quartiles <- c("Q1", "Q2", "Q3", "Q4", "Total")
  result_list <- list()
  
  for (q in quartiles) {
    if (q == "Total") {
      filtered_data <- natal_origins
    } else {
      filtered_data <- natal_origins %>% filter(quartile == q)
    }
    
    l <- nrow(filtered_data)
    
    if (l == 0) {
      result_list[[q]] <- rep(NA, length(pid_iso))
      next
    }
    
    assignment_matrix <- matrix(NA, nrow = length(pid_iso), ncol = l)
    
    for (i in 1:l) {
      iso_o <- filtered_data[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
      genP <- Genetics[i, ] # genetic posterior for each
      gen.prior <- rep(0, length = length(pid_iso))
      
      gen.prior[LYsites] <- genP[3] %>% as.numeric()
      gen.prior[MYsites] <- genP[4] %>% as.numeric()
      gen.prior[UYsites] <- genP[5] %>% as.numeric()
      
      StreamOrderPrior <- as.numeric(yuk_edges$Str_Ord > 2)
      
      #####. BAYES RULE ASSIGNMENT. ##################
      
      assign <- (1 / sqrt((2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2))) * pid_prior * gen.prior * StreamOrderPrior
      
      # normalize so all values sum to 1 (probability distribution)
      assign_norm <- assign / sum(assign) 
      assign_norm <- assign_norm * CPUE[i] # multiply times the CPUE
      
      # rescale so that all values are between 0 and 1 
      assign_rescaled <- assign_norm / max(assign_norm) 
      assign_rescale_removed <- ifelse(assign_rescaled >= sensitivity_threshold, assign_rescaled, 0)
      assignment_matrix[, i] <- assign_rescale_removed
      
      # TEMPORARILY, Asssign all NAs a 0 
      assignment_matrix[is.na(assignment_matrix)] <- 0
    }
    
    ###------- BASIN SCALE VALUES ----------------------------------------
    
    basin_assign_sum <- apply(assignment_matrix, 1, sum) # total probability for each location
    basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum) # SUMS to 1 
    
    result_list[[q]] <- basin_assign_rescale
  }
  
  result_df <- data.frame(
    Location = 1:length(pid_iso),
    Q1 = result_list[["Q1"]],
    Q2 = result_list[["Q2"]],
    Q3 = result_list[["Q3"]],
    Q4 = result_list[["Q4"]],
    Total = result_list[["Total"]]
  )
  
  return(result_df)
}




################################################################################
###################### Kuskokwim Assignment Code ##################################
################################################################################
################################################################################


#Isoscape and Basin Shapefile 
kusk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
#basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")

########################################################
#### Function to produce maps and production data 
########################################################

Kusko_assign <- function(year, sensitivity_threshold) {  
  
  # Read data based on the year
  natal_origins_path <- paste("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/03_Extracted Data/Natal Origins/Cleaned/Kuskokwim_", year, "_Cleaned_Natal_Origins.csv", sep = "")
  CPUE_path <- paste("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/04_CPUE Data/CPUE Weights/", year, "_Kusko_CPUE weights.csv", sep = "")
  
  natal_origins <- read.csv(natal_origins_path)
  CPUE <- read.csv(CPUE_path, sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()
  
  ## ----- Extract isoscape prediction + error values -----------------------------
  pid_iso <- kusk_edges$iso_pred # Sr8786 value
  pid_isose <- kusk_edges$isose_pred # Error
  pid_isose[pid_isose < .0005] <- .0005 # bump up error in really low error places 
  pid_prior <- kusk_edges$UniPh2oNoE # Habitat prior (RCA slope)
  
  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose^2 + within_site^2 + analyt^2)  # COMBINED error 
  
  ###----- Filter to various quartiles --------------------------------------------
  
  # Find the 4 Quartiles of the natal origin data based on Days_into_run
  natal_origins$quartile <- cut(natal_origins$Days_into_run, breaks = quantile(natal_origins$Days_into_run, probs = c(0, 0.25, 0.5, 0.75, 1)), labels = c("Q1", "Q2", "Q3", "Q4"))
  
  # give the first value Q1
  natal_origins$quartile[is.na(natal_origins$quartile)] <- "Q1"
  
  quartiles <- c("Q1", "Q2", "Q3", "Q4", "Total")
  result_list <- list()
  
  for (q in quartiles) {
    if (q == "Total") {
      filtered_data <- natal_origins
    } else {
      filtered_data <- natal_origins %>% filter(quartile == q)
    }
    
    l <- nrow(filtered_data)
    
    if (l == 0) {
      result_list[[q]] <- rep(NA, length(pid_iso))
      next
    }
    
    assignment_matrix <- matrix(NA, nrow = length(pid_iso), ncol = l)
    
    for (i in 1:l) {
      iso_o <- filtered_data[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
      StreamOrderPrior <- as.numeric(kusk_edges$Strahler > 2)
      
      #####. BAYES RULE ASSIGNMENT. ##################
      
      assign <- (1 / sqrt((2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2))) * pid_prior * StreamOrderPrior
      
      # normalize so all values sum to 1 (probability distribution)
      assign_norm <- assign / sum(assign) 
      assign_norm <- assign_norm * CPUE[i] # multiply times the CPUE
      
      # rescale so that all values are between 0 and 1 
      assign_rescaled <- assign_norm / max(assign_norm) 
      assign_rescale_removed <- ifelse(assign_rescaled >= sensitivity_threshold, assign_rescaled, 0)
      assignment_matrix[, i] <- assign_rescale_removed
      
      # TEMPORARILY, Assign all NAs a 0 
      assignment_matrix[is.na(assignment_matrix)] <- 0
    }
    
    ###------- BASIN SCALE VALUES ----------------------------------------
    
    basin_assign_sum <- apply(assignment_matrix, 1, sum) # total probability for each location
    basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum) # SUMS to 1 
    
    result_list[[q]] <- basin_assign_rescale
  }
  
  result_df <- data.frame(
    Location = 1:length(pid_iso),
    Q1 = result_list[["Q1"]],
    Q2 = result_list[["Q2"]],
    Q3 = result_list[["Q3"]],
    Q4 = result_list[["Q4"]],
    Total = result_list[["Total"]]
  )
  
  return(result_df)
}




################################################################################
################################################################################

# Functions for mapping 

Map_Base<- function(River,plotvar) { 
  if River == "Yukon" {
   edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
   basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")
  } else {
   edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
   basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  }
  # breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9)
  breaks <- c(0, .1, .2, .4, .6, .8, .9, 1)
  #breaks <- c(0, .3, .7, 1)
  nclr <- length(breaks)
  filename <- paste0(identifier, "_", sensitivity_threshold, "_.pdf")
  filepath <- file.path(here("Figures", "Maps", filename))
  pdf(file = filepath, width = 9, height = 6)
  plotvar <- plotvar
  class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
  plotclr <- brewer.pal(nclr, "YlOrRd")
  colcode <- findColours(class, plotclr, digits = 2)
  colcode[plotvar == 0] <- 'gray60'
  colcode[plotvar < .2] <- 'gray80'
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .6 * (exp(plotvar) - 1)))
  dev.off()
  
}
  }

  








