####################################### 
############# Kusko 
#######################################

KK_assign <- function(year, sensitivity_threshold, min_error, min_stream_order, HUC = 8) {
  
  # Generate identifier for output files
  identifier <- paste(year, "Kusko", sep = "_")
  
  # Load spatial data
  edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  edges <- st_transform(edges, st_crs(basin)) # Make sure all shapefiles are projected to the same CRS
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Kusko_Natal_Origins_Genetics_CPUE.csv"))
  
  # Remove any rows with NA in natal_iso or dailyCPUEprop
  Natal_Origins_clean <- Natal_Origins[!is.na(Natal_Origins$natal_iso) & !is.na(Natal_Origins$dailyCPUEprop), ]
  
  # Filter the shapefile to only StreamOrders equal to or above the min_stream_order
  edges <- edges[edges$Str_Order >= min_stream_order, ]
  
  # Plot CPUE by DOY
  gg_hist <- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "gray20", linewidth = 2) +
    theme_minimal() +
    ggtitle("Prop. CPUE by DOY")
  
  if (nrow(Natal_Origins) > 0) {
    gg_hist <- gg_hist +
      geom_ribbon(data = Natal_Origins, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                  fill = "gray", alpha = 0.7, inherit.aes = FALSE)
  }
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_prior <- edges$UniPh2oNoE
  
  # Constrain all pid_isose values above a given minimum error value
  pid_isose_mod <- pmax(pid_isose, min_error)
  
  # Variance Generating Processes
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  within_pop <- within_site - analyt
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  
  # Create empty matrices for assignments
  assignment_matrix <- matrix(NA, nrow = length(edges$iso_pred), ncol = nrow(Natal_Origins_clean))
  
  # Perform assignments
  for (i in 1:nrow(Natal_Origins_clean)) {
    iso_o <- as.numeric(Natal_Origins_clean[i, "natal_iso"])
    StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    
    # Bayesian assignment
    assign <- (1 / sqrt(2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2)) * pid_prior * StreamOrderPrior
    assign_norm <- assign / sum(assign)
    assign_rescaled <- assign_norm / max(assign_norm)
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # Rescale again so that the new values range from 0-1
    assign_rescaled <- assign_rescaled / max(assign_rescaled)
    
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins_clean[i, "COratio"])
    
    assignment_matrix[, i] <- assign_rescaled_wt
  }
  
   assignment_matrix <<- assignment_matrix
   edges<<- edges
   StreamOrderPrior<<- StreamOrderPrior
   pid_prior<<- pid_prior
   identifier<<- identifier
   basin<<- basin
   Huc<<- Huc
   
}


#######################################################
########### Yukon 
#######################################################


YK_assign <- function(year, sensitivity_threshold, min_error, min_stream_order, HUC = 8) {
  
  
  edges<- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp") #Shapefiles 
  basin<<- st_read("/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp")
  identifier <<- paste(year, "Yukon", sep = "_")
  Natal_Origins_ALL <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  Huc <<- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  

  #be sure to remove rows with NA in "Lower"
  Natal_Origins <- Natal_Origins_ALL[!is.na(Natal_Origins_ALL$Lower), ]
  edges <- edges[edges$Str_Order >= min_stream_order,]
  
  gg_hist <<- ggplot(Natal_Origins, aes(x = DOY, y = dailyCPUEprop)) + 
    geom_line(color = "gray20", linewidth = 2) +
    theme_grey() +
    ggtitle("Prop. CPUE by DOY")+
    geom_ribbon(data = Natal_Origins, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                  fill = "#FF6201", alpha = 0.7, inherit.aes = FALSE)
  
  #Shapefile with the tributaries from the each genetic grouping
  ly.gen <- st_read(here("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp"), quiet = TRUE)
  ly.gen_reachid <- ly.gen$reachid # reach ids of the lower Yukon tributaries
  my.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp", quiet = TRUE)
  my.gen_reachid <- my.gen$reachid # reach ids of the middle Yukon tributaries
  uy.gen <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp", quiet = TRUE)
  uy.gen_reachid <- uy.gen$reachid #reach ids of the upper Yukon tributaries
  
  edges$GenLMU <- 0
  edges$GenLMU[edges$reachid %in% ly.gen_reachid] <- "lower"
  edges$GenLMU[edges$reachid %in% my.gen_reachid] <- "middle"
  edges$GenLMU[edges$reachid %in% uy.gen_reachid] <- "upper"
  LYsites <- which(edges$GenLMU == "lower") # Create a vector of the INDICES associated with each genetic region
  MYsites <- which(edges$GenLMU == "middle")
  UYsites <- which(edges$GenLMU == "upper")
  
  ## ----- Extract isoscape prediction + error values -----------------------------
  
  pid_iso <- edges$iso_pred # Sr8786 value
  pid_isose <- edges$isose_pred # Error
  pid_prior <<- edges$PriorSl2 #Habitat prior ( RCA slope)
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  
  ###----- Variance Generating Processes ------------------------------------------
  within_site <- 0.0003133684 / 1.96  # Prediction interval from oto vs. water regression. Pred intervals should be 2SD, analogous to CI which are 2SE
  analyt <- 0.00011 / 2  # Mean 2 S.D. of shell standard measurements during an LA run. Error from the machine
  within_pop <- within_site - analyt # Population error
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)  # COMBINED error 
  
  ###----- CREATE EMPTY MATRICES -------------------------------------------------
  assignment_matrix <- matrix(NA, nrow = length(edges$iso_pred), ncol = nrow(Natal_Origins))
  
  #############################
  ###### ASSIGNMENTS HERE ##### 
  #############################
  ## loop for assingments
  
  for (i in 1:nrow(Natal_Origins)) {
    
    iso_o <- Natal_Origins[i, "natal_iso"] %>% as.numeric()  # Otolith ratio
    gen.prior <- rep(0, length = length(pid_iso))
    gen.prior[LYsites] <- Natal_Origins$Lower[i]%>% as.numeric()
    gen.prior[MYsites] <- Natal_Origins$Middle[i] %>% as.numeric()
    gen.prior[UYsites] <- Natal_Origins$Upper[i] %>% as.numeric()
    
    StreamOrderPrior <<- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    
    #####. BAYES RULE ASSIGNMENT. ##################
    
    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(iso_o-pid_iso)^2/(2*error^2))) * pid_prior * gen.prior * StreamOrderPrior
    
    # normalize so all values sum to 1 (probability distribution)
    assign_norm <- assign / sum(assign) 
    
    #rescale so that all values are between 0 and 1 
    assign_rescaled <- assign_norm / max(assign_norm) 
    
    # If the rescaled value is less than the threshold, then set the same index in assign_norm to 0, otherwise 1 
    assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
    
    # Multiply by the CPUE weight 
    assign_rescaled_wt <- assign_rescaled * as.numeric(Natal_Origins[i, "COratio"])
    
    #assign_rescaled_wt<- assign_rescaled
    
    assignment_matrix[, i] <- assign_rescaled_wt

  }
  
  assignment_matrix<<- assignment_matrix
  edges<<- edges
  identifier<<- identifier
}



