
library(sf)             # For spatial data handling
library(dplyr)          # For data manipulation
library(ggplot2)        # For plotting
library(RColorBrewer)   # For color palettes
library(scales)         # For rescaling values
library(here)           # For path management
library(grid)           # For viewport and grid graphics
library(gridExtra)      # For arranging multiple plots
library(tidyverse)
library(tidyr)


# # Test parameters
# year <- 2021
# sensitivity_threshold <- 0.001
# min_error <- 0.0003
# min_stream_order <- 3
# filter_by <- "DOY_Q"
# HUC <- 8
# watershed<- "Kusko"
# 



ALL_Map_func_Quartile <- function(year, sensitivity_threshold, min_error, min_stream_order = 3, filter_by = c("DOY_Q", "CPUE_Q"),HUC = 8) {
  
  # Generate identifier for output files
  identifier <- paste(year, watershed , sep = "_")
  
  ############# 
  ###### Load Shapefiles 
  #############
  
  if (watershed == "Kusko"){
  
  edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
  basin <- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp")
  
  } else if (watershed == "Yukon"){
  
  edges<- st_read("/Users/benjaminmakhlouf/Downloads/Results/yukon_edges_20191011_2015earlyStrata_acc.shp") 
  basin<<- st_read("/Users/benjaminmakhlouf/Spatial Data/Basin Map Necessary Shapefiles/Yuk_Mrg_final_alb.shp")
    
  }

  edges <- st_transform(edges, st_crs(basin))
  edges <- edges[edges$Str_Order >= min_stream_order,] #filter the stream network to only those above the min stream order 
  
  Huc <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC",HUC,"_Trimmed.shp"))
  Huc <- st_transform(Huc, st_crs(basin))

  #############
  ##### Natal Origin Data
  #############
  
  # Load natal origins data
  Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_",watershed,"_Natal_Origins_Genetics_CPUE.csv"))
  Natal_Origins <- Natal_Origins[!is.na(Natal_Origins$natal_iso) & !is.na(Natal_Origins$dailyCPUEprop),] #clean
  
  ### Split data into quartiles based on the filter type 
  
  # Initialize filter variables
  quartile_subsets <- list()
  x_var <- "DOY"
  x_lab <- "Day of Year"
  doy_breaks <- NULL
  cpue_breaks <- NULL
  
  ## DOY (Day of Year) Processing
  
  if (filter_by == "DOY_Q") {
    
    # Split DOY into 4 equal time chunks (quartiles)
    full_doy_range <- range(Natal_Origins$DOY, na.rm = TRUE)
    doy_breaks <- seq(full_doy_range[1], full_doy_range[2], length.out = 5)  # 5 points makes 4 intervals
    
    # Create 4 quartile subsets based on DOY ranges
    quartile_subsets <- lapply(1:4, function(i) {
      Natal_Origins %>% 
        filter(DOY >= doy_breaks[i] & DOY < doy_breaks[i+1])
    })
    
  } else if (filter_by == "CPUE_Q") {
    
    # Calculate cumulative CPUE proportions to split by run timing
    Natal_Origins <- Natal_Origins %>% 
      arrange(DOY) %>% 
      mutate(
        CumCPUE = cumsum(dailyCPUEprop),
        TotalCPUE = sum(dailyCPUEprop),
        CumProp = CumCPUE / TotalCPUE  # Cumulative proportion of run
      )
    
    # Find DOY values at 25%, 50%, and 75% of total run
    cpue_breaks <- sapply(c(0.25, 0.5, 0.75), function(p) {
      Natal_Origins$DOY[which.min(abs(Natal_Origins$CumProp - p))]
    })
    
    # Create 4 quartile subsets based on CPUE proportions
    quartile_subsets <- list(
      # Q1: Start to 25% of run
      Natal_Origins %>% filter(DOY <= cpue_breaks[1]),
      
      # Q2: 25% to 50% of run
      Natal_Origins %>% filter(DOY > cpue_breaks[1] & DOY <= cpue_breaks[2]),
      
      # Q3: 50% to 75% of run
      Natal_Origins %>% filter(DOY > cpue_breaks[2] & DOY <= cpue_breaks[3]),
      
      # Q4: 75% to end of run
      Natal_Origins %>% filter(DOY > cpue_breaks[3])
    )
    
  } else {  # "none" case - no filtering
    quartile_subsets <- list(Natal_Origins)
  }
  
  
  
  ############## At this point, the natal origin data has been split into quartiles based on the filter variable

  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  pid_isose_mod <- ifelse(pid_isose < min_error, min_error, pid_isose)
  
  
  if (watershed == "Kusko") {
    pid_prior <- edges$UniPh2oNoE
    StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
  } else if (watershed == "Yukon") {
    pid_prior <- edges$PriorSl2
    StreamOrderPrior <- ifelse(edges$Str_Order >= min_stream_order, 1, 0)
    
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

  }
  

  
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  error <- sqrt(pid_isose_mod^2 + within_site^2 + analyt^2)
  
  
  for (q in 1:length(quartile_subsets)) { #for each subset... 
    
    current_subset <- quartile_subsets[[q]]
   
    identifier2<- paste0(watershed,"_",year,"_",filter_by,q)
    
    assignment_matrix <- matrix(NA, nrow = length(pid_iso), ncol = nrow(current_subset))
    
    for (i in 1:nrow(current_subset)) {
      iso_o <- current_subset$natal_iso[i]
      
      
      if (watershed == "Kusko") {
        
        assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * pid_prior * StreamOrderPrior
        
      } else if (watershed == "Yukon") {
        
        gen.prior <- rep(0, length = length(pid_iso))
        gen.prior[LYsites] <- Natal_Origins$Lower[i]%>% as.numeric()
        gen.prior[MYsites] <- Natal_Origins$Middle[i] %>% as.numeric()
        gen.prior[UYsites] <- Natal_Origins$Upper[i] %>% as.numeric()
        
        assign <- (1/sqrt(2*pi*error^2)) * exp(-1*(iso_o - pid_iso)^2/(2*error^2)) * pid_prior * StreamOrderPrior * gen.prior
        
      }
      
      assign_norm <- assign / sum(assign)
      assign_rescaled <- assign_norm / max(assign_norm)
      assign_rescaled[assign_rescaled < sensitivity_threshold] <- 0
      assignment_matrix[,i] <- assign_rescaled * current_subset$COratio[i]
    }
    
    basin_assign_sum <- apply(assignment_matrix, 1, sum, na.rm = TRUE)
    basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum)
    basin_assign_norm <- basin_assign_rescale / max(basin_assign_rescale)
    
    ######### MAP FUNCTIONS
    
    # Tribs 
    TRIB_MAP(basin_assign_norm, identifier2, edges, basin)
    
    
    # # HUC
    HUC_MAP(basin_assign_rescale, identifier2, edges, basin, Huc)
    
    
  }
}



