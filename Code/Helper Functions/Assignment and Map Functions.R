library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(classInt)
library(RColorBrewer)
library(fabricatr)
library(ggplot2)
library(sf)

Basin_prov_assign <- function(region, year, sensitivity_threshold) {  
  # Determine file paths and parameters based on the region
  if (region == "Yukon") {
    shapefile_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp"
    natal_origins_path <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/03_Extracted Data/Natal Origins/Cleaned/Yukon_", year, "_Cleaned_Natal_Origins.csv")
    CPUE_path <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/04_CPUE Data/CPUE Weights/", year, "_Yukon_CPUE weights.csv")
    genetics_path <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/02_Genetic Data/03_Genetic Prior/", year, " Yukon_genetic_prior_.csv")
    shapefile_paths <- list(
      ly = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_LYGen.shp",
      my = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_MYGen.shp",
      uy = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/edges_UYGen.shp"
    )
    prior_col <- "PriorSl2"
  } else if (region == "Kuskokwim") {
    shapefile_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp"
    natal_origins_path <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/03_Extracted Data/Natal Origins/Cleaned/Kuskokwim_", year, "_Cleaned_Natal_Origins.csv")
    CPUE_path <- paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/04_CPUE Data/CPUE Weights/", year, "_Kusko_CPUE weights.csv")
    prior_col <- "UniPh2oNoE"
  } else {
    stop("Invalid region specified. Choose 'Yukon' or 'Kuskokwim'.")
  }
  
  # Load shapefiles and data
  edges <- st_read(shapefile_path)
  natal_origins <- read.csv(natal_origins_path)
  CPUE <- read.csv(CPUE_path, sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% unlist() %>% as.numeric()
  
  # If Yukon, load genetic data and shape files
  if (region == "Yukon") {
    Genetics <- read.csv(genetics_path)
    ly.gen <- st_read(shapefile_paths$ly, quiet = TRUE)
    ly.gen_reachid <- ly.gen$reachid
    my.gen <- st_read(shapefile_paths$my, quiet = TRUE)
    my.gen_reachid <- my.gen$reachid
    uy.gen <- st_read(shapefile_paths$uy, quiet = TRUE)
    uy.gen_reachid <- uy.gen$reachid
    
    edges$GenLMU <- 0
    edges$GenLMU[edges$reachid %in% ly.gen_reachid] <- "lower"
    edges$GenLMU[edges$reachid %in% my.gen_reachid] <- "middle"
    edges$GenLMU[edges$reachid %in% uy.gen_reachid] <- "upper"
    LYsites <- which(edges$GenLMU == "lower")
    MYsites <- which(edges$GenLMU == "middle")
    UYsites <- which(edges$GenLMU == "upper")
  }
  
  # Extract isoscape prediction and error values
  pid_iso <- edges$iso_pred
  pid_isose <- edges$isose_pred
  if (region == "Yukon") {
    pid_isose <- ifelse(pid_isose < 0.0031, 0.003, pid_isose)
  } else if (region == "Kuskokwim") {
    #Temporarily make all pid_isose values .0005 to avoid errors
    pid_isose <- ifelse(pid_isose != .0005, .0005, pid_isose)
    #pid_isose[pid_isose < .0005] <- .0005
  }
  pid_prior <- edges[[prior_col]]
  
  # Variance Generating Processes
  within_site <- 0.0003133684 / 1.96
  analyt <- 0.00011 / 2
  within_pop <- within_site - analyt
  error <- sqrt(pid_isose^2 + within_site^2 + analyt^2)
  
  # Filter to various quartiles
  natal_origins$quartile <- cut(natal_origins$Days_into_run, breaks = quantile(natal_origins$Days_into_run, probs = c(0, 0.25, 0.5, 0.75, 1)), labels = c("Q1", "Q2", "Q3", "Q4"))
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
      iso_o <- filtered_data[i, "natal_iso"] %>% as.numeric()
      
      if (region == "Yukon") {
        genP <- Genetics[i, ]
        gen.prior <- rep(0, length = length(pid_iso))
        
        gen.prior[LYsites] <- genP[3] %>% as.numeric()
        gen.prior[MYsites] <- genP[4] %>% as.numeric()
        gen.prior[UYsites] <- genP[5] %>% as.numeric()
        
        StreamOrderPrior <- as.numeric(edges$Str_Ord > 2)
        
        assign <- (1 / sqrt((2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2))) * pid_prior * gen.prior * StreamOrderPrior
      } else if (region == "Kuskokwim") {
        StreamOrderPrior <- as.numeric(edges$Strahler > 2)
        
        assign <- (1 / sqrt((2 * pi * error^2)) * exp(-1 * (iso_o - pid_iso)^2 / (2 * error^2))) * pid_prior * StreamOrderPrior
      }
      
      
      assign_norm <- assign / sum(assign)
      #assign_norm <- assign_norm * CPUE[i]
      
      # Ensure non-empty assign_norm
      if (length(assign_norm) == 0 || all(is.na(assign_norm))) {
        assign_rescaled <- rep(0, length(pid_iso))
      } else {
        assign_rescaled <- assign_norm / max(assign_norm, na.rm = TRUE)
        assign_rescaled <- ifelse(is.na(assign_rescaled), 0, assign_rescaled)
      }
      
      assign_rescale_removed <- ifelse(assign_rescaled >= sensitivity_threshold, assign_rescaled, 0)
      
      # Ensure non-empty assign_rescale_removed
      if (length(assign_rescale_removed) == 0) {
        assign_rescale_removed <- rep(0, length(pid_iso))
      }
      
      assignment_matrix[, i] <- assign_rescale_removed
      assignment_matrix[is.na(assignment_matrix)] <- 0
    }
    
    basin_assign_sum <- apply(assignment_matrix, 1, sum)
    basin_assign_rescale <- basin_assign_sum / sum(basin_assign_sum)
    
    sum(basin_assign_rescale)

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
  
  #export as a csv 
  write.csv(result_df, file = paste0("Outputs/Assignment Matrix/", year, "_", region, "_", sensitivity_threshold, ".csv"))
  
  return(result_df)
  

}



################################################################################
################################################################################

# Functions for mapping 

library(sf)
library(classInt)
library(RColorBrewer)

Map_Base <- function(River, plotvar, identifier, sensitivity_threshold) {
  # Load the appropriate shapefiles based on the River parameter
  if (River == "Yukon") {
    edges_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp"
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
    StreamOrderPrior <- as.numeric(edges$Str_Ord > 2)
    pid_prior <- edges$PriorSl2
  } else if (River == "Kuskokwim") {
    StreamOrderPrior <- as.numeric(edges$Strahler > 2)
    pid_prior <- edges$UniPh2oNoE
  }
  
  # Define breaks and color palette for the plot
  breaks <- c(0, .1, .2, .4, .6, .8, .9, 1)
  nclr <- length(breaks)
  filename <- paste0(identifier, "_", sensitivity_threshold, ".pdf")
  filepath <- file.path(here("Basin Maps", filename))
  
  pdf(file = filepath, width = 9, height = 6)
  class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
  plotclr <- brewer.pal(nclr, "YlOrRd")
  colcode <- findColours(class, plotclr, digits = 2)
  
  # Adjust colors based on conditions
  colcode[plotvar == 0] <- 'gray60'
  colcode[plotvar < .2] <- 'gray80'
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  
  # Plot the basin and edges with the appropriate colors and line widths
  plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = identifier)
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .6 * (exp(plotvar) - 1)))
  dev.off()
}



