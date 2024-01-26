#This script produced maps of CV for all years with different thresholds 

library(classInt)
library(RColorBrewer)
library(sf)

yuk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/UpdatedSSN_20190410/Results/yukon_edges_20191011_2015earlyStrata_acc.shp")
basin<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_final_alb.shp")
StreamOrderPrior <- as.numeric(yuk_edges$Str_Order > 2)
pid_prior <- yuk_edges$PriorSl2 #Habitat prior ( RCA slope)


generate_cv_plot <- function(threshold) {
  # Read in production data with CV metrics
  CV_data <- read_csv(here("Results/Variability", paste0("Yukon_Production_all_", threshold, ".csv")))
  
  
  # Extract the variation vector and handle NAs
  variation <- CV_data$cv
  variation[is.na(variation)] <- 0
  
  # Calculate breaks
  breaks <- seq(min(variation), max(variation), length = 9)
  if (max(variation) > max(breaks)) {
    breaks <- seq(min(variation[variation > 0]), max(variation), length = 10)
  }
  
  # Number of colors
  nclr <- length(breaks)
  
  # Filepath for saving the plot
  filename <- paste0("CV_", threshold, "_.pdf")
  filepath <- file.path(here("Figures", "Maps", "Yukon", filename))
  
  # Plotting setup
  pdf(file = filepath, width = 9, height = 6)
  plotvar <- variation
  class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = breaks, dataPrecision = 2)
  plotclr <- brewer.pal(nclr, "YlOrRd")
  colcode <- findColours(class, plotclr, digits = 2)
  colcode[plotvar == 0] <- 'gray80'
  
  # Additional code for the plot (assuming basin, yuk_edges, StreamOrderPrior, and pid_prior are defined)
  colcode[which(StreamOrderPrior == 0)] <- 'gray60'
  colcode[which(pid_prior == 0)] <- 'gray60'
  plot(st_geometry(basin), col = 'gray60', border = 'gray48', main = threshold)
  plot(st_geometry(yuk_edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = ifelse(plotvar == 0, 0.05, .6 * (exp(plotvar) - 1)))
  
  # Close the plot device
  dev.off()
}

thresholds <- c("0.4","0.5", "0.6", "0.7", "0.8", "0.9" ) 

for (threshold in thresholds) {
  generate_cv_plot(threshold)
}
