
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

