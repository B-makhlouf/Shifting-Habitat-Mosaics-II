library(classInt)

Yuk2015_assignment<- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv")

<- Yuk2017_assignment$Q1


################################################################################
##### Mapping using base R 
################################################################################


# breaks <- seq(min(basin_assign_norm), max(basin_assign_norm), length= 9)
breaks <- c(0, .1, .2, .4, .6, .8, .9, 1)
#breaks <- c(0, .3, .7, 1)
nclr <- length(breaks)
filename <- paste0(identifier, "_", sensitivity_threshold, "_.pdf")
filename <- "2015 Yukon Q1 .7.pdf"
filepath <- file.path(here("Figures", "Maps", filename))
pdf(file = filepath, width = 9, height = 6)
plotvar <- Yuk2017_assignment$Q1
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