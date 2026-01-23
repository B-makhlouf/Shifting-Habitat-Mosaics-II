# original shapefile 

orig<- st_read( "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_Reachbase_complete2.shp")
# new shapefile with added fields
new<- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_SlpDistkm.shp")

orig$Avg_Slop_1 <- new$Avg_Slop_1
orig$FlowLen_do <- new$FlowLen_do


# overwrite the original shapefile with the merged one
st_write(orig, "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_Reachbase_complete2.shp", delete_dsn = TRUE)

# now read that oen in just to double check 
check<- st_read("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_Reachbase_complete2.shp")

# histogram of Avg_Slop_1 to check values
hist(check$Avg_Slop_1, breaks = 50, main = "Histogram of Avg_Slop_1", xlab = "Avg_Slop_1")

# Save orig as the initial shapefile and over ride 
st_write(orig, "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/SpatialData/Kusko_Reachbase_complete2.shp", delete_dsn = TRUE)
