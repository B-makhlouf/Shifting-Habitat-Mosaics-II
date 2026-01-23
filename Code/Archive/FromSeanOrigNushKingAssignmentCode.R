############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################

#isoscape rasters

iso_raster <- raster("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/isogrid.txt")
#par(mfrow = c(1, 1))
plot(iso_raster)
#writeRaster(iso_raster, "~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/glmssn1pred.grd", overwrite=TRUE)

isose_raster <- raster("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/isosegrid.txt")
#par(mfrow = c(1, 1))
plot(isose_raster)
#writeRaster(isose_raster, "~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/glmssn1predse.grd")


iso_reclass <- raster("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/alllakesprior.txt")
#par(mfrow = c(1, 1))
plot(iso_reclass)
#writeRaster(iso_reclass, "~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/iso_reclass.grd")

habitat <- raster("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/habitat_spawn.txt")
#par(mfrow = c(1, 1))
plot(iso_reclass)
#writeRaster(iso_reclass, "~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/iso_reclass.grd")

#sockprior <- raster("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/Rfolder/sock_prior.txt")


############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################


#rasters as matrices/vectors needed for assignment
iso_matrix<-as.matrix(iso_raster)
isose_matrix<-as.matrix(isose_raster)
prior_matrix<-as.matrix(iso_reclass)
habitat_matrix<-as.matrix(habitat)

#site IDs
sites<-which(iso_matrix!=-9999)

#values at sites
iso_val<-iso_matrix[sites]
isose_val<-isose_matrix[sites]
prior_val<-prior_matrix[sites]

habitat_val<-habitat_matrix[sites]
#habitat_val<-habitat_val/(1+2+3+4)
#habitat_val<-habitat_val*prior_val
#habitat_norm<-habitat_val/sum(habitat_val)

#combined error
within_site<-0.0003133684/1.96 #prediction interval from oto vs. water regression. Pred intervals should be 1.96SD, analagous to CI which are 2SE
analyt<-0.00011/2 #mean 2 S.D. of shell standard measurements during an LA run 
within_pop<- within_site - analyt
error <- sqrt((isose_val)^2 + (within_pop)^2 + (analyt)^2) # aka: error = isoscape SE + within-site + analytical
#error <- sqrt((isose_val)^2 + (0.0003133684)^2 + (0.00011)^2) # aka: error = isoscape SE + within-site + analytical
#error <- sqrt((0.00035)^2 + (within_pop)^2 + (analyt)^2) # aka: error = isoscape SE + within-site + analytical
#checks
head(error)
max(error)
min(error)

#otolith data
#data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2011_Nushagak/2011_NBK.csv")
#data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2014 Nushagak/Jeff_preprocessed_2014/2014 Chinook/Jenna_Chinook2014/For Sean - Nush Kings 2014/Rfolder_2014chinook/outputfiles/Nush_2014_natal_king.csv", row.names=1)
#data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2015_Nushagak/2015_NushagakChinookCleanedData/outputfiles/Nush_2015_natal_king.csv", row.names=1) 

#data<-data[data$Age<1.3 & data$Age>1.1,] #1.2 fish
#data<-data[data$Age<1.4 & data$Age>1.2,] #1.3 fish
#data<-data[data$Age<1.5 & data$Age>1.3,] #1.4 fish
#data<-data[data$Age<1.6 & data$Age>1.4,] #1.5 fish



#iso_oto_vector<-c(9,4,3)

#ID<-2
#n <- length(data[,iso_oto])
#output matrix filling up with assignments
#output_matrix<-matrix(NA, nrow=length(sites), ncol=length(data[,iso_oto]) )#each row is a unique cell in raster-matrix (collapsed into a vector form) and each column is for an individual

#age vector
age<-c(1.2,1.3,1.4)

#sub basin by age matrix
#ageclass_matrix<-matrix(NA,nrow=length(allshp), ncol=9)
#MS_matrix<-matrix(NA,nrow=length(allshp), ncol=10)
#rownames(ageclass_matrix) <- allshp
#rownames(MS_matrix) <- allshp
#colnames(ageclass_matrix) <- c("Binary","Summed_Probabilities","Habitat_prior","Habitat_only","2014","Habitat_rescaled","","","","")
#olnames(ageclass_matrix) <- c("2011.1.3","2011.1.4","2011.1.5", "2014.1.3","2014.1.4","2014.1.5", "2015.1.3","2015.1.4","2015.1.5")
#colnames(MS_matrix) <- c("Binary","Summed_Probabilities","","","","","","","","")

#data.list<-list("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2014 Nushagak/Jeff_preprocessed_2014/2014 Chinook/Jenna_Chinook2014/For Sean - Nush Kings 2014/Rfolder_2014chinook/outputfiles/Nush_2014_natal_king.csv",
#                "~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2015_Nushagak/2015_NushagakChinookCleanedData/outputfiles/Nush_2015_natal_king.csv")

#file.list<-c("2011_1_2.asc","2011_1_3.asc", "2011_1_4.asc", "2011_1_5.asc")
#file.list<-c("2014_1_2.asc","2014_1_3.asc", "2014_1_4.asc", "2014_1_5.asc")
file.list<-c("2015_1_2.asc","2015_1_3.asc", "2015_1_4.asc")


#iso_oto<-13 # for 2011  
#iso_oto<-4 # for 2014  
iso_oto<-3 #  for 2015

####################################
#for summed probability method
####################################
#u=1
#for (u in 1:ncol(ageclass_matrix)){  
#p=1
#for (p in 1:length(data.list)){
  
  #data<-read.csv(as.character(data.list[p]))
  #iso_oto<-iso_oto_vector[p]
  
  # j=2
          for (j in 1:length(age)){
            
            #data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2011_Nushagak/2011_NBK.csv")
            #data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2014 Nushagak/Jeff_preprocessed_2014/2014 Chinook/Jenna_Chinook2014/For Sean - Nush Kings 2014/Rfolder_2014chinook/outputfiles/Nush_2014_natal_king.csv")
            data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2015_Nushagak/2015_NushagakChinookCleanedData/outputfiles/Nush_2015_natal_king.csv") 
            
            #data<-read.csv(as.character(data.list[p]))
            data<-data[data$NewAge==age[j],]
            n <- length(data[,iso_oto])   
            output_matrix<-matrix(NA, nrow=length(sites), ncol=length(data[,iso_oto]) )
            
                  for (i in 1:n){
                    indv.data <- data[i,]
                    #inindv.id <- indv.data[1, ID]
                    assign <- (1/sqrt((2*pi*error^2))*exp(-1*(indv.data[1,iso_oto]-iso_val)^2/(2*error^2)))*prior_val#*habitat_val
                    #assign <- (1/sqrt((2*pi*error^2))*exp(-1*(indv.data[1,d2Htissue]-rescaled_raster)^2/(2*error^2)))
                    assign_norm <- assign/sum(assign) #so all pixels sum to 1
                    assign_max <- assign_norm/max(assign_norm) #rescale so all pixels range from 0 to 1
                    binary_thresh <- assign_max>0.5 #set probability threshold so all cells >0.5 equal 1 while all others equal 0 
                    #norm_thresh <- (binary_thresh/sum(binary_thresh))*1e10 #normalize each assignment location by number of total locations corresponding to each assignment
                    #output_matrix[,i]<-binary_thresh #assign_max  #norm_thresh
                    #output_matrix[,i]<-assign_max 
                    #prob_thresh <- assign_max>0.5 #set probability threshold so all cells >0.5 equal 1 while all others equal 0 
                    output_matrix[,i]<-binary_thresh
                  } 
                  
            #sum across columns
            output_sum <- apply(output_matrix, 1, sum)
            #normalize so entire basin of assignments sums to 1
            output_sum_norm <- output_sum/sum(output_sum)
            
            output_scaled <- (output_sum_norm/max(output_sum_norm))
            output_scaled <- (output_scaled+1)*10000
            
            # place fish back into original matrix
            # empty matrix of same dimensions
            matrix_sum<-matrix(NA,nrow=6685, ncol=9272)
            
            #place output_sum values into matrix
            #if you want only summed values
            #matrix_sum[sites]<-output_sum
            #if you want it normalized
            #matrix_sum[sites]<-output_sum_norm
            #if you want it scaled
            matrix_sum[sites]<-output_scaled
            
            ## S4 method for signature 'matrix'
            nfish_raster<-raster(matrix_sum, xmn=-370477.5, xmx=58740.44, ymn=934291, ymx=1243752, crs="+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", template=iso_raster)
            
                              #sub basin summary
                              #setwd("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/sub_basins/merged")
                              #shpList <- list.files(recursive=T,pattern=glob2rx("*.shp"))
                              #allshp<- dir(pattern=glob2rx("*.shp"))
                              
                              #sub basin summary in a loop
                              #     q=1  
                              #for (q in 1:length(allshp)){
                                
                                #sub_basin <- shapefile(allshp[q])
                                #proj4string(sub_basin)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
                                
                                #clip<-mask(nfish_raster,sub_basin)
                                #basin_sum<-cellStats(clip, "sum")
                                
                                #ageclass_basin<-matrix(NA,nrow=length(allshp),ncol=1)
                                #ageclass_basin[q]<-basin_sum
                                
                             # } 
                              
                             # ageclass_ryear<-matrix(NA,nrow=length(allshp),ncol=length(age))
                             # ageclass_ryear[,j]<-ageclass_basin 
            
            setwd("/Volumes/mac/Nush_king_ageclass_binary")
            writeRaster(nfish_raster,  file=file.list[j] ,overwrite=TRUE, format="ascii")
            #write.table(ageclass_ryear, file=file.list[p] ,sep = ",", row.names=allshp, col.names=age) 
            
            }
          
  
#}

#}  
#####################################
#####################################

#sum across columns
output_sum <- apply(output_matrix, 1, sum)
#normalize so entire basin of assignments sums to 1
output_sum_norm <- output_sum/sum(output_sum)

max(output_sum)
#sum(output_sum)
sum(output_sum_norm)
#max(output_sum_norm)

#output_scaled <- (output_sum_norm/max(output_sum_norm))
#output_scaled <- (output_scaled+1)*10000

#max P
#output_max <- apply(output_matrix, 1, max)

#####################################
#####################################
# place fish back into original matrix
# empty matrix of same dimensions
matrix_sum<-matrix(NA,nrow=6685, ncol=9272)
#####################################
#####################################

#place output_sum values into matrix
#if you want only summed values
#matrix_sum[sites]<-output_sum
#if you want it normalized
matrix_sum[sites]<-output_sum_norm
#if you want it scaled
#matrix_sum[sites]<-output_scaled

# only habitat suitability
#matrix_sum[sites]<-habitat_norm

#max P into matrix
#matrix_max<-matrix(NA,nrow=6685, ncol=9272)
#matrix_max[sites]<-output_max

#single P-surface
#matrix_single<-matrix(NA,nrow=6685, ncol=9272)
#matrix_single[sites]<-assign_max

#################################
###   matrix to raster   ########
#################################
## S4 method for signature 'matrix'
nfish_raster<-raster(matrix_sum, xmn=-370477.5, xmx=58740.44, ymn=934291, ymx=1243752, crs="+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", template=iso_raster)

#habitat_norm_raster<-raster(matrix_sum, xmn=-370477.5, xmx=58740.44, ymn=934291, ymx=1243752, crs="+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", template=iso_raster)

#2014
#writeRaster(nfish_raster, "~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2014 Nushagak/Jeff_preprocessed_2014/2014 Chinook/Jenna_Chinook2014/For Sean - Nush Kings 2014/Rfolder_2014chinook/outputfiles/2014QCKing.asc",overwrite=TRUE, format="ascii")
#2015
#writeRaster(nfish_raster, "~/Documents/1_UAF PhD/8_UW_POSTDOC/Data/Nushagak/Otoliths/2015_Nushagak/2015_NushagakChinookCleanedData/outputfiles/2015QCKing.asc",overwrite=TRUE, format="ascii")



############################################################################################################
###  Sub basin summarys   ##################################################################################
############################################################################################################

#total run of Chinook in 2011 = 107,989. Pg.89, 2011 ADFG Report BB report

setwd("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/sub_basins/merged")

#shpList <- list.files(recursive=T,pattern=glob2rx("*.shp"))
allshp<- dir(pattern=glob2rx("*.shp"))
length(allshp)

basin_matrix<-matrix(NA,nrow=length(allshp), ncol=3)
#MS_matrix<-matrix(NA,nrow=length(allshp), ncol=10)
rownames(basin_matrix) <- allshp
#rownames(MS_matrix) <- allshp
#colnames(basin_matrix) <- c("Binary","Summed_Probabilities","Habitat_prior","Habitat_only","2014","Habitat_rescaled","","","","")
colnames(basin_matrix) <- c("2011","2014","2015")
#colnames(MS_matrix) <- c("Binary","Summed_Probabilities","","","","","","","","")

#project assignment raster
proj4string(nfish_raster)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
plot(nfish_raster)

#read in and project polygon shapefile
LR<-shapefile("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/sub_basins/merged/LowerRiver_mainstemMrg.shp")
Ko<-shapefile("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/sub_basins/merged/KoktuliMrg.shp")

proj4string(LR)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
plot(LR, add=TRUE )

proj4string(Ko)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
plot(Ko, add=TRUE )

un<-union(LR, Ko)
plot(un, add=TRUE)


#clip and mask for summing
#LR_clip<-mask(nfish_raster,LR)
#cellStats(LR_clip, "sum")

#in a loop
for (i in 1:length(allshp)){
  sub_basin <- shapefile(allshp[i])
  proj4string(sub_basin)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  #nfish_raster<-raster(matrix_sum, xmn=-370477.5, xmx=58740.44, ymn=934291, ymx=1243752, crs="+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", template=iso_raster)
  #proj4string(nfish_raster)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  
  clip<-mask(nfish_raster,sub_basin)
  basin_sum<-cellStats(clip, "sum")
  #basin_matrix[i,1]<-basin_sum #assign_max  #norm_thresh
  basin_matrix[i,"2014"]<-basin_sum #assign_max  #norm_thresh
} 

#sum(basin_matrix[,2])

#basin_binary<-cbind(as.numeric(basin_matrix[,1]), basin_matrix[,2])
#sum(bas_sum)-(0.0186866973765314+0.0426477775467851)

plot(nfish_raster)
plot(sub_basin, add=TRUE)

write.table(basin_matrix, file="habitatonly_2014.csv",sep = ",") 
write.table(MS_matrix, file="sub_basin_matrix_summary_mainstemonly.csv",sep = ",") 
basin_matrix<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/sub_basins/merged/sub_basin_matrix_summary_with_stream_as_proportion.csv")

basin.data<-read.csv("~/Documents/1_UAF PhD/8_UW_POSTDOC/STARS_SSN/Rfolder/sub_basins/sub_basin_summary.csv")
mean(abs(basin.data$diff))
sd(abs(basin.data$diff))