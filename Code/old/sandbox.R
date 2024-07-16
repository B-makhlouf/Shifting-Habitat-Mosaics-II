
full2017<- read.csv(here("Outputs/Assignment Matrix/full_2017_Kusko_0.75.csv"))
full2018<- read.csv(here("Outputs/Assignment Matrix/full_2018_Kusko_0.75.csv"))

#Combine into one data frame, with collumns labeled 2017 and 2018 
full_2017_2018<- bind_cols(full2017, full2018)

#rename collumns
colnames(full_2017_2018)<- c("2017", "2018")

#read in the kusko shapefile 
kusk_edges<- st_read("/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")

kusk_edges$prod2017<- full_2017_2018$`2017`

#plot with ggplot
ggplot()+
  geom_sf(data = kusk_edges, aes(color = prod2017))+
  scale_fill_viridis_c()+
  theme_minimal()+
  theme(legend.position = "bottom") 
