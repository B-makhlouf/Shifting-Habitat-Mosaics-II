# This is a script to analyze patterns of production 
library(sf)

# Read in clean shapefiles 
yuk_edges<-st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp")

## Read in all the data for the full years of the Yukon 
yuk_2015_all<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.7_basin_norm.csv")) 
yuk_2016_all<- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.7_basin_norm.csv"))
yuk_2017_all<- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.7_basin_norm.csv"))
yuk_2019_all<- read_csv(here("Data/Production/Yukon/2019_full_Yukon_0.7_basin_norm.csv"))
yuk_2021_all<- read_csv(here("Data/Production/Yukon/2021_full_Yukon_0.7_basin_norm.csv"))

#Add stream order values to production estimates based on the indices of edges
yuk_2015_all$stream_order<- yuk_edges$Str_Order
yuk_2016_all$stream_order<- yuk_edges$Str_Order
yuk_2017_all$stream_order<- yuk_edges$Str_Order
yuk_2019_all$stream_order<- yuk_edges$Str_Order
yuk_2021_all$stream_order<- yuk_edges$Str_Order

#Summarize rescaled by Stream Order 
StrO_yuk_2015<- yuk_2015_all %>% 
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled)) %>%
  mutate(year = 2015)

StrO_yuk_2016<- yuk_2016_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled))%>%
  mutate(year = 2016)

StrO_yuk_2017<- yuk_2017_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled)) %>%
  mutate(year = 2017)

StrO_yuk_2019<- yuk_2019_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled)) %>%
  mutate(year = 2019)

StrO_yuk_2021<- yuk_2021_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled)) %>%
  mutate(year = 2021)


StrO_yuk_all_pivoted <- spread(StrO_yuk_all, key = year, value = summed_production)

year_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")

# Plotting
ggplot(StrO_yuk_all, aes(x = factor(year), y = summed_production, fill = factor(stream_order))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Production by Stream Order in Yukon",
       x = "Year",
       y = "Production",
       fill = "Stream Order") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

