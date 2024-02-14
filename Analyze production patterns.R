# This is a script to analyze patterns of production 
library(sf)

# Read in clean shapefiles 
yuk_edges<-st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_cleaned.shp")

## Read in all the data for the full years of the Yukon 
yuk_2015_all<- read_csv(here("Data/Production/Yukon/2015_full_Yukon_0.5_basin_norm.csv")) 
yuk_2016_all<- read_csv(here("Data/Production/Yukon/2016_full_Yukon_0.5_basin_norm.csv"))
yuk_2017_all<- read_csv(here("Data/Production/Yukon/2017_full_Yukon_0.5_basin_norm.csv"))
yuk_2019_all<- read_csv(here("Data/Production/Yukon/2019_full_Yukon_0.5_basin_norm.csv"))
yuk_2021_all<- read_csv(here("Data/Production/Yukon/2021_full_Yukon_0.5_basin_norm.csv"))

#Add stream order values to production estimates based on the indices of edges
yuk_2015_all$stream_order<- yuk_edges$Str_Order
yuk_2016_all$stream_order<- yuk_edges$Str_Order
yuk_2017_all$stream_order<- yuk_edges$Str_Order
yuk_2019_all$stream_order<- yuk_edges$Str_Order
yuk_2021_all$stream_order<- yuk_edges$Str_Order

#Add Stream Length values 
yuk_2015_all$stream_length<- yuk_edges$Shape_Leng
yuk_2016_all$stream_length<- yuk_edges$Shape_Leng
yuk_2017_all$stream_length<- yuk_edges$Shape_Leng
yuk_2019_all$stream_length<- yuk_edges$Shape_Leng
yuk_2021_all$stream_length<- yuk_edges$Shape_Leng

#Add a proportional stream length collumn 
yuk_2015_all$prop_length<- yuk_2015_all$stream_length/sum(yuk_2015_all$stream_length)
yuk_2016_all$prop_length<- yuk_2016_all$stream_length/sum(yuk_2016_all$stream_length)
yuk_2017_all$prop_length<- yuk_2017_all$stream_length/sum(yuk_2017_all$stream_length)
yuk_2019_all$prop_length<- yuk_2019_all$stream_length/sum(yuk_2019_all$stream_length)
yuk_2021_all$prop_length<- yuk_2021_all$stream_length/sum(yuk_2021_all$stream_length)


#Combine all
yuk_all<- rbind(yuk_2015_all, yuk_2016_all, yuk_2017_all, yuk_2019_all, yuk_2021_all)

high_yuk_all <- yuk_all %>%
  filter(normalized > .8)

#Plot production vs proportional stream length 
ggplot(high_yuk_all, aes(x = prop_length, y = normalized, color = factor(stream_order))) +
  geom_point() +
  labs(title = "Production vs Proportional Stream Length in Yukon",
       x = "Proportional Stream Length",
       y = "Production",
       color = "Stream Order") +
  theme_minimal() +
  theme(legend.position = "none")

#############

#Summarize rescaled by Stream Order 
StrO_yuk_2015<- yuk_2015_all %>% 
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled),
            summed_prop_length = sum(prop_length)) %>%
  mutate(year = 2015)

StrO_yuk_2016<- yuk_2016_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled),
            summed_prop_length = sum(prop_length)) %>%
  mutate(year = 2016)

StrO_yuk_2017<- yuk_2017_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled),
            summed_prop_length = sum(prop_length)) %>%
  mutate(year = 2017)

StrO_yuk_2019<- yuk_2019_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled),
            summed_prop_length = sum(prop_length)) %>%
  mutate(year = 2019)

StrO_yuk_2021<- yuk_2021_all %>%
  group_by(stream_order) %>% 
  summarize(summed_production = sum(rescaled),
            summed_prop_length = sum(prop_length)) %>%
  mutate(year = 2021)


#combine 2015,2016, and 2021
StrO_yuk_all<- rbind(StrO_yuk_2015, StrO_yuk_2016, StrO_yuk_2021)

StrO_yuk_all<- StrO_yuk_all %>% 
  mutate(dif_prop_length = summed_production - summed_prop_length)

# Plotting
ggplot(StrO_yuk_all, aes(x = factor(year), y = summed_production, fill = factor(stream_order))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Production by Stream Order in Yukon",
       x = "Year",
       y = "Production",
       fill = "Stream Order") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Pick out only values greater than .7 from normalized
high_prod_yuk15<- yuk_2015_all %>% 
  arrange(desc(rescaled)) %>% 
  filter(normalized > .6) %>%
  mutate(year = 2015)

high_prod_yuk16<- yuk_2016_all %>% 
  arrange(desc(rescaled)) %>% 
  filter(normalized > .6) %>%
  mutate(year = 2016)

high_prod_yuk17<- yuk_2017_all %>%
  arrange(desc(rescaled)) %>% 
  filter(normalized > .6) %>%
  mutate(year = 2017)

high_prod_yuk19<- yuk_2019_all %>%
  arrange(desc(rescaled)) %>% 
  filter(normalized > .6) %>%
  mutate(year = 2019)

high_prod_yuk21<- yuk_2021_all %>%
  arrange(desc(rescaled)) %>% 
  filter(normalized > .6) %>%
  mutate(year = 2021)

#Combine all the high production values

high_prod_yuk_all<- rbind(high_prod_yuk15, high_prod_yuk16, high_prod_yuk17, high_prod_yuk19, high_prod_yuk21)

# Plotting

ggplot(high_prod_yuk_all, aes(x = year, y = rescaled, fill = factor(stream_order))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "High Production by Stream Order in Yukon",
       x = "Year",
       y = "Production",
       fill = "Stream Order") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


all_resc_df<- data_frame ( resc_2015 = yuk_2015_all$rescaled,
                           resc_2016 = yuk_2016_all$rescaled,
                           resc_2021 = yuk_2021_all$rescaled,)

# Calculate the coefficient of variation across all rows 
all_resc_df$cv<- apply(all_resc_df, 1, sd)/apply(all_resc_df, 1, mean)

# Calculate the mean production values across all
all_resc_df$mean_prod <- (all_resc_df$resc_2015 + all_resc_df$resc_2016 + all_resc_df$resc_2021)/3

#normalize the mean production values 
all_resc_df$norm_mean_prod<- (all_resc_df$mean_prod - min(all_resc_df$mean_prod))/(max(all_resc_df$mean_prod) - min(all_resc_df$mean_prod))

# Add in stream order and stream length variables 
all_resc_df$stream_order<- yuk_2015_all$stream_order
all_resc_df$prop_length<- yuk_2015_all$prop_length

#plot norm mean production vs cv

all_resc_df <- all_resc_df %>%
  filter(norm_mean_prod > .4)

ggplot(all_resc_df, aes(x = norm_mean_prod, y = cv, color = stream_order)) +
  geom_point(alpha = 0.7) +
  labs(title = "Normalized Mean Production vs Coefficient of Variation in Yukon",
       x = "Production",
       y = "CV") +
  facet_wrap(~stream_order, scales = "free")




