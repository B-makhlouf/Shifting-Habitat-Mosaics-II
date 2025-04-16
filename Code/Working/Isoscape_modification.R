############ This script is to explore the result of modifying isoscapes

library(here)
library(sf)
library(dplyr)

# Yukon Isoscape
Yukon_isoscape<- st_read("/Users/benjaminmakhlouf/Spatial Data/Shapefiles/AYK Shapefiles/Yukon_cleaned.shp")

Yukon_isoscape_filtered<- Yukon_isoscape %>% 
  filter(PriorSl2 != 0)

# Remove all stream orders below 4
Filtered_Yukon4<- Yukon_isoscape %>% 
  filter(Str_Order >= 4) %>%
  filter(PriorSl2 != 0)

# Remove all stream orders below 5
Filtered_Yukon5<- Yukon_isoscape %>% 
  filter(Str_Order >= 5) %>%
  filter(PriorSl2 != 0)

# Remove all stream orders below 6
Filtered_Yukon6<- Yukon_isoscape %>% 
  filter(Str_Order >= 6) %>%
  filter(PriorSl2 != 0)

# Save the shapefiles 
st_write(Filtered_Yukon4, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Yukon_filteredStrO4.shp")
st_write(Filtered_Yukon5, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Yukon_filteredStrO5.shp")
st_write(Filtered_Yukon6, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Yukon_filteredStrO6.shp")


# Create ggplot histograms of iso_pred for all four shapefiles 
library(ggplot2)

# Yukon Isoscape

ggplot(Yukon_isoscape_filtered, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Yukon Isoscape",
       x = "Iso_pred",
       y = "Frequency")

# Filtered Yukon 4

ggplot(Filtered_Yukon4, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered Yukon 4 Isoscape",
       x = "Iso_pred",
       y = "Frequency")

# Filtered Yukon 5

ggplot(Filtered_Yukon5, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered Yukon 5 Isoscape",
       x = "Iso_pred",
       y = "Frequency")

# Display these all in one figure using cowplot, with the same x and y axis limits
library(cowplot)

p1 <- ggplot(Yukon_isoscape_filtered, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Original (habitat prior removed)",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(.700, .750) 

p2 <- ggplot(Filtered_Yukon4, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 4",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(.700, .750)

p3 <- ggplot(Filtered_Yukon5, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 5",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(.700, .750) 

p4 <- ggplot(Filtered_Yukon6, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered SrOrd >=6",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(.700, .750)

plot_grid(p1, p2, p3, p4, ncol = 1, nrow = 4)

##### SAME plots, but for isose 

p1_se<- ggplot(Yukon_isoscape_filtered, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Original (habitat prior removed)",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(0, 0.0075)

p2_se<- ggplot(Filtered_Yukon4, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 4",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(0, 0.0075)

p3_se<- ggplot(Filtered_Yukon5, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 5",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(0, 0.0075)

p4_se<- ggplot(Filtered_Yukon6, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered SrOrd >=6",
       x = "Iso_pred",
       y = "Frequency") +
  xlim(0, 0.0075)

plot_grid(p1_se, p2_se, p3_se, p4_se, ncol = 1, nrow = 4)

# Make one big figure with all the plots in two columns and the same shapefiles side by side 
final_plot<-plot_grid(p1, p1_se, p2, p2_se, p3, p3_se, p4, p4_se, ncol = 2, nrow = 4)

title <- ggdraw() + 
  draw_label("Yukon Isoscape", fontface = "bold", size = 16, hjust = 0.5)

# Combine title and plot
plot_with_title <- plot_grid(title, final_plot, ncol = 1, rel_heights = c(0.1, 1))

# Display the final plot
plot_with_title

############ Kuskokwim 

Kusko_isoscape<- st_read("/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Kusko_cleaned.shp")

Kusko_isoscape_filtered<- Kusko_isoscape %>% 
  filter( UniPh2oNoE != 0)

# Remove all stream orders below 3
Filtered_Kusko3<- Kusko_isoscape %>% 
  filter(Str_Order >= 3) %>%
  filter(UniPh2oNoE != 0)

# Remove all stream orders below 4
Filtered_Kusko4<- Kusko_isoscape %>% 
  filter(Str_Order >= 4) %>%
  filter(UniPh2oNoE != 0)

# Remove all stream orders below 5
Filtered_Kusko5<- Kusko_isoscape %>% 
  filter(Str_Order >= 5) %>%
  filter(UniPh2oNoE != 0)

# Remove all stream orders below 6
Filtered_Kusko6<- Kusko_isoscape %>% 
  filter(Str_Order >= 6) %>%
  filter(UniPh2oNoE != 0)

# Save the shapefiles
st_write(Filtered_Kusko3, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Kusko_filteredStrO3.shp")
st_write(Filtered_Kusko4, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Kusko_filteredStrO4.shp")
st_write(Filtered_Kusko5, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Kusko_filteredStrO5.shp")
st_write(Filtered_Kusko6, "/Users/benjaminmakhlouf/Spatial Data/Cleaned AYK Shapefiles/Filtered StrO/Kusko_filteredStrO6.shp")

# Kusko Isoscape
p1_kusko<- ggplot(Kusko_isoscape_filtered, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Original (Habitat Prior Removed)",
       x = "Iso_pred",
       y = "Frequency")+ 
  xlim(.703, .714)

# Filtered Kusko 3
p2_kusko<- ggplot(Filtered_Kusko3, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 3",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(.703, .714)

# Filtered Kusko 4
p3_kusko<- ggplot(Filtered_Kusko4, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 4",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(.703, .714)

# Filtered Kusko 5
p4_kusko<- ggplot(Filtered_Kusko5, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 5",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(.703, .714)

# Filtered Kusko 6
p5_kusko<- ggplot(Filtered_Kusko6, aes(x = iso_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 6",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(.703, .714)

plot_grid(p1_kusko, p2_kusko, p3_kusko, p4_kusko, p5_kusko, ncol = 1, nrow = 5)


# Same plots but for isose

p1_kusko_se<- ggplot(Kusko_isoscape_filtered, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Original (Habitat Prior Removed)",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(0, 0.0017)

p2_kusko_se<- ggplot(Filtered_Kusko3, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 3",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(0, 0.0017)

p3_kusko_se<- ggplot(Filtered_Kusko4, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 4",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(0, 0.0017)

p4_kusko_se<- ggplot(Filtered_Kusko5, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 5",
       x = "Iso_presd",
       y = "Frequency")+
  xlim(0, 0.0017)

p5_kusko_se<- ggplot(Filtered_Kusko6, aes(x = isose_pred)) +
  geom_histogram(bins = 100) +
  labs(title = "Filtered StrOrd >= 6",
       x = "Iso_pred",
       y = "Frequency")+
  xlim(0, 0.0017)

plot_grid(p1_kusko_se, p2_kusko_se, p3_kusko_se, p4_kusko_se, p5_kusko_se, ncol = 1, nrow = 5)

# Make one big figure with all the plots in two columns and the same shapefiles side by side 
final_plot<-plot_grid(p1_kusko, p1_kusko_se, p2_kusko, p2_kusko_se, p3_kusko, p3_kusko_se, p4_kusko, p4_kusko_se, p5_kusko, p5_kusko_se, ncol = 2, nrow = 5)

title <- ggdraw() + 
  draw_label("Kusko Isoscape", fontface = "bold", size = 16, hjust = 0.5)

# Combine title and plot
plot_with_title <- plot_grid(title, final_plot, ncol = 1, rel_heights = c(0.1, 1))

# Display the final plot
plot_with_title


