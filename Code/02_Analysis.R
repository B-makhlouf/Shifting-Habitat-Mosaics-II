library("tidyverse")
library("here")
library("sf")


##---  Quantify variability across the landscape 

## Read in all of the assignment matrices 
yuk2015_assign<- read_csv(here("Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv"))
yuk2016_assign<- read_csv(here("Outputs/Assignment Matrix/Yukon_2016_0.7_basin_assignments.csv"))
yuk2017_assign<- read_csv(here("Outputs/Assignment Matrix/Yukon_2015_0.7_basin_assignments.csv"))
yuk2018_assign<- read_csv(here("Outputs/Assignment Matrix/Yukon_2017_0.7_basin_assignments.csv"))

# Combine all of the total assignments into one data frame 
full_yukon_assign<- bind_cols(yuk2015_assign$Total, yuk2016_assign$Total, yuk2017_assign$Total, yuk2018_assign$Total)

# Give collumn names yuk2015, yuk2016, yuk2017, yuk2018
colnames(full_yukon_assign)<- c("yuk2015", "yuk2016", "yuk2017", "yuk2018")

# Read in the shape file with the Tributary Names
yukon_withTribs<- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/Yukon_w.tribnames.shp")

# Merge the full yukon assign data frame into the yukon_withTribs data frame
yukon_withTribs$yuk2015<- full_yukon_assign$yuk2015
yukon_withTribs$yuk2016<- full_yukon_assign$yuk2016
yukon_withTribs$yuk2017<- full_yukon_assign$yuk2017


# Calculate the CV for each row across the three years 
yukon_withTribs <- yukon_withTribs %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(yuk2015, yuk2016, yuk2017), na.rm = TRUE),
    sd_prod = sd(c(yuk2015, yuk2016, yuk2017), na.rm = TRUE),
    cv = sd_prod / mean_prod * 100
  ) %>%
  #assign NA a 0 
  mutate(cv = ifelse(is.na(cv), 0, cv))

CVmap <- ggplot() +
  geom_sf(data = yukon_withTribs, aes(color = cv)) +
  scale_color_gradient2(
    low = "white",
    high = "firebrick"
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Coefficient of Variation in Production Across the Yukon Basin", color = "Coefficient of Variation")

# Export as a tiff
ggsave(here("Figures/Maps/CVmap.pdf"), plot = CVmap, width = 10, height = 10, units = "in", dpi = 300)




?scale_fill_brewer
?scale_color_gradient2






# Start a new data frame, and sum the total production by tributary for each year 
yukon_trib_summary<- yukon_withTribs %>% 
  group_by(trbtry_) %>% 
  summarize(totalprod2015 = sum(yuk2015), totalprod2016 = sum(yuk2016), totalprod2017 = sum(yuk2017))

# Calculate the cv for each tributary
yukon_trib_summary$cv2015<- sd(yukon_trib_summary$totalprod2015)/mean(yukon_trib_summary$totalprod2015)
yukon_trib_summary <- yukon_trib_summary %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(totalprod2015, totalprod2016, totalprod2017), na.rm = TRUE),
    sd_prod = sd(c(totalprod2015, totalprod2016, totalprod2017), na.rm = TRUE),
    cv = sd_prod / mean_prod
  )

# 

#Plot CV by trib, ordered by CV 
ggplot(yukon_trib_summary, aes(x = reorder(trbtry_, cv), y = cv))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Tributary", y = "Coefficient of Variation") 

# Plot Cv vs production
ggplot(yukon_trib_summary, aes(x = mean_prod, y = cv))+
  geom_point()+
  theme_minimal()+
  labs(x = "Mean Production", y = "Coefficient of Variation") 
