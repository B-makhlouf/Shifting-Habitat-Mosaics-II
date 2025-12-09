## This script will bin production values into HUCs 

# Read in the polygon 
huc<- st_read("/Users/benjaminmakhlouf/Spatial Data/SMH2/YkKkHuc7.shp")

# Read in the production data , lets do 2015 Yukon 
prod<- read_csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Yukon/2016_Yukon_Assignment_Results.csv")

# Read in the only canada shapefile 
edges<- st_read("/Users/benjaminmakhlouf/Spatial Data/SMH2/YukonUSGS_noCA.shp")

# Add prod$assigment_norm to edges by matching "reachid" 

edges_prod <- edges %>%
  left_join(prod %>% select(reachid, assignment_norm),
            by = "reachid")

# if NA fill with 0 
edges_prod$assignment_norm[is.na(edges_prod$assignment_norm)] <- 0

# make sure the polygon and edges have the same crs 
if (st_crs(edges_prod) != st_crs(huc)) {
  edges_prod <- st_transform(edges_prod, st_crs(huc))
}

# Fix 
sf::sf_use_s2(FALSE)

# run your st_join again
edges_in_huc <- st_join(edges_prod, huc, join = st_intersects)


# ---------------------------
# Sum assignment_norm for each HUC
# ---------------------------
huc_prod <- edges_in_huc %>%
  st_drop_geometry() %>%                  # drop geometry so we can summarize cleanly
  group_by(HYBAS_ID) %>%            # <-- replace HUC7 with your actual HUC ID field name
  summarize(total_prod = sum(assignment_norm, na.rm = TRUE))

# --------------------------

huc_final <- huc %>%
  left_join(huc_prod, by = c("HYBAS_ID"))  # adjust if your column names differ


# Quick plot
ggplot(data = huc_final) +
  geom_sf(aes(fill = total_prod)) +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +
  theme_minimal() +
  labs(title = "Total Production by HUC",
       fill = "Total Production")
