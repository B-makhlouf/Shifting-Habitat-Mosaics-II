library(tidyverse)

# =============================================================================
# LOAD DATA
# =============================================================================

gam_data <- read_csv("/Users/benjaminmakhlouf/Research_repos/03_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_MA.csv", show_col_types = FALSE)

metadata <- read_csv("/Users/benjaminmakhlouf/Research_repos/03_Western_Ak_otolith_stock_discrimination/Data/Final/Metadata_and_QC.csv", show_col_types = FALSE)

### filter gam_data to only Yukon 
gam_data_yukon <- gam_data %>%
  filter(Watershed == "Yukon") 

gam_data_yukon <- gam_data_yukon %>%
  left_join(
    metadata %>% select(Fish_ID, likely_gen),
    by = c("Fish_id" = "Fish_ID")
  )


### Now set filters for upper and lower iso (Natal_Iso) and likely gen 
LowerIso<- .7050
UpperIso<- .7060

gam_data_yukon_filtered <- gam_data_yukon %>%
  filter(Natal_Iso >= LowerIso & Natal_Iso <= UpperIso) %>%
  filter(likely_gen == "Lower_gen")


# =============================================================================

# =============================================================================
# EXTRACT TIME SERIES COLUMNS AND METADATA
# =============================================================================

# Identify all X1, X2, ... time-series columns
ts_cols <- grep("^X\\d+$", names(gam_data_yukon_filtered), value = TRUE)

# Extract the time-series matrix
ts_matrix <- gam_data_yukon_filtered %>%
  select(all_of(ts_cols)) %>%
  as.matrix()

# Extract metadata only (everything except the X columns)
metadata_only <- gam_data_yukon_filtered %>%
  select(-all_of(ts_cols))

# =============================================================================
# RESHAPE TO LONG FORMAT FOR PLOTTING
# =============================================================================

ts_long <- gam_data_yukon_filtered %>%
  pivot_longer(
    cols = all_of(ts_cols),
    names_to = "Index",
    values_to = "Value",
    names_prefix = "X",
    names_transform = list(Index = as.integer)
  )

# =============================================================================
# PLOT ALL TIME SERIES
# =============================================================================

library(ggplot2)

ggplot(ts_long, aes(x = Index, y = Value, group = Fish_id)) +
  geom_line(alpha = 0.35, color = "steelblue3") +
  theme_bw(base_size = 14) +
  labs(
    title = "All Yukon Time-Series (Filtered)",
    subtitle = paste0(
      "N = ", nrow(gam_data_yukon_filtered),
      " fish | Iso filter = ", LowerIso, "–", UpperIso,
      " | likely_gen = Lower_gen"
    ),
    x = "Index (X1, X2, …)",
    y = "Isotope Value"
  )
# =============================================================================

library(dtwclust)

# =============================================================================
# PERFORM TIME SERIES CLUSTERING

pc <- tsclust(ts_matrix, type = "partitional", k = 4L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))

plot(pc)
# =============================================================================

hc <- tsclust(ts_matrix, type = "hierarchical", k = 6L, 
              distance = "sbd", trace = TRUE,
              control = hierarchical_control(method = "average"))

plot(hc)


