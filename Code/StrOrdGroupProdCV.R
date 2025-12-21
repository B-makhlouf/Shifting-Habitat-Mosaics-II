# Load libraries
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(glue)

#------------------------------------------------------------------------------
# File paths
#------------------------------------------------------------------------------
tribcollect_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/Kusko_UpstreamReaches_ByStreamOrder.csv"
prod_data_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko"
edges_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp"
basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp"

# Output directories
output_data_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TributaryAnalysis/Kusko"
output_figure_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/TributaryAnalysis"

# Analysis parameters
kusko_years <- c(2017, 2018, 2019, 2020, 2021)

# Create output directories
dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figure_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
# Read static data
#------------------------------------------------------------------------------
tribcollect <- read_csv(tribcollect_path, show_col_types = FALSE)
edges <- st_read(edges_path, quiet = TRUE)
basin <- st_read(basin_path, quiet = TRUE)

#------------------------------------------------------------------------------
# Loop through years and aggregate production
#------------------------------------------------------------------------------

all_trib_production <- list()

for (year in kusko_years) {
  
  # Read production data for this year
  prod_data_path <- file.path(prod_data_dir, paste0(year, "_Kusko_Assignment_Results.csv"))
  prod_data <- read_csv(prod_data_path, show_col_types = FALSE)
  
  
  # filter to only 4th order tribs 
  prod_data <- prod_data %>%
    filter(Str_Order == 4)
  

  #------------------------------------------------------------------------------
  # Attach tributary_group_id to each reach in production data
  #------------------------------------------------------------------------------
  
  ### Add a tributary_group_id value by matching the upstream_reachid in tribcollect to reachid in prod_data
  
  prod_with_trib <- prod_data %>%
    left_join(
      tribcollect %>%
        select(reachid = upstream_reachid, tributary_group_id),
      by = "reachid"
    )
  
  tribcollect %>%
    count(upstream_reachid) %>%
    filter(n > 1)
  
  
  
  
  #------------------------------------------------------------------------------
  # Aggregate production at the tributary level
  #------------------------------------------------------------------------------
  trib_production <- prod_with_trib %>%
    group_by(tributary_group_id) %>%
    summarise(
      trib_total_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
      trib_total_assignment_individuals = sum(assignment_individuals, na.rm = TRUE),
      n_reaches = n_distinct(reachid),
      .groups = "drop"
    )
  
  #------------------------------------------------------------------------------
  # Remove NA tributary_group_id
  #------------------------------------------------------------------------------
  trib_production_na <- trib_production %>%
    filter(is.na(tributary_group_id))
  
  trib_production <- trib_production %>%
    filter(!is.na(tributary_group_id))
  
  #------------------------------------------------------------------------------
  # Assign tributary-level production totals back to each reach
  #------------------------------------------------------------------------------
  prod_data_trib_level <- prod_with_trib %>%
    left_join(
      trib_production,
      by = "tributary_group_id"
    )
  
  # Sum all of the 7th order tributary production values
  production <- prod_data_trib_level %>%
    filter(Str_Order == 7) %>%
    summarise(
      total_trib7_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
      total_trib7_assignment_individuals = sum(assignment_individuals, na.rm = TRUE)
    )
  
  # Assign this value to all 7th order tribs
  prod_data_trib_level <- prod_data_trib_level %>%
    mutate(
      trib7_total_assignment_rescale = ifelse(
        Str_Order == 7,
        production$total_trib7_assignment_rescale,
        trib_total_assignment_rescale
      ),
      trib7_total_assignment_individuals = ifelse(
        Str_Order == 7,
        production$total_trib7_assignment_individuals,
        trib_total_assignment_individuals
      )
    )
  
  # If NA, just use the original production values (assignment rescale) for that row
  prod_data_trib_level <- prod_data_trib_level %>%
    mutate(
      trib_total_assignment_rescale = ifelse(
        is.na(trib_total_assignment_rescale),
        assignment_rescale,
        trib_total_assignment_rescale
      ),
      trib_total_assignment_individuals = ifelse(
        is.na(trib_total_assignment_individuals),
        assignment_individuals,
        trib_total_assignment_individuals
      )
    )
  
  # Add year to data
  prod_data_trib_level <- prod_data_trib_level %>%
    mutate(year = year)
  
  # Store in list
  all_trib_production[[as.character(year)]] <- prod_data_trib_level
  
  cat(glue("Processed {year}\n"))
}

# Combine all years
combined_data <- bind_rows(all_trib_production)

#------------------------------------------------------------------------------
# Calculate tributary statistics (mean, SD, CV)
#------------------------------------------------------------------------------

trib_statistics <- combined_data %>%
  filter(!is.na(tributary_group_id)) %>%
  group_by(tributary_group_id) %>%
  summarise(
    stream_order = max(Str_Order, na.rm = TRUE),
    n_years = n_distinct(year),
    mean_individuals = mean(trib_total_assignment_individuals, na.rm = TRUE),
    sd_individuals = sd(trib_total_assignment_individuals, na.rm = TRUE),
    cv_individuals = ifelse(mean_individuals > 0,
                            sd_individuals / mean_individuals,
                            NA),
    .groups = "drop"
  ) %>%
  filter(n_years >= 2, !is.na(cv_individuals), !is.infinite(cv_individuals))

#------------------------------------------------------------------------------
# Calculate basin-wide CV
#------------------------------------------------------------------------------

library(readxl)
basin_data <- read_excel("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx")

kusko_basin_data <- basin_data %>%
  filter(River == "Kusko", Year %in% kusko_years) %>%
  select(Year, Total_Run)

basin_cv <- kusko_basin_data %>%
  summarise(
    mean_run = mean(Total_Run, na.rm = TRUE),
    sd_run = sd(Total_Run, na.rm = TRUE),
    cv = sd_run / mean_run
  ) %>%
  pull(cv)

#------------------------------------------------------------------------------
# Export data
#------------------------------------------------------------------------------

write_csv(trib_statistics, file.path(output_data_dir, "Kusko_Tributary_Statistics.csv"))
write_csv(combined_data, file.path(output_data_dir, "Kusko_Tributary_Production_ByYear.csv"))

#------------------------------------------------------------------------------
# Create scatter plot
#------------------------------------------------------------------------------

# Create color palette for stream orders
unique_so <- sort(unique(trib_statistics$stream_order))
n_so <- length(unique_so)

stream_order_colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(n_so)
names(stream_order_colors) <- as.character(unique_so)

# Create the scatter plot
p <- ggplot(trib_statistics, aes(x = mean_individuals,
                                 y = cv_individuals,
                                 color = as.factor(stream_order),
                                 size = as.factor(stream_order))) +
  geom_point(alpha = 0.6) +
  geom_hline(aes(yintercept = basin_cv), linetype = "dashed", color = "#88292F", size = 1) +
  scale_color_manual(values = stream_order_colors, name = "Stream Order") +
  scale_size_manual(values = rep(3.5, n_so), name = "Stream Order", guide = "none") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#EBEBEB", size = 0.3),
    panel.grid.minor = element_blank(),
    text = element_text(color = "#333333", family = "sans", size = 11),
    plot.title = element_text(size = 14, face = "bold", color = "#333333", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 15)),
    axis.title = element_text(size = 11, color = "#333333"),
    axis.text = element_text(size = 10, color = "#666666"),
    axis.line = element_line(color = "#CCCCCC", size = 0.3),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "#CCCCCC", size = 0.5)
  ) +
  labs(
    title = "Production vs Coefficient of Variation: Kuskokwim",
    subtitle = "Each point represents one tributary group; average production (all years) vs variability",
    x = "Average Production (Individuals)",
    y = "Coefficient of Variation"
  )

# Save plot
ggsave(file.path(output_figure_dir, "Kusko_Tributary_CV_Analysis.png"),
       p, width = 12, height = 8, dpi = 300, bg = "white")

print(p)
