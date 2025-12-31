library(dplyr)
library(readr)
library(sf)
library(RColorBrewer)
library(glue)
library(ggplot2)
library(readxl)
library(classInt)

#------------------------------------------------------------------------------
# File paths
#------------------------------------------------------------------------------
tribcollect_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/Kusko_UpstreamReaches_ByStreamOrder.csv"
prod_data_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko"
edges_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/SpatialData/Kusko_Reachbase_complete2.shp"
basin_path <- "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Kusko/Kusko_basin.shp"

# Output directories
maps_output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Maps/Kusko_Annual/TribAggregated"
data_output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated"
figures_output_dir <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures"

dir.create(maps_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_output_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
# Read static data
#------------------------------------------------------------------------------
tribcollect <- read_csv(tribcollect_path, show_col_types = FALSE)
edges <- st_read(edges_path, quiet = TRUE)
basin <- st_read(basin_path, quiet = TRUE)

#------------------------------------------------------------------------------
# Get list of all annual production files
#------------------------------------------------------------------------------
prod_files <- list.files(prod_data_dir, pattern = ".*_Assignment_Results.csv$", full.names = TRUE)
years <- as.numeric(gsub(".*([0-9]{4})_.*", "\\1", prod_files))
prod_files <- prod_files[order(years)]
years <- sort(years)

#------------------------------------------------------------------------------
# Calculate basin-wide CV
#------------------------------------------------------------------------------
basin_data <- read_excel("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx")

kusko_basin_data <- basin_data %>%
  filter(River == "Kusko", Year %in% years) %>%
  select(Year, Total_Run)

basin_cv <- kusko_basin_data %>%
  summarise(
    mean_run = mean(Total_Run, na.rm = TRUE),
    sd_run = sd(Total_Run, na.rm = TRUE),
    cv = sd_run / mean_run
  ) %>%
  pull(cv)

#------------------------------------------------------------------------------
# Initialize storage for multi-year summary
#------------------------------------------------------------------------------
all_years_trib_summary <- data.frame()

################################################################################
# LOOP THROUGH EACH YEAR
################################################################################

for (i in seq_along(prod_files)) {
  
  current_year <- years[i]
  current_file <- prod_files[i]
  
  cat("Processing year:", current_year, "\n")
  
  #------------------------------------------------------------------------------
  # Read annual production data
  #------------------------------------------------------------------------------
  prod_data <- read_csv(current_file, show_col_types = FALSE)
  
  #------------------------------------------------------------------------------
  # Attach tributary_group_id to each reach in production data
  #------------------------------------------------------------------------------
  prod_with_trib <- prod_data %>%
    left_join(
      tribcollect %>%
        select(
          TribID, reachid
        ),
      by = c("reachid" = "reachid")
    )
  
  ## Assign a unique TribID to all 7th order tribs 
  prod_with_trib <- prod_with_trib %>%
    mutate(
      TribID = ifelse(
        is.na(TribID) & Str_Order == 7,
        paste0("7thOrderTrib_", reachid),
        TribID
      )
    )
  
  
  #------------------------------------------------------------------------------
  # Aggregate production at the tributary level
  #------------------------------------------------------------------------------
  trib_production <- prod_with_trib %>%
    group_by(TribID) %>%
    summarise(
      trib_total_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
      trib_total_assignment_individuals = sum(assignment_individuals, na.rm = TRUE),
      n_reaches = n_distinct(reachid),
      .groups = "drop"
    )
  
  ## is there any with tributary_group_id == NA?
  trib_production_na <- trib_production %>%
    filter(is.na(TribID))
  
  # Remove the NA 
  trib_production <- trib_production %>%
    filter(!is.na(TribID))
  
  
  #### From the shapefile, calculate the length of each reach 
  edges_lengths <- edges %>%
    st_transform(st_crs(basin)) %>%
    mutate(
      reach_length_m = as.numeric(st_length(geometry))
    ) %>%
    st_set_geometry(NULL) %>%
    select(
      reachid,
      reach_length_m
    )
  
  # Add the length to the production data by 
  prod_with_trib <- prod_with_trib %>%
    left_join(
      edges_lengths,
      by = "reachid"
    )
  
  #------------------------------------------------------------------------------
  # Assign tributary-level production totals back to each reach
  #------------------------------------------------------------------------------
  prod_data_trib_level <- prod_with_trib %>%
    left_join(
      trib_production,
      by = "TribID"
    )
  
  
  
  # if there's still NA in either of these , replace with the original values (not trib aggregated)
  
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
  
  
  #------------------------------------------------------------------------------
  # TRIBUTARY-LEVEL LENGTHS
  #------------------------------------------------------------------------------
  trib_lengths <- prod_data_trib_level %>%
    distinct(TribID, reachid, reach_length_m) %>%
    group_by(TribID) %>%
    summarise(
      TribLength_m = sum(reach_length_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  #------------------------------------------------------------------------------
  # TRIBUTARY-LEVEL FISH PER METER
  #------------------------------------------------------------------------------
  trib_fpm <- trib_production %>%
    left_join(trib_lengths, by = "TribID") %>%
    mutate(
      fishperMeter = trib_total_assignment_individuals / TribLength_m
    )
  
  #------------------------------------------------------------------------------
  # BASIN-WIDE RESCALING (length-weighted; sums to 1)
  #------------------------------------------------------------------------------
  total_fish <- sum(trib_fpm$fishperMeter * trib_fpm$TribLength_m, na.rm = TRUE)
  
  trib_fpm <- trib_fpm %>%
    mutate(
      fishperMeter_basin =
        (fishperMeter * TribLength_m) / total_fish
    )
  
  # Sanity check
  print(
    paste0(
      "Basin sum (fishperMeter_basin): ",
      sum(trib_fpm$fishperMeter_basin, na.rm = TRUE)
    )
  )
  
  #------------------------------------------------------------------------------
  # NORMALIZE TO 0–1 ACROSS TRIBUTARIES
  #------------------------------------------------------------------------------
  trib_fpm <- trib_fpm %>%
    mutate(
      fishperMeter_norm =
        (fishperMeter_basin - min(fishperMeter_basin, na.rm = TRUE)) /
        (max(fishperMeter_basin, na.rm = TRUE) - min(fishperMeter_basin, na.rm = TRUE))
    )
  
  #------------------------------------------------------------------------------
  # JOIN BACK TO REACH-LEVEL DATA
  #------------------------------------------------------------------------------
  prod_data_trib_level <- prod_data_trib_level %>%
    left_join(
      trib_fpm %>%
        select(
          TribID,
          fishperMeter,
          fishperMeter_basin,
          fishperMeter_norm
        ),
      by = "TribID"
    )
  
  
  
  #------------------------------------------------------------------------------
  # Save annual tributary-level data
  #------------------------------------------------------------------------------
  annual_data_filename <- file.path(data_output_dir, paste0("Kusko_", current_year, "_TribAggregated.csv"))
  write_csv(prod_data_trib_level, annual_data_filename)
  
  ################################################################################
  # CREATE TRIBUTARY-AGGREGATED MAP FOR THIS YEAR
  ################################################################################
  
  # Pull out the assignment values 
  basin_assign_norm <- prod_data_trib_level$fishperMeter_norm
  
  palette <- brewer.pal(9, "YlOrRd")
  palette_expanded <- colorRampPalette(palette)(10)
  colcode <- rep("gray90", length(basin_assign_norm))
  
  colcode[basin_assign_norm > 0.0 & basin_assign_norm <= 0.1] <- palette_expanded[1]
  colcode[basin_assign_norm > 0.1 & basin_assign_norm <= 0.2] <- palette_expanded[2]
  colcode[basin_assign_norm > 0.2 & basin_assign_norm <= 0.3] <- palette_expanded[3]
  colcode[basin_assign_norm > 0.3 & basin_assign_norm <= 0.4] <- palette_expanded[4]
  colcode[basin_assign_norm > 0.4 & basin_assign_norm <= 0.5] <- palette_expanded[5]
  colcode[basin_assign_norm > 0.5 & basin_assign_norm <= 0.6] <- palette_expanded[6]
  colcode[basin_assign_norm > 0.6 & basin_assign_norm <= 0.7] <- palette_expanded[7]
  colcode[basin_assign_norm > 0.7 & basin_assign_norm <= 0.8] <- palette_expanded[8]
  colcode[basin_assign_norm > 0.8 & basin_assign_norm <= 0.9] <- palette_expanded[9]
  colcode[basin_assign_norm > 0.9 & basin_assign_norm <= 1.0] <- palette_expanded[10]
  
  legend_labels <- c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
  legend_colors <- c(palette_expanded[2], palette_expanded[4], palette_expanded[5], 
                     palette_expanded[7], palette_expanded[8], palette_expanded[9], 
                     palette_expanded[10])
  
  stream_order <- edges$Str_Order
  stream_order[is.na(stream_order)] <- 1
  
  colcode[stream_order < 3] <- "gray60" 
  
  linewidths <- ifelse(stream_order >= 9, 5,
                       ifelse(stream_order >= 8, 6,
                              ifelse(stream_order >= 7, 4.7,
                                     ifelse(stream_order >= 6, 4.2,
                                            ifelse(stream_order >= 5, 3.5,
                                                   ifelse(stream_order >= 4, 2.2,
                                                          ifelse(stream_order >= 3, 1.5, 0)))))))
  
  map_filename <- file.path(maps_output_dir, paste0("Kusko_", current_year, "_tribaggregated.png"))
  png(file = map_filename, width = 9, height = 8, units = "in", res = 300, bg = "white")
  
  par(mar = c(8, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = 'gray60', border = 'gray60', 
       main = paste0("Kusko ", current_year, " - Tributary Aggregated Production"), bg = "white")
  
  
  plot(st_geometry(edges), col = colcode, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
  
  # ADD LEGEND
  legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5, 
         title = "Relative posterior density", bty = "n", bg = "white")
  
  dev.off()
  
  #------------------------------------------------------------------------------
  # Prepare summary data for multi-year figure (by tributary)
  #------------------------------------------------------------------------------
  year_summary <- trib_production %>%
    mutate(year = current_year) %>%
    select(year, TribID, trib_total_assignment_individuals)
  
  all_years_trib_summary <- bind_rows(all_years_trib_summary, year_summary)
  
}

################################################################################
# CREATE MULTI-YEAR TRIBUTARY SUMMARY FIGURE
################################################################################

# Calculate average production and coefficient of variation by tributary
trib_cv_summary <- all_years_trib_summary %>%
  group_by(TribID) %>%
  summarise(
    avg_production = mean(trib_total_assignment_individuals, na.rm = TRUE),
    sd_production = sd(trib_total_assignment_individuals, na.rm = TRUE),
    cv = sd_production / avg_production,
    n_years = n(),
    .groups = "drop"
  ) %>%
  filter(n_years > 1 & !is.na(cv) & is.finite(cv))

# Stream order information comes from prod_data (last year has all reaches)
# Get the most common stream order for each tributary
if (exists("prod_data_trib_level") && nrow(trib_cv_summary) > 0) {
  stream_order_lookup <- prod_data_trib_level %>%
    select(TribID, Str_Order) %>%
    distinct() %>%
    group_by(TribID) %>%
    slice(1) %>%
    ungroup()
  
  trib_cv_summary <- trib_cv_summary %>%
    left_join(
      stream_order_lookup,
      by = "TribID"
    )
}

# Remove Str_Order 7 from the summary if it exists
trib_cv_summary <- trib_cv_summary %>%
  filter(Str_Order != 7 | is.na(Str_Order))

# Save the summary data
summary_data_filename <- file.path(data_output_dir, "Kusko_AllYears_TribProduction_CV.csv")
write_csv(trib_cv_summary, summary_data_filename)

# Create the production vs CV figure with ggplot2
figure_filename <- file.path(figures_output_dir, "Kusko_Production_vs_CV_Tributaries.png")

# Create color palette for stream orders using custom color scheme
# Colors: Dry Sage, Soft Peach, Vibrant Coral, Wine Plum, Deep Mocha
custom_colors <- c("#C9CBA3", "#FFE1A8", "#E26D5C", "#723D46", "#472D30")
unique_so <- sort(unique(trib_cv_summary$Str_Order))
unique_so <- unique_so[!is.na(unique_so)]
n_so <- length(unique_so)

if (n_so <= length(custom_colors)) {
  stream_order_colors <- custom_colors[1:n_so]
} else {
  stream_order_colors <- colorRampPalette(custom_colors)(n_so)
}
names(stream_order_colors) <- as.character(unique_so)

# Create scatter plot
p <- ggplot(trib_cv_summary, aes(x = avg_production,
                                 y = cv,
                                 color = as.factor(Str_Order),
                                 size = as.factor(Str_Order))) +
  geom_point(alpha = 0.75) +
  geom_hline(aes(yintercept = basin_cv), linetype = "dashed", color = "#88292F", size = 1) +
  annotate("text", x = Inf, y = basin_cv, label = "Basin-wide CV", 
           hjust = 1.15, vjust = 1.5, color = "#88292F", size = 3.5, fontface = "italic") +
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
ggsave(file.path(figures_output_dir, "Kusko_Tributary_CV_Analysis.png"),
       p, width = 12, height = 8, dpi = 300, bg = "white")




# ################################################################################
# ################################################################################
# # SUMMARY MAPS, Production vs variation 
# ################################################################################
# 
# # Read in each production year data seperately 
Kusk2017<- read_csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated/Kusko_2017_TribAggregated.csv")
Kusk2018<- read_csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated/Kusko_2018_TribAggregated.csv")
Kusk2019<- read_csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated/Kusko_2019_TribAggregated.csv")
Kusk2020<- read_csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated/Kusko_2020_TribAggregated.csv")
Kusk2021<- read_csv("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated/Kusko_2021_TribAggregated.csv")

library(dplyr)
library(purrr)

kusk_list <- list(
  "2017" = Kusk2017,
  "2018" = Kusk2018,
  "2019" = Kusk2019,
  "2020" = Kusk2020,
  "2021" = Kusk2021
)

trib_assignments <- map_dfc(
  kusk_list,
  ~ select(.x, trib_total_assignment_individuals)
)

colnames(trib_assignments) <- paste0("Kusk", names(kusk_list))

### ok, each column is now a year of Kusko data, each row is a reach

### I want to calculate the average and CV across years for each reach

library(dplyr)

trib_summary <- trib_assignments %>%
  mutate(
    mean_production = rowMeans(across(everything()), na.rm = TRUE),
    sd_production   = apply(across(everything()), 1, sd, na.rm = TRUE),
    cv_production   = sd_production / mean_production
  )


# some of these are going to have NaN because of 0, just add a 0 
trib_summary <- trib_summary %>%
  mutate(
    cv_production = ifelse(is.nan(cv_production), 0, cv_production)
  )


### Save as KuskoAllYearsProdCV.csv in /Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated
write_csv(trib_summary, "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/TribAggregated/KuskoAllYearsProdCV.csv")


# ########################
# ######################## Average production map 
# 
# prod <- trib_summary$mean_production
# 
# # Rescale to sum to 1 across the basin
# prod_rescaled <- prod / sum(prod, na.rm = TRUE)
# 
# # Normalize to 0–1 for relative comparison
# prod_normalized <- (prod_rescaled - min(prod_rescaled, na.rm = TRUE)) /
#   (max(prod_rescaled, na.rm = TRUE) - min(prod_rescaled, na.rm = TRUE))
# 
# # Sanity checks
# sum(prod_rescaled, na.rm = TRUE)        # should be 1
# range(prod_normalized, na.rm = TRUE)    # should be 0 to 1
# 
# avg_prod_norm <- prod_normalized
# 
# # Create color coding for average production
# palette <- brewer.pal(9, "YlOrRd")
# palette_expanded <- colorRampPalette(palette)(10)
# 
# colcode_avg <- rep("gray90", length(avg_prod_norm))
# colcode_avg[avg_prod_norm == 0] <- "white"
# colcode_avg[avg_prod_norm > 0.0 & avg_prod_norm <= 0.1] <- palette_expanded[1]
# colcode_avg[avg_prod_norm > 0.1 & avg_prod_norm <= 0.2] <- palette_expanded[2]
# colcode_avg[avg_prod_norm > 0.2 & avg_prod_norm <= 0.3] <- palette_expanded[3]
# colcode_avg[avg_prod_norm > 0.3 & avg_prod_norm <= 0.4] <- palette_expanded[4]
# colcode_avg[avg_prod_norm > 0.4 & avg_prod_norm <= 0.5] <- palette_expanded[5]
# colcode_avg[avg_prod_norm > 0.5 & avg_prod_norm <= 0.6] <- palette_expanded[6]
# colcode_avg[avg_prod_norm > 0.6 & avg_prod_norm <= 0.7] <- palette_expanded[7]
# colcode_avg[avg_prod_norm > 0.7 & avg_prod_norm <= 0.8] <- palette_expanded[8]
# colcode_avg[avg_prod_norm > 0.8 & avg_prod_norm <= 0.9] <- palette_expanded[9]
# colcode_avg[avg_prod_norm > 0.9 & avg_prod_norm <= 1.0] <- palette_expanded[10]
# 
# # Stream order linewidths
# stream_order <- edges$Str_Order
# stream_order[is.na(stream_order)] <- 1
# linewidths <- ifelse(stream_order >= 9, 5,
#                      ifelse(stream_order >= 8, 6,
#                             ifelse(stream_order >= 7, 4.7,
#                                    ifelse(stream_order >= 6, 4.2,
#                                           ifelse(stream_order >= 5, 3.5,
#                                                  ifelse(stream_order >= 4, 2.2,
#                                                         ifelse(stream_order >= 3, 1.5, 0)))))))
# 
# # Create map in Figures directory
# map_filename_avg <- file.path(figures_output_dir, "Kusko_AvgProduction.png")
# png(file = map_filename_avg, width = 9, height = 8, units = "in", res = 300, bg = "white")
# 
# par(mar = c(8, 4, 4, 2), bg = "white")
# plot(st_geometry(basin), col = 'gray60', border = 'gray60',
#      main = paste0("Kusko - Average Production (All Years)\nNormalized 0-1"), bg = "white")
# plot(st_geometry(edges), col = colcode_avg, pch = 16, axes = FALSE, add = TRUE, lwd = linewidths)
# 
# # Add legend
# legend_labels <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5",
#                    "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
# legend_colors <- palette_expanded
# 
# legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5,
#        title = "Average Production\n(Normalized)", bty = "n", bg = "white")
# 
# dev.off()
# 
# 
# 
# #------------------------------------------------------------------------------
# # MAP 2: COEFFICIENT OF VARIATION (RAW CV, EQUAL BREAKS)
# #------------------------------------------------------------------------------
# 
# library(classInt)
# library(RColorBrewer)
# 
# # Extract CV values
# cv_values <- trib_summary$cv_production
# n_classes <- 7
# palette_cv <- rev(colorRampPalette(brewer.pal(9, "YlGnBu"))(n_classes))
# 
# # Compute Jenks natural breaks
# jenks <- classIntervals(cv_values, n = n_classes, style = "quantile")
# 
# # Assign each CV value to a class
# cv_class <- findCols(jenks)   # returns integer class index 1:n_classes
# 
# # Map colors directly
# colcode_cv <- palette_cv[cv_class]
# 
# # Optional: generate legend labels
# legend_labels <- paste0(
#   round(jenks$brks[-length(jenks$brks)], 2),
#   " – ",
#   round(jenks$brks[-1], 2)
# )
# 
# #------------------------------------------------------------------------------
# # STREAM ORDER LINEWIDTHS (UNCHANGED)
# #------------------------------------------------------------------------------
# stream_order <- edges$Str_Order
# stream_order[is.na(stream_order)] <- 1
# 
# linewidths <- ifelse(stream_order >= 9, 5,
#                      ifelse(stream_order >= 8, 6,
#                             ifelse(stream_order >= 7, 4.7,
#                                    ifelse(stream_order >= 6, 4.2,
#                                           ifelse(stream_order >= 5, 3.5,
#                                                  ifelse(stream_order >= 4, 2.2,
#                                                         ifelse(stream_order >= 3, 1.5, 0)))))))
# 
# #------------------------------------------------------------------------------
# # CREATE MAP
# #------------------------------------------------------------------------------
# map_filename_cv <- file.path(figures_output_dir,
#                              "Kusko_MultiYear_CoefficientOfVariation_EQUAL.png")
# 
# png(file = map_filename_cv,
#     width = 9, height = 8, units = "in", res = 300, bg = "white")
# 
# par(mar = c(8, 4, 4, 2), bg = "white")
# 
# # Plot basin
# plot(st_geometry(basin),
#      col = "gray60",
#      border = "gray60",
#      main = "Kuskokwim – Coefficient of Variation (Raw, Equal Breaks)",
#      bg = "white")
# 
# # Plot reaches with CV colors
# plot(st_geometry(edges),
#      col = colcode_cv,
#      pch = 16,
#      axes = FALSE,
#      add = TRUE,
#      lwd = linewidths)
# 
# #------------------------------------------------------------------------------
# # LEGEND
# #------------------------------------------------------------------------------
# legend_labels <- levels(cv_class)
# 
# # legend("topleft",
# #        legend = legend_labels,
# #        col = palette_cv,
# #        lwd = 5,
# #        title = "Coefficient of Variation",
# #        bty = "n",
# #        bg = "white")
# 
# dev.off()
# 
