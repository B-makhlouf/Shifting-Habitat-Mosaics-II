# Load libraries
library(dplyr)
library(readr)
library(sf)
library(RColorBrewer)
library(glue)
library(ggplot2)
library(readxl)

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
  
  #------------------------------------------------------------------------------
  # Assign tributary-level production totals back to each reach
  #------------------------------------------------------------------------------
  prod_data_trib_level <- prod_with_trib %>%
    left_join(
      trib_production,
      by = "TribID"
    )
  
  # Sum all of the 7th order tributary production values 
  
  production <- prod_data_trib_level %>%
    filter(Str_Order == 7) %>%
    #sum assignment_rescale
    summarise(
      total_trib7_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
      total_trib7_assignment_individuals = sum(assignment_individuals, na.rm = TRUE)
    )
  
  # Assign this value as trib_assign_Rescale for all 7th order trib columns 
  
  seventhorder_rescale <- production$total_trib7_assignment_rescale
  seventhorder_individuals <- production$total_trib7_assignment_individuals
  
  prod_data_trib_level <- prod_data_trib_level %>%
    mutate(
      trib_total_assignment_rescale = ifelse(
        Str_Order == 7,
        seventhorder_rescale,
        trib_total_assignment_rescale
      ),
      trib_total_assignment_individuals = ifelse(
        Str_Order == 7,
        seventhorder_individuals,
        trib_total_assignment_individuals
      )
    )
  
  #------------------------------------------------------------------------------
  # Save annual tributary-level data
  #------------------------------------------------------------------------------
  annual_data_filename <- file.path(data_output_dir, paste0("Kusko_", current_year, "_TribAggregated.csv"))
  write_csv(trib_production, annual_data_filename)
  
  ################################################################################
  # CREATE TRIBUTARY-AGGREGATED MAP FOR THIS YEAR
  ################################################################################
  
  # Pull out the assignment values 
  basin_assign_norm <- prod_data_trib_level$trib_total_assignment_rescale
  
  # normalize to range from 0-1
  basin_assign_norm <- (basin_assign_norm - min(basin_assign_norm, na.rm = TRUE)) / 
    (max(basin_assign_norm, na.rm = TRUE) - min(basin_assign_norm, na.rm = TRUE))
  
  
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
  
  linewidths <- ifelse(stream_order >= 9, 5,
                       ifelse(stream_order >= 8, 6,
                              ifelse(stream_order >= 7, 5,
                                     ifelse(stream_order >= 6, 3,
                                            ifelse(stream_order >= 5, 2.5,
                                                   ifelse(stream_order >= 4, 2,
                                                          ifelse(stream_order >= 3, 1.5, 1.0)))))))
  
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

cat("\n=== PROCESSING COMPLETE ===\n")
cat("Maps saved to:", maps_output_dir, "\n")
cat("Data saved to:", data_output_dir, "\n")
cat("Figure saved to:", figures_output_dir, "\n")
