################################################################################
# TRIBUTARY GROUP PRODUCTION ANALYSIS - YUKON
# Pure loop version - no functions
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

#==============================================================================
# CONFIGURATION
#==============================================================================

BASE_DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II"
ESCAPEMENT_FILE <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx"

watershed <- "Yukon"
years <- c(2015, 2016, 2018, 2021)

group_lookup_file <- file.path(
  BASE_DATA_DIR,
  "Data/UpstreamReaches/SameGroupStrOrd/StrOrdGroup_Yukon.csv"
)

prod_data_dir <- file.path(BASE_DATA_DIR, "AnnualProdData/Yukon")
data_output_dir <- file.path(BASE_DATA_DIR, "Data/TributaryAnalysis/Yukon")
figure_output_dir <- file.path(BASE_DATA_DIR, "Figures/StrOrdGroups")

#==============================================================================
# SETUP
#==============================================================================

dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_output_dir, recursive = TRUE, showWarnings = FALSE)

group_lookup <- read_csv(group_lookup_file, show_col_types = FALSE)
escapement_data <- read_xlsx(ESCAPEMENT_FILE)

reachbases <- sort(unique(group_lookup$Reachbase[group_lookup$Reachbase != 0]))

all_group_production <- data.frame()

#==============================================================================
# MAIN LOOP: YEAR × REACHBASE
#==============================================================================

for (year in years) {
  
  # Load production data for year
  all_files <- list.files(prod_data_dir, full.names = TRUE)
  matching_files <- all_files[
    grepl(
      paste0("^", year, "_", watershed, "_Assignment_Results\\.csv$"),
      basename(all_files)
    )
  ]
  if (length(matching_files) == 0) next
  
  prod_data <- read_csv(matching_files[1], show_col_types = FALSE)
  if (!all(c("reachid", "assignment_individuals") %in% names(prod_data))) next
  
  basin_run <- escapement_data %>%
    filter(River == watershed, Year == year) %>%
    pull(Total_Run)
  if (length(basin_run) == 0) next
  
  for (rb in reachbases) {
    
    lookupfiltered <- group_lookup %>%
      filter(Reachbase == rb)
    
    all_groups <- lookupfiltered %>%
      distinct(GroupID)
    
    merged_data <- prod_data %>%
      right_join(lookupfiltered, by = c("reachid" = "ReachID"))
    
    group_production <- merged_data %>%
      group_by(GroupID) %>%
      summarise(
        group_individuals = sum(assignment_individuals, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      right_join(all_groups, by = "GroupID") %>%
      mutate(
        group_individuals = replace_na(group_individuals, 0),
        Year = year,
        Reachbase = rb
      )
    
    all_group_production <- bind_rows(
      all_group_production,
      group_production
    )
  }
}

#==============================================================================
# SUMMARY STATISTICS: MEAN & CV BY REACHBASE × GROUP
#==============================================================================

group_summary <- all_group_production %>%
  group_by(Reachbase, GroupID) %>%
  summarise(
    mean_production = mean(group_individuals),
    sd_production   = sd(group_individuals),
    cv_production   = ifelse(mean_production > 0,
                             sd_production / mean_production,
                             NA_real_),
    n_years = n(),
    .groups = "drop"
  )

library(ggplot2)
library(RColorBrewer)

#---------------------------------------
# Timeseries plot for all Reachbases
#---------------------------------------

# Prepare z-scored individual and basin totals
plot_data <- all_group_production %>%
  group_by(Reachbase, GroupID) %>%
  mutate(individuals_z = scale(group_individuals)[,1]) %>%
  ungroup()

basin_ts <- all_group_production %>%
  group_by(Reachbase, Year) %>%
  summarise(basin_total = sum(group_individuals), .groups = "drop") %>%
  group_by(Reachbase) %>%
  mutate(basin_z = scale(basin_total)[,1]) %>%
  ungroup()

p_ts_pub <- ggplot(plot_data, aes(x = Year, y = individuals_z, group = GroupID)) +
  geom_line(color = "#5eb3d6", alpha = 0.1, linewidth = 0.8) +
  geom_point(color = "#5eb3d6", alpha = 0.2, size = 1.5) +
  geom_line(data = basin_ts, mapping = aes(x = Year, y = basin_z),
            inherit.aes = FALSE, color = "#D78521", linewidth = 1.5) +
  geom_point(data = basin_ts, mapping = aes(x = Year, y = basin_z),
             inherit.aes = FALSE, color = "#D78521", size = 3)+

  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~Reachbase, ncol = 1, scales = "free_y") +
  labs(
    title = "Yukon Tributary Group Production Timeseries",
    subtitle = "Z-normalized individuals; orange line = basin escapement",
    x = "Year",
    y = "Z-normalized individuals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0)
  )

ggsave(
  file.path(figure_output_dir, "Yukon_Reachbase_Timeseries_Facet.png"),
  p_ts_pub, width = 12, height = 20, dpi = 300, bg = "white"
)

#---------------------------------------
# CV violin plot by Reachbase
#---------------------------------------
library(ggplot2)
library(RColorBrewer)

# Clip extreme CVs for better visualization
cv_for_plot_clipped <- cv_for_plot %>%
  mutate(cv_plot = pmin(cv_production, .7))

# Use nicer colors
pal <- brewer.pal(n = length(unique(cv_for_plot$Reachbase)), name = "Set2")

p_cv_pub <- ggplot(cv_for_plot_clipped, aes(x = Reachbase, y = cv_plot, fill = Reachbase)) +
  geom_boxplot(alpha = 0.7, color = "gray30", width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.4, color = "black") +
  geom_hline(yintercept = basin_cv, linetype = "dashed", color = "#D78521", size = 1.2) +
  annotate("text", x = 1, y = basin_cv + 0.05, 
           label = paste0("Basin CV = ", round(basin_cv, 3)),
           color = "#D78521", hjust = 0, fontface = "bold", size = 5) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Coefficient of Variation by Reachbase",
    x = "Reachbase",
    y = "Coefficient of Variation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0, size = 16),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figure_output_dir, "Yukon_CV_Boxplot.png"),
  p_cv_pub, width = 10, height = 7, dpi = 300, bg = "white"
)
