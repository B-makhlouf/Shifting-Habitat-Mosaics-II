################################################################################
# YUKON DATA AVAILABILITY FIGURE - CPUE + GENETICS + OTOLITHS
# WITH GENETIC COMPOSITION COLORING
################################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(patchwork)

# Configuration
DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures"
YEARS <- c(2015, 2016, 2017, 2018, 2019, 2021)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Compile data across all years WITH genetic composition
all_data <- list()

for (year in YEARS) {
  data_file <- file.path(DATA_DIR, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  
  if (file.exists(data_file)) {
    df <- read_csv(data_file, show_col_types = FALSE) %>%
      mutate(
        year = year,
        # Check if genetics values are actually present (not NA and not 0)
        has_genetics = !is.na(Lower) & !is.na(Middle) & !is.na(Upper) & 
          (Lower > 0 | Middle > 0 | Upper > 0),
        has_otolith = !is.na(natal_iso)
      ) %>%
      group_by(year, DOY) %>%
      summarise(
        cpue = first(dailyCPUEprop),
        n_genetics = sum(has_genetics, na.rm = TRUE),
        n_otolith = sum(has_otolith, na.rm = TRUE),
        n_total = n(),
        # Average genetic composition across fish with genetics on that day
        mean_Lower = mean(Lower[has_genetics], na.rm = TRUE),
        mean_Middle = mean(Middle[has_genetics], na.rm = TRUE),
        mean_Upper = mean(Upper[has_genetics], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        genetics_available = n_genetics > 0,
        otolith_available = n_otolith > 0
      )
    
    all_data[[as.character(year)]] <- df
  }
}

# Combine all years
combined_data <- bind_rows(all_data)

# Prepare data for stacked bar chart on days with genetics
stacked_data <- combined_data %>%
  filter(genetics_available) %>%
  select(year, DOY, cpue, mean_Lower, mean_Middle, mean_Upper) %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "genetic_group", 
               values_to = "proportion",
               names_prefix = "mean_") %>%
  mutate(
    genetic_group = factor(genetic_group, levels = c("Lower", "Middle", "Upper")),
    cpue_segment = cpue * proportion
  )

# Calculate proportion of CPUE WITH BOTH data types for each year
data_coverage_summary <- combined_data %>%
  group_by(year) %>%
  summarise(
    total_cpue = sum(cpue, na.rm = TRUE),
    cpue_with_both = sum(cpue[genetics_available & otolith_available], na.rm = TRUE),
    prop_with_both = cpue_with_both / total_cpue,
    pct_with_both = round(prop_with_both * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(label = paste0(pct_with_both, "%"))

# Define colors for genetic groups
genetic_colors <- c("Lower" = "#1b9e77", "Middle" = "#d95f02", "Upper" = "#7570b3")

# Create the multi-panel figure
p <- ggplot(combined_data, aes(x = DOY)) +
  # CPUE bars - gray for days without genetics
  geom_col(data = filter(combined_data, !genetics_available),
           aes(y = cpue), fill = "gray70", alpha = 0.8) +
  
  # Stacked CPUE bars colored by genetic composition for days WITH genetics
  geom_col(data = stacked_data, 
           aes(y = cpue_segment, fill = genetic_group), alpha = 0.85) +
  
  # Data availability markers for otolith data
  geom_point(data = filter(combined_data, otolith_available), 
             aes(y = max(combined_data$cpue, na.rm = TRUE) * 1.1), 
             color = "#e41a1c", shape = 16, size = 2, alpha = 0.7) +
  
  # Color scale for genetic groups
  scale_fill_manual(values = genetic_colors, name = "Genetic Group") +
  
  # Facet by year
  facet_wrap(~year, ncol = 1, scales = "free_y") +
  
  # Add text showing % CPUE with both data types
  geom_text(data = data_coverage_summary, 
            aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 4, 
            color = "firebrick", fontface = "bold") +
  
  # Labels and theme
  labs(title = "Yukon River: CPUE with Genetic Composition by Year",
       subtitle = "Bars colored by genetic group proportion on days with genetics  |  Red circles = Otolith data available",
       x = "Day of Year",
       y = "Daily CPUE Proportion") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom"
  )

# Save figure
output_file <- file.path(OUTPUT_DIR, "Yukon_Data_Availability_With_Genetics.png")
ggsave(output_file, p, width = 12, height = 10, dpi = 300)

cat("\n✓ Saved:", output_file, "\n")

# Print summary table
cat("\n=== DATA AVAILABILITY SUMMARY ===\n")
summary_table <- combined_data %>%
  group_by(year) %>%
  summarise(
    days_total = n(),
    days_with_genetics = sum(genetics_available),
    days_with_otolith = sum(otolith_available),
    days_with_both = sum(genetics_available & otolith_available),
    .groups = 'drop'
  )

print(summary_table)

cat("\n=== CPUE WITH BOTH DATA TYPES ===\n")
print(data_coverage_summary %>% select(year, total_cpue, cpue_with_both, pct_with_both))