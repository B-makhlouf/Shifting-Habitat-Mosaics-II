################################################################################
# YUKON GENETIC GROUP CPUE BY DAY - STACKED BAR CHART (ALL YEARS)
################################################################################
library(dplyr)
library(ggplot2)
library(readr)

# Configuration
DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures"
YEARS <- c(2015, 2016, 2018, 2021)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Compile data across all years
all_cpue_data <- list()
all_genetic_data <- list()

for (year in YEARS) {
  cat(paste("Processing Yukon", year, "...\n"))
  
  data_file <- file.path(DATA_DIR, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv"))
  if (!file.exists(data_file)) {
    cat("  File not found - skipping\n")
    next
  }
  
  natal_data <- read_csv(data_file, show_col_types = FALSE) %>%
    filter(!is.na(dailyCPUEprop), !is.na(DOY))
  
  # Get ALL CPUE data (for grey bars)
  all_cpue <- natal_data %>%
    group_by(DOY) %>%
    summarise(
      total_cpue = first(dailyCPUEprop),
      .groups = 'drop'
    ) %>%
    mutate(year = year)
  
  all_cpue_data[[as.character(year)]] <- all_cpue
  
  # Get genetic data only where available
  genetic_data <- natal_data %>%
    filter(!is.na(Lower), !is.na(Middle), !is.na(Upper),
           (Lower > 0 | Middle > 0 | Upper > 0)) %>%
    mutate(
      cpue_lower = dailyCPUEprop * Lower,
      cpue_middle = dailyCPUEprop * Middle,
      cpue_upper = dailyCPUEprop * Upper
    ) %>%
    group_by(DOY) %>%
    summarise(
      Lower = sum(cpue_lower, na.rm = TRUE),
      Middle = sum(cpue_middle, na.rm = TRUE),
      Upper = sum(cpue_upper, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(year = year) %>%
    tidyr::pivot_longer(cols = c(Lower, Middle, Upper), names_to = "Genetic_Group", values_to = "CPUE")
  
  all_genetic_data[[as.character(year)]] <- genetic_data
}

# Combine data
combined_cpue <- bind_rows(all_cpue_data)
combined_genetic <- bind_rows(all_genetic_data)

# Create multi-panel plot with grey background and genetic overlay
p <- ggplot() +
  # Grey bars for ALL CPUE
  geom_col(data = combined_cpue, aes(x = DOY, y = total_cpue), 
           fill = "grey70", width = 1) +
  # Colored stacked bars for genetic data
  geom_col(data = combined_genetic, aes(x = DOY, y = CPUE, fill = Genetic_Group), 
           width = 1) +
  scale_fill_manual(
    values = c("Lower" = "#5ABCB9", "Middle" = "#4F5D75", "Upper" = "#42CAFD"),
    name = "Genetic Group"
  ) +
  facet_wrap(~year, ncol = 1, scales = "free_y") +
  labs(
    title = "Yukon River: Daily CPUE by Genetic Group",
    x = "Day of Year",
    y = "Daily CPUE Proportion"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

# Save plot
output_file <- file.path(OUTPUT_DIR, "Yukon_Genetic_CPUE_AllYears.png")
ggsave(output_file, p, width = 12, height = 10, dpi = 300)

cat("\n✓ Saved:", output_file, "\n")