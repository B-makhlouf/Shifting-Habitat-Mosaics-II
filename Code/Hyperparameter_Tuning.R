library(ggspatial)
library(tidyverse)
library(here)
library(sf)
library(cowplot)

# Source custom mapping functions
source(here("Code/Map Functions Full.R"))
source(here("Code/Map Functions Quartile.R"))

################################################################################
# DATA PREPARATION

# Set baseline parameters
baseline_params <- list(
  year = "2019",
  huc_level = 8,
  threshold = 0.5,
  error = 0.0006,
  stream_order = 3
)

# Custom color palette for visualization
custom_colors <- c(
  "Holitna River" = "#233d4d", 
  "Takotna River" = "#fe7f2d",
  "Stony River" = "#3b8ea5",
  "North Fork Kuskokwim River" = "#fcca46",
  "Middle Fork Kuskokwim River" = "#a1c181",
  "Kuskokwim Delta" = "#619b8a",
  "South Fork Kuskokwim River" = "#ab3428",
  "Aniak" = "#540804"
)

################################################################################
# ANALYSIS FUNCTION

run_sensitivity_analysis <- function(params, param_to_vary, values_to_test) {
  results <- list()
  
  for (value in values_to_test) {
    # Create parameter list with the varied parameter
    test_params <- params
    test_params[[param_to_vary]] <- value
    
    map_result <- KK_Map_func(
      year = test_params$year,
      sensitivity_threshold = test_params$threshold,
      min_error = test_params$error,
      min_stream_order = test_params$stream_order,
      HUC = test_params$huc_level,
      return_values = TRUE
    )
    
    if (!is.null(map_result)) {
      huc_data <- map_result$huc_data %>%
        st_drop_geometry() %>%
        select(HUC8, NAME, production_proportion) %>%
        mutate(
          varied_param = param_to_vary,
          param_value = value,
          production_proportion = production_proportion / sum(production_proportion, na.rm = TRUE)
        )
      
      results[[as.character(value)]] <- huc_data
    }
  }
  
  bind_rows(results)
}

# Run analyses for each parameter
stream_order_results <- run_sensitivity_analysis(
  baseline_params,
  "stream_order",
  c(2, 3, 4, 5)
)

threshold_results <- run_sensitivity_analysis(
  baseline_params,
  "threshold",
  c(0.001, 0.5, 0.7, .9, .95)
)

error_results <- run_sensitivity_analysis(
  baseline_params,
  "error",
  c(0.0006, 0.0008, 0.001)
)

# Combine all results
all_results <- bind_rows(
  stream_order_results,
  threshold_results,
  error_results
) %>%
  mutate(
    varied_param = factor(varied_param,
                          levels = c("stream_order", "threshold", "error"),
                          labels = c("Minimum Stream Order", "Sensitivity Threshold", "Minimum Error")),
    param_value = as.factor(param_value)
  )

################################################################################
# CREATE PLOTS

# 1. Orientation map (reference)
kusk_edges <- st_read("/Users/benjaminmakhlouf/Desktop/Clean_shapefiles/kusko_cleaned_wgroups.shp")
huc_polygons <- st_read(paste0("/Users/benjaminmakhlouf/Spatial Data/HUC", baseline_params$huc_level, "_Trimmed.shp")) %>%
  st_transform(st_crs(kusk_edges))

named_polygons <- huc_polygons %>% 
  filter(!is.na(NAME), NAME %in% unique(all_results$NAME))

orientation_map <- ggplot() +
  geom_sf(data = named_polygons, aes(fill = NAME), color = "white", size = 0.3) +
  scale_fill_manual(values = custom_colors, name = "Tributary Areas") +
  theme_void() +
  theme(
    legend.position = "none",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(title = "Kuskokwim River\nTributary Areas")

# 2. Create bar plots for each parameter
create_bar_plot <- function(data, param_name) {
  ggplot(data, aes(x = param_value, y = production_proportion, fill = NAME)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Varying", param_name),
      x = param_name,
      y = "Proportion of Total Production"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 12),
      axis.title = element_text(size = 10)
    )
}

stream_order_plot <- create_bar_plot(
  filter(all_results, varied_param == "Minimum Stream Order"),
  "Stream Order")

threshold_plot <- create_bar_plot(
  filter(all_results, varied_param == "Sensitivity Threshold"),
  "Threshold")

error_plot <- create_bar_plot(
  filter(all_results, varied_param == "Minimum Error"),
  "Minimum Error")

# Combine the bar plots
bar_plots <- plot_grid(
  stream_order_plot,
  threshold_plot,
  error_plot,
  ncol = 3,
  align = "h"
)

# Combine with orientation map
combined_plot <- plot_grid(
  orientation_map,
  bar_plots,
  nrow = 1,
  rel_widths = c(1, 2) # Adjust width ratio for better balance
)

# Add title
title <- ggdraw() + 
  draw_label(
    paste("Kuskokwim Salmon Production Sensitivity Analysis -", baseline_params$year),
    fontface = "bold",
    size = 14,
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

final_plot <- plot_grid(
  title,
  combined_plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# Save the final plot
ggsave(
  here("Figures/Hyperparameter Tuning/parameter_sensitivity_analysis.pdf"),
  final_plot,
  width = 16,
  height = 8,
  dpi = 300
)
