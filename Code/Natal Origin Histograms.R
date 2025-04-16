library(here)
library(ggplot2)
library(patchwork)
library(scales)

if (T) {
  # Set directory path
  dest_dir <- here("Data/Natal Origin Data for analysis")
  file_names <- list.files(dest_dir, pattern = "\\.csv$", full.names = FALSE)
  
  # Filter for only Kusko files
  kusko_files <- grep("_Kusko_", file_names, value = TRUE)
  
  # Create a list to store all plots
  plot_list <- list()
  
  # Define consistent axis limits
  x_limits <- c(0.704, 0.712)
  x_breaks <- seq(0.704, 0.712, by = 0.001)
  y_limits <- c(0, 35)  # Set consistent y-axis limits
  
  for (file in kusko_files) {
    # Extract year and watershed from filename
    parts <- strsplit(file, "_")[[1]]
    year <- parts[1]
    watershed <- parts[2]
    
    # Read the data
    data <- read.csv(file.path(dest_dir, file))
    
    # Create high-quality histogram
    p <- ggplot(data, aes(x = natal_iso)) + 
      geom_histogram(bins = 30, fill = "#1f77b4", color = "white", alpha = 0.9, linewidth = 0.4) +
      scale_x_continuous(limits = x_limits, breaks = x_breaks, 
                         labels = scales::number_format(accuracy = 0.001)) +
      scale_y_continuous(limits = y_limits, expand = c(0, 0)) +  # Fixed y-axis limits
      labs(title = paste(year),
           x = expression(paste("Natal Isotope Ratio (", {}^87, "Sr/", {}^86, "Sr)")),
           y = "Count") +
      theme_minimal(base_size = 14) +  # Slightly larger base size for readability
      theme(
        text = element_text(family = "Lora", color = "black"),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14, face = "plain"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        plot.margin = margin(25, 15, 25, 15) # Extra margin for spacing
      )
    
    # Add to plot list
    plot_list[[file]] <- p
  }
  
  # Combine plots using patchwork with title and subtitle
  final_plot <- wrap_plots(plot_list, ncol = 1) + 
    plot_layout(guides = "collect") + 
    plot_annotation(
      title = "Kuskokwim Chinook Natal Origin Sr87/86 Ratios",
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
        plot.margin = margin(50, 30, 50, 30) # Extra margin for clarity
      )
    )
  
  # Save high-quality PDF with embedded fonts
  ggsave("kusko_natal_origin_histograms.pdf", final_plot, 
         width = 8.5, 
         height = 3 * length(plot_list),  # More height for spacing
         device = cairo_pdf)
  
  # Save high-resolution PNG (print-quality 600 DPI)
  ggsave("kusko_natal_origin_histograms.png", final_plot, 
         width = 8.5, 
         height = 3 * length(plot_list),  
         dpi = 600, 
         bg = "white")
}
