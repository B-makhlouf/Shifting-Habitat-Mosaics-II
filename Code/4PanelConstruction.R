library(ggplot2)
library(patchwork)
library(magick)
library(cowplot)
library(stringr)
library(dplyr)
library(tidyr)


#### HUC 4 Panel 


if (T){

  # Directory containing your PDF files
pdf_dir <- "/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Basin Maps/Quartile/HUC/"

# Get list of all PDF files
pdf_files <- list.files(path = pdf_dir, 
                        pattern = "\\.pdf$", 
                        full.names = TRUE)

# Extract metadata from filenames
file_info <- data.frame(path = pdf_files) %>%
  mutate(
    filename = basename(path),
    year = str_extract(filename, "\\d{4}"),
    quartile = str_extract(filename, "QQ\\d"),
    quartile_num = as.numeric(str_remove(quartile, "QQ"))
  ) %>%
  arrange(year, quartile_num)

# Get unique years and quartiles for ordering
years <- unique(file_info$year)
quartiles <- unique(file_info$quartile_num)

# Create a plot for each file with annotation
plot_list <- list()
for (i in 1:nrow(file_info)) {
  img <- image_read_pdf(file_info$path[i])
  
  # Create a letter label based on position
  row_idx <- which(years == file_info$year[i])
  col_idx <- which(quartiles == file_info$quartile_num[i])
  label <- LETTERS[(row_idx - 1) * length(quartiles) + col_idx]
  
  plot_list[[i]] <- ggdraw() + 
    draw_image(img) +
    draw_label(label, x = 0.05, y = 0.95, size = 14, fontface = "bold") +
    labs(title = paste("Year:", file_info$year[i]),
         subtitle = paste("Quartile:", file_info$quartile_num[i]))
}

# Organize plots into a grid by year (rows) and quartile (columns)
combined <- wrap_plots(plotlist = plot_list, 
                       nrow = length(years), 
                       ncol = length(quartiles),
                       byrow = TRUE) +
  plot_annotation(
    title = "Kuskokwim River Basin Habitat Quartiles",
    subtitle = "Seasonal habitat distribution across years and flow quartiles",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  ) &
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8)
  )

# Save with high quality
ggsave(here("Basin Maps/Quartile/HUC/multi_year_quartile_figure.pdf"), 
       combined, 
       width = 4 * length(quartiles), 
       height = 3 * length(years), 
       dpi = 300)
}


## Trib panel - Annual Habitat Distribution
pdf_dir <- "/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Basin Maps/Full_year_all_individuals/Tribs"

# Get and process PDF files
pdf_files <- list.files(path = pdf_dir, 
                        pattern = "\\.pdf$", 
                        full.names = TRUE) %>% 
  # Create tibble with file info
  tibble(path = .) %>%
  mutate(
    filename = basename(path),
    year = str_extract(filename, "\\d{4}") %>% as.numeric()
  ) %>%
  arrange(year)  # Sort chronologically

# Create plot list with letter labels
plot_list <- map(1:nrow(pdf_files), function(i) {
  ggdraw() + 
    draw_image(image_read_pdf(pdf_files$path[i])) +
    labs(title = paste("Year:", pdf_files$year[i]))
})

# Create combined plot
combined_plot <- wrap_plots(plotlist = plot_list, 
                            nrow = 1,
                            widths = rep(1, nrow(pdf_files))) +
  plot_annotation(
    title = "Tributary Habitat Distribution by Year",
    theme = theme(
      plot.title = element_text(hjust = 0.1, face = "bold", size = 14),
      plot.margin = margin(0.1, 0.1, 0.2, 0.2, "cm")  # Small overall margin
    )
  ) &
  theme(
    plot.background = element_blank(),
    panel.spacing = unit(0, "cm")  # No space between panels
  )

# Save output
ggsave(here("Basin Maps/Full_year_all_individuals/Tribs/annual_tributary_panel.pdf"), 
       combined_plot,
       width = 3 * nrow(pdf_files),  # 3" per plot
       height = 5, 
       dpi = 300)

