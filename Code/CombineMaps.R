library(cowplot)
library(magick)
library(ggplot2)

BASE <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II"
MAPS_DIR <- file.path(BASE, "Figures/Maps/Yukon_Annual")
OUTPUT_DIR <- file.path(BASE, "Figures")
YEARS <- c(2015, 2016, 2021)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

for (year in YEARS) {
  
  maps <- list(
    age1 = file.path(
      MAPS_DIR, "AgeClass",
      paste0(year, "_Age1.3_Yukon.png")
    ),
    
    age2 = file.path(
      MAPS_DIR, "AgeClass",
      paste0(year, "_Age1.4_Yukon.png")
    )
  )
  
  if (!all(sapply(maps, file.exists))) {
    cat("Skipping", year, "- missing maps\n")
    print(maps[!sapply(maps, file.exists)])
    next
  }
  
  img1 <- ggdraw() + draw_image(maps$age1)
  img2 <- ggdraw() + draw_image(maps$age2)
  
  p <- plot_grid(
    img1, img2,
    labels = c("A", "B"),
    label_size = 20,
    ncol = 1, nrow = 2
  )
  
  output_file <- file.path(
    OUTPUT_DIR,
    paste0("Yukon_AgeClass_", year, ".png")
  )
  
  ggsave(
    output_file, p,
    width = 9, height = 10,
    dpi = 300, bg = "white"
  )
  
  cat("✓", year, "\n")
}