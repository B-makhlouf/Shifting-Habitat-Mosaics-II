library(cowplot)
library(magick)
library(ggplot2)

BASE <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II"

MAPS_DIR <- file.path(BASE, "Maps/Yukon_Annual")
OUTPUT_DIR <- file.path(BASE, "Figures")
YEARS <- c(2015, 2016, 2021)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

for (year in YEARS) {
  
  maps <- list(
    production = file.path(
      MAPS_DIR, "Production", "Full_Year",
      paste0(year, "_Yukon_Annual_Production.png")
    ),
    
    growth = file.path(
      MAPS_DIR, "Growth",
      paste0(year, "_Top20Growth_Yukon.png")
    ),
    
    age1 = file.path(
      MAPS_DIR, "AgeClass",
      paste0(year, "_1.3_AgeClass_Yukon.png")
    ),
    
    age2 = file.path(
      MAPS_DIR, "AgeClass",
      paste0(year, "_1.4_AgeClass_Yukon.png")
    )
  )
  
  if (!all(sapply(maps, file.exists))) {
    cat("Skipping", year, "- missing maps\n")
    print(maps[!sapply(maps, file.exists)])
    next
  }
  
  img2 <- ggdraw() + draw_image(maps$growth)
  img3 <- ggdraw() + draw_image(maps$age1)
  img4 <- ggdraw() + draw_image(maps$age2)
  
  p <- plot_grid(
    img2, img3, img4,
    labels = c("A", "B", "C", "D"),
    label_size = 20,
    ncol = , nrow = 1
  )
  
  output_file <- file.path(
    OUTPUT_DIR,
    paste0("Yukon_FourPanel_", year, ".png")
  )
  
  ggsave(
    output_file, p,
    width = 20, height = 5,
    dpi = 300, bg = "white"
  )
  
  cat("✓", year, "\n")
}
