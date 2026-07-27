################################################################################
# THRESHOLD SENSITIVITY ANALYSIS
#
# Runs the full Contours.R pipeline (Yukon + Kuskokwim contour panels and
# top-N% basin maps) for CONTOUR_THRESHOLD values from 0.5 to 0.9 in steps
# of 0.1.
#
# Output structure:
#   Figures/Sensitivity/
#     threshold_0.5/
#       Contours/Yukon/   ← Yukon_WtrshdSlp_vs_DistUpstre.png
#                          ← Yukon_Top50_Contours_SixPanel.png
#       Contours/Kusko/   ← Kusko_WtrshdSlp_vs_DistUpstre.png
#                          ← Kusko_Top50_Contours_TwelvePanel.png
#       Maps/Yukon/        ← Yukon_YYYY_top50prod.png (one per year)
#       Maps/Kusko/        ← Kusko_YYYY_top50prod.png (one per year)
#     threshold_0.6/ ...
#     ...
#     threshold_0.9/ ...
#
# Spatial data (shapefiles) are loaded once on the first iteration and reused.
# Run time is dominated by KDE fitting — expect ~5–15 min total depending on
# hardware.
#
# USAGE:
#   Rscript Code/Analysis/01_DensityContours/Contours_sensitivity.R
#   # or from an R session at the project root:
#   source("Code/Analysis/01_DensityContours/Contours_sensitivity.R")
################################################################################

library(here)

THRESHOLDS <- seq(0.5, 0.9, by = 0.1)

cat("=================================================\n")
cat("  Threshold sensitivity sweep:", paste(round(THRESHOLDS, 1), collapse = ", "), "\n")
cat("=================================================\n")

for (thresh in THRESHOLDS) {

  thresh_label <- sprintf("%.1f", thresh)
  cat(sprintf("\n\n========== THRESHOLD: %s ==========\n", thresh_label))

  base_dir <- here("Figures", "Sensitivity", paste0("threshold_", thresh_label))

  # ------------------------------------------------------------------
  # Pre-set the four path variables that Contours.R and its sourced
  # scripts would otherwise define themselves. The if (!exists(...))
  # guards added to each script ensure these values are respected.
  # ------------------------------------------------------------------
  CONTOUR_THRESHOLD <- thresh
  out_dir           <- file.path(base_dir, "Contours", "Yukon")
  kusko_out_dir     <- file.path(base_dir, "Contours", "Kusko")
  map_dir_t50       <- file.path(base_dir, "Maps", "Yukon")
  map_dir_k50       <- file.path(base_dir, "Maps", "Kusko")

  dir.create(out_dir,       recursive = TRUE, showWarnings = FALSE)
  dir.create(kusko_out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(map_dir_t50,   recursive = TRUE, showWarnings = FALSE)
  dir.create(map_dir_k50,   recursive = TRUE, showWarnings = FALSE)

  # ------------------------------------------------------------------
  # Run the full pipeline. Spatial objects (YUKON_EDGES, YUKON_BASIN,
  # KUSKO_EDGES, KUSKO_BASIN) are cached in the global environment by
  # Contours.R / top50prod.R / kusko_top50prod.R after the first
  # iteration, so shapefiles are only read once.
  # ------------------------------------------------------------------
  source(here("Code", "Analysis", "01_DensityContours", "Contours.R"),
         local = FALSE)

  cat(sprintf("\nThreshold %s complete.\n  Saved to: %s\n",
              thresh_label, base_dir))

  # Clear path variables so next iteration's assignments are fresh
  rm(out_dir, kusko_out_dir, map_dir_t50, map_dir_k50, CONTOUR_THRESHOLD)
}

cat("\n=================================================\n")
cat("  All thresholds complete!\n")
cat("  Figures saved under:", here("Figures", "Sensitivity"), "\n")
cat("=================================================\n")
