################################################################################
# 00_run_all.R — Run the full analysis pipeline in order
#
# USAGE (from project root):
#   Rscript Code/Analysis/00_run_all.R
#   source("Code/Analysis/00_run_all.R")
################################################################################

library(here)

cat("=======================================================\n")
cat("  Shifting Habitat Mosaics II — Full Analysis Pipeline\n")
cat("=======================================================\n\n")

# --- Clear all existing figures so they repopulate visibly on each run -------
# Removes every file under Figures/ (all subfolders) but keeps the directory
# tree, so each script's dir.create() still finds its target folder.
fig_root <- here("Figures")
old_figs <- list.files(fig_root, recursive = TRUE, full.names = TRUE,
                       include.dirs = FALSE)
if (length(old_figs)) {
  removed <- suppressWarnings(file.remove(old_figs))
  if (any(!removed))
    cat(sprintf("Note: %d figure(s) locked (likely open in a viewer), skipped:\n  %s\n",
                sum(!removed), paste(basename(old_figs[!removed]), collapse = ", ")))
  cat(sprintf("Cleared %d existing figure file(s) from %s\n\n",
              sum(removed), fig_root))
} else {
  cat(sprintf("Cleared 0 existing figure file(s) from %s\n\n", fig_root))
}

source(here("Code", "Analysis", "01_FullBasinRelativeProdMaps.R"))
source(here("Code", "Analysis", "02_ContourThreshnew.R"))
source(here("Code", "Analysis", "03_VarianceBuffering.R"))
source(here("Code", "Analysis", "03b_DistinctTributaryPairwiseFigure.R"))
source(here("Code", "Analysis", "03c_BrennanPairwiseFigure.R"))
source(here("Code", "Analysis", "03d_AbsoluteCVStandaloneFigure.R"))
source(here("Code", "Analysis", "03e_TributaryPairwiseFigure.R"))
source(here("Code", "Analysis", "03f_TributaryPairwiseByCatchmentLength.R"))
source(here("Code", "Analysis", "PresentationFigures.R"))

cat("\n=======================================================\n")
cat("  Pipeline complete.\n")
cat("=======================================================\n")
