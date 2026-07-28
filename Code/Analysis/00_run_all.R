################################################################################
# 00_run_all.R — Run the full analysis pipeline in order
#
# USAGE (from project root):
#   Rscript Code/Analysis/00_run_all.R
#   source("Code/Analysis/00_run_all.R")
################################################################################

project_library <- file.path(getwd(), ".r-library")
if (dir.exists(project_library)) {
  .libPaths(c(project_library, .libPaths()))
}

library(here)

cat("=======================================================\n")
cat("  Shifting Habitat Mosaics II — Full Analysis Pipeline\n")
cat("=======================================================\n\n")

# --- Strict clean build: remove every existing figure -------------------------
# Nothing under Figures/ is preserved. If a viewer has locked any file, stop
# rather than silently mixing stale and newly generated outputs.
fig_root <- here("Figures")
old_figs <- list.files(fig_root, recursive = TRUE, full.names = TRUE,
                       include.dirs = FALSE)
if (length(old_figs)) {
  remaining <- old_figs
  for (attempt in seq_len(3L)) {
    suppressWarnings(file.remove(remaining))
    remaining <- remaining[file.exists(remaining)]
    if (!length(remaining)) break
    Sys.sleep(0.5)
  }
  if (length(remaining)) {
    stop(
      "Strict clean build could not delete these locked figure files:\n  ",
      paste(remaining, collapse = "\n  "),
      "\nClose them in any image/PDF viewer and rerun 00_run_all.R.",
      call. = FALSE
    )
  }
  cat(sprintf("Cleared all %d existing figure file(s) from %s\n\n",
              length(old_figs), fig_root))
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

# --- Verify the complete expected figure inventory ----------------------------
expected_figures <- c(
  here(
    "Figures", "01_ProdMaps", "Kusko",
    sprintf("Kusko_%d_relprod.png", KUSKO_YEARS)
  ),
  here(
    "Figures", "01_ProdMaps", "Yukon",
    sprintf("Yukon_Full_%d_relprod.png", YUKON_YEARS)
  ),
  here(
    "Figures", "02_Contours", "01_annual_contours",
    sprintf(
      "Kusko_%d_contours_thresh%s.png",
      KUSKO_YEARS,
      format(CONTOUR_FILT_THRESH, scientific = FALSE, trim = TRUE)
    )
  ),
  here(
    "Figures", "02_Contours", "01_annual_contours",
    sprintf(
      "Yukon_%d_contours_thresh%s.png",
      YUKON_YEARS,
      format(CONTOUR_FILT_THRESH, scientific = FALSE, trim = TRUE)
    )
  ),
  here(
    "Figures", "02_Contours", "02_change_from_average",
    sprintf("Kusko_%d_change_from_average.png", KUSKO_YEARS)
  ),
  here(
    "Figures", "02_Contours", "02_change_from_average",
    sprintf("Yukon_%d_change_from_average.png", YUKON_YEARS)
  ),
  here(
    "Figures", "00_PubFigures",
    sprintf(
      "Figure%d_%s.png",
      1:7,
      c(
        "KuskoMultiPanel", "YukonMultiPanel", "SpawnerAbundanceCV",
        "RelativeProdCV", "BrennanPairwiseChange",
        "TributaryPairwiseChange", "TributaryPairwiseByCatchmentLength"
      )
    )
  ),
  here(
    "Figures", "03_PortfolioEffect",
    "BrennanPairwiseChange_upstreamLength_t0.7.png"
  )
)
missing_figures <- expected_figures[!file.exists(expected_figures)]
if (length(missing_figures)) {
  stop(
    "Pipeline finished with missing expected figures:\n  ",
    paste(missing_figures, collapse = "\n  "),
    call. = FALSE
  )
}
cat(sprintf(
  "\nVerified clean regeneration of %d expected figure files.\n",
  length(expected_figures)
))

cat("\n=======================================================\n")
cat("  Pipeline complete.\n")
cat("=======================================================\n")
