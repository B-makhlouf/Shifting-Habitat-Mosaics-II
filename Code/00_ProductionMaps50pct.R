################################################################################
# COMBINED YUKON + KUSKOKWIM — FIRST 50% CPUE PRODUCTION ANALYSIS
# 
# Goal: For each overlapping year (2017, 2018, 2019, 2021), run the Bayesian
#        natal assignment for BOTH the Kuskokwim and Yukon (Full)
#        using only fish from the first 50% of the CPUE run (based on external
#        CPUE time series files). Combine the assignment vectors into a single 
#        data frame, scale to individuals, normalize, and rescale.
#
# This script is intentionally linear (no functions) for step-by-step clarity.
################################################################################


# ==============================================================================
# STEP 0: LIBRARIES
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(readxl)
  library(here)
})


# ==============================================================================
# STEP 1: CONFIGURATION
# ==============================================================================

PATHS <- list(
  # -- Kuskokwim shapefiles --
  kusko_edges  = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges.shp"),
  kusko_basin  = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  
  # -- Yukon shapefiles --
  yukon_edges  = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges2.shp"),
  yukon_basin  = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  yukon_ly_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_lYGen.shp"),
  yukon_my_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_mYGen.shp"),
  yukon_uy_gen = here("Data", "Spatial Data", "AnalysisShapefiles", "edges_uYGen.shp"),
  
  # -- Data inputs --
  natal_data_dir = here("Data", "Natal Origins"),
  cpue_data_dir  = here("Data", "CPUE"),
  runsize_data   = here("Data", "AYKEscapement.xlsx"),
  
  # -- Outputs --
  output_dir = here("Outputs", "ProductionData", "Combined_50pct")
)

# Years with data in BOTH rivers
YEARS <- c(2017, 2018, 2019, 2021)

# Kuskokwim-specific parameters
KUSKO_PARAMS <- list(
  min_stream_order      = 3,
  sensitivity_threshold = 0.7
)

# Yukon-specific parameters
YUKON_PARAMS <- list(
  min_stream_order      = 4,
  min_error             = 0.0035,
  sensitivity_threshold = 0.0
)


# ==============================================================================
# STEP 2: LOOP OVER YEARS
# ==============================================================================

for (year in YEARS) {
  
  cat("\n################################################################################\n")
  cat(paste0("# YEAR: ", year, " — Combined Kusko + Yukon 50% CPUE\n"))
  cat("################################################################################\n\n")
  
  tryCatch({
    
    
    # ==========================================================================
    # PART A: KUSKOKWIM
    # ==========================================================================
    
    cat("----------------------------------------------\n")
    cat("  PART A: KUSKOKWIM\n")
    cat("----------------------------------------------\n\n")
    
    
    # -- A1. Load Kuskokwim spatial data --------------------------------------
    
    kusko_edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
    kusko_basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
    kusko_edges <- st_transform(kusko_edges, st_crs(kusko_basin))
    
    n_kusko_segments <- nrow(kusko_edges)
    cat(paste("  Loaded", n_kusko_segments, "Kuskokwim stream segments\n"))
    
    
    # -- A2. Load Kuskokwim natal data ----------------------------------------
    
    kusko_natal_raw <- read_csv(
      file.path(PATHS$natal_data_dir, paste0(year, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
      show_col_types = FALSE
    ) %>%
      filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
    
    cat(paste("  Kusko observations (raw):", nrow(kusko_natal_raw), "\n"))
    
    
    # -- A3. Apply 50% CPUE cutoff to Kuskokwim -------------------------------
    #        Using external CPUE time series file
    
    kusko_cpue_raw <- read_csv(
      file.path(PATHS$cpue_data_dir, paste0("Kusko_CPUE_", year, ".csv")),
      show_col_types = FALSE
    ) %>%
      filter(!is.na(Date), !is.na('CumCPUE'))
    
    # Final cumulative CPUE = total run index
    kusko_total_cpue <- max(kusko_cpue_raw$cumCPUE, na.rm = TRUE)
    kusko_cpue_half  <- kusko_total_cpue / 2
    
    # Find the last date where cumulative CPUE <= 50% of total
    kusko_cutoff_date <- max(kusko_cpue_raw$Date[kusko_cpue_raw$cumCPUE <= kusko_cpue_half])
    kusko_cutoff_doy  <- as.numeric(format(as.Date(kusko_cutoff_date), "%j"))
    
    # Actual proportion at the cutoff date
    kusko_cpue_at_cutoff  <- kusko_cpue_raw$cumCPUE[kusko_cpue_raw$Date == kusko_cutoff_date]
    kusko_cpue_proportion <- kusko_cpue_at_cutoff / kusko_total_cpue
    
    # Filter natal data
    kusko_natal <- kusko_natal_raw %>%
      filter(DOY <= kusko_cutoff_doy)
    
    cat(paste("  Kusko total CPUE:", round(kusko_total_cpue, 2), "\n"))
    cat(paste("  Kusko 50% cutoff date:", kusko_cutoff_date, "(DOY:", kusko_cutoff_doy, ")\n"))
    cat(paste("  Kusko actual CPUE proportion at cutoff:", round(kusko_cpue_proportion, 4), "\n"))
    cat(paste("  Kusko observations retained:", nrow(kusko_natal),
              "(", round(nrow(kusko_natal) / nrow(kusko_natal_raw) * 100, 1), "%)\n"))
    
    if (nrow(kusko_natal) == 0) stop("No Kusko data after 50% CPUE filter!")
    
    
    # -- A4. Calculate Kuskokwim prediction error -----------------------------
    
    kusko_pid_iso       <- kusko_edges$iso_pred
    kusko_pid_isose     <- kusko_edges$isose_pred
    kusko_pid_isose_mod <- rep(mean(kusko_pid_isose, na.rm = TRUE), length(kusko_pid_isose))
    kusko_error         <- sqrt(kusko_pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
    
    
    # -- A5. Setup Kuskokwim priors -------------------------------------------
    
    kusko_StreamOrderPrior <- ifelse(kusko_edges$Str_Order >= KUSKO_PARAMS$min_stream_order, 1, 0)
    kusko_PresencePrior    <- ifelse((kusko_edges$Str_Order %in% c(6, 7)) & kusko_edges$SPAWNING_C == 0, 0, 1)
    kusko_NewHabitatPrior  <- ifelse(kusko_edges$Channel_sl > 2.5, 0, 1)
    kusko_pid_prior        <- kusko_edges$UniPh2oNoE
    
    
    # -- A6. Kuskokwim Bayesian assignment ------------------------------------
    
    cat("  Running Kusko Bayesian assignment...\n")
    
    n_kusko_fish <- nrow(kusko_natal)
    kusko_assignment_matrix <- matrix(0, nrow = n_kusko_segments, ncol = n_kusko_fish)
    
    for (i in 1:n_kusko_fish) {
      fish_iso <- kusko_natal$natal_iso[i]
      
      assign <- (1 / sqrt(2 * pi * kusko_error^2)) *
        exp(-1 * (fish_iso - kusko_pid_iso)^2 / (2 * kusko_error^2)) *
        kusko_StreamOrderPrior * kusko_PresencePrior *
        kusko_pid_prior * kusko_NewHabitatPrior
      
      assign_norm     <- assign / sum(assign)
      assign_rescaled <- assign_norm / max(assign_norm)
      assign_rescaled[assign_rescaled < KUSKO_PARAMS$sensitivity_threshold] <- 0
      
      kusko_assignment_matrix[, i] <- assign_rescaled * as.numeric(kusko_natal$COratio[i])
    }
    
    
    # -- A7. Sum across fish -> one value per Kusko segment -------------------
    
    kusko_basin_assign_sum <- apply(kusko_assignment_matrix, 1, sum, na.rm = TRUE)
    
    cat(paste("  Kusko segments with assignment > 0:",
              sum(kusko_basin_assign_sum > 0), "/", n_kusko_segments, "\n\n"))
    
    
    # ==========================================================================
    # PART B: YUKON (FULL Yukon)
    # ==========================================================================
    
    cat("----------------------------------------------\n")
    cat("  PART B: YUKON\n")
    cat("----------------------------------------------\n\n")
    
    
    # -- B1. Load Yukon spatial data ------------------------------------------
    
    yukon_edges <- st_read(PATHS$yukon_edges, quiet = TRUE)
    yukon_basin <- st_read(PATHS$yukon_basin, quiet = TRUE)
    yukon_edges <- st_transform(yukon_edges, st_crs(yukon_basin))
    
    # Load genetic region lookups
    ly_gen <- st_read(PATHS$yukon_ly_gen, quiet = TRUE)
    my_gen <- st_read(PATHS$yukon_my_gen, quiet = TRUE)
    uy_gen <- st_read(PATHS$yukon_uy_gen, quiet = TRUE)
    
    # Tag each edge with its genetic region
    yukon_edges$GenLMU <- "none"
    yukon_edges$GenLMU[yukon_edges$reachid %in% ly_gen$reachid] <- "lower"
    yukon_edges$GenLMU[yukon_edges$reachid %in% my_gen$reachid] <- "middle"
    yukon_edges$GenLMU[yukon_edges$reachid %in% uy_gen$reachid] <- "upper"
    
    # Index vectors for genetic regions
    LYsites <- which(yukon_edges$GenLMU == "lower")
    MYsites <- which(yukon_edges$GenLMU == "middle")
    UYsites <- which(yukon_edges$GenLMU == "upper")
    
    n_yukon_segments <- nrow(yukon_edges)
    cat(paste("  Loaded", n_yukon_segments, "Yukon stream segments\n"))
    cat(paste("    Lower:", length(LYsites), " | Middle:", length(MYsites),
              " | Upper:", length(UYsites), "\n"))
    
    
    # -- B2. Load Yukon natal data --------------------------------------------
    
    yukon_natal_raw <- read_csv(
      file.path(PATHS$natal_data_dir, paste0(year, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
      show_col_types = FALSE
    ) %>%
      filter(!is.na(Lower), !is.na(natal_iso), !is.na(dailyCPUEprop))
    
    cat(paste("  Yukon observations (raw):", nrow(yukon_natal_raw), "\n"))
    
    
    # -- B3. Apply 50% CPUE cutoff to Yukon -----------------------------------
    #        Using external CPUE time series file
    
    yukon_cpue_raw <- read_csv(
      file.path(PATHS$cpue_data_dir, paste0("Yukon_CPUE_", year, ".csv")),
      show_col_types = FALSE
    ) %>%
      filter(!is.na(Date), !is.na(`cumCPUE`))
    
    # Final cumulative CPUE = total run index
    yukon_total_cpue <- max(yukon_cpue_raw$cumCPUE, na.rm = TRUE)
    yukon_cpue_half  <- yukon_total_cpue / 2
    
    # Find the last date where cumulative CPUE <= 50% of total
    yukon_cutoff_date <- max(yukon_cpue_raw$Date[yukon_cpue_raw$cumCPUE <= yukon_cpue_half])
    yukon_cutoff_doy  <- as.numeric(format(as.Date(yukon_cutoff_date), "%j"))
    
    # Actual proportion at the cutoff date
    yukon_cpue_at_cutoff  <- yukon_cpue_raw$cumCPUE[yukon_cpue_raw$Date == yukon_cutoff_date]
    yukon_cpue_proportion <- yukon_cpue_at_cutoff / yukon_total_cpue
    
    # Filter natal data
    yukon_natal <- yukon_natal_raw %>%
      filter(DOY <= yukon_cutoff_doy)
    
    cat(paste("  Yukon total CPUE:", round(yukon_total_cpue, 2), "\n"))
    cat(paste("  Yukon 50% cutoff date:", yukon_cutoff_date, "(DOY:", yukon_cutoff_doy, ")\n"))
    cat(paste("  Yukon actual CPUE proportion at cutoff:", round(yukon_cpue_proportion, 4), "\n"))
    cat(paste("  Yukon observations retained:", nrow(yukon_natal),
              "(", round(nrow(yukon_natal) / nrow(yukon_natal_raw) * 100, 1), "%)\n"))
    
    if (nrow(yukon_natal) == 0) stop("No Yukon data after 50% CPUE filter!")
    
    
    # -- B4. Calculate Yukon prediction error ---------------------------------
    
    yukon_pid_iso       <- yukon_edges$iso_pred
    yukon_pid_isose     <- yukon_edges$isose_pred
    yukon_pid_isose_mod <- ifelse(yukon_pid_isose < YUKON_PARAMS$min_error,
                                  YUKON_PARAMS$min_error,
                                  yukon_pid_isose)
    yukon_error <- sqrt(yukon_pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
    
    
    # -- B5. Setup Yukon priors -----------------------------------------------
    
    yukon_StreamOrderPrior <- ifelse(yukon_edges$Str_Order >= YUKON_PARAMS$min_stream_order, 1, 0)
    yukon_PresencePrior    <- ifelse((yukon_edges$Str_Order %in% c(7, 8, 9)) &
                                       yukon_edges$SPAWNING_C == 0, 0, 1)
    
    
    # -- B6. Yukon Bayesian assignment ----------------------------------------
    
    cat("  Running Yukon Bayesian assignment...\n")
    
    n_yukon_fish <- nrow(yukon_natal)
    yukon_assignment_matrix <- matrix(0, nrow = n_yukon_segments, ncol = n_yukon_fish)
    
    for (i in 1:n_yukon_fish) {
      fish_iso <- yukon_natal$natal_iso[i]
      
      # Build genetic prior (fish-specific)
      gen_prior <- rep(0, n_yukon_segments)
      gen_prior[LYsites] <- as.numeric(yukon_natal$Lower[i])
      gen_prior[MYsites] <- as.numeric(yukon_natal$Middle[i])
      gen_prior[UYsites] <- as.numeric(yukon_natal$Upper[i])
      
      # Assignment probability
      assign <- (1 / sqrt(2 * pi * yukon_error^2)) *
        exp(-1 * (fish_iso - yukon_pid_iso)^2 / (2 * yukon_error^2)) *
        yukon_StreamOrderPrior * gen_prior * yukon_PresencePrior
      
      # Normalize and rescale
      assign_norm     <- assign / sum(assign)
      assign_rescaled <- assign_norm / max(assign_norm)
      assign_rescaled[assign_rescaled < YUKON_PARAMS$sensitivity_threshold] <- 0
      
      # Weight by CPUE
      yukon_assignment_matrix[, i] <- assign_rescaled * as.numeric(yukon_natal$COratio[i])
    }
    
    
    # -- B7. Sum across fish -> one value per Yukon segment -------------------
    
    yukon_basin_assign_sum <- apply(yukon_assignment_matrix, 1, sum, na.rm = TRUE)
    
    cat(paste("  Yukon segments with assignment > 0:",
              sum(yukon_basin_assign_sum > 0), "/", n_yukon_segments, "\n\n"))
    
    
    # ==========================================================================
    # PART C: COMBINE INTO A SINGLE DATA FRAME
    # ==========================================================================
    
    cat("----------------------------------------------\n")
    cat("  PART C: COMBINE RESULTS\n")
    cat("----------------------------------------------\n\n")
    
    # Build Kuskokwim rows
    kusko_df <- data.frame(
      river          = "Kusko",
      reachid        = st_drop_geometry(kusko_edges)$reachid,
      Str_Order      = st_drop_geometry(kusko_edges)$Str_Order,
      iso_pred       = st_drop_geometry(kusko_edges)$iso_pred,
      assignment_sum = kusko_basin_assign_sum
    )
    
    # Build Yukon rows
    yukon_df <- data.frame(
      river          = "Yukon",
      reachid        = st_drop_geometry(yukon_edges)$reachid,
      Str_Order      = st_drop_geometry(yukon_edges)$Str_Order,
      iso_pred       = st_drop_geometry(yukon_edges)$iso_pred,
      assignment_sum = yukon_basin_assign_sum
    )
    
    # Stack into one data frame
    combined_df <- bind_rows(kusko_df, yukon_df)
    
    cat(paste("  Combined data frame:", nrow(combined_df), "rows\n"))
    cat(paste("    Kusko rows:", nrow(kusko_df), "\n"))
    cat(paste("    Yukon rows:", nrow(yukon_df), "\n"))
    cat(paste("    Total segments with assignment > 0:",
              sum(combined_df$assignment_sum > 0), "\n\n"))
    
    
    # ==========================================================================
    # PART D: SCALE TO INDIVIDUALS, NORMALIZE, AND RESCALE
    # ==========================================================================
    
    cat("----------------------------------------------\n")
    cat("  PART D: SCALE, NORMALIZE, RESCALE\n")
    cat("----------------------------------------------\n\n")
    
    
    # -- D1. Load run size data -----------------------------------------------
    
    runsizedat <- read_excel(PATHS$runsize_data)
    
    kusko_runsize <- as.numeric(
      runsizedat$Total_Run[runsizedat$River == "Kusko" & runsizedat$Year == year]
    )
    yukon_runsize <- as.numeric(
      runsizedat$Total_Run[runsizedat$River == "Yukon" & runsizedat$Year == year]
    )
    
    # Scale run size by the actual CPUE proportion retained (from external files)
    kusko_runsize_scaled <- kusko_runsize * kusko_cpue_proportion
    yukon_runsize_scaled <- yukon_runsize * yukon_cpue_proportion
    
    cat(paste("  Kusko full run size:", kusko_runsize, "\n"))
    cat(paste("  Kusko CPUE proportion:", round(kusko_cpue_proportion, 4),
              "| Scaled run size:", round(kusko_runsize_scaled), "\n"))
    cat(paste("  Yukon full run size:", yukon_runsize, "\n"))
    cat(paste("  Yukon CPUE proportion:", round(yukon_cpue_proportion, 4),
              "| Scaled run size:", round(yukon_runsize_scaled), "\n\n"))
    
    
    # -- D2. Multiply each river's assignment_sum by its escapement -----------
    
    combined_df$runsize_scaled <- ifelse(combined_df$river == "Kusko",
                                         kusko_runsize_scaled,
                                         yukon_runsize_scaled)
    
    combined_df <- combined_df %>%
      group_by(river) %>%
      mutate(
        river_total = sum(assignment_sum, na.rm = TRUE),
        river_proportion = ifelse(river_total > 0, assignment_sum / river_total, 0),
        assignment_individuals = river_proportion * runsize_scaled
      ) %>%
      ungroup()
    
    cat(paste("  Kusko total individuals:",
              round(sum(combined_df$assignment_individuals[combined_df$river == "Kusko"])), "\n"))
    cat(paste("  Yukon total individuals:",
              round(sum(combined_df$assignment_individuals[combined_df$river == "Yukon"])), "\n"))
    
    
    # -- D3. Normalize across both rivers (sum to 1) --------------------------
    
    total_individuals <- sum(combined_df$assignment_individuals, na.rm = TRUE)
    combined_df$assignment_rescale <- combined_df$assignment_individuals / total_individuals
    
    cat(paste("  Total individuals (both rivers):", round(total_individuals), "\n"))
    cat(paste("  Sum of assignment_rescale:",
              round(sum(combined_df$assignment_rescale, na.rm = TRUE), 4), "\n"))
    
    
    # -- D4. Rescale to range 0-1 ---------------------------------------------
    
    max_rescale <- max(combined_df$assignment_rescale, na.rm = TRUE)
    combined_df$assignment_norm <- combined_df$assignment_rescale / max_rescale
    
    cat(paste("  Max assignment_norm:", max(combined_df$assignment_norm, na.rm = TRUE), "\n"))
    cat(paste("  Segments with norm > 0:", sum(combined_df$assignment_norm > 0), "\n\n"))
    
    
    # -- D5. Clean up helper columns ------------------------------------------
    
    combined_df <- combined_df %>%
      select(river, reachid, Str_Order, iso_pred,
             assignment_sum, assignment_individuals,
             assignment_rescale, assignment_norm)
    
    
    # ==========================================================================
    # PART E: EXPORT
    # ==========================================================================
    
    cat("----------------------------------------------\n")
    cat("  PART E: EXPORT\n")
    cat("----------------------------------------------\n\n")
    
    dir.create(PATHS$output_dir, recursive = TRUE, showWarnings = FALSE)
    
    filepath <- file.path(PATHS$output_dir,
                          paste0(year, "_Combined_50pct_Assignment_Results.csv"))
    write_csv(combined_df, filepath)
    cat(paste("  Exported:", filepath, "\n"))
    
    # ==========================================================================
    # PART F: CREATE COMBINED MAP
    # ==========================================================================
    
    cat("----------------------------------------------\n")
    cat("  PART F: CREATE COMBINED MAP\n")
    cat("----------------------------------------------\n\n")
    
    library(RColorBrewer)
    
    # -- F1. Get assignment_norm vectors for each river -----------------------
    
    kusko_assign_norm <- combined_df$assignment_norm[combined_df$river == "Kusko"]
    yukon_assign_norm <- combined_df$assignment_norm[combined_df$river == "Yukon"]
    
    
    # -- F2. Color scheme (same for both rivers) ------------------------------
    
    palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)
    
    legend_labels <- c("0.0-0.4", "0.4-0.7", "0.7-0.8", "0.8-0.9", "0.9-0.95", "0.95-1.0")
    legend_colors <- palette[c(2, 5, 7, 8, 9, 10)]
    
    # Kuskokwim colors
    kusko_colcode <- rep("gray90", length(kusko_assign_norm))
    kusko_colcode[kusko_assign_norm == 0] <- "white"
    kusko_colcode[kusko_assign_norm > 0.0  & kusko_assign_norm <= 0.4]  <- palette[2]
    kusko_colcode[kusko_assign_norm > 0.4  & kusko_assign_norm <= 0.7]  <- palette[5]
    kusko_colcode[kusko_assign_norm > 0.7  & kusko_assign_norm <= 0.8]  <- palette[7]
    kusko_colcode[kusko_assign_norm > 0.8  & kusko_assign_norm <= 0.9]  <- palette[8]
    kusko_colcode[kusko_assign_norm > 0.9  & kusko_assign_norm <= 0.95] <- palette[9]
    kusko_colcode[kusko_assign_norm > 0.95]                             <- palette[10]
    
    # Yukon colors
    yukon_colcode <- rep("gray90", length(yukon_assign_norm))
    yukon_colcode[yukon_assign_norm == 0] <- "white"
    yukon_colcode[yukon_assign_norm > 0.0  & yukon_assign_norm <= 0.4]  <- palette[2]
    yukon_colcode[yukon_assign_norm > 0.4  & yukon_assign_norm <= 0.7]  <- palette[5]
    yukon_colcode[yukon_assign_norm > 0.7  & yukon_assign_norm <= 0.8]  <- palette[7]
    yukon_colcode[yukon_assign_norm > 0.8  & yukon_assign_norm <= 0.9]  <- palette[8]
    yukon_colcode[yukon_assign_norm > 0.9  & yukon_assign_norm <= 0.95] <- palette[9]
    yukon_colcode[yukon_assign_norm > 0.95]                             <- palette[10]
    
    
    # -- F3. Line widths (river-specific) -------------------------------------
    
    # Kuskokwim line widths
    kusko_stream_order <- kusko_edges$Str_Order
    kusko_stream_order[is.na(kusko_stream_order)] <- 1
    
    kusko_linewidths <- ifelse(kusko_stream_order >= 9, 5,
                               ifelse(kusko_stream_order >= 8, 6,
                                      ifelse(kusko_stream_order >= 7, 5,
                                             ifelse(kusko_stream_order >= 6, 3.0,
                                                    ifelse(kusko_stream_order >= 5, 2.7,
                                                           ifelse(kusko_stream_order >= 4, 2.7,
                                                                  ifelse(kusko_stream_order >= 3, 1.2, 0)))))))
    
    kusko_linewidths[kusko_stream_order < KUSKO_PARAMS$min_stream_order] <- 0
    
    # Yukon line widths
    yukon_stream_order <- yukon_edges$Str_Order
    yukon_stream_order[is.na(yukon_stream_order)] <- 1
    
    yukon_linewidths <- ifelse(yukon_stream_order >= 9, 3.7,
                               ifelse(yukon_stream_order >= 8, 5,
                                      ifelse(yukon_stream_order >= 7, 2.0,
                                             ifelse(yukon_stream_order >= 6, 1.5,
                                                    ifelse(yukon_stream_order >= 5, 1.4,
                                                           ifelse(yukon_stream_order >= 4, 1.0, 0))))))
    
    yukon_linewidths[yukon_stream_order < YUKON_PARAMS$min_stream_order] <- 0
    
    
    # -- F4. Reproject to common CRS ------------------------------------------
    
    common_crs <- st_crs(kusko_basin)
    
    yukon_edges_reproj <- st_transform(yukon_edges, common_crs)
    yukon_basin_reproj <- st_transform(yukon_basin, common_crs)
    kusko_edges_reproj <- kusko_edges   # already in this CRS
    kusko_basin_reproj <- kusko_basin   # already in this CRS
    
    # Combined bounding box
    kusko_bbox <- st_bbox(kusko_basin_reproj)
    yukon_bbox <- st_bbox(yukon_basin_reproj)
    
    combined_bbox <- st_bbox(
      c(xmin = min(kusko_bbox["xmin"], yukon_bbox["xmin"]),
        ymin = min(kusko_bbox["ymin"], yukon_bbox["ymin"]),
        xmax = max(kusko_bbox["xmax"], yukon_bbox["xmax"]),
        ymax = max(kusko_bbox["ymax"], yukon_bbox["ymax"])),
      crs = common_crs
    )
    
    
    # -- F5. Plot -------------------------------------------------------------
    
    MAP_OUTPUT_DIR <- here("Figures", "Maps", "Combined_50pct")
    dir.create(MAP_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    map_filename <- file.path(MAP_OUTPUT_DIR, paste0("Combined_50pct_", year, ".png"))
    
    png(file = map_filename, width = 14, height = 10, units = "in", res = 300, bg = "white")
    par(mar = c(4, 4, 4, 2), bg = "white")
    
    # Initialize plot with combined extent
    plot(st_as_sfc(combined_bbox),
         col = NA, border = NA,
         main = paste0("Combined Production (First 50% CPUE)\nYear: ", year),
         axes = FALSE)
    
    # Draw both basins
    plot(st_geometry(kusko_basin_reproj), col = "gray60", border = "gray60", add = TRUE)
    plot(st_geometry(yukon_basin_reproj), col = "gray60", border = "gray60", add = TRUE)
    
    # Draw both stream networks
    plot(st_geometry(kusko_edges_reproj), col = kusko_colcode, pch = 16,
         axes = FALSE, add = TRUE, lwd = kusko_linewidths)
    plot(st_geometry(yukon_edges_reproj), col = yukon_colcode, pch = 16,
         axes = FALSE, add = TRUE, lwd = yukon_linewidths)
    
    # Legend
    legend("topleft", legend = legend_labels, col = legend_colors, lwd = 5,
           title = "Relative posterior density", bty = "n", bg = "white")
    
    dev.off()
    par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
    
    cat(paste("  Saved map:", map_filename, "\n"))
    
    
  }, error = function(e) {
    cat(paste("ERROR in year", year, ":", e$message, "\n"))
  })
}


cat("\n Combined 50% CPUE analysis complete\n")