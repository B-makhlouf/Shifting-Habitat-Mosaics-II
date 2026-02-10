################################################################################
# YUKON QUARTILE PRODUCTION + TEMPERATURE + CONTOUR FIGURES
# 
# Unified workflow that:
#   1. Reads NetCDF stream temperature data (daily, June-July)
#   2. Runs the Bayesian quartile production assignment per year
#      — ANALYSIS A: Yuk_Canada (Upper Yukon only)
#      — ANALYSIS B: Yuk_US (Lower & Middle Yukon combined)
#   3. Matches temperature to each quartile's ACTUAL date range
#   4. Computes production-weighted temperature per quartile
#   5. Produces contour figures (stream temp vs slope, air temp vs slope)
#      for both Canada and US analyses
#
# Key change from prior scripts: temperature is now averaged over each quartile's
# real date window (from the natal data), NOT hardcoded June weeks.
#
# Mirrors the Kuskokwim Quartile script exactly in methodology.
################################################################################

# =============================================================================
# LIBRARIES
# =============================================================================
suppressPackageStartupMessages({
  library(ncdf4)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  library(RColorBrewer)
  library(here)
  library(conflicted)
})

conflict_prefer("select",  "dplyr")
conflict_prefer("filter",  "dplyr")

# =============================================================================
# PATHS
# =============================================================================
PATHS <- list(
  # Shapefiles
  yukon_edges = here("Data","Spatial Data","AnalysisShapefiles","Yukon_edges.shp"),
  yukon_basin = here("Data","Spatial Data","AnalysisShapefiles","Yukon_basin.shp"),
  
  # NetCDF temperature directory (same Blaskey hindcast data)
  nc_temp_dir = here("Data","Spatial Data","Blaskey_Hindcast_simdata","Production"),
  
  # Natal origins & run size
  natal_data_dir = here("Data","Natal Origins"),
  runsize_data   = here("Data","AYKEscapement.xlsx"),
  
  # Annual production results (non-quartile) — used for assignment_norm
  annual_prod_dir_canada = here("Outputs","ProductionData","Yuk_Canada"),
  annual_prod_dir_us     = here("Outputs","ProductionData","Yuk_US"),
  
  # Outputs
  output_prod_canada = here("Outputs","ProductionData","Yuk_Canada","Quartiles"),
  output_prod_us     = here("Outputs","ProductionData","Yuk_US","Quartiles"),
  output_figures     = here("Figures","ContourMaps")
)

# Analysis years (Yukon available years)
YEARS <- c(2015, 2016, 2018, 2021)

# Yukon-specific parameters (matching 00_TotalProductionMaps.R)
MIN_STREAM_ORDER     <- 4
SENSITIVITY_THRESHOLD <- 0.7


################################################################################
# PART 1: EXTRACT DAILY STREAM TEMPERATURE FROM NetCDF FILES
################################################################################
# File structure: [hru, no_seg, time], variable = T_stream
# We want no_seg = 2 (downstream segment).  Time origin: <year>-01-01
# Filtered to June-July only.

cat("\n================================================================\n")
cat("PART 1: EXTRACTING DAILY STREAM TEMPERATURE FROM NetCDF FILES\n")
cat("================================================================\n")

nc_temp_files <- list.files(
  PATHS$nc_temp_dir,
  pattern = "^\\d+_(2015|2016|2017|2018|2019|2020|2021)\\.nc$",
  full.names = TRUE
)
cat("  Temperature files found:", length(nc_temp_files), "\n")

temp_daily_list <- vector("list", length(nc_temp_files))
pb <- txtProgressBar(min = 0, max = length(nc_temp_files), style = 3)

for (i in seq_along(nc_temp_files)) {
  setTxtProgressBar(pb, i)
  nc <- nc_open(nc_temp_files[i])
  
  vals      <- ncvar_get(nc, "T_stream")           # [hru, no_seg, time]
  reach_ids <- ncvar_get(nc, "hru")
  time_vals <- ncvar_get(nc, "time")
  nc_close(nc)
  
  yr    <- as.numeric(sub(".*_(\\d{4})\\.nc$", "\\1", basename(nc_temp_files[i])))
  dates <- as.Date(paste0(yr, "-01-01")) + time_vals
  vals2 <- vals[, 2, ]                             # downstream segment: [hru, time]
  jj    <- which(month(dates) %in% 6:7)
  
  if (length(jj) == 0) next
  
  # Subset to June-July
  dates_jj <- dates[jj]
  vals_jj  <- vals2[, jj, drop = FALSE]            # [n_reach, n_days]
  
  n_reach <- length(reach_ids)
  n_days  <- length(jj)
  
  # Vectorized expansion: each reach gets n_days rows
  temp_daily_list[[i]] <- data.frame(
    COMID = rep(reach_ids, times = n_days),
    date  = rep(dates_jj, each = n_reach),
    value = as.vector(vals_jj)                      # columns (days) stacked
  )
}
close(pb)

temp_daily <- bind_rows(temp_daily_list) %>% distinct(COMID, date, .keep_all = TRUE)
rm(temp_daily_list)  # free memory

cat("\n  Temperature daily rows:", nrow(temp_daily), "\n")
cat("  Unique COMIDs:", n_distinct(temp_daily$COMID), "\n")
cat("  Date range:", as.character(min(temp_daily$date)),
    "to", as.character(max(temp_daily$date)), "\n")


################################################################################
# PART 2: LOAD SPATIAL DATA (once — shared across all years and analyses)
################################################################################

cat("\n================================================================\n")
cat("PART 2: LOADING SPATIAL DATA\n")
cat("================================================================\n")

edges_full <- st_read(PATHS$yukon_edges, quiet = TRUE)
basin      <- st_read(PATHS$yukon_basin, quiet = TRUE)
edges_full <- st_transform(edges_full, st_crs(basin))

cat("  Total Yukon stream segments:", nrow(edges_full), "\n")

# Load run-size table once
runsizedat <- read_excel(PATHS$runsize_data)


################################################################################
# HELPER FUNCTION: Run Quartile Production + Temperature for one analysis type
################################################################################
#
# analysis_type: "canada" or "us"
# Returns a named list of year_results dataframes (for contour figures)
#
run_yukon_quartile_analysis <- function(analysis_type = "canada") {
  
  if (analysis_type == "canada") {
    analysis_label <- "Yuk_Canada (Upper only)"
    output_prod    <- PATHS$output_prod_canada
    annual_prod_dir <- PATHS$annual_prod_dir_canada
    annual_suffix  <- "_Yuk_Canada_Assignment_Results.csv"
    quartile_suffix <- "_Yuk_Canada_Quartile_Assignment_Results.csv"
  } else {
    analysis_label <- "Yuk_US (Lower + Middle)"
    output_prod    <- PATHS$output_prod_us
    annual_prod_dir <- PATHS$annual_prod_dir_us
    annual_suffix  <- "_Yuk_US_Assignment_Results.csv"
    quartile_suffix <- "_Yuk_US_Quartile_Assignment_Results.csv"
  }
  
  cat("\n================================================================\n")
  cat("RUNNING QUARTILE PRODUCTION + TEMPERATURE:", analysis_label, "\n")
  cat("================================================================\n")
  
  year_results <- list()
  
  for (yr in YEARS) {
    
    cat("\n--- Year", yr, "---\n")
    
    # ── Filter edges to the appropriate genetic region ───────────────────────
    if (analysis_type == "canada") {
      site_idx <- which(tolower(edges_full$GenLMU) == "upper")
    } else {
      site_idx <- which(tolower(edges_full$GenLMU) %in% c("lower", "middle"))
    }
    edges <- edges_full[site_idx, ]
    
    yukon_shp <- st_drop_geometry(edges)
    COMID     <- yukon_shp$COMID
    n_basins  <- nrow(edges)
    
    cat("  Stream segments:", n_basins, "\n")
    
    # ── Pre-compute error and priors ─────────────────────────────────────────
    pid_iso       <- edges$iso_pred
    pid_isose     <- edges$isose_pred
    pid_isose_mod <- rep(mean(pid_isose, na.rm = TRUE), length(pid_isose))
    error         <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)
    
    StreamOrderPrior  <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER, 1, 0)
    PresencePrior     <- ifelse((edges$Str_Order %in% c(7, 8, 9)) & edges$SPAWNING_C == 0, 0, 1)
    newhabitatprior   <- ifelse(edges$Channel_sl > 2.3, 0, 1)
    porcpupinepr      <- edges$Porc_off
    
    # For Canada (Upper), genetic prior uses edges$Upper column from natal data
    # For US (Lower+Middle), genetic prior is region-specific per fish
    # These are handled in the inner fish loop below
    
    # Identify Lower/Middle site indices within the filtered edges (US only)
    if (analysis_type == "us") {
      LYsites_filtered <- which(tolower(edges$GenLMU) == "lower")
      MYsites_filtered <- which(tolower(edges$GenLMU) == "middle")
    }
    
    # ── Load natal data & define quartile date breaks ────────────────────────
    natal_data <- read_csv(
      file.path(PATHS$natal_data_dir,
                paste0(yr, "_Yukon_Natal_Origins_Genetics_CPUE.csv")),
      show_col_types = FALSE
    )
    
    # Filter based on analysis type
    if (analysis_type == "canada") {
      natal_data <- natal_data %>%
        filter(!is.na(Upper), !is.na(natal_iso), !is.na(dailyCPUEprop))
    } else {
      natal_data <- natal_data %>%
        filter(!is.na(Lower), !is.na(Middle), !is.na(natal_iso), !is.na(dailyCPUEprop))
    }
    
    cat("  Natal observations:", nrow(natal_data), "\n")
    
    if (nrow(natal_data) == 0) {
      cat("  WARNING: No natal data for", yr, "— skipping\n")
      next
    }
    
    date_col <- if ("date" %in% names(natal_data)) "date" else "Date"
    natal_data[[date_col]] <- as.Date(natal_data[[date_col]])
    date_range  <- range(natal_data[[date_col]], na.rm = TRUE)
    date_breaks <- seq(date_range[1], date_range[2], length.out = 5)
    
    natal_data$quartile <- cut(
      natal_data[[date_col]],
      breaks = date_breaks,
      labels = c("Q1","Q2","Q3","Q4"),
      include.lowest = TRUE
    )
    
    cat("  Quartile date ranges:\n")
    for (q in 1:4) {
      cat("    Q", q, ": ", as.character(date_breaks[q]),
          " to ", as.character(date_breaks[q + 1]), "\n", sep = "")
    }
    
    # ── CPUE-weighted quartile run sizes ─────────────────────────────────────
    total_runsize <- as.numeric(
      runsizedat$Total_Run[runsizedat$River == "Yukon" & runsizedat$Year == yr]
    )
    total_cpue <- sum(natal_data$dailyCPUEprop, na.rm = TRUE)
    
    # Scale total_runsize by the genetic proportion for this analysis
    if (analysis_type == "canada") {
      avg_gen_prop <- mean(natal_data$Upper, na.rm = TRUE)
    } else {
      avg_gen_prop <- mean(natal_data$Lower + natal_data$Middle, na.rm = TRUE)
    }
    scaled_runsize <- total_runsize * avg_gen_prop
    
    quartile_cpue <- natal_data %>%
      group_by(quartile) %>%
      summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE), .groups = "drop") %>%
      mutate(cpue_proportion   = cpue_sum / total_cpue,
             quartile_runsize  = cpue_proportion * scaled_runsize)
    
    cat("  Total run size:", total_runsize, 
        " | Scaled (", analysis_type, "):", round(scaled_runsize), "\n")
    
    # ── Bayesian assignment per quartile ─────────────────────────────────────
    q_individuals <- matrix(0, nrow = n_basins, ncol = 4)
    colnames(q_individuals) <- paste0("Q", 1:4)
    
    for (qi in 1:4) {
      q_label  <- paste0("Q", qi)
      natal_q  <- natal_data %>% filter(quartile == q_label)
      q_runsize <- quartile_cpue$quartile_runsize[quartile_cpue$quartile == q_label]
      if (length(q_runsize) == 0) q_runsize <- 0
      
      if (nrow(natal_q) == 0) next
      
      n_fish   <- nrow(natal_q)
      asgn_mat <- matrix(0, nrow = n_basins, ncol = n_fish)
      
      for (fi in 1:n_fish) {
        fish_iso <- natal_q$natal_iso[fi]
        
        # Build genetic prior based on analysis type
        if (analysis_type == "canada") {
          gen_prior <- rep(as.numeric(natal_q$Upper[fi]), n_basins)
        } else {
          gen_prior <- rep(0, n_basins)
          gen_prior[LYsites_filtered] <- as.numeric(natal_q$Lower[fi])
          gen_prior[MYsites_filtered] <- as.numeric(natal_q$Middle[fi])
        }
        
        assign <- (1/sqrt(2*pi*error^2)) *
          exp(-1*(fish_iso - pid_iso)^2 / (2*error^2)) *
          StreamOrderPrior * gen_prior * PresencePrior * porcpupinepr * newhabitatprior
        
        assign_norm     <- assign / sum(assign)
        assign_rescaled <- assign_norm / max(assign_norm)
        assign_rescaled[assign_rescaled < SENSITIVITY_THRESHOLD] <- 0
        asgn_mat[, fi]  <- assign_rescaled * as.numeric(natal_q$COratio[fi])
      }
      
      basin_sum  <- rowSums(asgn_mat, na.rm = TRUE)
      total_sum  <- sum(basin_sum, na.rm = TRUE)
      
      if (total_sum > 0) {
        q_individuals[, qi] <- (basin_sum / total_sum) * q_runsize
      }
      
      cat("    ", q_label, ": ", nrow(natal_q), " fish, ",
          sum(basin_sum > 0), " segments assigned\n", sep = "")
    }
    
    # Total individuals across quartiles
    total_indiv <- rowSums(q_individuals)
    
    # ── Read assignment_norm from the ANNUAL (non-quartile) production file ──
    annual_file <- file.path(annual_prod_dir,
                             paste0(yr, annual_suffix))
    
    if (!file.exists(annual_file)) {
      cat("  WARNING: Annual production file not found:", annual_file, "\n")
      cat("  Computing assignment_norm from quartile totals instead.\n")
      # Fallback: derive from quartile totals
      basin_assign_sum <- total_indiv
      total_s <- sum(basin_assign_sum, na.rm = TRUE)
      if (total_s > 0) {
        basin_assign_rescale <- basin_assign_sum / total_s
        assignment_norm <- basin_assign_rescale / max(basin_assign_rescale, na.rm = TRUE)
      } else {
        assignment_norm <- rep(0, n_basins)
      }
    } else {
      annual_prod <- read_csv(annual_file, show_col_types = FALSE)
      assignment_norm <- annual_prod$assignment_norm[
        match(yukon_shp$reachid, annual_prod$reachid)
      ]
      assignment_norm[is.na(assignment_norm)] <- 0
    }
    
    # ── Match daily temperature to each quartile's date window ───────────────
    temp_yr <- temp_daily %>% filter(date >= date_range[1], date <= date_range[2])
    
    temp_yr$quartile <- as.character(cut(
      temp_yr$date, breaks = date_breaks,
      labels = paste0("Q", 1:4), include.lowest = TRUE
    ))
    
    # Average temperature per COMID x quartile
    temp_q_means <- temp_yr %>%
      filter(!is.na(quartile)) %>%
      group_by(COMID, quartile) %>%
      summarise(mean_temp = mean(value, na.rm = TRUE), .groups = "drop")
    
    # Build reach x quartile table with individuals + temperature
    prod_long <- data.frame(
      reachid     = rep(yukon_shp$reachid, 4),
      COMID       = rep(COMID, 4),
      quartile    = rep(paste0("Q", 1:4), each = n_basins),
      individuals = as.vector(q_individuals)
    ) %>%
      left_join(temp_q_means, by = c("COMID", "quartile"))
    
    # ── Production-weighted temperature per reach ────────────────────────────
    weighted_summary <- prod_long %>%
      group_by(reachid, COMID) %>%
      summarise(
        weighted_avg_temp = weighted.mean(mean_temp, w = individuals, na.rm = TRUE),
        total_individuals = sum(individuals, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        weighted_avg_temp = replace_na(weighted_avg_temp, 0),
        total_individuals = replace_na(total_individuals, 0)
      )
    
    # ── Build final analysis dataframe for this year ─────────────────────────
    snap_temp_col <- paste0("SnapTp", yr)
    
    # Check if SNAP temp column exists in shapefile
    has_snap <- snap_temp_col %in% names(yukon_shp)
    
    df_yr <- weighted_summary %>%
      mutate(Production = assignment_norm[match(reachid, yukon_shp$reachid)])
    
    if (has_snap) {
      df_yr <- df_yr %>%
        left_join(
          yukon_shp %>% select(reachid, all_of(snap_temp_col), Channel_sl),
          by = "reachid"
        ) %>%
        rename(
          mean_summer_temp = weighted_avg_temp,
          SNAP_temp        = !!sym(snap_temp_col)
        )
    } else {
      df_yr <- df_yr %>%
        left_join(
          yukon_shp %>% select(reachid, Channel_sl),
          by = "reachid"
        ) %>%
        rename(mean_summer_temp = weighted_avg_temp) %>%
        mutate(SNAP_temp = NA_real_)
      cat("  NOTE: SNAP temperature column '", snap_temp_col, 
          "' not found in shapefile — SNAP_temp set to NA\n", sep = "")
    }
    
    year_results[[as.character(yr)]] <- df_yr
    
    # ── Export quartile assignment CSV ────────────────────────────────────────
    dir.create(output_prod, recursive = TRUE, showWarnings = FALSE)
    
    output_data <- data.frame(
      reachid   = yukon_shp$reachid,
      Str_Order = yukon_shp$Str_Order,
      iso_pred  = yukon_shp$iso_pred,
      Q1_assignment_individuals = q_individuals[, 1],
      Q2_assignment_individuals = q_individuals[, 2],
      Q3_assignment_individuals = q_individuals[, 3],
      Q4_assignment_individuals = q_individuals[, 4],
      total_individuals         = total_indiv,
      assignment_norm           = assignment_norm,
      GENLMU                    = yukon_shp$GenLMU
    )
    
    write_csv(output_data,
              file.path(output_prod,
                        paste0(yr, quartile_suffix)))
    
    cat("  Exported production CSV for", yr, "\n")
  }
  
  return(year_results)
}


################################################################################
# PART 3: RUN BOTH ANALYSES
################################################################################

cat("\n================================================================\n")
cat("PART 3A: YUK_CANADA (UPPER YUKON ONLY)\n")
cat("================================================================\n")

year_results_canada <- run_yukon_quartile_analysis("canada")

cat("\n================================================================\n")
cat("PART 3B: YUK_US (LOWER & MIDDLE YUKON)\n")
cat("================================================================\n")

year_results_us <- run_yukon_quartile_analysis("us")


################################################################################
# PART 4: CONTOUR FIGURES
################################################################################
# Helper function to build a contour figure for one analysis
# Mirrors the Kuskokwim 10-panel layout exactly.

build_contour_figure <- function(year_results, years, analysis_label, filename_tag,
                                 x_lim_temp_override = NULL,
                                 y_lim_slope_override = NULL) {
  
  cat("\n================================================================\n")
  cat("BUILDING CONTOUR FIGURE:", analysis_label, "\n")
  cat("================================================================\n")
  
  # Check which years actually have results
  avail_years <- years[as.character(years) %in% names(year_results)]
  
  if (length(avail_years) == 0) {
    cat("  No year results available — skipping figure.\n")
    return(invisible(NULL))
  }
  
  # ── Filter to high-production reaches ──────────────────────────────────────
  filtered_list <- lapply(avail_years, function(yr) {
    year_results[[as.character(yr)]] %>%
      filter(Production > 0.7) %>%
      mutate(year = yr)
  })
  names(filtered_list) <- as.character(avail_years)
  
  # Check if we have enough data for contours
  for (yr_char in names(filtered_list)) {
    n_rows <- nrow(filtered_list[[yr_char]])
    cat("  Year", yr_char, "- high-production reaches:", n_rows, "\n")
    if (n_rows < 5) {
      cat("    WARNING: Very few points — contour may fail for this year.\n")
    }
  }
  
  # ── Determine if SNAP temp is available ────────────────────────────────────
  has_snap_data <- any(sapply(filtered_list, function(df) {
    any(!is.na(df$SNAP_temp))
  }))
  
  # ── Global axis limits ─────────────────────────────────────────────────────
  all_filtered <- bind_rows(filtered_list)
  
  x_lim_temp  <- range(all_filtered$mean_summer_temp, na.rm = TRUE)
  x_lim_temp  <- c(floor(x_lim_temp[1]), ceiling(x_lim_temp[2]))
  y_lim_slope <- c(0, min(3, ceiling(max(all_filtered$Channel_sl, na.rm = TRUE))))
  
  # Apply user overrides if provided
  if (!is.null(x_lim_temp_override))  x_lim_temp  <- x_lim_temp_override
  if (!is.null(y_lim_slope_override)) y_lim_slope <- y_lim_slope_override
  
  if (has_snap_data) {
    snap_vals <- all_filtered$SNAP_temp[!is.na(all_filtered$SNAP_temp)]
    if (length(snap_vals) > 0) {
      x_lim_air <- range(snap_vals, na.rm = TRUE)
      x_lim_air <- c(floor(x_lim_air[1]), ceiling(x_lim_air[2]))
    } else {
      has_snap_data <- FALSE
    }
  }
  
  # ── Colors ─────────────────────────────────────────────────────────────────
  fill_colors <- brewer.pal(9, "YlOrRd")[-1]
  
  # ── Shared theme ───────────────────────────────────────────────────────────
  base_theme <- theme_minimal() +
    theme(
      axis.text       = element_text(size = 8, color = "grey30"),
      axis.title      = element_blank(),
      legend.position = "none",
      panel.grid.major = element_line(color = "grey50", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.ontop      = TRUE,
      panel.background = element_rect(fill = NA, color = NA),
      plot.margin     = margin(1, 2, 1, 2),
      plot.title      = element_blank()
    )
  
  n_years <- length(avail_years)
  
  # ── Column 1: Stream Temperature vs Channel Slope ──────────────────────────
  plots_col1 <- lapply(seq_along(avail_years), function(i) {
    df <- filtered_list[[as.character(avail_years[i])]]
    is_bottom <- (i == n_years)
    
    p <- ggplot(df, aes(mean_summer_temp, Channel_sl)) +
      annotate("rect", xmin = -Inf, xmax = Inf,
               ymin = -Inf, ymax = Inf, fill = "white")
    
    # Only add contour if enough data points
    if (nrow(df) >= 5) {
      p <- p + stat_density_2d_filled(bins = 8)
    } else {
      p <- p + geom_point(alpha = 0.5, color = "firebrick")
    }
    
    p + scale_fill_manual(values = fill_colors) +
      scale_x_continuous(
        limits = x_lim_temp,
        expand = c(0, 0),
        labels = if (is_bottom) waiver() else NULL
      ) +
      scale_y_continuous(
        limits = y_lim_slope,
        expand = c(0, 0)
      ) +
      coord_cartesian(clip = "off") +
      base_theme +
      theme(
        axis.text.x = if (is_bottom)
          element_text(size = 8, color = "grey30")
        else element_blank()
      )
  })
  
  # ── Column 2: SNAP Air Temperature vs Channel Slope ────────────────────────
  if (has_snap_data) {
    plots_col2 <- lapply(seq_along(avail_years), function(i) {
      df <- filtered_list[[as.character(avail_years[i])]]
      is_bottom <- (i == n_years)
      
      p <- ggplot(df, aes(SNAP_temp, Channel_sl)) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -Inf, ymax = Inf, fill = "white")
      
      if (nrow(df) >= 5 && sum(!is.na(df$SNAP_temp)) >= 5) {
        p <- p + stat_density_2d_filled(bins = 8)
      } else {
        p <- p + geom_point(alpha = 0.5, color = "firebrick")
      }
      
      p + scale_fill_manual(values = fill_colors) +
        scale_x_continuous(
          limits = x_lim_air,
          expand = c(0, 0),
          labels = if (is_bottom) waiver() else NULL
        ) +
        scale_y_continuous(
          limits = y_lim_slope,
          expand = c(0, 0),
          labels = NULL
        ) +
        coord_cartesian(clip = "off") +
        base_theme +
        theme(
          axis.text.x = if (is_bottom)
            element_text(size = 8, color = "grey30")
          else element_blank(),
          axis.text.y = element_blank()
        )
    })
    
    ncols <- 3
    col_widths <- c(0.15, 1, 1)
    title_expr <- expression(
      paste("Stream Temperature vs Slope",
            "                         ",
            "Air Temperature vs Slope")
    )
    caption_expr <- expression(
      paste("Mean Summer Stream Temperature (\u00B0C)",
            "                                     ",
            "SNAP Air Temperature (\u00B0C)")
    )
    
  } else {
    # No SNAP data: single-column figure (stream temp only)
    plots_col2 <- NULL
    ncols <- 2
    col_widths <- c(0.15, 1)
    title_expr <- "Stream Temperature vs Slope"
    caption_expr <- expression(paste("Mean Summer Stream Temperature (\u00B0C)"))
  }
  
  # ── Year label panels ─────────────────────────────────────────────────────
  year_labels <- lapply(avail_years, function(yr) {
    ggplot() +
      annotate(
        "text",
        x = 0.5, y = 0.5,
        label = yr,
        hjust = 0.5,
        size = 4,
        fontface = "bold",
        color = "grey20"
      ) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  })
  
  # ── Assemble ───────────────────────────────────────────────────────────────
  flat_list <- list()
  for (i in seq_along(avail_years)) {
    if (has_snap_data) {
      flat_list <- c(flat_list, list(
        year_labels[[i]],
        plots_col1[[i]],
        plots_col2[[i]]
      ))
    } else {
      flat_list <- c(flat_list, list(
        year_labels[[i]],
        plots_col1[[i]]
      ))
    }
  }
  
  combined_plot <- wrap_plots(flat_list, ncol = ncols,
                              widths = col_widths) +
    plot_layout(heights = rep(1, n_years))
  
  # Column titles
  combined_plot <- combined_plot +
    plot_annotation(
      title = title_expr,
      theme = theme(
        plot.title = element_text(
          size = 12, face = "bold", hjust = 0.5,
          color = "grey10", margin = margin(b = 4)
        )
      )
    )
  
  # Shared y-axis label
  final_plot <- wrap_elements(combined_plot) +
    labs(tag = "Channel Slope") +
    theme(
      plot.tag          = element_text(size = 11, angle = 90, color = "grey20"),
      plot.tag.position = "left"
    )
  
  # Shared x-axis label
  final_with_xlab <- final_plot +
    plot_annotation(
      caption = caption_expr,
      theme = theme(
        plot.caption = element_text(
          size = 10, hjust = 0.55, color = "grey20",
          margin = margin(t = 2)
        )
      )
    )
  
  # ── Save ───────────────────────────────────────────────────────────────────
  dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)
  
  fig_height <- max(10, n_years * 3.4)
  fig_width  <- if (has_snap_data) 8.5 else 5.5
  
  outfile <- file.path(
    PATHS$output_figures,
    paste0("Quartile_StreamTemp_AirTemp_vs_Slope_", filename_tag, ".png")
  )
  
  ggsave(
    outfile,
    plot   = final_with_xlab,
    width  = fig_width,
    height = fig_height,
    dpi    = 300,
    bg     = "white"
  )
  
  cat("  Figure saved:", outfile, "\n")
  
  print(final_with_xlab)
  
  return(invisible(final_with_xlab))
}


################################################################################
# PART 4A: CANADA CONTOUR FIGURE
################################################################################

build_contour_figure(
  year_results  = year_results_canada,
  years         = YEARS,
  analysis_label = "Yuk_Canada (Upper Yukon)",
  filename_tag   = paste0("Yuk_Canada_", paste(range(YEARS), collapse = "-"))
)


################################################################################
# PART 4B: US CONTOUR FIGURE
################################################################################

build_contour_figure(
  year_results  = year_results_us,
  years         = YEARS,
  analysis_label = "Yuk_US (Lower & Middle Yukon)",
  filename_tag   = paste0("Yuk_US_", paste(range(YEARS), collapse = "-")),
  x_lim_temp_override  = c(6, 14),
  y_lim_slope_override = c(0, 2)
)


cat("\n================================================================\n")
cat("ALL YUKON FIGURES COMPLETE\n")
cat("================================================================\n")