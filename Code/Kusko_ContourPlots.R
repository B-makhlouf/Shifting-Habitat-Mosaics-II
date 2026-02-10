################################################################################
# KUSKOKWIM QUARTILE PRODUCTION + TEMPERATURE + CONTOUR FIGURES
# 
# Unified workflow that:
#   1. Reads NetCDF stream temperature data (daily, June-July)
#   2. Runs the Bayesian quartile production assignment per year
#   3. Matches temperature to each quartile's ACTUAL date range
#   4. Computes production-weighted temperature per quartile
#   5. Produces the 10-panel contour figure (stream temp vs slope, air temp vs slope)
#
# Key change from prior scripts: temperature is now averaged over each quartile's
# real date window (from the natal data), NOT hardcoded June weeks.
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
  kusko_edges = here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"),
  kusko_basin = here("Data","Spatial Data","AnalysisShapefiles","Kusko_basin.shp"),
  
  # NetCDF temperature directory
  nc_temp_dir = here("Data","Spatial Data","Blaskey_Hindcast_simdata","Production"),
  
  # Natal origins & run size
  natal_data_dir = here("Data","Natal Origins"),
  runsize_data   = here("Data","AYKEscapement.xlsx"),
  
  # Annual production results (non-quartile)
  annual_prod_dir = here("Outputs","ProductionData"),
  
  # Outputs
  output_prod    = here("Outputs","ProductionData","Quartiles"),
  output_figures = here("Figures","ContourMaps")
)

# Analysis years
YEARS <- 2017:2021


################################################################################
# PART 1: EXTRACT DAILY STREAM TEMPERATURE FROM NetCDF FILES
################################################################################
# File structure: [hru, no_seg, time], variable = T_stream
# We want no_seg = 2 (downstream segment).  Time origin: <year>-01-01
# Filtered to June-July only.
#
# PERFORMANCE NOTE: We use vectorized matrix expansion (rep + as.vector) instead
# of per-reach lapply to avoid R's slow row-binding bottleneck.

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
# PART 2: LOAD SPATIAL DATA (once — shared across all years)
################################################################################

cat("\n================================================================\n")
cat("PART 2: LOADING SPATIAL DATA\n")
cat("================================================================\n")

edges <- st_read(PATHS$kusko_edges, quiet = TRUE)
basin <- st_read(PATHS$kusko_basin, quiet = TRUE)
edges <- st_transform(edges, st_crs(basin))

kusko_shp <- st_drop_geometry(edges)
COMID     <- kusko_shp$COMID

cat("  Stream segments:", nrow(edges), "\n")

# Pre-compute error and priors (constant across years)
pid_iso       <- edges$iso_pred
pid_isose     <- edges$isose_pred
pid_isose_mod <- rep(mean(pid_isose, na.rm = TRUE), length(pid_isose))
error         <- sqrt(pid_isose_mod^2 + (0.0003133684/1.96)^2 + (0.00011/2)^2)

StreamOrderPrior  <- ifelse(edges$Str_Order >= 3, 1, 0)
PresencePrior     <- ifelse((edges$Str_Order %in% c(6, 7)) & edges$SPAWNING_C == 0, 0, 1)
NewHabitatPrior   <- ifelse(edges$Channel_sl > 2.5, 0, 1)
pid_prior         <- edges$UniPh2oNoE

# Pre-compute the combined spatial prior (element-wise product, same for all fish)
combined_prior <- StreamOrderPrior * PresencePrior * pid_prior * NewHabitatPrior

# Load run-size table once
runsizedat <- read_excel(PATHS$runsize_data)


################################################################################
# PART 3: YEAR LOOP — Quartile assignment + temperature matching
################################################################################

cat("\n================================================================\n")
cat("PART 3: RUNNING QUARTILE PRODUCTION + TEMPERATURE PER YEAR\n")
cat("================================================================\n")

year_results <- list()

for (yr in YEARS) {
  
  cat("\n--- Year", yr, "---\n")
  
  # ── 3a. Load natal data & define quartile date breaks ──────────────────────
  natal_data <- read_csv(
    file.path(PATHS$natal_data_dir,
              paste0(yr, "_Kusko_Natal_Origins_Genetics_CPUE.csv")),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(natal_iso), !is.na(dailyCPUEprop))
  
  cat("  Natal observations:", nrow(natal_data), "\n")
  
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
  
  # ── 3b. CPUE-weighted quartile run sizes ───────────────────────────────────
  total_runsize <- as.numeric(
    runsizedat$Total_Run[runsizedat$River == "Kusko" & runsizedat$Year == yr]
  )
  total_cpue <- sum(natal_data$dailyCPUEprop, na.rm = TRUE)
  
  quartile_cpue <- natal_data %>%
    group_by(quartile) %>%
    summarise(cpue_sum = sum(dailyCPUEprop, na.rm = TRUE), .groups = "drop") %>%
    mutate(cpue_proportion   = cpue_sum / total_cpue,
           quartile_runsize  = cpue_proportion * total_runsize)
  
  cat("  Total run size:", total_runsize, "\n")
  
  # ── 3c. Bayesian assignment per quartile ───────────────────────────────────
  n_basins <- nrow(edges)
  q_individuals <- matrix(0, nrow = n_basins, ncol = 4)
  colnames(q_individuals) <- paste0("Q", 1:4)
  
  for (qi in 1:4) {
    q_label  <- paste0("Q", qi)
    natal_q  <- natal_data %>% filter(quartile == q_label)
    q_runsize <- quartile_cpue$quartile_runsize[quartile_cpue$quartile == q_label]
    if (length(q_runsize) == 0) q_runsize <- 0
    
    if (nrow(natal_q) == 0) next
    
    n_fish <- nrow(natal_q)
    asgn_mat <- matrix(0, nrow = n_basins, ncol = n_fish)
    
    for (fi in 1:n_fish) {
      fish_iso <- natal_q$natal_iso[fi]
      assign <- (1/sqrt(2*pi*error^2)) *
        exp(-1*(fish_iso - pid_iso)^2 / (2*error^2)) *
        combined_prior
      
      assign_norm     <- assign / sum(assign)
      assign_rescaled <- assign_norm / max(assign_norm)
      assign_rescaled[assign_rescaled < 0.7] <- 0
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
  
  # ── 3d. Read assignment_norm from the ANNUAL (non-quartile) production file ──
  # This matches the original contour script, which used the annual assignment
  # results rather than deriving assignment_norm from quartile totals.
  annual_prod <- read_csv(
    file.path(PATHS$annual_prod_dir,
              paste0(yr, "_Kusko_Assignment_Results.csv")),
    show_col_types = FALSE
  )
  
  assignment_norm <- annual_prod$assignment_norm[
    match(kusko_shp$reachid, annual_prod$reachid)
  ]
  assignment_norm[is.na(assignment_norm)] <- 0
  
  # ── 3e. Match daily temperature to each quartile's date window ─────────────
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
    reachid     = rep(kusko_shp$reachid, 4),
    COMID       = rep(COMID, 4),
    quartile    = rep(paste0("Q", 1:4), each = n_basins),
    individuals = as.vector(q_individuals)
  ) %>%
    left_join(temp_q_means, by = c("COMID", "quartile"))
  
  # ── 3f. Production-weighted temperature per reach ──────────────────────────
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
  
  # ── 3g. Build final analysis dataframe for this year ───────────────────────
  snap_temp_col <- paste0("SnapTp", yr)
  
  df_yr <- weighted_summary %>%
    mutate(Production = assignment_norm[match(reachid, kusko_shp$reachid)]) %>%
    left_join(
      kusko_shp %>% select(reachid, all_of(snap_temp_col), Channel_sl),
      by = "reachid"
    ) %>%
    rename(
      mean_summer_temp = weighted_avg_temp,
      SNAP_temp        = !!sym(snap_temp_col)
    )
  
  year_results[[as.character(yr)]] <- df_yr
  
  # ── 3h. Export quartile assignment CSV ─────────────────────────────────────
  dir.create(PATHS$output_prod, recursive = TRUE, showWarnings = FALSE)
  
  output_data <- data.frame(
    reachid   = kusko_shp$reachid,
    Str_Order = kusko_shp$Str_Order,
    iso_pred  = kusko_shp$iso_pred,
    Q1_assignment_individuals = q_individuals[, 1],
    Q2_assignment_individuals = q_individuals[, 2],
    Q3_assignment_individuals = q_individuals[, 3],
    Q4_assignment_individuals = q_individuals[, 4],
    total_individuals         = total_indiv,
    assignment_norm           = assignment_norm
  )
  
  write_csv(output_data,
            file.path(PATHS$output_prod,
                      paste0(yr, "_Kusko_Quartile_Assignment_Results.csv")))
  
  cat("  Exported production CSV for", yr, "\n")
}


################################################################################
# PART 4: 10-PANEL CONTOUR FIGURE (FIXED SPACING + LABELS)
################################################################################

cat("\n================================================================\n")
cat("PART 4: BUILDING 10-PANEL CONTOUR FIGURE\n")
cat("================================================================\n")

# ------------------------------------------------------------------
# Filter to high-production reaches
# ------------------------------------------------------------------
filtered_list <- lapply(YEARS, function(yr) {
  year_results[[as.character(yr)]] %>%
    filter(Production > 0.7) %>%
    mutate(year = yr)
})
names(filtered_list) <- as.character(YEARS)

# ------------------------------------------------------------------
# Global axis limits
# ------------------------------------------------------------------
x_lim_temp  <- c(5, 15)
y_lim_slope <- c(0, 3)
x_lim_air   <- c(11, 17)

# ------------------------------------------------------------------
# Colors
# ------------------------------------------------------------------

fill_colors <- brewer.pal(9, "YlOrRd")[-1]

# ------------------------------------------------------------------
# Shared theme — NO aspect.ratio (this was causing the column gap)
# patchwork + aspect.ratio forces padding to maintain square panels.
# Instead we let the figure dimensions control panel proportions.
# ------------------------------------------------------------------
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

# ------------------------------------------------------------------
# Column 1: Stream Temperature vs Channel Slope
# ------------------------------------------------------------------
plots_col1 <- lapply(seq_along(YEARS), function(i) {
  df <- filtered_list[[as.character(YEARS[i])]]
  is_bottom <- (i == length(YEARS))
  
  ggplot(df, aes(mean_summer_temp, Channel_sl)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +
    
    stat_density_2d_filled(bins = 8) +
    
    scale_fill_manual(values = fill_colors) +
    
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

# ------------------------------------------------------------------
# Column 2: SNAP Air Temperature vs Channel Slope
# ------------------------------------------------------------------
plots_col2 <- lapply(seq_along(YEARS), function(i) {
  df <- filtered_list[[as.character(YEARS[i])]]
  is_bottom <- (i == length(YEARS))
  
  ggplot(df, aes(SNAP_temp, Channel_sl)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = "white") +
    
    stat_density_2d_filled(bins = 8) +
    
    scale_fill_manual(values = fill_colors) +
    
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

# ------------------------------------------------------------------
# Year label panels — centered text
# ------------------------------------------------------------------
year_labels <- lapply(YEARS, function(yr) {
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

# ------------------------------------------------------------------
# Assemble — flat 3-column grid (no nested rows)
# ------------------------------------------------------------------
flat_list <- list()
for (i in seq_along(YEARS)) {
  flat_list <- c(flat_list, list(
    year_labels[[i]],
    plots_col1[[i]],
    plots_col2[[i]]
  ))
}

combined_plot <- wrap_plots(flat_list, ncol = 3,
                            widths = c(0.15, 1, 1)) +
  plot_layout(heights = rep(1, length(YEARS)))

# ------------------------------------------------------------------
# Column titles
# ------------------------------------------------------------------
combined_plot <- combined_plot +
  plot_annotation(
    title = expression(
      paste("Stream Temperature vs Slope",
            "                         ",
            "Air Temperature vs Slope")
    ),
    theme = theme(
      plot.title = element_text(
        size = 12, face = "bold", hjust = 0.5,
        color = "grey10", margin = margin(b = 4)
      )
    )
  )

# ------------------------------------------------------------------
# Shared y-axis label (rotated on left)
# ------------------------------------------------------------------
final_plot <- wrap_elements(combined_plot) +
  labs(tag = "Channel Slope") +
  theme(
    plot.tag          = element_text(size = 11, angle = 90, color = "grey20"),
    plot.tag.position = "left"
  )

# ------------------------------------------------------------------
# Shared x-axis label (bottom caption)
# ------------------------------------------------------------------
final_with_xlab <- final_plot +
  plot_annotation(
    caption = expression(
      paste("Mean Summer Stream Temperature (\u00B0C)",
            "                                     ",
            "SNAP Air Temperature (\u00B0C)")
    ),
    theme = theme(
      plot.caption = element_text(
        size = 10, hjust = 0.55, color = "grey20",
        margin = margin(t = 2)
      )
    )
  )

# ------------------------------------------------------------------
# Save
# Figure dimensions chosen so panels are roughly square:
#   Each panel column ~3.8" wide, each row ~1.8" tall → ~square
# ------------------------------------------------------------------
dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(PATHS$output_figures,
            "Quartile_StreamTemp_AirTemp_vs_Slope_2017-2021.png"),
  plot   = final_with_xlab,
  width  = 8.5,
  height = 17,
  dpi    = 300,
  bg     = "white"
)

print(final_with_xlab)

cat("\n================================================================\n")
cat("FIGURE COMPLETE\n")
cat("================================================================\n")