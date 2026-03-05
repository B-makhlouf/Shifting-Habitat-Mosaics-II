################################################################################
# RIDGELINE PLOTS — Single column, all four variables stacked
# Total landscape (all reaches, no production filter)
# Publication-ready: light background, refined typography
#
# Layout: 4 panels stacked vertically in one column
#   Panel 1: Stream Temperature
#   Panel 2: Air Temperature
#   Panel 3: Log10 Discharge
#   Panel 4: Log10 Precipitation
#
# Requires `all_landscape` from ridgeline_total_landscape.R
# Install once: install.packages("ggridges")
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(sf)
  library(ggplot2)
  library(ggridges)
  library(patchwork)
  library(here)
})


# ==============================================================================
# REBUILD all_landscape IF NEEDED
# ==============================================================================

if (!exists("all_landscape")) {
  all_landscape <- map_dfr(YEARS, function(yr) {
    june_start    <- as.Date(paste0(yr, "-06-01"))
    july_end      <- as.Date(paste0(yr, "-07-31"))
    date_seq      <- seq(june_start, july_end, by = TEMP_INTERVAL_DAYS)
    snap_temp_col <- paste0("SnapTp", yr)
    snap_prec_col <- paste0("SnapPr", yr)
    
    kusko_t <- kusko_temp_daily %>% filter(date %in% date_seq) %>%
      group_by(COMID) %>%
      summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
    yukon_t <- yukon_temp_daily %>% filter(date %in% date_seq) %>%
      group_by(COMID) %>%
      summarise(mean_summer_temp = mean(value, na.rm = TRUE), .groups = "drop")
    disch_t <- disch_daily %>% filter(date %in% date_seq) %>%
      group_by(COMID) %>%
      summarise(mean_summer_disch = mean(value, na.rm = TRUE), .groups = "drop")
    
    bind_rows(
      st_drop_geometry(kusko_edges) %>%
        left_join(kusko_t, by = "COMID") %>%
        left_join(disch_t, by = "COMID") %>%
        mutate(Basin = "Kuskokwim", year = yr,
               SNAP_temp = .data[[snap_temp_col]],
               SNAP_prec = .data[[snap_prec_col]]) %>%
        select(COMID, Basin, year, mean_summer_temp, mean_summer_disch,
               SNAP_temp, SNAP_prec),
      st_drop_geometry(yukon_edges) %>%
        left_join(yukon_t, by = "COMID") %>%
        left_join(disch_t, by = "COMID") %>%
        mutate(Basin = "Yukon", year = yr,
               SNAP_temp = .data[[snap_temp_col]],
               SNAP_prec = .data[[snap_prec_col]]) %>%
        select(COMID, Basin, year, mean_summer_temp, mean_summer_disch,
               SNAP_temp, SNAP_prec)
    )
  }) %>%
    mutate(
      log_disch = ifelse(!is.na(mean_summer_disch) & mean_summer_disch > 0,
                         log10(mean_summer_disch), NA_real_),
      log_prec  = ifelse(!is.na(SNAP_prec) & SNAP_prec > 0,
                         log10(SNAP_prec), NA_real_)
    )
}

# Earliest year at top of each panel
all_landscape <- all_landscape %>%
  mutate(year_f = factor(year, levels = rev(sort(unique(year)))))


# ==============================================================================
# COLOUR PALETTE — muted, print-safe, one accent per variable
# Inspired by scientific cartography: dusty but distinct
# ==============================================================================

COL <- list(
  stream = c(low = "#FEE8C8", high = "#B03A2E"),   # cream → brick red
  air    = c(low = "#FEF9E7", high = "#B7770D"),   # ivory → deep amber
  disch  = c(low = "#EBF5FB", high = "#1A5276"),   # ice → deep navy
  prec   = c(low = "#F0FFF0", high = "#1D6A39")    # mint → forest green
)

# Subtle strip labels per variable (shown inside the panel)
LABELS <- list(
  stream = "a",
  air    = "b",
  disch  = "c",
  prec   = "d"
)


# ==============================================================================
# SHARED BASE THEME — clean, publication-ready
# ==============================================================================

pub_theme <- theme_minimal(base_size = 11) +
  theme(
    text               = element_text(color = "grey10"),
    
    # White background, clean panels
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    
    # Faint horizontal grid only
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor   = element_blank(),
    
    # Axes
    axis.title.x       = element_text(size = 10, color = "grey25",
                                      margin = margin(t = 8)),
    axis.title.y       = element_blank(),
    axis.text.x        = element_text(size = 10, color = "grey30"),
    axis.text.y        = element_text(size = 11, face = "bold",
                                      color = "grey20", vjust = 0),
    axis.ticks.x       = element_line(color = "grey70", linewidth = 0.3),
    axis.ticks.y       = element_blank(),
    
    # Left border accent line — subtle visual anchor
    panel.border       = element_blank(),
    
    # Titles
    plot.title         = element_text(size = 11, face = "bold",
                                      color = "grey10", margin = margin(b = 1)),
    plot.subtitle      = element_text(size = 8.5, color = "grey45",
                                      margin = margin(b = 4)),
    plot.margin        = margin(6, 14, 2, 14),
    
    legend.position    = "none"
  )


# ==============================================================================
# HELPER: one ridgeline panel
# ==============================================================================

make_panel <- function(var_name, x_label, col_low, col_high,
                       panel_label, plot_subtitle,
                       scale = 2.8, is_bottom = FALSE) {
  
  df <- all_landscape %>%
    filter(!is.na(.data[[var_name]])) %>%
    select(year_f, value = all_of(var_name))
  
  # Median per year — plotted as a vertical tick inside each ridge
  med_df <- df %>%
    group_by(year_f) %>%
    summarise(med = median(value, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(df, aes(x = value, y = year_f,
                      fill = after_stat(x), group = year_f)) +
    
    # Ridge gradient
    geom_density_ridges_gradient(
      scale          = scale,
      rel_min_height = 0.008,
      color          = alpha("white", 0.6),
      linewidth      = 0.3,
      gradient_lwd   = 0.1
    ) +
    
    # Median tick — short vertical segment drawn as a point with shape = 124 ("|")
    geom_point(
      data  = med_df,
      aes(x = med, y = year_f),
      inherit.aes = FALSE,
      shape = 124,          # vertical bar character
      size  = 3,
      color = "white",
      alpha = 0.95
    ) +
    
    # Panel label (a, b, c, d) — top-left inside plot
    annotate("text",
             x = -Inf, y = Inf,
             label    = panel_label,
             hjust    = -0.6, vjust = 2.2,
             size     = 4.5, fontface = "bold",
             color    = "grey15") +
    
    scale_fill_gradient(low = col_low, high = col_high) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1.2))) +
    
    labs(x = if (is_bottom) x_label else NULL) +
    
    pub_theme +
    
    # Only show x-axis text on the bottom panel
    theme(
      axis.text.x  = if (is_bottom) element_text(size = 10, color = "grey30")
      else element_blank(),
      axis.ticks.x = if (is_bottom) element_line(color = "grey70", linewidth = 0.3)
      else element_blank(),
      axis.title.x = if (is_bottom) element_text(size = 10, color = "grey25",
                                                 margin = margin(t = 8))
      else element_blank()
    )
  
  p
}


# ==============================================================================
# BUILD FOUR PANELS
# ==============================================================================

p1 <- make_panel(
  var_name     = "mean_summer_temp",
  x_label      = "Mean Summer Stream Temperature (\u00B0C)",
  col_low      = COL$stream["low"],
  col_high     = COL$stream["high"],
  panel_label  = "a",
  plot_subtitle = NULL,
  is_bottom    = FALSE
)

p2 <- make_panel(
  var_name     = "SNAP_temp",
  x_label      = "SNAP Air Temperature (\u00B0C)",
  col_low      = COL$air["low"],
  col_high     = COL$air["high"],
  panel_label  = "b",
  plot_subtitle = NULL,
  is_bottom    = FALSE
)

p3 <- make_panel(
  var_name     = "log_disch",
  x_label      = "Log\u2081\u2080 Discharge (m\u00B3/s)",
  col_low      = COL$disch["low"],
  col_high     = COL$disch["high"],
  panel_label  = "c",
  plot_subtitle = NULL,
  is_bottom    = FALSE
)

p4 <- make_panel(
  var_name     = "log_prec",
  x_label      = "Log\u2081\u2080 Precipitation (mm)",
  col_low      = COL$prec["low"],
  col_high     = COL$prec["high"],
  panel_label  = "d",
  plot_subtitle = NULL,
  is_bottom    = TRUE    # show x-axis labels only on bottom panel
)


# ==============================================================================
# VARIABLE LABEL STRIP — narrow right-side annotation per panel
# Adds a coloured label strip flush right: "Stream Temp", "Air Temp", etc.
# Implemented as a plot.tag so it sits outside the panel area cleanly
# ==============================================================================

add_tag <- function(p, label, col_high) {
  p + labs(tag = label) +
    theme(
      plot.tag          = element_text(size = 9, face = "bold",
                                       color = col_high, angle = 270,
                                       vjust = 0.5, hjust = 0.5),
      plot.tag.position = "right"
    )
}

p1 <- add_tag(p1, "Stream Temp",  COL$stream["high"])
p2 <- add_tag(p2, "Air Temp",     COL$air["high"])
p3 <- add_tag(p3, "Discharge",    COL$disch["high"])
p4 <- add_tag(p4, "Precipitation", COL$prec["high"])


# ==============================================================================
# ASSEMBLE SINGLE-COLUMN FIGURE
# ==============================================================================

final <- p1 / p2 / p3 / p4 +
  plot_layout(heights = c(1, 1, 1, 1)) +
  plot_annotation(
    title    = "Environmental Conditions Across the Landscape",
    subtitle = "Yukon + Kuskokwim \u00B7 All reaches \u00B7 June\u2013July mean \u00B7 white tick = median \u00B7 colour intensity = value",
    theme    = theme(
      plot.title      = element_text(size = 14, face = "bold", color = "grey10",
                                     margin = margin(b = 3)),
      plot.subtitle   = element_text(size = 9,  color = "grey45",
                                     margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(14, 10, 12, 10)
    )
  )


# ==============================================================================
# SAVE
# ==============================================================================

dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(PATHS$output_figures, "ridgeline_single_column.png"),
  final,
  width  = 6,     # narrow single-column — fits a journal column
  height = 11,
  dpi    = 300,
  bg     = "white"
)

# Also save a wider version for presentations / supplementary
ggsave(
  file.path(PATHS$output_figures, "ridgeline_single_column_wide.png"),
  final,
  width  = 8,
  height = 12,
  dpi    = 300,
  bg     = "white"
)

cat("\nSingle-column ridgeline figure saved to:", PATHS$output_figures, "\n")
cat("  ridgeline_single_column.png       (6 x 11 in — journal column width)\n")
cat("  ridgeline_single_column_wide.png  (8 x 12 in — wider / supplementary)\n")

print(final)