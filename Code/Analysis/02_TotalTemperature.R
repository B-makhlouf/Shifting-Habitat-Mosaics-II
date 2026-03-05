################################################################################
# RIDGELINE PLOTS — Single column, all four variables stacked
# Total landscape (all reaches, no production filter)
# Publication-ready: light background, refined typography
# v3: every panel has its own x-axis label + ticks, no shared axis suppression
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

all_landscape <- all_landscape %>%
  mutate(year_f = factor(year, levels = rev(sort(unique(year)))))


# ==============================================================================
# COLOUR PALETTE
# ==============================================================================

COL <- list(
  stream = c(low = "#FEE8C8", high = "#B03A2E"),
  air    = c(low = "#FEF9E7", high = "#B7770D"),
  disch  = c(low = "#EBF5FB", high = "#1A5276"),
  prec   = c(low = "#F0FFF0", high = "#1D6A39")
)


# ==============================================================================
# SHARED THEME — every panel is self-contained with its own axes
# ==============================================================================

pub_theme <- theme_minimal(base_size = 11) +
  theme(
    text               = element_text(color = "grey10"),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.35),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.35),
    panel.grid.minor   = element_blank(),
    axis.title.x       = element_text(size = 10, color = "grey25",
                                      margin = margin(t = 6)),
    axis.title.y       = element_blank(),
    axis.text.x        = element_text(size = 9,  color = "grey35"),
    axis.text.y        = element_text(size = 11, face = "bold",
                                      color = "grey20", vjust = 0),
    axis.ticks         = element_blank(),
    panel.border       = element_blank(),
    plot.margin        = margin(8, 14, 4, 14),
    legend.position    = "none"
  )


# ==============================================================================
# HELPER: one ridgeline panel
# Every panel always shows its own x-axis label and text
# ==============================================================================

make_panel <- function(var_name, x_label, col_low, col_high, panel_label,
                       scale = 1.6) {
  
  df <- all_landscape %>%
    filter(!is.na(.data[[var_name]])) %>%
    select(year_f, value = all_of(var_name))
  
  ggplot(df, aes(x = value, y = year_f,
                 fill = after_stat(x), group = year_f)) +
    
    geom_density_ridges_gradient(
      scale          = scale,
      rel_min_height = 0.008,
      color          = alpha("white", 0.55),
      linewidth      = 0.3,
      gradient_lwd   = 0.1
    ) +
    
    # Panel letter top-left
    annotate("text",
             x = -Inf, y = Inf,
             label    = panel_label,
             hjust    = -0.6, vjust = 2.2,
             size     = 4.5, fontface = "bold",
             color    = "grey15") +
    
    scale_fill_gradient(low = col_low, high = col_high) +
    scale_x_continuous(expand = c(0.02, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1.2))) +
    
    labs(x = x_label) +
    pub_theme
}


# ==============================================================================
# BUILD PANELS
# ==============================================================================

p1 <- make_panel("mean_summer_temp",
                 "Stream Temperature (\u00B0C)",
                 COL$stream["low"], COL$stream["high"], "a")

p2 <- make_panel("SNAP_temp",
                 "Air Temperature (\u00B0C)",
                 COL$air["low"], COL$air["high"], "b")

p3 <- make_panel("log_disch",
                 "Log Discharge (m\u00B3/s)",
                 COL$disch["low"], COL$disch["high"], "c")

p4 <- make_panel("log_prec",
                 "Log  Precipitation (mm)",
                 COL$prec["low"], COL$prec["high"], "d")


# ==============================================================================
# VARIABLE TAGS (right side, coloured, rotated)
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

p1 <- add_tag(p1, "Stream Temp",   COL$stream["high"])
p2 <- add_tag(p2, "Air Temp",      COL$air["high"])
p3 <- add_tag(p3, "Discharge",     COL$disch["high"])
p4 <- add_tag(p4, "Precipitation", COL$prec["high"])


# ==============================================================================
# ASSEMBLE SINGLE-COLUMN FIGURE
# ==============================================================================

final <- p1 / p2 / p3 / p4 +
  plot_layout(heights = c(1, 1, 1, 1)) +
  plot_annotation(
    title    = "Environmental Conditions Across the Landscape",
    subtitle = "Yukon + Kuskokwim \u00B7 All reaches \u00B7 June\u2013July mean",
    theme    = theme(
      plot.title      = element_text(size = 14, face = "bold", color = "grey10",
                                     margin = margin(b = 3)),
      plot.subtitle   = element_text(size = 9, color = "grey45",
                                     margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(14, 10, 12, 10)
    )
  )


# ==============================================================================
# SAVE
# ==============================================================================

dir.create(PATHS$output_figures, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(PATHS$output_figures, "ridgeline_single_column.png"),
       final, width = 6, height = 13, dpi = 300, bg = "white")

ggsave(file.path(PATHS$output_figures, "ridgeline_single_column_wide.png"),
       final, width = 8, height = 14, dpi = 300, bg = "white")

cat("\nSaved to:", PATHS$output_figures, "\n")
print(final)