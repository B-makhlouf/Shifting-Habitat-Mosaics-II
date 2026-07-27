# Publication figure: absolute-production CV and basin-wide difference
# ============================================================================
# Top row: tributary-level absolute-production CV distributions by stream order.
# Bottom row: mean percent difference from the sampled-year basin-wide CV.

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
local_library <- file.path(project_root, ".r-library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

for (package in c("ggplot2", "patchwork")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(package, " is required. Install it in .r-library before running this script.")
  }
}

library(ggplot2)
library(patchwork)

output_tag <- "_t0.7"
data_dir <- file.path(project_root, "Outputs", "PortfolioEffect")
figure_dir <- file.path(project_root, "Figures", "03_PortfolioEffect")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

cv_values <- read.csv(file.path(
  data_dir, paste0("DistinctTributary_absoluteCV_values", output_tag, ".csv")))
anomaly <- read.csv(file.path(
  data_dir, paste0("DistinctTributary_CV_anomaly_by_order", output_tag, ".csv")))

mainstem_order <- c(Kuskokwim = 7L, Yukon = 8L)
basin_colours <- c(Kuskokwim = "#D95F3D", Yukon = "#347AA5")
basin_cv <- setNames(
  vapply(split(cv_values$basin_cv, cv_values$basin), unique, numeric(1)),
  names(split(cv_values$basin_cv, cv_values$basin))
)

# Exclude the basin outlet, whose CV ratio is structurally one and whose anomaly
# is therefore zero. Retain only distinct tributaries below the mainstem cutoff.
cv_values <- cv_values[
  cv_values$stream_order < mainstem_order[cv_values$basin], ]
anomaly <- anomaly[
  anomaly$stream_order < mainstem_order[anomaly$basin], ]

theme_publication <- theme_minimal(base_size = 17, base_family = "sans") +
  theme(
    plot.title = element_text(size = 20, face = "bold", colour = "#111111",
                              hjust = 0.5, margin = margin(b = 8)),
    axis.title = element_text(size = 17.5, face = "bold", colour = "#222222"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 15, colour = "#303030"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5E7EB", linewidth = 0.45),
    axis.line.x = element_line(colour = "#4B5563", linewidth = 0.65),
    axis.line.y = element_line(colour = "#4B5563", linewidth = 0.65),
    plot.margin = margin(10, 16, 10, 12)
  )

make_boxplot <- function(basin, limits, breaks) {
  d <- cv_values[cv_values$basin == basin, ]
  ggplot(d, aes(x = factor(stream_order), y = absolute_cv)) +
    geom_hline(yintercept = basin_cv[[basin]], colour = "#555555",
               linewidth = 0.8, linetype = "dotted") +
    geom_boxplot(
      width = 0.62, outlier.shape = NA, linewidth = 0.8,
      fill = basin_colours[[basin]], colour = basin_colours[[basin]],
      alpha = 0.42, median.colour = NA
    ) +
    stat_summary(
      fun = mean, geom = "point", shape = 21, size = 3.2,
      stroke = 0.9, fill = "white", colour = basin_colours[[basin]]
    ) +
    coord_cartesian(ylim = limits) +
    scale_y_continuous(breaks = breaks, expand = expansion(mult = c(0.02, 0.04))) +
    labs(title = basin, x = NULL, y = NULL) +
    theme_publication +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

make_barplot <- function(basin) {
  d <- anomaly[anomaly$basin == basin, ]
  ggplot(d, aes(x = factor(stream_order), y = mean_anomaly_pct)) +
    geom_hline(yintercept = 0, colour = "#6B7280", linewidth = 0.65) +
    geom_col(
      width = 0.66, fill = basin_colours[[basin]],
      colour = basin_colours[[basin]], alpha = 0.88
    ) +
    scale_y_continuous(
      limits = c(0, 60), breaks = seq(0, 60, 20),
      expand = expansion(mult = c(0, 0.04))
    ) +
    labs(x = "Stream order", y = NULL) +
    theme_publication
}

# Linear, basin-specific crops make the modest Kuskokwim differences visible.
# The displayed axes explicitly show that the two top panels use different spans.
top_kuskokwim <- make_boxplot(
  "Kuskokwim", limits = c(0.10, 0.50), breaks = seq(0.1, 0.5, 0.1))
top_yukon <- make_boxplot(
  "Yukon", limits = c(0, 1.05), breaks = seq(0, 1, 0.25))
bottom_kuskokwim <- make_barplot("Kuskokwim")
bottom_yukon <- make_barplot("Yukon")

shared_y_label <- function(label) {
  wrap_elements(full = grid::textGrob(
    label, rot = 90,
    gp = grid::gpar(
      fontsize = 17.5, fontface = "bold", col = "#222222",
      fontfamily = "sans"
    )
  ))
}

top_row <- shared_y_label("CV number of returning spawners") |
  top_kuskokwim | top_yukon
top_row <- top_row + plot_layout(widths = c(0.10, 1, 1))

bottom_row <- shared_y_label(
  "Mean difference from\nbasin-wide CV (%)"
) | bottom_kuskokwim | bottom_yukon
bottom_row <- bottom_row + plot_layout(widths = c(0.10, 1, 1))

figure <- top_row / plot_spacer() / bottom_row +
  plot_layout(heights = c(1.18, 0.075, 0.82)) &
  theme(plot.margin = margin(8, 12, 8, 12))

publication_dir <- file.path(project_root, "Figures", "00_PubFigures")
dir.create(publication_dir, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(publication_dir, "Figure3_SpawnerAbundanceCV.png")

ggsave(png_path, figure, width = 12.6, height = 9.3, units = "in",
       dpi = 600, bg = "white")

message("Wrote publication absolute-CV figure:\n  ", png_path)
