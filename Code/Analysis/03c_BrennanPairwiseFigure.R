# Publication figure: Brennan-style pairwise change across nested reaches
# =============================================================================
# Observed assignment_rescale values are used exactly as stored. They sum to
# one across local reaches in each basin-year and represent each reach's
# proportion of the total run. These unchanged proportions are accumulated
# through the river network and compared among every pair of sampled years.
# Each point summarized here is a nested accumulated reach, grouped by order.
#
# Pairwise change is the absolute symmetric percent difference:
#   200 * abs(p2 - p1) / (p2 + p1)
#
# The independent expectation assigns each local reach a long-term mean
# proportional to its channel length, draws independent lognormal production,
# normalizes each simulated year to sum to one, and accumulates through the same
# topology. The two reference lines span reach CV assumptions of 0.25 and 1.0.

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
local_library <- file.path(project_root, ".r-library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is required. Install it in .r-library before running this script.")
}
library(ggplot2)

output_tag <- "_t0.7"
data_dir <- file.path(project_root, "Outputs", "PortfolioEffect")
publication_dir <- file.path(project_root, "Figures", "00_PubFigures")
dir.create(publication_dir, recursive = TRUE, showWarnings = FALSE)

observed <- read.csv(file.path(
  data_dir, paste0("BrennanPairwise_observed_by_order", output_tag, ".csv")
))
null <- read.csv(file.path(
  data_dir, paste0("BrennanPairwise_independentCV_null", output_tag, ".csv")
))

basin_levels <- c("Kuskokwim", "Yukon")
observed$basin <- factor(observed$basin, levels = basin_levels)
null$basin <- factor(null$basin, levels = basin_levels)

null$assumption <- factor(
  ifelse(
    abs(null$assumed_cv - 0.25) < 1e-8,
    "Independent reaches: CV = 0.25",
    "Independent reaches: CV = 1.0"
  ),
  levels = c(
    "Independent reaches: CV = 0.25",
    "Independent reaches: CV = 1.0"
  )
)

basin_colours <- c(Kuskokwim = "#D95F3D", Yukon = "#347AA5")

p <- ggplot() +
  geom_ribbon(
    data = observed,
    aes(
      x = stream_order,
      ymin = q25_absolute_change,
      ymax = q75_absolute_change,
      fill = basin,
      group = basin
    ),
    alpha = 0.24, colour = NA, show.legend = FALSE
  ) +
  geom_line(
    data = observed,
    aes(
      x = stream_order,
      y = median_absolute_change,
      colour = basin,
      group = basin
    ),
    linewidth = 1.35, lineend = "round", show.legend = FALSE
  ) +
  geom_point(
    data = observed,
    aes(
      x = stream_order,
      y = median_absolute_change,
      colour = basin
    ),
    size = 3.1, show.legend = FALSE
  ) +
  geom_line(
    data = null,
    aes(
      x = stream_order,
      y = median_absolute_change,
      linetype = assumption,
      group = interaction(basin, assumption)
    ),
    colour = "#171717", linewidth = 1.05, lineend = "round"
  ) +
  facet_wrap(~basin, nrow = 1, scales = "free_x") +
  scale_x_continuous(
    breaks = sort(unique(observed$stream_order)),
    expand = expansion(add = 0.25)
  ) +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, 10),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_colour_manual(values = basin_colours) +
  scale_fill_manual(values = basin_colours) +
  scale_linetype_manual(
    name = "Length-weighted independent expectation",
    values = c(
      "Independent reaches: CV = 0.25" = "dotted",
      "Independent reaches: CV = 1.0" = "longdash"
    ),
    labels = c("Reach CV = 0.25", "Reach CV = 1.0")
  ) +
  labs(
    x = "Stream order",
    y = "Pairwise change in proportion of total run (%)",
    caption = "Observed line = median; coloured band = interquartile range across all nested reach-year pairs"
  ) +
  theme_minimal(base_size = 17, base_family = "sans") +
  theme(
    strip.text = element_text(
      size = 20, face = "bold", colour = "#111111",
      margin = margin(b = 8)
    ),
    axis.title = element_text(size = 17.5, face = "bold", colour = "#222222"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.text = element_text(size = 15, colour = "#303030"),
    axis.line.x = element_line(colour = "#4B5563", linewidth = 0.65),
    axis.line.y = element_line(colour = "#4B5563", linewidth = 0.65),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5E7EB", linewidth = 0.45),
    panel.spacing.x = grid::unit(1.25, "cm"),
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    legend.key.width = grid::unit(1.55, "cm"),
    plot.caption = element_text(
      size = 11.5, colour = "#444444", hjust = 0,
      margin = margin(t = 10)
    ),
    plot.margin = margin(16, 20, 14, 16)
  )

png_path <- file.path(
  publication_dir, "Figure5_BrennanPairwiseChange.png"
)
ggsave(
  png_path, p, width = 13.2, height = 7.8, units = "in",
  dpi = 600, bg = "white"
)

message("Wrote Brennan-style pairwise-change figure:\n  ", png_path)
