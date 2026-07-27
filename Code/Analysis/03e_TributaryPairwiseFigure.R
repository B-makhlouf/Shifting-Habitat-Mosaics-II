# Publication figure: pairwise change at the tributary-assignment level
# =============================================================================
# One accumulated value is retained at the downstream endpoint of each maximal
# same-order tributary. Observed assignment_rescale proportions are used exactly
# as stored and compared among every pair of sampled years.
#
# The independent null begins with local expected production proportional to
# local reach length. After annual closure and network accumulation, expected
# production at each tributary outlet is therefore proportional to that
# tributary's total upstream channel length.

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
  data_dir,
  paste0("BrennanPairwise_tributary_observed_by_order", output_tag, ".csv")
))
observed_points <- read.csv(file.path(
  data_dir,
  paste0("BrennanPairwise_tributary_observed", output_tag, ".csv")
))
null <- read.csv(file.path(
  data_dir,
  paste0("BrennanPairwise_tributary_lengthNull", output_tag, ".csv")
))

basin_levels <- c("Kuskokwim", "Yukon")
observed$basin <- factor(observed$basin, levels = basin_levels)
observed_points$basin <- factor(observed_points$basin, levels = basin_levels)
observed_points$absolute_change <- abs(observed_points$pct_change)
null$basin <- factor(null$basin, levels = basin_levels)
null$assumption <- factor(
  ifelse(
    abs(null$assumed_cv - 0.25) < 1e-8,
    "Independent local production: CV = 0.25",
    "Independent local production: CV = 1.0"
  ),
  levels = c(
    "Independent local production: CV = 0.25",
    "Independent local production: CV = 1.0"
  )
)

basin_colours <- c(Kuskokwim = "#D95F3D", Yukon = "#347AA5")

p <- ggplot() +
  geom_point(
    data = observed_points[observed_points$basin == "Kuskokwim", ],
    aes(x = stream_order, y = absolute_change, colour = basin),
    position = position_jitter(width = 0.16, height = 0, seed = 20260726),
    size = 0.65, alpha = 0.28, shape = 16, show.legend = FALSE
  ) +
  geom_point(
    data = observed_points[observed_points$basin == "Yukon", ],
    aes(x = stream_order, y = absolute_change, colour = basin),
    position = position_jitter(width = 0.16, height = 0, seed = 20260726),
    size = 0.55, alpha = 0.10, shape = 16, show.legend = FALSE
  ) +
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
    limits = c(0, 200),
    breaks = seq(0, 200, 25),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_colour_manual(values = basin_colours) +
  scale_fill_manual(values = basin_colours) +
  scale_linetype_manual(
    name = "Tributary-length expectation",
    values = c(
      "Independent local production: CV = 0.25" = "dotted",
      "Independent local production: CV = 1.0" = "longdash"
    ),
    labels = c("Local CV = 0.25", "Local CV = 1.0")
  ) +
  labs(
    x = "Tributary stream order",
    y = "Pairwise change in proportion of total run (%)",
    caption = paste(
      "Observed line = median; coloured band = interquartile range",
      "across distinct tributary-year pairs"
    )
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
  publication_dir, "Figure6_TributaryPairwiseChange.png"
)
ggsave(
  png_path, p, width = 13.2, height = 7.8, units = "in",
  dpi = 600, bg = "white"
)

message("Wrote tributary-level pairwise-change figure:\n  ", png_path)
