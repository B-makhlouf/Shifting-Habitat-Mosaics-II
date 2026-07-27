# Publication figure: temporal CV across all maximal tributary units
# =============================================================================
# Observed relative-production CVs are calculated across every sampled year.
# Independent-reach null simulations are closed to sum to one within each year
# before upstream accumulation, matching the compositional observed data.

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
  data_dir, paste0("AllCatchments_relativeCV", output_tag, ".csv")
))
null <- read.csv(file.path(
  data_dir,
  paste0("AllCatchments_relativeCV_closedNull", output_tag, ".csv")
))

basin_levels <- c("Kuskokwim", "Yukon")
observed$basin <- factor(observed$basin, levels = basin_levels)
null$basin <- factor(null$basin, levels = basin_levels)
order_levels <- sort(unique(c(observed$stream_order, null$stream_order)))
observed$stream_order <- factor(observed$stream_order, levels = order_levels)
null$stream_order <- factor(null$stream_order, levels = order_levels)

basin_colours <- c(Kuskokwim = "#D95F3D", Yukon = "#347AA5")

null_lines <- null
null_lines$null_model <- factor(
  ifelse(
    abs(null_lines$assumed_cv - 0.25) < 1e-8,
    "Independent reaches: CV = 0.25",
    "Independent reaches: CV = 1.0"
  ),
  levels = c(
    "Independent reaches: CV = 0.25",
    "Independent reaches: CV = 1.0"
  )
)

p <- ggplot() +
  geom_point(
    data = observed[observed$basin == "Kuskokwim", ],
    aes(x = stream_order, y = observed_cv, colour = basin),
    position = position_jitter(width = 0.16, height = 0, seed = 20260726),
    size = 0.70, alpha = 0.45, shape = 16, show.legend = FALSE
  ) +
  geom_point(
    data = observed[observed$basin == "Yukon", ],
    aes(x = stream_order, y = observed_cv, colour = basin),
    position = position_jitter(width = 0.16, height = 0, seed = 20260726),
    size = 0.65, alpha = 0.30, shape = 16, show.legend = FALSE
  ) +
  geom_line(
    data = null_lines,
    aes(
      x = stream_order, y = median, linetype = null_model,
      group = interaction(basin, null_model)
    ),
    colour = "#171717", linewidth = 1.05, lineend = "round"
  ) +
  facet_wrap(~basin, nrow = 1, scales = "free_x") +
  scale_x_discrete(drop = TRUE, expand = expansion(add = 0.35)) +
  scale_y_continuous(
    breaks = seq(0, 1.5, 0.25),
    limits = c(0, 1.5),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_colour_manual(values = basin_colours) +
  scale_linetype_manual(
    name = "Independent-reach expectation",
    values = c(
      "Independent reaches: CV = 0.25" = "dotted",
      "Independent reaches: CV = 1.0" = "longdash"
    ),
    labels = c(
      "Reach CV = 0.25 (median)",
      "Reach CV = 1.0 (median)"
    )
  ) +
  labs(
    x = "Stream order",
    y = "CV of relative production"
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
    legend.title = element_text(size = 13.5, face = "bold"),
    legend.text = element_text(size = 12.5),
    legend.key.width = grid::unit(1.5, "cm"),
    plot.margin = margin(16, 20, 14, 16)
  )

png_path <- file.path(publication_dir, "Figure4_RelativeProdCV.png")
ggsave(
  png_path, p, width = 13.2, height = 7.8, units = "in",
  dpi = 600, bg = "white"
)

message("Wrote all-tributary relative-CV figure:\n  ", png_path)
