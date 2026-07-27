################################################################################
# SPAWNER INTRINSIC POTENTIAL (IP) SENSITIVITY ANALYSIS
#
# Explores the distribution and environmental context of Spawner IP across
# both watersheds (Yukon = YK, Kuskokwim = KK).
#
# All reaches are included (IP = 0 and IP > 0), so that the full stream-order
# range is represented. Reaches with Spawner_IP = 0 reflect stream segments
# evaluated but found to have no spawner habitat.
#
# Note on stream-order coverage:
#   YK (YkIPall.shp)  : orders 3–9  (orders 1–2 absent from this shapefile)
#   KK (KkIPall.shp)  : orders 1–7  (orders 1–2 have 0% spawner habitat)
#
# Figure 1 — SpawnerIP_StreamOrder_Ridgeline.png
#   Ridgeline density plots of Spawner_IP by stream order (all reaches).
#   Left panel = YK, right panel = KK. Colour gradient: light = small order,
#   dark = large order. Orders with all-zero IP appear as spikes at x = 0.
#
# Figure 2 — SpawnerIP_WtrshdSlp_Scatter.png
#   Scatter plot of Spawner_IP vs. watershed slope (WtrshdSlp) for all
#   reaches. Left panel = YK, right panel = KK. Points coloured by stream
#   order.
#
# Outputs: Figures/IPAnalysis/
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggridges)
  library(scales)
  library(here)
})


# ==============================================================================
# CONFIGURATION
# ==============================================================================

OUT_DIR <- here("Figures", "IPAnalysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Colour palettes — consistent with project conventions
#   YK (Yukon)      : warm reds/oranges
#   KK (Kuskokwim)  : cool blues
YK_PAL_LO <- "#FCBBA1"
YK_PAL_HI <- "#99000D"
KK_PAL_LO <- "#C6DBEF"
KK_PAL_HI <- "#084594"

SHARED_THEME <- theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text       = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 9, color = "grey40",
                                    margin = margin(b = 10)),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 9),
    legend.key.size  = unit(0.45, "cm"),
    legend.text      = element_text(size = 9)
  )


# ==============================================================================
# LOAD & PREPARE DATA
# ==============================================================================

cat("Reading shapefiles...\n")

yk_raw <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "YkIPall.shp"),
  quiet = TRUE
)
kk_raw <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "KkIPall.shp"),
  quiet = TRUE
)

# Drop geometry; normalise stream order to integer; retain all reaches
yk <- st_drop_geometry(yk_raw) %>%
  mutate(Str_Order = as.integer(as.numeric(Str_Order)),
         Watershed = "Yukon (YK)") %>%
  select(Watershed, Str_Order, Spawner_IP, WtrshdSlp)

kk <- st_drop_geometry(kk_raw) %>%
  mutate(Str_Order = as.integer(as.numeric(Str_Order)),
         Watershed = "Kuskokwim (KK)") %>%
  select(Watershed, Str_Order, Spawner_IP, WtrshdSlp)

combined <- bind_rows(yk, kk) %>%
  mutate(
    Watershed = factor(Watershed,
                       levels = c("Yukon (YK)", "Kuskokwim (KK)")),
    Str_Order = factor(Str_Order)
  )

cat(sprintf("  YK: %d total reaches  |  stream orders: %s\n",
            nrow(yk),
            paste(sort(unique(yk$Str_Order)), collapse = ", ")))
cat(sprintf("  KK: %d total reaches  |  stream orders: %s\n",
            nrow(kk),
            paste(sort(unique(kk$Str_Order)), collapse = ", ")))


# ==============================================================================
# BUILD COLOUR SCALES FOR STREAM ORDER
# ==============================================================================

yk_orders  <- sort(unique(yk$Str_Order))
kk_orders  <- sort(unique(kk$Str_Order))

yk_colours <- setNames(
  colorRampPalette(c(YK_PAL_LO, YK_PAL_HI))(length(yk_orders)),
  as.character(yk_orders)
)
kk_colours <- setNames(
  colorRampPalette(c(KK_PAL_LO, KK_PAL_HI))(length(kk_orders)),
  as.character(kk_orders)
)

# Combine into a single named vector (colour per order within each watershed)
# Build a lookup: Watershed × Str_Order → colour
order_colour_lookup <- bind_rows(
  tibble(Watershed = "Yukon (YK)",
         Str_Order = as.character(yk_orders),
         colour    = yk_colours),
  tibble(Watershed = "Kuskokwim (KK)",
         Str_Order = as.character(kk_orders),
         colour    = kk_colours)
)

combined <- combined %>%
  left_join(order_colour_lookup,
            by = c("Watershed", "Str_Order" = "Str_Order"))


# ==============================================================================
# FIGURE 1 — RIDGELINE DENSITY PLOTS
# ==============================================================================
# All reaches included. Orders where Spawner_IP = 0 for all reaches will
# appear as a spike at x = 0. A tiny offset (1e-7) is added to exact-zero
# values so the KDE bandwidth calculation does not collapse.
# ==============================================================================

cat("\nBuilding Figure 1: Spawner IP by stream order (ridgeline)...\n")

ridge_data <- combined %>%
  mutate(
    # Prevent KDE singularity for all-zero stream orders
    Spawner_IP_plot = ifelse(Spawner_IP == 0,
                             Spawner_IP + runif(n(), 0, 1e-7),
                             Spawner_IP)
  )

# Build per-watershed figures separately so independent colour scales apply,
# then combine with patchwork-style cowplot or just facet with manual colours.
# Here we use facet_wrap and supply a single merged colour vector keyed by
# Str_Order label (orders shared across watersheds use different colours per
# watershed via a workaround: prefix the label with watershed initial).

ridge_data <- ridge_data %>%
  mutate(order_label = paste0(
    ifelse(Watershed == "Yukon (YK)", "Y", "K"),
    Str_Order
  ))

# Build full colour vector keyed on order_label
order_label_colours <- setNames(
  c(yk_colours, kk_colours),
  c(paste0("Y", names(yk_colours)),
    paste0("K", names(kk_colours)))
)

# Order the y-axis factor levels (ascending so small orders at bottom)
all_labels_yk <- paste0("Y", sort(yk_orders))
all_labels_kk <- paste0("K", sort(kk_orders))

ridge_data <- ridge_data %>%
  mutate(order_label = factor(order_label,
                               levels = c(all_labels_yk, all_labels_kk)))

# Y-axis tick labels: strip the watershed prefix
order_label_names <- c(
  setNames(paste("Order", sort(yk_orders)), all_labels_yk),
  setNames(paste("Order", sort(kk_orders)), all_labels_kk)
)

fig1 <- ggplot(
  ridge_data,
  aes(x     = Spawner_IP_plot,
      y     = order_label,
      fill  = order_label,
      color = order_label)
) +
  geom_density_ridges(
    alpha          = 0.72,
    scale          = 1.3,
    rel_min_height = 0.005,
    linewidth      = 0.45,
    bandwidth      = 0.025      # fixed bw avoids collapse on near-zero orders
  ) +
  facet_wrap(~ Watershed, ncol = 2, scales = "free_y") +
  scale_fill_manual( values = order_label_colours, guide = "none") +
  scale_color_manual(values = order_label_colours, guide = "none") +
  scale_x_continuous(
    limits = c(-0.02, 1.05),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    labels = number_format(accuracy = 0.1)
  ) +
  scale_y_discrete(labels = order_label_names) +
  labs(
    title    = "Spawner IP Distribution by Stream Order",
    subtitle = paste0(
      "All reaches shown. Orders with no spawner habitat appear as spikes at IP = 0.\n",
      "Colour gradient: light = small order, dark = large order."
    ),
    x = "Spawner IP",
    y = "Stream order"
  ) +
  SHARED_THEME +
  theme(panel.spacing.x = unit(1.2, "lines"))

ggsave(
  file.path(OUT_DIR, "SpawnerIP_StreamOrder_Ridgeline.png"),
  plot   = fig1,
  width  = 11,
  height = 7,
  dpi    = 300,
  bg     = "white"
)
cat(sprintf("  Saved: %s\n",
            file.path(OUT_DIR, "SpawnerIP_StreamOrder_Ridgeline.png")))


# ==============================================================================
# FIGURE 2 — SCATTER: Spawner IP vs. WtrshdSlp
# 2nd- and 3rd-order reaches excluded. Axes flipped: IP on x, slope on y.
# ==============================================================================

cat("\nBuilding Figure 2: Spawner IP vs. watershed slope (scatter)...\n")

scatter_data <- ridge_data %>%
  filter(!Str_Order %in% c("2", "3"))

fig2 <- ggplot(
  scatter_data,
  aes(x     = Spawner_IP,
      y     = WtrshdSlp,
      color = order_label)
) +
  geom_point(
    size   = 0.8,
    alpha  = 0.40,
    stroke = 0
  ) +
  facet_wrap(~ Watershed, ncol = 2, scales = "free_y") +
  scale_color_manual(values = order_label_colours, guide = "none") +
  scale_x_continuous(
    limits = c(-0.02, 1.05),
    breaks = seq(0, 1, by = 0.2),
    labels = number_format(accuracy = 0.1)
  ) +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  labs(
    title    = "Spawner IP vs. Watershed Slope",
    subtitle = paste0(
      "2nd- and 3rd-order reaches excluded. Points coloured by stream order ",
      "(light = small order, dark = large order).\n",
      "YK = Yukon  |  KK = Kuskokwim"
    ),
    x = "Spawner IP",
    y = "Watershed slope (%)"
  ) +
  SHARED_THEME +
  theme(panel.spacing.x = unit(1.2, "lines"))

# Per-panel n annotations
panel_n <- scatter_data %>%
  group_by(Watershed) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = paste0("n = ", formatC(n, format = "d", big.mark = ",")))

fig2 <- fig2 +
  geom_text(
    data        = panel_n,
    aes(x = 1.02, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust       = 1.05,
    vjust       = 1.5,
    size        = 3.2,
    color       = "grey40"
  )

ggsave(
  file.path(OUT_DIR, "SpawnerIP_WtrshdSlp_Scatter.png"),
  plot   = fig2,
  width  = 11,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
cat(sprintf("  Saved: %s\n",
            file.path(OUT_DIR, "SpawnerIP_WtrshdSlp_Scatter.png")))


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n--- Spawner IP summary by watershed and stream order ---\n")
combined %>%
  group_by(Watershed, Str_Order) %>%
  summarise(
    n_reaches     = n(),
    pct_nonzero   = round(mean(Spawner_IP > 0) * 100, 1),
    median_all    = round(median(Spawner_IP), 3),
    median_nonzero = round(median(Spawner_IP[Spawner_IP > 0]), 3),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()

cat("\nDone. Figures written to Figures/IPAnalysis/\n")
