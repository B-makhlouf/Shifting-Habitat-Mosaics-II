################################################################################
# CONTOUR AND EMPIRICAL CHANGE FIGURES
#
# Produces only:
#   01_annual_contours
#   02_change_from_average
#   Figures 1-2 manuscript composites (map + contour + change heatmap)
#
# Annual contours use the production assignments made by step 01, retain
# reaches with assignment_norm > CONTOUR_FILT_THRESH, and weight the KDE by
# assignment_rescale. The empirical heatmaps use unsmoothed annual population
# shares and show each year's percentage-point departure from the equal-year
# basin average.
################################################################################

project_library <- file.path(getwd(), ".r-library")
if (dir.exists(project_library)) {
  .libPaths(c(project_library, .libPaths()))
}

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(ks)
library(scales)

source(here("Code", "Analysis", "params.R"))

# ---- Configuration -----------------------------------------------------------
FILT_THRESH <- as.numeric(CONTOUR_FILT_THRESH)
if (length(FILT_THRESH) != 1L || !is.finite(FILT_THRESH) ||
    FILT_THRESH < 0 || FILT_THRESH > 1) {
  stop(
    "CONTOUR_FILT_THRESH in params.R must be one finite value from 0 to 1.",
    call. = FALSE
  )
}

THRESH_LABEL <- format(
  FILT_THRESH, scientific = FALSE, trim = TRUE, digits = 10
)
QUANTILES <- c(0, 0.2, 0.4, 0.6, 0.8)

fig_root <- here("Figures", "02_Contours")
annual_dir <- file.path(fig_root, "01_annual_contours")
for (path in annual_dir) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

csv_root <- here("Outputs", "ProductionData")

# ---- Load spatial attributes and annual assignments --------------------------
cat("Loading spatial attributes and production assignments...\n")
yukon_attr <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_GEO2.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  select(reachid, WtrshdSlp, DistUpstre)

kusko_attr <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  select(reachid, WtrshdSlp, DistUpstre)

load_annual_data <- function(basin_subdir, file_pattern, years, attributes) {
  setNames(lapply(years, function(year) {
    read_csv(
      file.path(csv_root, basin_subdir, sprintf(file_pattern, year)),
      show_col_types = FALSE
    ) %>%
      select(reachid, assignment_rescale, assignment_norm) %>%
      left_join(attributes, by = "reachid") %>%
      filter(
        assignment_norm > FILT_THRESH,
        is.finite(assignment_rescale),
        is.finite(assignment_norm),
        is.finite(WtrshdSlp), WtrshdSlp > 0,
        is.finite(DistUpstre)
      ) %>%
      mutate(log_slope = log10(WtrshdSlp))
  }), years)
}

yukon_data <- load_annual_data(
  "Yukon_full", "%d_Yukon_Full_Assignment_Results.csv",
  YUKON_YEARS, yukon_attr
)
kusko_data <- load_annual_data(
  "Kusko", "%d_Kusko_Assignment_Results.csv",
  KUSKO_YEARS, kusko_attr
)

# ---- Axes and shared visual style --------------------------------------------
YUKON_DIST_LIM <- range(yukon_attr$DistUpstre, na.rm = TRUE)
KUSKO_DIST_LIM <- c(0, 8e5)

YUKON_LOG_LIM <- quantile(
  log10(yukon_attr$WtrshdSlp[yukon_attr$WtrshdSlp > 0]),
  c(0.01, 0.99), na.rm = TRUE
)
KUSKO_LOG_LIM <- quantile(
  log10(kusko_attr$WtrshdSlp[kusko_attr$WtrshdSlp > 0]),
  c(0.01, 0.99), na.rm = TRUE
)
KUSKO_LOG_LIM[2] <- log10(50)

SLOPE_BREAK_VALUES <- c(1, 2.5, 7, 20, 50)
make_slope_breaks <- function(log_limits) {
  values <- SLOPE_BREAK_VALUES[
    log10(SLOPE_BREAK_VALUES) >= log_limits[1] &
      log10(SLOPE_BREAK_VALUES) <= log_limits[2]
  ]
  list(
    positions = log10(values),
    labels = format(
      values, trim = TRUE, scientific = FALSE, drop0trailing = TRUE
    )
  )
}
yukon_slope_breaks <- make_slope_breaks(YUKON_LOG_LIM)
kusko_slope_breaks <- make_slope_breaks(KUSKO_LOG_LIM)

make_distance_scale <- function(distance_limits) {
  breaks <- pretty(distance_limits, n = 5)
  breaks <- breaks[
    breaks >= distance_limits[1] & breaks <= distance_limits[2]
  ]
  scale_y_continuous(
    breaks = breaks,
    labels = round(breaks / 1e5, 1)
  )
}

X_AXIS_LABEL <- "Watershed Slope (log10 scale)"
base_theme <- theme_bw() +
  theme(
    axis.text = element_text(
      size = 44, face = "bold", colour = "grey20"
    ),
    axis.title = element_text(
      size = 52, face = "bold", colour = "grey15"
    ),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid = element_blank(),
    plot.title = element_text(
      size = 44, face = "bold", hjust = 0.5, margin = margin(b = 10)
    ),
    legend.title = element_text(size = 32, face = "bold"),
    legend.text = element_text(size = 24, face = "bold"),
    legend.key.size = unit(1.2, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(16, 16, 16, 16)
  )

# ---- KDE utilities ------------------------------------------------------------
normalise_kde_weights <- function(weights) {
  total <- sum(weights)
  if (!length(weights) || any(!is.finite(weights)) || total <= 0) {
    stop("KDE weights must be finite and have a positive sum.", call. = FALSE)
  }
  weights / total * length(weights)
}

kde_mass_levels <- function(fit, probabilities) {
  density <- as.vector(fit$estimate)
  density <- density[is.finite(density) & density >= 0]
  ordered <- sort(density, decreasing = TRUE)
  cumulative <- cumsum(ordered) / sum(ordered)

  vapply(probabilities, function(probability) {
    if (probability <= 0) {
      return(max(ordered) * (1 + sqrt(.Machine$double.eps)))
    }
    ordered[which(cumulative >= probability)[1]]
  }, numeric(1))
}

fit_annual_kde <- function(data) {
  xy <- cbind(data$log_slope, data$DistUpstre)
  bandwidth <- Hpi(x = xy)
  bandwidth <- (bandwidth + t(bandwidth)) / 2
  fit <- kde(
    x = xy,
    H = bandwidth,
    w = normalise_kde_weights(data$assignment_rescale),
    gridsize = c(200, 200)
  )
  # ks::kde returns a regular rectangular grid. Therefore equal-area grid-cell
  # sums correctly recover highest-density-region probability mass.
  if (any(!is.finite(fit$estimate)) || sum(fit$estimate) <= 0) {
    stop("Annual KDE produced an invalid density grid.", call. = FALSE)
  }
  list(
    fit = fit,
    breaks = sort(unique(kde_mass_levels(fit, QUANTILES)))
  )
}

kde_grid <- function(fit, value_name = "density") {
  grid <- expand.grid(
    x = fit$eval.points[[1]],
    y = fit$eval.points[[2]]
  )
  grid[[value_name]] <- as.vector(fit$estimate)
  grid
}

# ---- Fixed reference cross ----------------------------------------------------
reference_center <- function(annual_data) {
  valid <- annual_data[vapply(annual_data, nrow, integer(1)) >= 5]
  pooled <- bind_rows(lapply(valid, function(data) {
    data %>%
      mutate(weight = assignment_norm / sum(assignment_norm))
  }))
  c(
    x = weighted.mean(pooled$log_slope, pooled$weight),
    y = weighted.mean(pooled$DistUpstre, pooled$weight)
  )
}

reference_cross <- function(point) {
  list(
    geom_vline(
      xintercept = unname(point["x"]),
      colour = "grey15", linewidth = 1.4, alpha = 0.3
    ),
    geom_hline(
      yintercept = unname(point["y"]),
      colour = "grey15", linewidth = 1.4, alpha = 0.3
    )
  )
}

# ---- 1. Annual contour figures ------------------------------------------------
annual_panel <- function(data, year, log_limits, slope_breaks,
                         distance_limits, reference_point) {
  if (nrow(data) < 5) {
    return(
      ggplot() +
        annotate(
          "text",
          x = mean(log_limits), y = mean(distance_limits),
          label = sprintf("n = %d\n(too few)", nrow(data)),
          colour = "firebrick", size = 6
        ) +
        scale_x_continuous(
          breaks = slope_breaks$positions,
          labels = slope_breaks$labels
        ) +
        make_distance_scale(distance_limits) +
        coord_cartesian(xlim = log_limits, ylim = distance_limits) +
        labs(x = X_AXIS_LABEL, y = "Distance upstream (100 km)") +
        base_theme +
        ggtitle(year)
    )
  }

  result <- fit_annual_kde(data)
  grid <- kde_grid(result$fit)

  ggplot() +
    geom_contour_filled(
      data = grid,
      aes(x = x, y = y, z = density, fill = after_stat(level)),
      breaks = result$breaks
    ) +
    scale_fill_brewer(
      "Quantiles",
      labels = percent(rev(QUANTILES[-1])),
      palette = "YlOrRd", direction = -1
    ) +
    reference_cross(reference_point) +
    scale_x_continuous(
      breaks = slope_breaks$positions,
      labels = slope_breaks$labels
    ) +
    make_distance_scale(distance_limits) +
    coord_cartesian(xlim = log_limits, ylim = distance_limits) +
    labs(x = X_AXIS_LABEL, y = "Distance upstream (100 km)") +
    base_theme +
    ggtitle(year)
}

save_annual_figures <- function(annual_data, years, basin, log_limits,
                                slope_breaks, distance_limits,
                                reference_point) {
  for (year in years) {
    figure <- annual_panel(
      annual_data[[as.character(year)]], year,
      log_limits, slope_breaks, distance_limits, reference_point
    )
    path <- file.path(
      annual_dir,
      sprintf("%s_%d_contours_thresh%s.png", basin, year, THRESH_LABEL)
    )
    ggsave(path, figure, width = 10, height = 7, dpi = 150)
    cat("Saved:", path, "\n")
  }
}

# ---- 2–3. Density change figures ---------------------------------------------
fit_common_kdes <- function(annual_data, years, log_limits, distance_limits) {
  valid <- annual_data[vapply(annual_data, nrow, integer(1)) >= 5]
  pooled_xy <- bind_rows(valid) %>%
    select(log_slope, DistUpstre)
  common_bandwidth <- Hpi(x = as.matrix(pooled_xy))
  common_bandwidth <- (common_bandwidth + t(common_bandwidth)) / 2

  setNames(lapply(years, function(year) {
    data <- annual_data[[as.character(year)]]
    if (nrow(data) < 5) return(NULL)
    kde(
      x = cbind(data$log_slope, data$DistUpstre),
      H = common_bandwidth,
      w = normalise_kde_weights(data$assignment_norm),
      gridsize = c(200, 200),
      xmin = c(log_limits[1], distance_limits[1]),
      xmax = c(log_limits[2], distance_limits[2])
    )
  }), years)
}

mean_kde <- function(fits) {
  valid <- fits[!vapply(fits, is.null, logical(1))]
  result <- valid[[1]]
  result$estimate <- Reduce(
    "+", lapply(valid, function(fit) fit$estimate)
  ) / length(valid)
  result
}

change_pairs <- function(fits, years, comparison) {
  if (comparison == "previous") {
    indices <- seq_along(years)[-1]
    lapply(indices, function(index) {
      list(
        year = years[index],
        reference = as.character(years[index - 1]),
        baseline = fits[[index - 1]],
        current = fits[[index]]
      )
    })
  } else {
    baseline <- mean_kde(fits)
    lapply(seq_along(years), function(index) {
      list(
        year = years[index],
        reference = "average",
        baseline = baseline,
        current = fits[[index]]
      )
    })
  }
}

displayed_support <- function(fit) {
  cutoff <- kde_mass_levels(fit, max(QUANTILES))
  as.vector(fit$estimate) >= cutoff
}

pair_change_values <- function(pair, mask_outside = FALSE) {
  change <- as.vector(
    pair$current$estimate - pair$baseline$estimate
  )
  support <- displayed_support(pair$current) |
    displayed_support(pair$baseline)
  if (mask_outside) {
    change[!support] <- NA_real_
  } else {
    change <- change[support]
  }
  change
}

change_limit <- function(pairs) {
  values <- unlist(lapply(pairs, function(pair) {
    if (is.null(pair$baseline) || is.null(pair$current)) return(numeric())
    pair_change_values(pair)
  }), use.names = FALSE)
  max(abs(values[is.finite(values)]))
}

change_colours <- function(n) {
  colorRampPalette(
    c("#2166AC", "#67A9CF", "#F7F7F7", "#EF8A62", "#B2182B")
  )(n)
}

change_panel <- function(pair, log_limits, slope_breaks, distance_limits,
                         reference_point, limit) {
  grid <- kde_grid(pair$current, "change")
  # Match the annual panels: show change only where at least one of the two
  # compared KDEs lies inside its displayed 80% density region. The union
  # retains genuine expansions and contractions while suppressing KDE tails
  # that are white in both source contour figures.
  grid$change <- pair_change_values(pair, mask_outside = TRUE)
  n_intervals <- 10
  breaks <- seq(-limit, limit, length.out = n_intervals + 1)

  # Invisible-to-panel training contours keep all legend bins visible.
  x_span <- diff(log_limits)
  legend_grid <- expand.grid(
    x = seq(
      log_limits[2] + x_span,
      log_limits[2] + 2 * x_span,
      length.out = n_intervals + 1
    ),
    y = seq(
      distance_limits[1], distance_limits[2],
      length.out = n_intervals + 1
    )
  )
  legend_grid$change <- rep(
    seq(-limit, limit, length.out = n_intervals + 1),
    times = n_intervals + 1
  )

  ggplot() +
    geom_contour_filled(
      data = legend_grid,
      aes(x = x, y = y, z = change, fill = after_stat(level)),
      breaks = breaks
    ) +
    geom_contour_filled(
      data = grid,
      aes(x = x, y = y, z = change, fill = after_stat(level)),
      breaks = breaks,
      na.rm = TRUE
    ) +
    scale_fill_manual(
      "Density\nchange",
      values = change_colours(n_intervals),
      labels = c("Decrease", rep("", n_intervals - 2), "Increase"),
      drop = FALSE
    ) +
    reference_cross(reference_point) +
    scale_x_continuous(
      breaks = slope_breaks$positions,
      labels = slope_breaks$labels
    ) +
    make_distance_scale(distance_limits) +
    coord_cartesian(xlim = log_limits, ylim = distance_limits) +
    labs(x = X_AXIS_LABEL, y = "Distance upstream (100 km)") +
    base_theme +
    ggtitle(sprintf("%s \u2212 %s", pair$year, pair$reference))
}

save_density_changes <- function(pairs, basin, output_directory,
                                 log_limits, slope_breaks, distance_limits,
                                 reference_point) {
  # Scale is calculated independently for each watershed and comparison type.
  limit <- change_limit(pairs)
  for (pair in pairs) {
    if (is.null(pair$baseline) || is.null(pair$current)) next
    figure <- change_panel(
      pair, log_limits, slope_breaks, distance_limits,
      reference_point, limit
    )
    path <- file.path(
      output_directory,
      sprintf(
        "%s_%d_density_change_from_%s_thresh%s.png",
        basin, pair$year, pair$reference, THRESH_LABEL
      )
    )
    ggsave(path, figure, width = 10, height = 7, dpi = 150)
    cat("Saved:", path, "\n")
  }
}

# ---- 4. Nine equal-cell change from previous year ----------------------------
nine_cell_shares <- function(annual_data, years, basin,
                             log_limits, distance_limits) {
  x_edges <- seq(log_limits[1], log_limits[2], length.out = 4)
  y_edges <- seq(distance_limits[1], distance_limits[2], length.out = 4)
  cells <- expand.grid(x_bin = 1:3, y_bin = 1:3)

  bind_rows(lapply(years, function(year) {
    data <- annual_data[[as.character(year)]]
    total <- sum(data$assignment_rescale)
    if (nrow(data) == 0 || !is.finite(total) || total <= 0) return(NULL)

    observed <- data.frame(
      x_bin = findInterval(
        data$log_slope, x_edges, all.inside = TRUE
      ),
      y_bin = findInterval(
        data$DistUpstre, y_edges, all.inside = TRUE
      ),
      weight = data$assignment_rescale / total
    ) %>%
      group_by(x_bin, y_bin) %>%
      summarise(share_pct = 100 * sum(weight), .groups = "drop")

    cells %>%
      left_join(observed, by = c("x_bin", "y_bin")) %>%
      mutate(
        share_pct = coalesce(share_pct, 0),
        basin = basin,
        year = year,
        xmin = x_edges[x_bin],
        xmax = x_edges[x_bin + 1],
        ymin = y_edges[y_bin],
        ymax = y_edges[y_bin + 1]
      )
  }))
}

nine_cell_previous_changes <- function(shares) {
  years <- sort(unique(shares$year))
  bind_rows(lapply(seq_along(years)[-1], function(index) {
    current <- shares %>%
      filter(year == years[index]) %>%
      rename(current_pct = share_pct)
    baseline <- shares %>%
      filter(year == years[index - 1]) %>%
      select(x_bin, y_bin, baseline_pct = share_pct)

    current %>%
      left_join(baseline, by = c("x_bin", "y_bin")) %>%
      mutate(
        reference = as.character(years[index - 1]),
        comparison_label = sprintf(
          "%d \u2212 %d", years[index], years[index - 1]
        ),
        change_pp = current_pct - baseline_pct
      )
  }))
}

nine_cell_panel <- function(data, log_limits, slope_breaks,
                            distance_limits, limit) {
  data <- data %>%
    mutate(
      label_x = (xmin + xmax) / 2,
      label_y = (ymin + ymax) / 2,
      label = sprintf(
        "%+.1f pp\n%.1f%% \u2192 %.1f%%",
        change_pp, baseline_pct, current_pct
      )
    )

  ggplot(data) +
    geom_rect(
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = change_pp
      ),
      colour = "white", linewidth = 1.5
    ) +
    geom_text(
      aes(x = label_x, y = label_y, label = label),
      size = 4.6, lineheight = 0.92, fontface = "bold",
      colour = "grey10"
    ) +
    geom_vline(
      xintercept = seq(log_limits[1], log_limits[2], length.out = 4)[2:3],
      colour = "grey15", linewidth = 1.2, alpha = 0.3
    ) +
    geom_hline(
      yintercept = seq(
        distance_limits[1], distance_limits[2], length.out = 4
      )[2:3],
      colour = "grey15", linewidth = 1.2, alpha = 0.3
    ) +
    scale_fill_gradient2(
      "Change in run\ncontribution",
      low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
      midpoint = 0, limits = c(-limit, limit),
      labels = function(x) sprintf("%+.0f pp", x),
      oob = squish
    ) +
    scale_x_continuous(
      breaks = slope_breaks$positions,
      labels = slope_breaks$labels
    ) +
    make_distance_scale(distance_limits) +
    coord_cartesian(
      xlim = log_limits, ylim = distance_limits, expand = FALSE
    ) +
    labs(
      x = X_AXIS_LABEL,
      y = "Distance upstream (100 km)",
      title = unique(data$comparison_label)
    ) +
    base_theme
}

save_nine_cell_changes <- function(changes, basin, log_limits,
                                   slope_breaks, distance_limits) {
  # Scale is calculated independently for each watershed.
  limit <- ceiling(max(abs(changes$change_pp), na.rm = TRUE))
  for (label in unique(changes$comparison_label)) {
    data <- changes %>% filter(comparison_label == label)
    year <- unique(data$year)
    reference <- unique(data$reference)
    figure <- nine_cell_panel(
      data, log_limits, slope_breaks, distance_limits, limit
    )
    path <- file.path(
      nine_cell_dir,
      sprintf(
        "%s_%d_nine_cell_change_from_%s_thresh%s.png",
        basin, year, reference, THRESH_LABEL
      )
    )
    ggsave(path, figure, width = 10, height = 7, dpi = 150)
    cat("Saved:", path, "\n")
  }
}

# ---- Generate annual contour figures ----------------------------------------
yukon_reference <- reference_center(yukon_data)
kusko_reference <- reference_center(kusko_data)

save_annual_figures(
  yukon_data, YUKON_YEARS, "Yukon",
  YUKON_LOG_LIM, yukon_slope_breaks, YUKON_DIST_LIM, yukon_reference
)
save_annual_figures(
  kusko_data, KUSKO_YEARS, "Kusko",
  KUSKO_LOG_LIM, kusko_slope_breaks, KUSKO_DIST_LIM, kusko_reference
)

# The empirical change calculation is kept in one focused helper so this script
# remains readable. Running 02_ContourThreshnew.R always runs it.
source(here(
  "Code", "Analysis", "02_EmpiricalChangeFromAverage.R"
), local = TRUE)

# Rebuild manuscript Figures 1-2 in a clean R session after both annual source
# series exist. Keeping the raster assembly isolated avoids state collisions
# when this script itself is sourced from 00_run_all.R or an interactive session.
presentation_script <- here(
  "Code", "Analysis", "PresentationFigures.R"
)
rscript <- file.path(R.home("bin"), "Rscript.exe")
presentation_status <- system2(
  rscript,
  args = shQuote(presentation_script)
)
if (!identical(presentation_status, 0L)) {
  stop(
    "PresentationFigures.R failed with status ", presentation_status,
    call. = FALSE
  )
}

cat(
  "\nDone. Contours, empirical changes, and manuscript Figures 1-2 ",
  "were refreshed from current production data.\n",
  sep = ""
)
