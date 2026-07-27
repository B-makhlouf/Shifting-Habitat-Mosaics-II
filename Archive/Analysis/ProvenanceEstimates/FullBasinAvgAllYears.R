################################################################################
# FULL BASIN AVERAGE ACROSS ALL YEARS — RELATIVE PRODUCTION
#
# Reads the per-year assignment CSVs produced by 00_FullBasinRelativeProdMaps.R
# and computes a simple mean of `assignment_rescale` (proportional production)
# across all available years for each reach.
#
# Methodology and aesthetics are IDENTICAL to 00_FullBasinRelativeProdMaps.R:
#   - Color encodes the mean `rescale` = mean(basin_sum / total) across years.
#   - color_continuous() scales each map to its own maximum (0–1), exactly as
#     the yearly maps do, so the average map is directly comparable to any
#     individual year.
#   - Gradient colorbar with 0–1 tick labels and title "Relative production".
#   - MIN_STREAM_ORDER: 3 for Kuskokwim, 4 for Yukon (matching yearly script).
#   - Same shapefiles as the yearly script.
#
# Outputs:
#   - Figures/Maps/FullBasin_AvgAllYears/Kusko/Kusko_avg_relprod.png
#   - Figures/Maps/FullBasin_AvgAllYears/Yukon/Yukon_avg_relprod.png
#   - Outputs/ProductionData/Kusko/Kusko_avg_relprod.csv
#   - Outputs/ProductionData/Yukon_full/Yukon_avg_relprod.csv
################################################################################

suppressPackageStartupMessages({
  library(sf);       library(dplyr);       library(readr)
  library(tibble);   library(RColorBrewer); library(here)
})

# ---- Paths -------------------------------------------------------------------
PATHS <- list(
  kusko_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_edges_geomorphAdded.shp"),
  kusko_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_basin.shp"),
  yukon_edges    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_edges_geomorphAdded.shp"),
  yukon_basin    = here("Data", "Spatial Data", "AnalysisShapefiles", "Yukon_basin.shp"),
  in_kusko       = here("Outputs", "ProductionData", "Kusko"),
  in_yukon_full  = here("Outputs", "ProductionData", "Yukon_full"),
  out_kusko      = here("Outputs", "ProductionData", "Kusko"),
  out_yukon_full = here("Outputs", "ProductionData", "Yukon_full"),
  map_kusko      = here("Figures", "Maps", "FullBasin_RelProd", "Kusko"),
  map_yukon_full = here("Figures", "Maps", "FullBasin_RelProd", "Yukon")
)

KUSKO_YEARS <- c(2017, 2018, 2019, 2020, 2021, 2022)
YUKON_YEARS <- c(2015, 2016, 2021)

# ---- Continuous color helpers (identical to 00_FullBasinRelativeProdMaps.R) --
N_PAL    <- 500
PAL_CONT <- colorRampPalette(brewer.pal(9, "YlOrRd"))(N_PAL)

# Map a vector of proportional production values to colors, scaling to its max.
color_continuous <- function(rescale_vals) {
  max_val <- max(rescale_vals, na.rm = TRUE)
  cols    <- rep("grey85", length(rescale_vals))
  if (max_val > 0) {
    has_prod       <- rescale_vals > 0
    idx            <- pmax(1L, ceiling(rescale_vals[has_prod] / max_val * N_PAL))
    cols[has_prod] <- PAL_CONT[idx]
  }
  cols
}

# Draw a vertical gradient colorbar. Labels show relative scale (0–1).
draw_colorbar <- function(n_steps = 200,
                          title = "Relative production") {
  usr <- par("usr")
  pw  <- usr[2] - usr[1]
  ph  <- usr[4] - usr[3]

  bx0 <- usr[1] + 0.030 * pw
  bx1 <- bx0    + 0.022 * pw
  by0 <- usr[3] + 0.55  * ph
  by1 <- usr[3] + 0.88  * ph

  pal  <- colorRampPalette(brewer.pal(9, "YlOrRd"))(n_steps)
  step <- (by1 - by0) / n_steps
  for (k in seq_len(n_steps)) {
    rect(bx0, by0 + (k - 1) * step, bx1, by0 + k * step,
         col = pal[k], border = NA)
  }
  rect(bx0, by0, bx1, by1, border = "black", lwd = 0.5)

  tick_fracs <- c(0, 0.25, 0.5, 0.75, 1.0)
  tick_y     <- by0 + tick_fracs * (by1 - by0)
  text(bx1 + 0.008 * pw, tick_y,
       tick_fracs, adj = 0, cex = 0.62)

  text((bx0 + bx1) / 2, by1 + 0.030 * ph,
       title, adj = 0.5, cex = 0.70, font = 2)
}

# ---- Spatial layers (loaded once) -------------------------------------------
cat("Loading spatial layers...\n")
KUSKO_EDGES <- st_read(PATHS$kusko_edges, quiet = TRUE)
KUSKO_BASIN <- st_read(PATHS$kusko_basin, quiet = TRUE)
KUSKO_EDGES <- st_transform(KUSKO_EDGES, st_crs(KUSKO_BASIN))

YUKON_EDGES <- st_read(PATHS$yukon_edges, quiet = TRUE)
YUKON_BASIN <- st_read(PATHS$yukon_basin, quiet = TRUE)
YUKON_EDGES <- st_transform(YUKON_EDGES, st_crs(YUKON_BASIN))


# ==============================================================================
# AVERAGE RELATIVE PRODUCTION ACROSS YEARS
# ==============================================================================

# Read all per-year CSVs for one river and return a data frame with one row per
# reach containing the mean assignment_rescale across all years that have data.
# Reaches absent from a given year's CSV (no assignment) are treated as 0.
average_relprod <- function(years, csv_dir, file_pattern) {
  all_years <- lapply(years, function(yr) {
    path <- file.path(csv_dir, sprintf(file_pattern, yr))
    if (!file.exists(path)) {
      cat(sprintf("  [skip] %s not found\n", basename(path)))
      return(NULL)
    }
    df <- read_csv(path, show_col_types = FALSE)
    df <- df %>% select(reachid, Str_Order, assignment_rescale)
    names(df)[names(df) == "assignment_rescale"] <- as.character(yr)
    df
  })

  valid <- Filter(Negate(is.null), all_years)
  if (length(valid) == 0) stop("No CSV files found.")

  # Full outer join across years so every reach that ever appeared is included
  merged <- Reduce(function(a, b) full_join(a, b, by = c("reachid", "Str_Order")),
                   valid)

  year_cols <- as.character(years)[as.character(years) %in% names(merged)]
  merged[year_cols] <- lapply(merged[year_cols], function(x) replace(x, is.na(x), 0))

  merged$avg_rescale <- rowMeans(merged[year_cols], na.rm = TRUE)
  merged$n_years     <- rowSums(!is.na(merged[year_cols]))

  merged %>% select(reachid, Str_Order, avg_rescale, n_years)
}


# ==============================================================================
# KUSKOKWIM
# ==============================================================================
cat("\n=== Kuskokwim: averaging relative production across years ===\n")

MIN_STREAM_ORDER_KUSKO <- 3

kusko_avg <- average_relprod(
  years        = KUSKO_YEARS,
  csv_dir      = PATHS$in_kusko,
  file_pattern = "%d_Kusko_Assignment_Results.csv"
)
cat(sprintf("  Reaches with data: %d | Mean avg_rescale: %.6f\n",
            sum(kusko_avg$avg_rescale > 0),
            mean(kusko_avg$avg_rescale, na.rm = TRUE)))

# Join average back to spatial edges.
# Drop Str_Order from kusko_avg before joining — KUSKO_EDGES already has it,
# and a duplicate column causes dplyr to create Str_Order.x / Str_Order.y,
# making edges$Str_Order NULL and breaking all stream-order-based styling.
kusko_sf <- KUSKO_EDGES %>%
  left_join(kusko_avg %>% select(reachid, avg_rescale, n_years), by = "reachid") %>%
  mutate(avg_rescale = replace(avg_rescale, is.na(avg_rescale), 0))

# Save CSV
dir.create(PATHS$out_kusko, recursive = TRUE, showWarnings = FALSE)
write_csv(
  kusko_avg,
  file.path(PATHS$out_kusko, "Kusko_avg_relprod.csv")
)
cat(sprintf("  CSV saved -> %s\n",
            file.path(PATHS$out_kusko, "Kusko_avg_relprod.csv")))


# ==============================================================================
# YUKON (Full basin)
# ==============================================================================
cat("\n=== Yukon: averaging relative production across years ===\n")

MIN_STREAM_ORDER_YUKON <- 4

yukon_avg <- average_relprod(
  years        = YUKON_YEARS,
  csv_dir      = PATHS$in_yukon_full,
  file_pattern = "%d_Yukon_Full_Assignment_Results.csv"
)
cat(sprintf("  Reaches with data: %d | Mean avg_rescale: %.6f\n",
            sum(yukon_avg$avg_rescale > 0),
            mean(yukon_avg$avg_rescale, na.rm = TRUE)))

# Same fix as Kusko: drop Str_Order from yukon_avg to avoid duplicate column
# conflict with the Str_Order already present in YUKON_EDGES.
yukon_sf <- YUKON_EDGES %>%
  left_join(yukon_avg %>% select(reachid, avg_rescale, n_years), by = "reachid") %>%
  mutate(avg_rescale = replace(avg_rescale, is.na(avg_rescale), 0))

# Save CSV
dir.create(PATHS$out_yukon_full, recursive = TRUE, showWarnings = FALSE)
write_csv(
  yukon_avg,
  file.path(PATHS$out_yukon_full, "Yukon_avg_relprod.csv")
)
cat(sprintf("  CSV saved -> %s\n",
            file.path(PATHS$out_yukon_full, "Yukon_avg_relprod.csv")))


# ==============================================================================
# MAP — KUSKOKWIM
# ==============================================================================
cat("\n=== Mapping Kuskokwim average ===\n")

{
  edges      <- kusko_sf
  basin      <- KUSKO_BASIN
  avg_rp   <- edges$avg_rescale
  norm_avg <- avg_rp / max(avg_rp, na.rm = TRUE)   # 0-1 relative scale (matches colorbar)

  stream_order_prior <- ifelse(edges$Str_Order >= MIN_STREAM_ORDER_KUSKO, 1, 0)
  stream_order_prior[is.na(stream_order_prior)] <- 0

  colcode <- color_continuous(avg_rp)
  colcode[stream_order_prior == 0] <- "gray70"

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 5.0,
        ifelse(so >= 8, 6.0,
        ifelse(so >= 7, 5.0,
        ifelse(so >= 6, 3.0,
        ifelse(so >= 5, 2.7,
        ifelse(so >= 4, 2.7,
        ifelse(so >= 3, 2.5,
        ifelse(so >= 2, 1.5, 0))))))))
  lw[so < MIN_STREAM_ORDER_KUSKO] <- 0
  lw[is.finite(norm_avg) & norm_avg > 0.7 & lw > 0] <-
    lw[is.finite(norm_avg) & norm_avg > 0.7 & lw > 0] + 0.8

  dir.create(PATHS$map_kusko, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(PATHS$map_kusko, "00_AVERAGE_Kusko_avg_relprod.png")
  png(outfile, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf(
         "Average Annual Production - Kuskokwim\nYears: %s  |  N = %d years",
         paste(KUSKO_YEARS, collapse = ", "), length(KUSKO_YEARS)),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved -> %s\n", outfile))
}


# ==============================================================================
# MAP — YUKON (Full basin)
# ==============================================================================
cat("\n=== Mapping Yukon average ===\n")

{
  edges    <- yukon_sf
  basin    <- YUKON_BASIN
  avg_rp   <- edges$avg_rescale
  norm_avg <- avg_rp / max(avg_rp, na.rm = TRUE)   # 0-1 relative scale (matches colorbar)

  below_min <- !is.na(edges$Str_Order) & edges$Str_Order < MIN_STREAM_ORDER_YUKON

  colcode <- color_continuous(avg_rp)
  colcode[below_min] <- NA

  so <- ifelse(is.na(edges$Str_Order), 1, edges$Str_Order)
  lw <- ifelse(so >= 9, 3.7,
        ifelse(so >= 8, 5.0,
        ifelse(so >= 7, 3.0,
        ifelse(so >= 6, 2.0,
        ifelse(so >= 5, 1.5,
        ifelse(so >= 4, 1.5,
        ifelse(so >= 3, 1.2,
        ifelse(so >= 2, 0.8, 0))))))))
  lw[so < MIN_STREAM_ORDER_YUKON] <- 0
  lw[is.finite(norm_avg) & norm_avg > 0.7 & lw > 0] <-
    lw[is.finite(norm_avg) & norm_avg > 0.7 & lw > 0] + 0.8

  dir.create(PATHS$map_yukon_full, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(PATHS$map_yukon_full, "00_AVERAGE_Yukon_avg_relprod.png")
  png(outfile, width = 9, height = 8, units = "in", res = 300, bg = "white")
  par(mar = c(4, 4, 4, 2), bg = "white")
  plot(st_geometry(basin), col = "gray60", border = "gray60",
       main = sprintf(
         "Average Annual Production - Full Yukon Basin\nYears: %s  |  N = %d years",
         paste(YUKON_YEARS, collapse = ", "), length(YUKON_YEARS)),
       bg = "white")
  plot(st_geometry(edges), col = colcode, axes = FALSE, add = TRUE, lwd = lw)
  draw_colorbar()
  dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1, bg = "white")
  cat(sprintf("  Saved -> %s\n", outfile))
}

cat("\nDone.\n")
