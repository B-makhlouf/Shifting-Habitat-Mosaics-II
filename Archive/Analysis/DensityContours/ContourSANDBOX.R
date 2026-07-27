################################################################################
# ContourSANDBOX.R
#
# Builds three ready-to-use data frames for the Kuskokwim (2017, 2018, 2019):
#   - assignment_rescale    : proportional production (sums to 1 across basin)
#   - assignment_norm       : normalized production   (max = 1)
#   - assignment_individuals: estimated fish count per reach
#
# Geomorphic attributes (WtrshdSlp, DistUpstre) from Kusko_GEO.shp are joined
# so the frames are immediately ready for contour experiments.
#
# No threshold filtering is applied here — apply whatever cutoff you want
# during the plotting step.
#
# Output objects (in environment):
#   kusko_2017, kusko_2018, kusko_2019
################################################################################

library(sf)
library(dplyr)
library(readr)
library(here)

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------
KUSKO_YEARS   <- c(2017, 2018, 2019)
kusko_prod_dir <- here("Outputs", "ProductionData", "Kusko")

# ------------------------------------------------------------------------------
# Load shapefile and pull geomorphic attributes
# ------------------------------------------------------------------------------
KUSKO_EDGES <- sf::st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_GEO.shp"),
  quiet = TRUE
)

kusko_attr <- KUSKO_EDGES %>%
  st_drop_geometry() %>%
  dplyr::select(reachid, WtrshdSlp, DistUpstre)

# ------------------------------------------------------------------------------
# Load production CSVs and join geomorphic attributes
# ------------------------------------------------------------------------------
kusko_data <- setNames(
  lapply(KUSKO_YEARS, function(yr) {
    read_csv(
      file.path(kusko_prod_dir, sprintf("%d_Kusko_Assignment_Results.csv", yr)),
      show_col_types = FALSE
    ) %>%
      dplyr::select(
        reachid,
        assignment_rescale,
        assignment_norm,
        assignment_individuals
      ) %>%
      left_join(kusko_attr, by = "reachid") %>%
      mutate(year = yr) %>%
      dplyr::select(year, reachid, WtrshdSlp, DistUpstre,
                    assignment_rescale, assignment_norm, assignment_individuals)
  }),
  as.character(KUSKO_YEARS)
)

# ------------------------------------------------------------------------------
# Assign to named objects for easy access
# ------------------------------------------------------------------------------
kusko_2017 <- kusko_data[["2017"]]
kusko_2018 <- kusko_data[["2018"]]
kusko_2019 <- kusko_data[["2019"]]

# ------------------------------------------------------------------------------
# Quick sanity check
# ------------------------------------------------------------------------------
cat("=== ContourSANDBOX: data loaded ===\n\n")
for (yr in KUSKO_YEARS) {
  df <- kusko_data[[as.character(yr)]]
  cat(sprintf(
    "%d | %d reaches | rescale range [%.5f, %.5f] | norm range [%.3f, %.3f] | individuals range [%.0f, %.0f]\n",
    yr,
    nrow(df),
    min(df$assignment_rescale, na.rm = TRUE),
    max(df$assignment_rescale, na.rm = TRUE),
    min(df$assignment_norm,    na.rm = TRUE),
    max(df$assignment_norm,    na.rm = TRUE),
    min(df$assignment_individuals, na.rm = TRUE),
    max(df$assignment_individuals, na.rm = TRUE)
  ))
}
cat("\nObjects ready: kusko_2017, kusko_2018, kusko_2019\n")





##############################################

ggplot(kusko_2017 %>% filter(assignment_norm > 0), aes(
  x = WtrshdSlp, 
  y = assignment_norm
)) +
  geom_density_2d_filled() +
  scale_x_log10() +
  coord_cartesian(xlim = c(0.01, 20), ylim = c(0, 1))


ggplot(kusko_2018 %>% filter(assignment_norm > 0), aes(
  x = WtrshdSlp, 
  y = assignment_norm
)) +
  geom_density_2d_filled() +
  scale_x_log10() +
  coord_cartesian(xlim = c(0.01, 20), ylim = c(0, 1))

ggplot(kusko_2019 %>% filter(assignment_norm > 0), aes(
  x = WtrshdSlp, 
  y = assignment_norm
)) +
  geom_density_2d_filled() +
  scale_x_log10() +
  coord_cartesian(xlim = c(0.01, 20), ylim = c(0, 1))


