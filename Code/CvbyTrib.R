### CV by Same-Tributary Groups (sameTrbID) — Kuskokwim
### Groups production by tributary (sameTrbID), computes CV per tributary,
### then calculates basin-wide PE across all stream orders.
### Stream order is retained for visualization/interpretation.

library(tidyverse)
library(sf)
library(here)
library(readxl)
library(patchwork)

# --- Escapement data ---
allEsc <- read_excel(here("Data", "AYKEscapement.xlsx"))

# ============================================================
# LOAD DATA
# ============================================================

Kusko_edges <- st_read(
  here("Data", "Spatial Data", "AnalysisShapefiles", "Kusko_sametrib.shp"),
  quiet = TRUE
)

# Production data
kusko_prod2017 <- read.csv(here("Outputs", "ProductionData", "Kusko", "2017_Kusko_Assignment_Results.csv"))
kusko_prod2018 <- read.csv(here("Outputs", "ProductionData", "Kusko", "2018_Kusko_Assignment_Results.csv"))
kusko_prod2019 <- read.csv(here("Outputs", "ProductionData", "Kusko", "2019_Kusko_Assignment_Results.csv"))
kusko_prod2020 <- read.csv(here("Outputs", "ProductionData", "Kusko", "2020_Kusko_Assignment_Results.csv"))
kusko_prod2021 <- read.csv(here("Outputs", "ProductionData", "Kusko", "2021_Kusko_Assignment_Results.csv"))
kusko_prod2022 <- read.csv(here("Outputs", "ProductionData", "Kusko", "2022_Kusko_Assignment_Results.csv"))

# Escapement
kusko_esc2017 <- allEsc %>% filter(Year == 2017, River == "Kusko") %>% pull(Total_Run)
kusko_esc2018 <- allEsc %>% filter(Year == 2018, River == "Kusko") %>% pull(Total_Run)
kusko_esc2019 <- allEsc %>% filter(Year == 2019, River == "Kusko") %>% pull(Total_Run)
kusko_esc2020 <- allEsc %>% filter(Year == 2020, River == "Kusko") %>% pull(Total_Run)
kusko_esc2021 <- allEsc %>% filter(Year == 2021, River == "Kusko") %>% pull(Total_Run)
kusko_esc2022 <- allEsc %>% filter(Year == 2022, River == "Kusko") %>% pull(Total_Run)
kusko_esc_all <- c(kusko_esc2017, kusko_esc2018, kusko_esc2019, kusko_esc2020, kusko_esc2021, kusko_esc2022)

# ============================================================
# BUILD PRODUCTION DATA FRAME
# ============================================================

# Scale production to total run and attach sameTrbID + Strahler
kusko_prod_df <- data.frame(
  prod_2017  = kusko_prod2017$assignment_rescale * kusko_esc2017,
  prod_2018  = kusko_prod2018$assignment_rescale * kusko_esc2018,
  prod_2019  = kusko_prod2019$assignment_rescale * kusko_esc2019,
  prod_2020  = kusko_prod2020$assignment_rescale * kusko_esc2020,
  prod_2021  = kusko_prod2021$assignment_rescale * kusko_esc2021,
  prod_2022  = kusko_prod2022$assignment_rescale * kusko_esc2022,
  sameTrbID  = Kusko_edges$sameTrbID,
  Strahler   = Kusko_edges$Str_Order
)

# Drop reaches with no sameTrbID (not part of reachbase 4/5/6 or mainstem)
kusko_prod_df <- kusko_prod_df %>% filter(!is.na(sameTrbID))

# ============================================================
# SUMMARIZE BY TRIBUTARY (sameTrbID)
# ============================================================

# Get the stream order label for each tributary
# (all reaches in a sameTrbID share the same Strahler, take the first)
trib_stream_order <- kusko_prod_df %>%
  group_by(sameTrbID) %>%
  summarise(stream_order = first(Strahler), .groups = "drop")

# Sum production per tributary per year
kusko_trib_summary <- kusko_prod_df %>%
  group_by(sameTrbID) %>%
  summarise(
    total_2017 = sum(prod_2017),
    total_2018 = sum(prod_2018),
    total_2019 = sum(prod_2019),
    total_2020 = sum(prod_2020),
    total_2021 = sum(prod_2021),
    total_2022 = sum(prod_2022),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(total_2017, total_2018, total_2019, total_2020, total_2021, total_2022)),
    sd_prod   = sd(c(total_2017, total_2018, total_2019, total_2020, total_2021, total_2022)),
    cv_prod   = sd_prod / mean_prod
  ) %>%
  ungroup() %>%
  left_join(trib_stream_order, by = "sameTrbID") %>%
  # Label for plotting
  mutate(
    order_label = case_when(
      stream_order %in% c(7, 8) ~ "Mainstem (7-8)",
      TRUE ~ paste0("Order ", stream_order)
    )
  )

# ============================================================
# BASIN-WIDE CV (for reference line)
# ============================================================

basin_cv_kusko <- sd(kusko_esc_all) / mean(kusko_esc_all)

# Filter out tributaries with NaN CV (zero production)
kusko_valid <- kusko_trib_summary %>% filter(!is.nan(cv_prod))

cat("--- Kuskokwim (sameTrbID tributaries) ---\n")
cat("Basin-wide CV:  ", round(basin_cv_kusko, 4), "\n")
cat("Total tributaries:", nrow(kusko_valid), "\n")
cat("Mean CV:         ", round(mean(kusko_valid$cv_prod), 4), "\n\n")

# ============================================================
# PRODUCTION-WEIGHTED MEAN CV BY STREAM ORDER
# ============================================================

weighted_cv_by_order <- kusko_valid %>%
  group_by(order_label) %>%
  summarise(
    n_tribs     = n(),
    mean_cv     = mean(cv_prod),
    median_cv   = median(cv_prod),
    weighted_cv = sum((mean_prod / sum(mean_prod)) * cv_prod),
    .groups     = "drop"
  )

cat("--- Production-weighted mean CV by stream order ---\n")
print(weighted_cv_by_order)
cat("\n")

# ============================================================
# VISUALIZATION — Strip chart with weighted mean CV per order
# ============================================================

cv_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank())

p_strip <- ggplot(kusko_valid, aes(x = order_label, y = cv_prod, color = order_label)) +
  geom_jitter(aes(size = mean_prod), alpha = 0.7, width = 0.2) +
  # Production-weighted mean CV per stream order (black diamond)
  geom_point(data = weighted_cv_by_order, 
             aes(x = order_label, y = weighted_cv),
             shape = 18, size = 5, color = "black", inherit.aes = FALSE) +
  geom_hline(yintercept = basin_cv_kusko, linetype = "dashed", 
             color = "red", linewidth = 0.9) +
  annotate("text", x = 0.5, y = basin_cv_kusko,
           label = paste0("Basin CV = ", round(basin_cv_kusko, 3)),
           hjust = 0, size = 3.5, fontface = "bold", color = "red") +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_color_discrete(name = "Stream Order") +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "Stream Order", y = "Coefficient of Variation",
       title = "Kuskokwim — CV of Production by Tributary",
       subtitle = "Black diamonds = production-weighted mean CV per stream order") +
  cv_theme +
  guides(color = "none")

p_strip