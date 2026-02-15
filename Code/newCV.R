### CV Upstream Groups — Kuskokwim & Yukon
### Computes CVs of salmon production by upstream group (and cluster for Kuskokwim)
### and compares them to basin-wide CV.
### ADJUSTED: Uses population SD (dividing by n) instead of sample SD (dividing by n-1)

library(tidyverse)
library(sf)
library(here)
library(readxl)

# --- Population SD helper ---
sd_pop <- function(x) {
  n <- length(x)
  sqrt(sum((x - mean(x))^2) / n)
}

# --- Escapement data (shared) ---
allEsc <- read_excel(here("Data", "AYKEscapement.xlsx"))
#

# ============================================================
# KUSKOKWIM
# ============================================================

Kusko_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_edges.shp"))

# Production data
kusko_prod2017 <- read.csv(here("Outputs","ProductionData","Kusko","2017_Kusko_Assignment_Results.csv"))
kusko_prod2018 <- read.csv(here("Outputs","ProductionData","Kusko","2018_Kusko_Assignment_Results.csv"))
kusko_prod2019 <- read.csv(here("Outputs","ProductionData","Kusko","2019_Kusko_Assignment_Results.csv"))
kusko_prod2020 <- read.csv(here("Outputs","ProductionData","Kusko","2020_Kusko_Assignment_Results.csv"))
kusko_prod2021 <- read.csv(here("Outputs","ProductionData","Kusko","2021_Kusko_Assignment_Results.csv"))

# Escapement
kusko_esc2017 <- allEsc %>% filter(Year == 2017, River == "Kusko") %>% pull(Total_Run)
kusko_esc2018 <- allEsc %>% filter(Year == 2018, River == "Kusko") %>% pull(Total_Run)
kusko_esc2019 <- allEsc %>% filter(Year == 2019, River == "Kusko") %>% pull(Total_Run)
kusko_esc2020 <- allEsc %>% filter(Year == 2020, River == "Kusko") %>% pull(Total_Run)
kusko_esc2021 <- allEsc %>% filter(Year == 2021, River == "Kusko") %>% pull(Total_Run)
kusko_esc_all <- c(kusko_esc2017, kusko_esc2018, kusko_esc2019, kusko_esc2020, kusko_esc2021)

# Scale production to total run
kusko_prod_df <- data.frame(
  prod_2017 = kusko_prod2017$assignment_rescale * kusko_esc2017,
  prod_2018 = kusko_prod2018$assignment_rescale * kusko_esc2018,
  prod_2019 = kusko_prod2019$assignment_rescale * kusko_esc2019,
  prod_2020 = kusko_prod2020$assignment_rescale * kusko_esc2020,
  prod_2021 = kusko_prod2021$assignment_rescale * kusko_esc2021,
  group   = Kusko_edges$upstrm_grp,
  cluster = Kusko_edges$cluster
)

# Summarize by group
kusko_group_summary <- kusko_prod_df %>%
  group_by(group) %>%
  summarise(
    total_2017 = sum(prod_2017), total_2018 = sum(prod_2018),
    total_2019 = sum(prod_2019), total_2020 = sum(prod_2020),
    total_2021 = sum(prod_2021)
  ) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    sd_prod   = sd_pop(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    cv_prod   = sd_prod / mean_prod
  ) %>%
  ungroup()

# Summarize by cluster
kusko_cluster_summary <- kusko_prod_df %>%
  group_by(cluster) %>%
  summarise(
    total_2017 = sum(prod_2017), total_2018 = sum(prod_2018),
    total_2019 = sum(prod_2019), total_2020 = sum(prod_2020),
    total_2021 = sum(prod_2021)
  ) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    sd_prod   = sd_pop(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    cv_prod   = sd_prod / mean_prod
  ) %>%
  ungroup()

# Basin-wide CV
basin_cv_kusko <- sd_pop(kusko_esc_all) / mean(kusko_esc_all)

# Weighted mean CV and Portfolio Effect (Schindler et al. 2010)
# Filter out groups with NaN CV (zero or missing production)
kusko_valid <- kusko_group_summary %>% filter(!is.nan(cv_prod))
kusko_weights <- kusko_valid$mean_prod / sum(kusko_valid$mean_prod)
kusko_weighted_mean_cv <- sum(kusko_weights * kusko_valid$cv_prod)
kusko_PE <- basin_cv_kusko / kusko_weighted_mean_cv
sw2cat("--- Kuskokwim ---\n")
cat("Basin-wide CV:       ", round(basin_cv_kusko, 4), "\n")
cat("Unweighted mean CV:  ", round(mean(kusko_group_summary$cv_prod), 4), "\n")
cat("Weighted mean CV:    ", round(kusko_weighted_mean_cv, 4), "\n")
cat("Portfolio Effect (PE):", round(kusko_PE, 4), "\n")
cat("  PE = 1: no buffering | PE < 1: portfolio effect present\n\n")

# ============================================================
# YUKON
# ============================================================

Yukon_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Yukon_new.shp"))

# Production data
yukon_prod2015 <- read.csv(here("Outputs","ProductionData","Yukon_full","2015_Yukon_Full_Assignment_Results.csv"))
yukon_prod2016 <- read.csv(here("Outputs","ProductionData","Yukon_full","2016_Yukon_Full_Assignment_Results.csv"))
yukon_prod2018 <- read.csv(here("Outputs","ProductionData","Yukon_full","2018_Yukon_Full_Assignment_Results.csv"))
yukon_prod2021 <- read.csv(here("Outputs","ProductionData","Yukon_full","2021_Yukon_Full_Assignment_Results.csv"))

# Escapement
yukon_esc2015 <- allEsc %>% filter(Year == 2015, River == "Yukon") %>% pull(Total_Run)
yukon_esc2016 <- allEsc %>% filter(Year == 2016, River == "Yukon") %>% pull(Total_Run)
yukon_esc2018 <- allEsc %>% filter(Year == 2018, River == "Yukon") %>% pull(Total_Run)
yukon_esc2021 <- allEsc %>% filter(Year == 2021, River == "Yukon") %>% pull(Total_Run)
yukon_esc_all <- c(yukon_esc2015, yukon_esc2016, yukon_esc2018, yukon_esc2021)

# Scale production to total run
yukon_prod_df <- data.frame(
  prod_2015 = yukon_prod2015$assignment_rescale * yukon_esc2015,
  prod_2016 = yukon_prod2016$assignment_rescale * yukon_esc2016,
  prod_2018 = yukon_prod2018$assignment_rescale * yukon_esc2018,
  prod_2021 = yukon_prod2021$assignment_rescale * yukon_esc2021,
  group = Yukon_edges$up_grp
)

# Summarize by group
yukon_group_summary <- yukon_prod_df %>%
  group_by(group) %>%
  summarise(
    total_2015 = sum(prod_2015), total_2016 = sum(prod_2016),
    total_2018 = sum(prod_2018), total_2021 = sum(prod_2021)
  ) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(total_2015, total_2016, total_2018, total_2021)),
    sd_prod   = sd_pop(c(total_2015, total_2016, total_2018, total_2021)),
    cv_prod   = sd_prod / mean_prod
  ) %>%
  ungroup()

# Basin-wide CV
basin_cv_yukon <- sd_pop(yukon_esc_all) / mean(yukon_esc_all)

# Weighted mean CV and Portfolio Effect (Schindler et al. 2010)
# Filter out groups with NaN CV (zero or missing production)
yukon_valid <- yukon_group_summary %>% filter(!is.nan(cv_prod))
yukon_weights <- yukon_valid$mean_prod / sum(yukon_valid$mean_prod)
yukon_weighted_mean_cv <- sum(yukon_weights * yukon_valid$cv_prod)
yukon_PE <- basin_cv_yukon / yukon_weighted_mean_cv

cat("--- Yukon ---\n")
cat("Basin-wide CV:       ", round(basin_cv_yukon, 4), "\n")
cat("Unweighted mean CV:  ", round(mean(yukon_group_summary$cv_prod), 4), "\n")
cat("Weighted mean CV:    ", round(yukon_weighted_mean_cv, 4), "\n")
cat("Portfolio Effect (PE):", round(yukon_PE, 4), "\n")
cat("  PE = 1: no buffering | PE < 1: portfolio effect present\n\n")


# ============================================================
# VISUALIZATIONS
# ============================================================

library(patchwork)

cv_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank())

# --- Plot 1: Kuskokwim ---

kusko_mean_cv <- mean(kusko_group_summary$cv_prod)

p_kusko <- ggplot(kusko_valid, aes(y = cv_prod)) +
  geom_boxplot(aes(x = ""), fill = "steelblue", alpha = 0.3, 
               color = "steelblue", width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = "", size = mean_prod), color = "steelblue", alpha = 0.7, 
              width = 0.1) +
  # Weighted mean CV (black diamond)
  annotate("point", x = 1, y = kusko_weighted_mean_cv, 
           shape = 18, size = 5, color = "black") +
  annotate("text", x = 1.35, y = kusko_weighted_mean_cv,
           label = paste0("Weighted mean CV = ", round(kusko_weighted_mean_cv, 3)),
           hjust = 0, size = 3.5, fontface = "bold") +
  # Basin-wide CV
  geom_hline(yintercept = basin_cv_kusko, linetype = "dashed", 
             color = "red", linewidth = 0.9) +
  annotate("text", x = 1.35, y = basin_cv_kusko,
           label = paste0("Basin CV = ", round(basin_cv_kusko, 3)),
           hjust = 0, size = 3.5, fontface = "bold", color = "red") +
  # PE annotation
  annotate("text", x = 0.6, y = max(kusko_valid$cv_prod),
           label = paste0("PE = ", round(kusko_PE, 2)),
           hjust = 1, size = 4, fontface = "italic", color = "grey30") +
  scale_x_discrete() +
  coord_flip(clip = "off") +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation", title = "Kuskokwim") +
  cv_theme +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(5, 80, 5, 5))

p_kusko

# --- Plot 2: Yukon ---

yukon_mean_cv <- mean(yukon_group_summary$cv_prod)

p_yukon <- ggplot(yukon_valid, aes(y = cv_prod)) +
  geom_boxplot(aes(x = ""), fill = "darkorange", alpha = 0.3, 
               color = "darkorange", width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = "", size = mean_prod), color = "darkorange", alpha = 0.7, 
              width = 0.1) +
  # Weighted mean CV (black diamond)
  annotate("point", x = 1, y = yukon_weighted_mean_cv, 
           shape = 18, size = 5, color = "black") +
  annotate("text", x = 1.35, y = yukon_weighted_mean_cv,
           label = paste0("Weighted mean CV = ", round(yukon_weighted_mean_cv, 3)),
           hjust = 0, size = 3.5, fontface = "bold") +
  # Basin-wide CV
  geom_hline(yintercept = basin_cv_yukon, linetype = "dashed", 
             color = "red", linewidth = 0.9) +
  annotate("text", x = 1.35, y = basin_cv_yukon,
           label = paste0("Basin CV = ", round(basin_cv_yukon, 3)),
           hjust = 0, size = 3.5, fontface = "bold", color = "red") +
  # PE annotation
  annotate("text", x = 0.6, y = max(yukon_valid$cv_prod),
           label = paste0("PE = ", round(yukon_PE, 2)),
           hjust = 1, size = 4, fontface = "italic", color = "grey30") +
  scale_x_discrete() +
  coord_flip(clip = "off") +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation", title = "Yukon") +
  cv_theme +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(5, 80, 5, 5))

p_yukon

# --- Combined Plot (side by side with patchwork) ---

p_kusko / p_yukon + plot_annotation(
  title = "CV of Production: Upstream Groups vs Basin-wide"
)
