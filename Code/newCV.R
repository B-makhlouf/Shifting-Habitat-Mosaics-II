### CV Upstream Groups — Kuskokwim & Yukon
### Computes CVs of salmon production by upstream group (and cluster for Kuskokwim)
### and compares them to basin-wide CV.

library(tidyverse)
library(sf)
library(here)
library(readxl)

# --- Escapement data (shared) ---
allEsc <- read_excel(here("Data", "AYKEscapement.xlsx"))

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
    sd_prod   = sd(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
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
    sd_prod   = sd(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    cv_prod   = sd_prod / mean_prod
  ) %>%
  ungroup()

# Basin-wide CV
basin_cv_kusko <- sd(kusko_esc_all) / mean(kusko_esc_all)

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
    sd_prod   = sd(c(total_2015, total_2016, total_2018, total_2021)),
    cv_prod   = sd_prod / mean_prod
  ) %>%
  ungroup()

# Basin-wide CV
basin_cv_yukon <- sd(yukon_esc_all) / mean(yukon_esc_all)


# ============================================================
# VISUALIZATIONS
# ============================================================

library(patchwork)

# --- Shared plot theme ---
cv_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank())

# --- Plot 1: Kuskokwim Groups vs Basin ---

kusko_plot_df <- data.frame(
  label = c(rep("Groups", nrow(kusko_group_summary)), "Basin"),
  cv = c(kusko_group_summary$cv_prod, basin_cv_kusko),
  mean_prod = c(kusko_group_summary$mean_prod, NA),
  type = c(rep("group", nrow(kusko_group_summary)), "basin")
)
kusko_plot_df$label <- factor(kusko_plot_df$label, levels = c("Groups", "Basin"))

kusko_mean_cv <- mean(kusko_group_summary$cv_prod)

p_kusko <- ggplot() +
  geom_col(data = data.frame(label = factor(c("Groups", "Basin"), levels = c("Groups", "Basin")),
                             mean_cv = c(kusko_mean_cv, basin_cv_kusko)),
           aes(x = label, y = mean_cv), fill = "steelblue", alpha = 0.25, width = 0.6) +
  geom_jitter(data = kusko_plot_df %>% filter(type == "group"),
              aes(x = label, y = cv, size = mean_prod),
              color = "steelblue", alpha = 0.7, width = 0.15) +
  geom_point(data = kusko_plot_df %>% filter(type == "basin"),
             aes(x = label, y = cv), shape = 18, size = 5, color = "red") +
  geom_text(data = data.frame(label = factor(c("Groups", "Basin"), levels = c("Groups", "Basin")),
                              mean_cv = c(kusko_mean_cv, basin_cv_kusko)),
            aes(x = label, y = mean_cv, label = round(mean_cv, 2)),
            vjust = -0.5, size = 4) +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation",
       title = "Kuskokwim") +
  cv_theme

p_kusko

# --- Plot 2: Yukon Groups vs Basin ---

yukon_plot_df <- data.frame(
  label = c(rep("Groups", nrow(yukon_group_summary)), "Basin"),
  cv = c(yukon_group_summary$cv_prod, basin_cv_yukon),
  mean_prod = c(yukon_group_summary$mean_prod, NA),
  type = c(rep("group", nrow(yukon_group_summary)), "basin")
)
yukon_plot_df$label <- factor(yukon_plot_df$label, levels = c("Groups", "Basin"))

yukon_mean_cv <- mean(yukon_group_summary$cv_prod)

p_yukon <- ggplot() +
  geom_col(data = data.frame(label = factor(c("Groups", "Basin"), levels = c("Groups", "Basin")),
                             mean_cv = c(yukon_mean_cv, basin_cv_yukon)),
           aes(x = label, y = mean_cv), fill = "steelblue", alpha = 0.25, width = 0.6) +
  geom_jitter(data = yukon_plot_df %>% filter(type == "group"),
              aes(x = label, y = cv, size = mean_prod),
              color = "steelblue", alpha = 0.7, width = 0.15) +
  geom_point(data = yukon_plot_df %>% filter(type == "basin"),
             aes(x = label, y = cv), shape = 18, size = 5, color = "red") +
  geom_text(data = data.frame(label = factor(c("Groups", "Basin"), levels = c("Groups", "Basin")),
                              mean_cv = c(yukon_mean_cv, basin_cv_yukon)),
            aes(x = label, y = mean_cv, label = round(mean_cv, 2)),
            vjust = -0.5, size = 4) +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation",
       title = "Yukon") +
  cv_theme

p_yukon



combined_plot_df <- data.frame(
  label = c(rep("Upstream\nGroups", nrow(kusko_group_summary) + nrow(yukon_group_summary)),
            rep("Basin-wide", 2)),
  cv = c(kusko_group_summary$cv_prod, yukon_group_summary$cv_prod,
         basin_cv_kusko, basin_cv_yukon),
  mean_prod = c(kusko_group_summary$mean_prod, yukon_group_summary$mean_prod, NA, NA),
  river = c(rep("Kuskokwim", nrow(kusko_group_summary)),
            rep("Yukon", nrow(yukon_group_summary)),
            "Kuskokwim", "Yukon"),
  type = c(rep("group", nrow(kusko_group_summary) + nrow(yukon_group_summary)),
           "basin", "basin")
)
combined_plot_df$label <- factor(combined_plot_df$label,
                                 levels = c("Upstream\nGroups", "Basin-wide"))



all_group_cvs <- c(kusko_group_summary$cv_prod, yukon_group_summary$cv_prod)

combined_bar_df <- data.frame(
  label = factor(c("Upstream\nGroups", "Basin-wide"),
                 levels = c("Upstream\nGroups", "Basin-wide")),
  mean_cv = c(mean(all_group_cvs), mean(c(basin_cv_kusko, basin_cv_yukon)))
)

ggplot() +
  geom_col(data = combined_bar_df, aes(x = label, y = mean_cv),
           fill = "steelblue", alpha = 0.25, width = 0.6) +
  geom_jitter(data = combined_plot_df %>% filter(type == "group"),
              aes(x = label, y = cv, size = mean_prod, color = river),
              alpha = 0.7, width = 0.15) +
  geom_point(data = combined_plot_df %>% filter(type == "basin"),
             aes(x = label, y = cv, color = river),
             shape = 18, size = 5) +
  geom_text(data = combined_bar_df,
            aes(x = label, y = mean_cv, label = round(mean_cv, 2)),
            vjust = -0.5, size = 4) +
  scale_color_manual(name = "Watershed", values = c("Kuskokwim" = "steelblue", "Yukon" = "darkorange")) +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation",
       title = "CV of Production: Upstream Groups vs Basin-wide") +
  cv_theme


