### CV Upstream groups 

# --- Settings ---
prod_quantile_cutoff <- 0.00  # Remove groups/clusters below this quantile of mean production

# ============================================================
# KUSKOKWIM
# ============================================================

Kusko_edges <- st_read(here("Data","Spatial Data","AnalysisShapefiles","Kusko_new3.shp"))

# Read production data 
prod2017 <- read.csv(here("Outputs","ProductionData","Kusko","2017_Kusko_Assignment_Results.csv"))
prod2018 <- read.csv(here("Outputs","ProductionData","Kusko","2018_Kusko_Assignment_Results.csv"))
prod2019 <- read.csv(here("Outputs","ProductionData","Kusko","2019_Kusko_Assignment_Results.csv"))
prod2020 <- read.csv(here("Outputs","ProductionData","Kusko","2020_Kusko_Assignment_Results.csv"))
prod2021 <- read.csv(here("Outputs","ProductionData","Kusko","2021_Kusko_Assignment_Results.csv"))

# All escapement 
library(readxl)
allEsc <- read_excel(here("Data", "AYKEscapement.xlsx"))

# Pull Kuskokwim Total_Run from each year 
kusko_esc2017 <- allEsc %>% filter(Year == 2017, River == "Kusko") %>% pull(`Total_Run`)
kusko_esc2018 <- allEsc %>% filter(Year == 2018, River == "Kusko") %>% pull(`Total_Run`)
kusko_esc2019 <- allEsc %>% filter(Year == 2019, River == "Kusko") %>% pull(`Total_Run`)
kusko_esc2020 <- allEsc %>% filter(Year == 2020, River == "Kusko") %>% pull(`Total_Run`)
kusko_esc2021 <- allEsc %>% filter(Year == 2021, River == "Kusko") %>% pull(`Total_Run`)

kusko_esc_all <- c(kusko_esc2017, kusko_esc2018, kusko_esc2019, kusko_esc2020, kusko_esc2021)

# Multiply assignment_rescale by total escapement
prod_df <- data.frame(
  prod_2017 = prod2017$assignment_rescale * kusko_esc2017,
  prod_2018 = prod2018$assignment_rescale * kusko_esc2018,
  prod_2019 = prod2019$assignment_rescale * kusko_esc2019,
  prod_2020 = prod2020$assignment_rescale * kusko_esc2020,
  prod_2021 = prod2021$assignment_rescale * kusko_esc2021
)

prod_df$group <- Kusko_edges$upstrm_grp
prod_df$cluster <- Kusko_edges$cluster

# ============================================================
# SUMMARIZE & COMPUTE CVs
# ============================================================

# Summarize by group
group_summary <- prod_df %>%
  group_by(group) %>%
  summarise(
    total_2017 = sum(prod_2017),
    total_2018 = sum(prod_2018),
    total_2019 = sum(prod_2019),
    total_2020 = sum(prod_2020),
    total_2021 = sum(prod_2021)
  ) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    sd_prod = sd(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    cv_prod = sd_prod / mean_prod
  ) %>%
  ungroup() %>%
  filter(mean_prod >= quantile(mean_prod, prod_quantile_cutoff))

# Summarize by cluster
cluster_summary <- prod_df %>%
  group_by(cluster) %>%
  summarise(
    total_2017 = sum(prod_2017),
    total_2018 = sum(prod_2018),
    total_2019 = sum(prod_2019),
    total_2020 = sum(prod_2020),
    total_2021 = sum(prod_2021)
  ) %>%
  rowwise() %>%
  mutate(
    mean_prod = mean(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    sd_prod = sd(c(total_2017, total_2018, total_2019, total_2020, total_2021)),
    cv_prod = sd_prod / mean_prod
  ) %>%
  ungroup() %>%
  filter(mean_prod >= quantile(mean_prod, prod_quantile_cutoff))

# Basin-wide CV
basin_cv_kusko <- sd(kusko_esc_all) / mean(kusko_esc_all)

# ============================================================
# FIGURE 1: CV by Upstream Group
# ============================================================

group_plot_df <- data.frame(
  label = c(rep("Kuskokwim\nGroups", nrow(group_summary)),
            "Kuskokwim\nBasin"),
  cv = c(group_summary$cv_prod, basin_cv_kusko),
  mean_prod = c(group_summary$mean_prod, NA),
  type = c(rep("group", nrow(group_summary)), "basin")
)
group_plot_df$label <- factor(group_plot_df$label, 
                              levels = c("Kuskokwim\nGroups", "Kuskokwim\nBasin"))

# Bar data — both categories get a bar showing mean CV
group_bar_df <- data.frame(
  label = factor(c("Kuskokwim\nGroups", "Kuskokwim\nBasin"),
                 levels = c("Kuskokwim\nGroups", "Kuskokwim\nBasin")),
  mean_cv = c(mean(group_summary$cv_prod), basin_cv_kusko)
)

ggplot() +
  # Bars behind everything
  geom_col(data = group_bar_df, aes(x = label, y = mean_cv),
           fill = "steelblue", alpha = 0.25, width = 0.6) +
  # Jittered points for groups (sized by mean production)
  geom_jitter(data = group_plot_df %>% filter(type == "group"),
              aes(x = label, y = cv, size = mean_prod),
              color = "steelblue", alpha = 0.7, width = 0.15) +
  # Basin-level diamond
  geom_point(data = group_plot_df %>% filter(type == "basin"),
             aes(x = label, y = cv), shape = 18, size = 5, color = "red") +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation",
       title = "CV of Production by Upstream Group") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank())

# ============================================================
# FIGURE 2: CV by Cluster
# ============================================================

cluster_bar_df <- data.frame(
  label = factor(c("Kuskokwim\nClusters", "Kuskokwim\nBasin"),
                 levels = c("Kuskokwim\nClusters", "Kuskokwim\nBasin")),
  mean_cv = c(mean(cluster_summary$cv_prod), basin_cv_kusko)
)

cluster_points <- cluster_summary %>%
  mutate(label = factor("Kuskokwim\nClusters", levels = c("Kuskokwim\nClusters", "Kuskokwim\nBasin"))) %>%
  select(label, cv_prod, mean_prod)

ggplot() +
  # Bars behind everything
  geom_col(data = cluster_bar_df, aes(x = label, y = mean_cv),
           fill = "steelblue", alpha = 0.25, width = 0.6) +
  # Jittered points for clusters (sized by mean production)
  geom_jitter(data = cluster_points,
              aes(x = label, y = cv_prod, size = mean_prod),
              color = "steelblue", alpha = 0.7, width = 0.15) +
  # Basin-level diamond
  geom_point(data = cluster_bar_df %>% filter(label == "Kuskokwim\nBasin"),
             aes(x = label, y = mean_cv), shape = 18, size = 5, color = "red") +
  scale_size_continuous(name = "Mean Production", range = c(1, 8)) +
  labs(x = "", y = "Coefficient of Variation",
       title = "CV of Production by Cluster") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank())