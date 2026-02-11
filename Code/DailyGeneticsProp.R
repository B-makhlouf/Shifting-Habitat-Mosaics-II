dat<- read.csv(here("Data","LYTF_2015-2022_Otoliths_Genetics_data.csv"))
library(tidyverse)


# Clean column names if needed
dat <- dat %>%
  rename(
    Lower = `Broad...Lower`,
    Middle = `Broad...Middle`,
    Upper = `Broad...Upper`
  )

# Assign genetic group based on >= 0.9 threshold
dat <- dat %>%
  mutate(
    genetic_assignment = case_when(
      Lower >= 0.9  ~ "Lower",
      Middle >= 0.9 ~ "Middle",
      Upper >= 0.9  ~ "Upper",
      TRUE ~ NA_character_
    )
  )

dat <- dat %>%
  mutate(
    sampleDate = as.Date(sampleDate, format = "%m/%d/%Y"),
    DOY = as.numeric(format(sampleDate, "%j"))
  )
# Calculate daily proportions by year
daily_props <- dat %>%
  filter(!is.na(genetic_assignment)) %>%
  count(sampleYear, DOY, genetic_assignment) %>%
  pivot_wider(names_from = genetic_assignment, values_from = n, values_fill = 0) %>%
  pivot_longer(cols = c(Lower, Middle, Upper), names_to = "genetic_assignment", values_to = "n") %>%
  group_by(sampleYear, DOY) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# Set factor order so Lower is on bottom, Middle in middle, Upper on top
daily_props$genetic_assignment <- factor(daily_props$genetic_assignment, 
                                         levels = c("Upper", "Middle", "Lower"))

ggplot(daily_props, aes(x = DOY, y = proportion, fill = genetic_assignment)) +
  geom_col(position = "stack", width = 1) +
  facet_wrap(~ sampleYear, ncol = 1) +
  scale_fill_manual(values = c("Lower" = "#66c2a5", "Middle" = "#fc8d62", "Upper" = "#8da0cb")) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(limits = c(145, 200)) +
  labs(
    title = "Yukon River: Daily Genetic Composition by Year",
    x = "Day of Year",
    y = "Proportion",
    fill = "Genetic Group"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

# Save as a .csv 
write.csv(daily_props, here("Data","Genetics", "daily_genetic_proportions.csv"), row.names = FALSE)

