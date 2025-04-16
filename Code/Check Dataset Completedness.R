plot_dataset_qc <- function(year, watershed) {
  
  # Read the appropriate dataset
  if (watershed == "Yukon") {
    Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Yukon_Natal_Origins_Genetics_CPUE.csv"))
    
    # Add genetics flag only for Yukon
    dataset <- Natal_Origins %>%
      mutate(NoGenetics = is.na(Lower))
    
  } else if (watershed == "Kusko") {
    Natal_Origins <- read.csv(paste0("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE/",year,"_Kusko_Natal_Origins_Genetics_CPUE.csv"))
    
    dataset <- Natal_Origins
  }
  
  # Create common plots (1 and 2) for both watersheds
  p1 <- ggplot(dataset, aes(x = DOY, y = dailyCPUEprop)) +
    geom_line(color = "black") +
    labs(title = "Daily CPUE Proportion", y = "Proportion") +
    theme_minimal()
  
  p2 <- ggplot(dataset, aes(x = DOY, y = OtoPropDaily)) +
    geom_line(color = "black") +
    labs(title = "Otolith Proportion Daily", y = "Proportion") +
    theme_minimal()
  
  # Create combined plot based on watershed
  if (watershed == "Yukon") {
    # Add genetics plot only for Yukon
    p3 <- ggplot(dataset, aes(x = DOY, y = 1, color = NoGenetics)) +
      geom_point(size = 3) +
      geom_line(aes(group = 1), color = "green", alpha = 0.3) +
      scale_color_manual(values = c("green", "red")) +
      labs(title = "Days Without Genetics Data", y = "") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    combined_plot <- p1 / p2 / p3 +
      plot_layout(ncol = 1)
    
  } else if (watershed == "Kusko") {
    combined_plot <- p1 / p2 +
      plot_layout(ncol = 1)
  }
  
  # Save the plot
  identifier <- paste0(year, "_", watershed)
  filename <- paste0("/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Figures/Dataset QC/",identifier,"completedness_.pdf")
  ggsave(filename, combined_plot, width = 8, height = 10)
}
