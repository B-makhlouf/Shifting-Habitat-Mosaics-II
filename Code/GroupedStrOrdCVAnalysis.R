################################################################################
# TRIBUTARY GROUP PRODUCTION ANALYSIS - REFACTORED
# Three modular functions for clean, organized workflow
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(readxl)

#==============================================================================
# FUNCTION 1: LOAD AND PREPARE DATA
#==============================================================================

load_scenario_data <- function(config, ESCAPEMENT_FILE) {
  
  # Load upstream relationships
  if (!file.exists(config$upstream_relationships)) {
    stop(sprintf("Upstream relationships file not found: %s", config$upstream_relationships))
  }
  
  upstream_df <- read_csv(config$upstream_relationships, show_col_types = FALSE) %>%
    rename(focal_reach = original_reachid, 
           tributary_reach = upstream_reachid,
           stream_order = reachbase)
  
  # Create tributary groups (focal reach + all upstream reaches)
  focal_reaches_only <- upstream_df %>%
    distinct(focal_reach, stream_order) %>%
    mutate(tributary_reach = focal_reach)
  
  groups <- bind_rows(focal_reaches_only, upstream_df) %>%
    distinct() %>%
    rename(reach_in_group = tributary_reach)
  
  # Process each year
  all_results <- data.frame()
  years_processed <- vector()
  
  for (year in config$years) {
    tryCatch({
      # Get basin total run
      basin_data <- read_xlsx(ESCAPEMENT_FILE)
      basin_total_run <- basin_data %>%
        filter(River == config$watershed, Year == year) %>%
        pull(Total_Run)
      
      if (length(basin_total_run) == 0) next
      
      # Load production data
      all_files <- list.files(config$prod_data_dir, full.names = TRUE)
      matching_files <- all_files[
        grepl(paste0(year), basename(all_files)) & 
          grepl(paste0("_", config$watershed, "_Assignment_Results"), basename(all_files)) &
          grepl(config$file_pattern, basename(all_files))
      ]
      
      if (length(matching_files) == 0) next
      
      prod_data <- read_csv(matching_files[1], show_col_types = FALSE)
      
      if (!all(c("reachid", "assignment_individuals") %in% names(prod_data))) next
      
      # Calculate production for each tributary group
      unique_focal <- groups %>% distinct(focal_reach, stream_order)
      
      for (i in 1:nrow(unique_focal)) {
        focal_id <- unique_focal$focal_reach[i]
        so <- unique_focal$stream_order[i]
        
        group_reaches <- groups %>%
          filter(focal_reach == focal_id) %>%
          pull(reach_in_group)
        
        group_individuals <- sum(
          prod_data$assignment_individuals[prod_data$reachid %in% group_reaches],
          na.rm = TRUE
        )
        
        all_results <- rbind(all_results, data.frame(
          year = year,
          stream_order = so,
          focal_reach = focal_id,
          group_individuals = group_individuals,
          n_reaches_in_group = length(group_reaches),
          n_reaches_with_production = sum(
            prod_data$reachid %in% group_reaches & prod_data$assignment_individuals > 0,
            na.rm = TRUE
          ),
          basin_total_run = basin_total_run,
          stringsAsFactors = FALSE
        ))
      }
      
      years_processed <- c(years_processed, year)
      
    }, error = function(e) {})
  }
  
  if (nrow(all_results) == 0) {
    stop("No data found for scenario")
  }
  
  return(list(results = all_results, groups = groups, years_processed = years_processed))
}

#==============================================================================
# FUNCTION 2: ANALYZE AND EXPORT RESULTS
#==============================================================================

analyze_and_export <- function(all_results, config) {
  
  type_label <- ifelse(config$data_type == "full_year", "", "_HalfYear")
  dir.create(config$data_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Export long format
  detailed_file <- file.path(config$data_output_dir, 
                             paste0("TributaryGroups_Individuals_LongFormat", type_label, ".csv"))
  write_csv(all_results, detailed_file)
  
  # Export timeseries (wide format)
  timeseries_pivot <- all_results %>%
    select(focal_reach, stream_order, year, group_individuals) %>%
    pivot_wider(
      names_from = year,
      values_from = group_individuals,
      names_prefix = "Year_"
    ) %>%
    mutate(focal_reach = as.integer(focal_reach), stream_order = as.integer(stream_order)) %>%
    arrange(stream_order, focal_reach)
  
  timeseries_file <- file.path(config$data_output_dir, 
                               paste0("TributaryGroups_Individuals_Timeseries", type_label, ".csv"))
  write_csv(timeseries_pivot, timeseries_file)
  
  # Export summary by stream order
  summary_by_order <- all_results %>%
    group_by(stream_order, year) %>%
    summarise(
      n_groups = n_distinct(focal_reach),
      total_individuals = sum(group_individuals),
      mean_individuals_per_group = mean(group_individuals),
      sd_individuals_per_group = sd(group_individuals),
      max_individuals_group = max(group_individuals),
      .groups = 'drop'
    )
  
  summary_file <- file.path(config$data_output_dir, 
                            paste0("TributaryGroups_SummaryByStreamOrder", type_label, ".csv"))
  write_csv(summary_by_order, summary_file)
  
  # CV analysis
  cv_analysis <- all_results %>%
    filter(group_individuals > 0) %>%
    group_by(focal_reach, stream_order) %>%
    summarise(
      n_years_with_individuals = n(),
      mean_individuals = mean(group_individuals, na.rm = TRUE),
      sd_individuals = sd(group_individuals, na.rm = TRUE),
      cv = sd_individuals / mean_individuals,
      .groups = 'drop'
    ) %>%
    filter(!is.na(cv))
  
  cv_file <- file.path(config$data_output_dir, 
                       paste0("TributaryGroups_CoefficientOfVariation", type_label, ".csv"))
  write_csv(cv_analysis, cv_file)
  
  # CV summary by stream order
  cv_summary <- cv_analysis %>%
    group_by(stream_order) %>%
    summarise(
      n_groups = n(),
      mean_cv = mean(cv, na.rm = TRUE),
      median_cv = median(cv, na.rm = TRUE),
      sd_cv = sd(cv, na.rm = TRUE),
      min_cv = min(cv, na.rm = TRUE),
      max_cv = max(cv, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(stream_order)
  
  cv_summary_file <- file.path(config$data_output_dir, 
                               paste0("TributaryGroups_CV_Summary", type_label, ".csv"))
  write_csv(cv_summary, cv_summary_file)
  
  # Basin-wide CV
  basin_cv <- all_results %>%
    distinct(year, basin_total_run) %>%
    summarise(cv_basin = sd(basin_total_run) / mean(basin_total_run)) %>%
    pull(cv_basin)
  
  return(list(
    cv_analysis = cv_analysis,
    cv_summary = cv_summary,
    timeseries_pivot = timeseries_pivot,
    basin_cv = basin_cv,
    all_results = all_results
  ))
}

#==============================================================================
# FUNCTION 3: CREATE VISUALIZATION
#==============================================================================

create_visualization <- function(analysis_results, config) {
  
  type_label <- ifelse(config$data_type == "full_year", "", "_HalfYear")
  dir.create(config$figure_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  timeseries_pivot <- analysis_results$timeseries_pivot
  cv_analysis <- analysis_results$cv_analysis
  all_results <- analysis_results$all_results
  basin_cv <- analysis_results$basin_cv
  
  # Prepare plot data
  plot_data <- timeseries_pivot %>%
    pivot_longer(cols = starts_with("Year_"), names_to = "year", values_to = "individuals") %>%
    mutate(year = as.numeric(gsub("Year_", "", year))) %>%
    group_by(focal_reach) %>%
    mutate(individuals_z = (individuals - mean(individuals, na.rm = TRUE)) / sd(individuals, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate basin-wide timeseries (z-normalized)
  basin_timeseries <- all_results %>%
    distinct(year, basin_total_run) %>%
    arrange(year) %>%
    mutate(basin_z = (basin_total_run - mean(basin_total_run)) / sd(basin_total_run))
  
  # Get stream orders
  actual_stream_orders <- sort(unique(cv_analysis$stream_order))
  actual_stream_orders <- actual_stream_orders[actual_stream_orders != 0]
  n_plots <- length(actual_stream_orders)
  
  # Calculate dynamic layout
  if (n_plots == 1) {
    layout_matrix <- matrix(c(1, 2), nrow = 1, byrow = TRUE)
    fig_height <- 6
    fig_width <- 14
  } else if (n_plots == 2) {
    layout_matrix <- matrix(c(1, 3, 2, 3), nrow = 2, byrow = TRUE)
    fig_height <- 10
    fig_width <- 14
  } else if (n_plots == 3) {
    layout_matrix <- matrix(c(1, 4, 2, 4, 3, 4), nrow = 3, byrow = TRUE)
    fig_height <- 14
    fig_width <- 16
  } else if (n_plots <= 6) {
    n_rows <- ceiling(n_plots / 2)
    layout_matrix <- matrix(0, nrow = n_rows, ncol = 3)
    plot_counter <- 1
    for (row in 1:n_rows) {
      for (col in 1:2) {
        if (plot_counter <= n_plots) {
          layout_matrix[row, col] <- plot_counter
          plot_counter <- plot_counter + 1
        }
      }
    }
    layout_matrix[, 3] <- n_plots + 1
    fig_height <- 4 + (n_rows * 4.5)
    fig_width <- 18
  } else if (n_plots <= 12) {
    n_rows <- ceiling(n_plots / 3)
    layout_matrix <- matrix(0, nrow = n_rows, ncol = 4)
    plot_counter <- 1
    for (row in 1:n_rows) {
      for (col in 1:3) {
        if (plot_counter <= n_plots) {
          layout_matrix[row, col] <- plot_counter
          plot_counter <- plot_counter + 1
        }
      }
    }
    layout_matrix[, 4] <- n_plots + 1
    fig_height <- 4 + (n_rows * 4)
    fig_width <- 20
  } else {
    n_rows <- ceiling(n_plots / 4)
    layout_matrix <- matrix(0, nrow = n_rows, ncol = 5)
    plot_counter <- 1
    for (row in 1:n_rows) {
      for (col in 1:4) {
        if (plot_counter <= n_plots) {
          layout_matrix[row, col] <- plot_counter
          plot_counter <- plot_counter + 1
        }
      }
    }
    layout_matrix[, 5] <- n_plots + 1
    fig_height <- 4 + (n_rows * 3.5)
    fig_width <- 22
  }
  
  # Create figure
  png_file <- file.path(config$figure_output_dir, 
                        paste0("TributaryGroups_", config$name, "_Combined_Analysis", type_label, ".png"))
  png(png_file, width = fig_width, height = fig_height, units = "in", res = 300, bg = "white")
  layout(layout_matrix, widths = c(rep(1, ncol(layout_matrix) - 1), 1.2))
  
  # Create timeseries plots
  for (plot_idx in 1:n_plots) {
    so <- actual_stream_orders[plot_idx]
    data_subset <- plot_data %>% filter(stream_order == so)
    
    par(bg = "#2d3a42", fg = "#ffffff", col.main = "#ffffff", col.lab = "#ffffff",
        col.axis = "#ffffff", mar = c(4, 5, 3, 1), mgp = c(3, 0.8, 0), family = "sans", lwd = 1.5)
    
    y_range <- range(c(data_subset$individuals_z, basin_timeseries$basin_z), na.rm = TRUE)
    
    plot(range(data_subset$year), y_range, type = "n",
         main = paste("Stream Order", so),
         xlab = if(plot_idx == n_plots) "Year" else "",
         ylab = "Z-normalized Individuals",
         las = 1, bty = "n", axes = FALSE, cex.main = 1.3, cex.lab = 1.0)
    
    axis(1, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff")
    axis(2, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", las = 1)
    abline(h = axTicks(2), col = "#4a5f67", lwd = 0.5, lty = 1)
    
    # Plot individual groups
    for (focal in sort(unique(data_subset$focal_reach))) {
      focal_data <- data_subset %>% filter(focal_reach == focal) %>% arrange(year)
      lines(focal_data$year, focal_data$individuals_z, 
            col = rgb(94, 179, 214, 80, maxColorValue = 255), lwd = 0.8)
    }
    
    # Plot basin trend
    lines(basin_timeseries$year, basin_timeseries$basin_z, type = "l", col = "#1dd4d4", lwd = 3.5)
    points(basin_timeseries$year, basin_timeseries$basin_z, pch = 19, col = "#1dd4d4", cex = 2)
    
    # Add run size labels
    for (i in 1:nrow(basin_timeseries)) {
      text(basin_timeseries$year[i], basin_timeseries$basin_z[i],
           labels = format(round(basin_timeseries$basin_total_run[i]), big.mark = ","),
           col = "#ffffff", cex = 1.2, pos = 3, offset = 0.5, font = 2)
    }
    
    abline(h = 0, lty = 2, col = "#4a5f67", lwd = 1.2)
  }
  
  # Create boxplot
  par(bg = "#2d3a42", fg = "#ffffff", col.main = "#ffffff", col.lab = "#ffffff",
      col.axis = "#ffffff", mar = c(4, 4, 3, 2), mgp = c(3, 0.8, 0), family = "sans")
  
  cv_data_for_plot <- cv_analysis %>% mutate(stream_order_char = as.character(stream_order))
  
  boxplot(cv ~ stream_order_char, data = cv_data_for_plot,
          main = "CV by Stream Order", ylab = "Coefficient of Variation", xlab = "",
          ylim = c(0, max(cv_data_for_plot$cv, na.rm = TRUE) * 1.15),
          col = "#ff5555", border = "#ffffff", las = 1, cex.main = 1.2, cex.lab = 0.9,
          cex.axis = 0.85, outline = TRUE, pch = 19)
  
  abline(h = basin_cv, lty = 2, col = "#1dd4d4", lwd = 4)
  legend("topright", legend = paste("Basin CV =", round(basin_cv, 3)), 
         lty = 2, col = "#1dd4d4", bty = "n", cex = 0.8, text.col = "#ffffff")
  
  dev.off()
  
  return(list(n_stream_orders = n_plots, png_file = png_file, fig_height = fig_height, fig_width = fig_width))
}

#==============================================================================
# MAIN EXECUTION
#==============================================================================

# Configuration
BASE_DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II"
ESCAPEMENT_FILE <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx"

SCENARIOS <- list(
  list(
    name = "Yukon_HalfYear",
    watershed = "Yukon",
    data_type = "half_year",
    years = c(2015, 2016, 2017, 2018, 2019, 2021),
    upstream_relationships = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/Yukon_UpstreamReaches_Relationships.csv"),
    prod_data_dir = file.path(BASE_DATA_DIR, "AnnualProdData/Yukon"),
    data_output_dir = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/TribGroupProdByYear/Yukon_HalfYear"),
    figure_output_dir = file.path(BASE_DATA_DIR, "Figures/CVbyStrOrd"),
    file_pattern = "CPUE50pct_.*_Yukon_Assignment_Results\\.csv$"
  ),
  list(
    name = "Yukon_FullYear",
    watershed = "Yukon",
    data_type = "full_year",
    years = c(2015, 2016, 2018, 2021),
    upstream_relationships = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/Yukon_UpstreamReaches_Relationships.csv"),
    prod_data_dir = file.path(BASE_DATA_DIR, "AnnualProdData/Yukon"),
    data_output_dir = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/TribGroupProdByYear/Yukon_FullYear"),
    figure_output_dir = file.path(BASE_DATA_DIR, "Figures/CVbyStrOrd"),
    file_pattern = "^\\d{4}_Yukon_Assignment_Results\\.csv$"
  ),
  list(
    name = "Kusko_FullYear",
    watershed = "Kusko",
    data_type = "full_year",
    years = c(2017, 2018, 2019, 2020, 2021),
    upstream_relationships = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/Kusko_UpstreamReaches_Relationships.csv"),
    prod_data_dir = file.path(BASE_DATA_DIR, "AnnualProdData/Kusko"),
    data_output_dir = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/TribGroupProdByYear/Kusko_FullYear"),
    figure_output_dir = file.path(BASE_DATA_DIR, "Figures/CVbyStrOrd"),
    file_pattern = "^\\d{4}_Kusko_Assignment_Results\\.csv$"
  )
)

# Run all scenarios
results_summary <- data.frame()

for (scenario_idx in seq_along(SCENARIOS)) {
  config <- SCENARIOS[[scenario_idx]]
  
  cat(sprintf("\n%s\nScenario %d: %s\n%s\n\n", 
              paste(rep("=", 80), collapse = ""),
              scenario_idx, config$name,
              paste(rep("=", 80), collapse = "")))
  
  tryCatch({
    # Load and prepare data
    data_result <- load_scenario_data(config, ESCAPEMENT_FILE)
    cat(sprintf("✓ Loaded %d years, %d tributary groups\n", 
                length(data_result$years_processed), 
                n_distinct(data_result$results$focal_reach)))
    
    # Analyze and export
    analysis_result <- analyze_and_export(data_result$results, config)
    cat("✓ Exported 5 CSV files\n")
    
    # Create visualization
    viz_result <- create_visualization(analysis_result, config)
    cat(sprintf("✓ Created figure: %d stream orders\n", viz_result$n_stream_orders))
    
    # Summary
    results_summary <- rbind(results_summary, data.frame(
      scenario = config$name,
      watershed = config$watershed,
      data_type = config$data_type,
      status = "COMPLETE",
      years = paste(data_result$years_processed, collapse = ", "),
      n_groups = n_distinct(data_result$results$focal_reach),
      n_stream_orders = viz_result$n_stream_orders,
      basin_cv = analysis_result$basin_cv
    ))
    
  }, error = function(e) {
    cat(sprintf("✗ FAILED: %s\n", e$message))
    results_summary <<- rbind(results_summary, data.frame(
      scenario = config$name,
      watershed = config$watershed,
      data_type = config$data_type,
      status = "FAILED",
      years = "",
      n_groups = 0,
      n_stream_orders = 0,
      basin_cv = NA
    ))
  })
}

# Final summary
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
print(results_summary)
cat("\n")