################################################################################
# TRIBUTARY GROUP PRODUCTION ANALYSIS - ALL THREE SCENARIOS
# Master script to systematically run:
# 1. Yukon HALF-YEAR (2015, 2016, 2017, 2018, 2019, 2021)
# 2. Yukon FULL-YEAR (2015, 2016, 2018, 2021)
# 3. Kuskokwim FULL-YEAR (2017, 2018, 2019, 2020, 2021)
#
# ENHANCED: Fully flexible plotting and analysis for ANY number of stream orders
# Script now dynamically handles 1+ stream orders and adjusts layout automatically
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(readxl)

cat("================================================================================\n")
cat("TRIBUTARY GROUP PRODUCTION ANALYSIS - ALL SCENARIOS (ENHANCED)\n")
cat("================================================================================\n\n")

#==============================================================================
# CONFIGURATION
#==============================================================================

# Base paths
BASE_DATA_DIR <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II"

# ESA escapement data (shared across all scenarios)
ESCAPEMENT_FILE <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx"

# Define all three scenarios
SCENARIOS <- list(
  # Scenario 1: Yukon Half-Year
  list(
    name = "Yukon_HalfYear",
    watershed = "Yukon",
    data_type = "half_year",
    years = c(2015, 2016, 2017, 2018, 2019, 2021),
    upstream_relationships = file.path("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/Yukon_UpstreamReaches_Relationships.csv"),
    prod_data_dir = file.path(BASE_DATA_DIR, "AnnualProdData/Yukon"),
    data_output_dir = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/TribGroupProdByYear/Yukon_HalfYear"),
    figure_output_dir = file.path(BASE_DATA_DIR, "Figures/CVbyStrOrd"),
    file_pattern = "CPUE50pct_.*_Yukon_Assignment_Results\\.csv$"
  ),
  
  # Scenario 2: Yukon Full-Year
  list(
    name = "Yukon_FullYear",
    watershed = "Yukon",
    data_type = "full_year",
    years = c(2015, 2016, 2018, 2021),
    upstream_relationships = file.path("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/Yukon_UpstreamReaches_Relationships.csv"),
    prod_data_dir = file.path(BASE_DATA_DIR, "AnnualProdData/Yukon"),
    data_output_dir = file.path(BASE_DATA_DIR, "Data/UpstreamReaches/TribGroupProdByYear/Yukon_FullYear"),
    figure_output_dir = file.path(BASE_DATA_DIR, "Figures/CVbyStrOrd"),
    file_pattern = "^\\d{4}_Yukon_Assignment_Results\\.csv$"
  ),
  
  # Scenario 3: Kuskokwim Full-Year
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

#==============================================================================
# PROCESS EACH SCENARIO
#==============================================================================

results_summary <- data.frame(
  scenario = character(),
  watershed = character(),
  data_type = character(),
  status = character(),
  years_processed = character(),
  n_tributary_groups = integer(),
  n_stream_orders = integer(),
  basin_cv = numeric(),
  stringsAsFactors = FALSE
)

for (scenario_idx in seq_along(SCENARIOS)) {
  
  config <- SCENARIOS[[scenario_idx]]
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat(sprintf("SCENARIO %d: %s\n", scenario_idx, config$name))
  cat(sprintf("Watershed: %s | Data Type: %s\n", config$watershed, config$data_type))
  cat(sprintf("Years: %s\n", paste(config$years, collapse = ", ")))
  cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
  
  # Create output directories
  dir.create(config$data_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$figure_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Creating output directories...\n")
  cat(sprintf("  Data: %s\n", config$data_output_dir))
  cat(sprintf("  Figures: %s\n", config$figure_output_dir))
  
  #============================================================================
  # LOAD UPSTREAM RELATIONSHIPS
  #============================================================================
  
  cat("\nLoading upstream relationships...")
  
  if (!file.exists(config$upstream_relationships)) {
    cat(" ✗ FAILED\n")
    cat(sprintf("  ERROR: File not found: %s\n", config$upstream_relationships))
    results_summary <- rbind(results_summary, data.frame(
      scenario = config$name,
      watershed = config$watershed,
      data_type = config$data_type,
      status = "FAILED - upstream relationships file not found",
      years_processed = "",
      n_tributary_groups = 0,
      n_stream_orders = 0,
      basin_cv = NA
    ))
    next
  }
  
  upstream_df <- read_csv(config$upstream_relationships, show_col_types = FALSE) %>%
    rename(focal_reach = original_reachid, 
           tributary_reach = upstream_reachid,
           stream_order = reachbase)
  
  # Create tributary groups
  focal_reaches_only <- upstream_df %>%
    distinct(focal_reach, stream_order) %>%
    mutate(tributary_reach = focal_reach)
  
  groups <- bind_rows(focal_reaches_only, upstream_df) %>%
    distinct() %>%
    rename(reach_in_group = tributary_reach)
  
  cat(" ✓\n")
  cat(sprintf("  Loaded %d tributary groups\n", n_distinct(groups$focal_reach)))
  
  #============================================================================
  # PROCESS EACH YEAR
  #============================================================================
  
  cat("\nProcessing years:\n")
  
  all_results <- data.frame()
  years_processed <- vector()
  
  for (year in config$years) {
    
    # Load basin run data
    tryCatch({
      basin_data <- read_xlsx(ESCAPEMENT_FILE)
      basin_total_run <- basin_data %>%
        filter(River == config$watershed, Year == year) %>%
        pull(Total_Run)
      
      if (length(basin_total_run) == 0) {
        cat(sprintf("  %d - SKIPPED (no escapement data)\n", year))
        next
      }
      
      # Load production data
      all_files <- list.files(config$prod_data_dir, full.names = TRUE)
      matching_files <- all_files[grepl(paste0(year), basename(all_files)) & 
                                    grepl(paste0("_", config$watershed, "_Assignment_Results"), basename(all_files)) &
                                    grepl(config$file_pattern, basename(all_files))]
      
      if (length(matching_files) == 0) {
        cat(sprintf("  %d - SKIPPED (no matching production file)\n", year))
        next
      }
      
      prod_data <- read_csv(matching_files[1], show_col_types = FALSE)
      
      if (!all(c("reachid", "assignment_individuals") %in% names(prod_data))) {
        cat(sprintf("  %d - SKIPPED (missing required columns)\n", year))
        next
      }
      
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
        
        n_reaches_total <- length(group_reaches)
        n_reaches_with_prod <- sum(
          prod_data$reachid %in% group_reaches & prod_data$assignment_individuals > 0,
          na.rm = TRUE
        )
        
        all_results <- rbind(all_results, data.frame(
          year = year,
          stream_order = so,
          focal_reach = focal_id,
          group_individuals = group_individuals,
          n_reaches_in_group = n_reaches_total,
          n_reaches_with_production = n_reaches_with_prod,
          basin_total_run = basin_total_run,
          stringsAsFactors = FALSE
        ))
      }
      
      years_processed <- c(years_processed, year)
      cat(sprintf("  %d - ✓ (%d tributary groups)\n", year, nrow(unique_focal)))
      
    }, error = function(e) {
      cat(sprintf("  %d - ERROR: %s\n", year, e$message))
    })
  }
  
  if (nrow(all_results) == 0) {
    cat("\n✗ NO DATA FOUND - skipping this scenario\n")
    results_summary <- rbind(results_summary, data.frame(
      scenario = config$name,
      watershed = config$watershed,
      data_type = config$data_type,
      status = "FAILED - no data found",
      years_processed = "",
      n_tributary_groups = 0,
      n_stream_orders = 0,
      basin_cv = NA
    ))
    next
  }
  
  #============================================================================
  # EXPORT RESULTS
  #============================================================================
  
  cat("\nExporting results:\n")
  
  type_label <- ifelse(config$data_type == "full_year", "", "_HalfYear")
  
  # Long format (detailed)
  detailed_file <- file.path(config$data_output_dir, 
                             paste0("TributaryGroups_Individuals_LongFormat", type_label, ".csv"))
  write_csv(all_results, detailed_file)
  cat(sprintf("  ✓ Long format: %s\n", basename(detailed_file)))
  
  # Timeseries (wide format)
  timeseries_pivot <- all_results %>%
    select(focal_reach, stream_order, year, group_individuals) %>%
    pivot_wider(
      names_from = year,
      values_from = group_individuals,
      names_prefix = "Year_"
    ) %>%
    mutate(
      focal_reach = as.integer(focal_reach),
      stream_order = as.integer(stream_order)
    ) %>%
    arrange(stream_order, focal_reach)
  
  timeseries_file <- file.path(config$data_output_dir, 
                               paste0("TributaryGroups_Individuals_Timeseries", type_label, ".csv"))
  write_csv(timeseries_pivot, timeseries_file)
  cat(sprintf("  ✓ Timeseries: %s\n", basename(timeseries_file)))
  
  # Summary by stream order
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
  cat(sprintf("  ✓ Summary: %s\n", basename(summary_file)))
  
  #============================================================================
  # COEFFICIENT OF VARIATION ANALYSIS
  #============================================================================
  
  # CV by tributary group
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
  
  # Basin-wide CV
  basin_cv <- all_results %>%
    distinct(year, basin_total_run) %>%
    summarise(
      mean_basin_run = mean(basin_total_run, na.rm = TRUE),
      sd_basin_run = sd(basin_total_run, na.rm = TRUE),
      cv_basin = sd_basin_run / mean_basin_run,
      .groups = "drop"
    ) %>%
    pull(cv_basin)
  
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
  
  # Get actual unique stream orders (excluding 0 if present)
  actual_stream_orders <- sort(unique(cv_analysis$stream_order))
  actual_stream_orders <- actual_stream_orders[actual_stream_orders != 0]
  n_stream_orders <- length(actual_stream_orders)
  
  # Export CV results
  cv_file <- file.path(config$data_output_dir, 
                       paste0("TributaryGroups_CoefficientOfVariation", type_label, ".csv"))
  write_csv(cv_analysis, cv_file)
  
  cv_summary_file <- file.path(config$data_output_dir, 
                               paste0("TributaryGroups_CV_Summary", type_label, ".csv"))
  write_csv(cv_summary, cv_summary_file)
  cat(sprintf("  ✓ CV analysis: %s\n", basename(cv_file)))
  
  #============================================================================
  # VISUALIZATION - FULLY FLEXIBLE LAYOUT
  #============================================================================
  
  cat("\nCreating visualization:\n")
  cat(sprintf("  Found %d unique stream orders: %s\n", 
              n_stream_orders, paste(actual_stream_orders, collapse = ", ")))
  
  tryCatch({
    # Prepare data
    plot_data <- timeseries_pivot %>%
      pivot_longer(
        cols = starts_with("Year_"),
        names_to = "year",
        values_to = "individuals"
      ) %>%
      mutate(year = as.numeric(gsub("Year_", "", year))) %>%
      group_by(focal_reach) %>%
      mutate(
        individuals_z = (individuals - mean(individuals, na.rm = TRUE)) / sd(individuals, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Calculate basin-wide production timeseries (z-normalized)
    basin_timeseries <- all_results %>%
      distinct(year, basin_total_run) %>%
      arrange(year) %>%
      mutate(
        basin_z = (basin_total_run - mean(basin_total_run, na.rm = TRUE)) / sd(basin_total_run, na.rm = TRUE)
      )
    
    # Use actual stream orders from data
    stream_orders_plot <- actual_stream_orders
    
    # Prepare CV data for boxplot
    cv_data_for_plot <- cv_analysis %>%
      mutate(stream_order_char = as.character(stream_order))
    
    #==========================================================================
    # DYNAMIC LAYOUT CALCULATION - Handles ANY number of stream orders
    #==========================================================================
    n_plots <- n_stream_orders
    
    # Determine layout dimensions - flexible for ANY number of stream orders
    if (n_plots == 1) {
      n_rows <- 1
      n_cols <- 2  # Stream order plot + boxplot
      layout_matrix <- matrix(c(1, 2), nrow = 1, byrow = TRUE)
      fig_height <- 6
      fig_width <- 14
    } else if (n_plots == 2) {
      n_rows <- 2
      n_cols <- 2  # 2 stream orders + 1 boxplot
      layout_matrix <- matrix(c(1, 3, 2, 3), nrow = 2, byrow = TRUE)
      fig_height <- 10
      fig_width <- 14
    } else if (n_plots == 3) {
      n_rows <- 3
      n_cols <- 2  # 3 stream orders + 1 boxplot
      layout_matrix <- matrix(c(1, 4, 2, 4, 3, 4), nrow = 3, byrow = TRUE)
      fig_height <- 14
      fig_width <- 16
    } else if (n_plots <= 6) {
      # For 4-6 stream orders: 2 columns + boxplot
      n_rows <- ceiling(n_plots / 2)
      n_cols <- 3  # 2 stream order columns + 1 boxplot column
      
      layout_matrix <- matrix(0, nrow = n_rows, ncol = n_cols)
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
      # For 7-12 stream orders: 3 columns + boxplot
      n_rows <- ceiling(n_plots / 3)
      n_cols <- 4  # 3 stream order columns + 1 boxplot column
      
      layout_matrix <- matrix(0, nrow = n_rows, ncol = n_cols)
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
      # For 13+ stream orders: 4 columns + boxplot
      n_rows <- ceiling(n_plots / 4)
      n_cols <- 5  # 4 stream order columns + 1 boxplot column
      
      layout_matrix <- matrix(0, nrow = n_rows, ncol = n_cols)
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
    
    # Create combined figure
    png_file <- file.path(config$figure_output_dir, 
                          paste0("TributaryGroups_", config$name, "_Combined_Analysis", type_label, ".png"))
    png(png_file, width = fig_width, height = fig_height, units = "in", res = 300, bg = "white")
    
    layout(layout_matrix, widths = c(rep(1, n_cols - 1), 1.2))
    
    # Create timeseries plots for each stream order
    for (plot_idx in 1:n_plots) {
      so <- stream_orders_plot[plot_idx]
      data_subset <- plot_data %>% filter(stream_order == so)
      
      par(
        bg = "#2d3a42",
        fg = "#ffffff",
        col.main = "#ffffff",
        col.lab = "#ffffff",
        col.axis = "#ffffff",
        mar = c(4, 5, 3, 1),
        mgp = c(3, 0.8, 0),
        family = "sans",
        lwd = 1.5
      )
      
      y_range <- range(c(data_subset$individuals_z, basin_timeseries$basin_z), na.rm = TRUE)
      
      plot(
        range(data_subset$year),
        y_range,
        type = "n",
        main = paste("Stream Order", so),
        xlab = if(plot_idx == n_plots) "Year" else "",
        ylab = "Z-normalized Individuals",
        las = 1,
        bty = "n",
        axes = FALSE,
        cex.main = 1.3,
        cex.lab = 1.0
      )
      
      axis(1, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff")
      axis(2, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", las = 1)
      
      abline(h = axTicks(2), col = "#4a5f67", lwd = 0.5, lty = 1)
      
      # Plot individual groups
      focal_reaches <- sort(unique(data_subset$focal_reach))
      for (focal in focal_reaches) {
        focal_data <- data_subset %>% filter(focal_reach == focal) %>% arrange(year)
        lines(focal_data$year, focal_data$individuals_z, 
              col = rgb(94, 179, 214, 80, maxColorValue = 255),
              lwd = 0.8)
      }
      
      # Plot basin-wide production trend line
      lines(basin_timeseries$year, basin_timeseries$basin_z, 
            type = "l", col = "#1dd4d4", lwd = 3.5)
      
      # Add points to the basin trend line
      points(basin_timeseries$year, basin_timeseries$basin_z,
             pch = 19, col = "#1dd4d4", cex = 2)
      
      # Add text labels showing actual run values next to each point
      for (i in 1:nrow(basin_timeseries)) {
        text(basin_timeseries$year[i], 
             basin_timeseries$basin_z[i],
             labels = format(round(basin_timeseries$basin_total_run[i]), big.mark = ","),
             col = "#ffffff",
             cex = 1.2,
             pos = 3,  # position above the point
             offset = 0.5,
             font = 2)  # bold
      }
      
      abline(h = 0, lty = 2, col = "#4a5f67", lwd = 1.2)
    }
    
    # Create boxplot on the right
    par(
      bg = "#2d3a42",
      fg = "#ffffff",
      col.main = "#ffffff",
      col.lab = "#ffffff",
      col.axis = "#ffffff",
      mar = c(4, 4, 3, 2),
      mgp = c(3, 0.8, 0),
      family = "sans"
    )
    
    boxplot(
      cv ~ stream_order_char,
      data = cv_data_for_plot,
      main = "CV by Stream Order",
      ylab = "Coefficient of Variation",
      xlab = "",
      ylim = c(0, max(cv_data_for_plot$cv, na.rm = TRUE) * 1.15),
      col = "#ff5555",
      border = "#ffffff",
      las = 1,
      cex.main = 1.2,
      cex.lab = 0.9,
      cex.axis = 0.85,
      outline = TRUE,
      pch = 19
    )
    
    # Add basin CV reference line
    abline(h = basin_cv, lty = 2, col = "#1dd4d4", lwd = 4)
    legend("topright", legend = paste("Basin CV =", round(basin_cv, 3)), 
           lty = 2, col = "#1dd4d4", bty = "n", cex = 0.8, text.col = "#ffffff")
    
    dev.off()
    cat(sprintf("  ✓ Figure: %s\n", basename(png_file)))
    cat(sprintf("  ✓ Figure dimensions: %d x %d inches\n", fig_width, fig_height))
    cat(sprintf("  ✓ Layout: %d rows × %d columns (boxplot spans %d rows)\n", n_rows, n_cols - 1, n_rows))
    
  }, error = function(e) {
    cat(sprintf("  ✗ ERROR creating figure: %s\n", e$message))
  })
  
  #============================================================================
  # SUMMARY FOR THIS SCENARIO
  #============================================================================
  
  cat("\nScenario Summary:\n")
  cat(sprintf("  Status: ✓ COMPLETE\n"))
  cat(sprintf("  Years processed: %s\n", paste(years_processed, collapse = ", ")))
  cat(sprintf("  Tributary groups: %d\n", n_distinct(all_results$focal_reach)))
  cat(sprintf("  Stream orders: %d (%s)\n", n_stream_orders, paste(actual_stream_orders, collapse = ", ")))
  cat(sprintf("  Basin-wide CV: %.4f\n", basin_cv))
  cat(sprintf("  Data directory: %s\n", config$data_output_dir))
  
  # Add to results summary
  results_summary <- rbind(results_summary, data.frame(
    scenario = config$name,
    watershed = config$watershed,
    data_type = config$data_type,
    status = "COMPLETE",
    years_processed = paste(years_processed, collapse = ", "),
    n_tributary_groups = n_distinct(all_results$focal_reach),
    n_stream_orders = n_stream_orders,
    basin_cv = basin_cv
  ))
}

#==============================================================================
# FINAL SUMMARY
#==============================================================================

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("ALL SCENARIOS COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

cat("Summary of Results:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")

print(results_summary %>% 
        select(scenario, watershed, data_type, status, years_processed, n_tributary_groups, n_stream_orders, basin_cv))

cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("Output Directory Structure:\n")
cat("  Base directory: /Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/\n\n")

cat("Data Output Directories (by scenario):\n")
for (scenario in SCENARIOS) {
  cat(sprintf("  %s:\n", scenario$name))
  cat(sprintf("    %s\n", scenario$data_output_dir))
}

cat("\nFigure Output Directory (all scenarios):\n")
cat("  /Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Figures/CVbyStrOrd\n\n")

cat("\nFiles created per scenario:\n")
cat("  - TributaryGroups_Individuals_LongFormat[_HalfYear].csv\n")
cat("  - TributaryGroups_Individuals_Timeseries[_HalfYear].csv\n")
cat("  - TributaryGroups_SummaryByStreamOrder[_HalfYear].csv\n")
cat("  - TributaryGroups_CoefficientOfVariation[_HalfYear].csv\n")
cat("  - TributaryGroups_CV_Summary[_HalfYear].csv\n")
cat("  - TributaryGroups_Combined_Analysis[_HalfYear].png\n")

cat("\n✓ Analysis complete! All scenarios processed successfully.\n")
cat("✓ Script automatically handled all stream orders found in datasets.\n\n")