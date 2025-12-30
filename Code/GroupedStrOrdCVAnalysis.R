################################################################################
# TRIBUTARY GROUP PRODUCTION ANALYSIS - SEQUENTIAL LOOP VERSION
# Walk through this line-by-line to understand the workflow
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(readxl)

#==============================================================================
# CONFIGURATION
#==============================================================================

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

results_summary <- data.frame()

#==============================================================================
# MAIN SCENARIO LOOP
#==============================================================================

for (scenario_idx in seq_along(SCENARIOS)) {
  
  config <- SCENARIOS[[scenario_idx]]
  
  cat(sprintf("\n%s\nScenario %d: %s\n%s\n\n", 
              paste(rep("=", 80), collapse = ""),
              scenario_idx, config$name,
              paste(rep("=", 80), collapse = "")))
  
  tryCatch({
    
    #==========================================================================
    # STEP 1: LOAD UPSTREAM RELATIONSHIPS
    #==========================================================================
    
    cat("Step 1: Loading upstream relationships...\n")
    
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
    
    cat(sprintf("  ✓ Loaded %d tributary groups\n\n", n_distinct(groups$focal_reach)))
    
    #==========================================================================
    # STEP 2: PROCESS EACH YEAR
    #==========================================================================
    
    cat("Step 2: Processing production data by year...\n")
    
    all_results <- data.frame()
    years_processed <- vector()
    
    for (year in config$years) {
      
      tryCatch({
        
        # Load basin total run from escapement file
        basin_data <- read_xlsx(ESCAPEMENT_FILE)
        basin_total_run <- basin_data %>%
          filter(River == config$watershed, Year == year) %>%
          pull(Total_Run)
        
        if (length(basin_total_run) == 0) {
          cat(sprintf("    %d - SKIPPED (no escapement data)\n", year))
          next
        }
        
        # Find and load production file for this year
        all_files <- list.files(config$prod_data_dir, full.names = TRUE)
        matching_files <- all_files[
          grepl(paste0(year), basename(all_files)) & 
            grepl(paste0("_", config$watershed, "_Assignment_Results"), basename(all_files)) &
            grepl(config$file_pattern, basename(all_files))
        ]
        
        if (length(matching_files) == 0) {
          cat(sprintf("    %d - SKIPPED (no production file)\n", year))
          next
        }
        
        prod_data <- read_csv(matching_files[1], show_col_types = FALSE)
        
        if (!all(c("reachid", "assignment_individuals") %in% names(prod_data))) {
          cat(sprintf("    %d - SKIPPED (missing required columns)\n", year))
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
        cat(sprintf("    %d - ✓\n", year))
        
      }, error = function(e) {
        cat(sprintf("    %d - ERROR: %s\n", year, e$message))
      })
    }
    
    if (nrow(all_results) == 0) {
      stop("No data found for scenario")
    }
    
    cat(sprintf("\n  ✓ Processed %d years with %d tributary groups\n\n", 
                length(years_processed), n_distinct(all_results$focal_reach)))
    
    #==========================================================================
    # STEP 3: EXPORT DATA FILES
    #==========================================================================
    
    cat("Step 3: Exporting CSV files...\n")
    
    type_label <- ifelse(config$data_type == "full_year", "", "_HalfYear")
    dir.create(config$data_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Export long format
    detailed_file <- file.path(config$data_output_dir, 
                               paste0("TributaryGroups_Individuals_LongFormat", type_label, ".csv"))
    write_csv(all_results, detailed_file)
    cat(sprintf("  ✓ %s\n", basename(detailed_file)))
    
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
    cat(sprintf("  ✓ %s\n", basename(timeseries_file)))
    
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
    cat(sprintf("  ✓ %s\n", basename(summary_file)))
    
    #==========================================================================
    # STEP 4: CALCULATE COEFFICIENT OF VARIATION
    #==========================================================================
    
    cat("\nStep 4: Calculating coefficient of variation...\n")
    
    # CV analysis by tributary group
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
    cat(sprintf("  ✓ %s\n", basename(cv_file)))
    
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
    cat(sprintf("  ✓ %s\n", basename(cv_summary_file)))
    
    # Basin-wide CV
    basin_cv <- all_results %>%
      distinct(year, basin_total_run) %>%
      summarise(cv_basin = sd(basin_total_run) / mean(basin_total_run)) %>%
      pull(cv_basin)
    
    cat(sprintf("\n  Basin-wide CV: %.4f\n\n", basin_cv))
    
    #==========================================================================
    # STEP 5: PREPARE VISUALIZATION DATA
    #==========================================================================
    
    cat("Step 5: Preparing visualization data...\n")
    
    # Prepare timeseries plot data
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
    
    # Get stream orders for plotting
    actual_stream_orders <- sort(unique(cv_analysis$stream_order))
    actual_stream_orders <- actual_stream_orders[actual_stream_orders != 0]
    n_stream_orders <- length(actual_stream_orders)
    
    cat(sprintf("  ✓ Found %d stream orders\n\n", n_stream_orders))
    
    #==========================================================================
    # STEP 6: CREATE TIMESERIES FIGURE
    #==========================================================================
    
    cat("Step 6: Creating timeseries figure...\n")
    
    dir.create(config$figure_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    fig_height_ts <- 4 + (n_stream_orders * 3.5)
    fig_width_ts <- 10
    
    ts_png_file <- file.path(config$figure_output_dir, 
                             paste0("TributaryGroups_", config$name, "_Timeseries", type_label, ".png"))
    png(ts_png_file, width = fig_width_ts, height = fig_height_ts, units = "in", res = 300, bg = "white")
    
    layout(matrix(1:n_stream_orders, ncol = 1))
    
    for (plot_idx in 1:n_stream_orders) {
      so <- actual_stream_orders[plot_idx]
      data_subset <- plot_data %>% filter(stream_order == so)
      
      par(bg = "#2d3a42", fg = "#ffffff", col.main = "#ffffff", col.lab = "#ffffff",
          col.axis = "#ffffff", mar = c(4, 5, 3, 1), mgp = c(3, 0.8, 0), family = "sans", lwd = 1.5)
      
      y_range <- range(c(data_subset$individuals_z, basin_timeseries$basin_z), na.rm = TRUE)
      
      plot(range(data_subset$year), y_range, type = "n",
           main = paste("Stream Order", so),
           xlab = if(plot_idx == n_stream_orders) "Year" else "",
           ylab = "Z-normalized Individuals",
           las = 1, bty = "n", axes = FALSE, cex.main = 1.3, cex.lab = 1.0)
      
      axis(1, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff")
      axis(2, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", las = 1)
      abline(h = axTicks(2), col = "#4a5f67", lwd = 0.5, lty = 1)
      
      # Plot individual tributary groups
      for (focal in sort(unique(data_subset$focal_reach))) {
        focal_data <- data_subset %>% filter(focal_reach == focal) %>% arrange(year)
        lines(focal_data$year, focal_data$individuals_z, 
              col = rgb(94, 179, 214, 80, maxColorValue = 255), lwd = 0.8)
      }
      
      # Plot basin-wide trend
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
    
    dev.off()
    cat(sprintf("  ✓ %s\n", basename(ts_png_file)))
    cat(sprintf("  ✓ Dimensions: %.0f x %.0f inches\n\n", fig_width_ts, fig_height_ts))
    
    #==========================================================================
    # STEP 7: CREATE BOXPLOT FIGURE
    #==========================================================================
    
    cat("Step 7: Creating boxplot figure...\n")
    
    cv_data_for_plot <- cv_analysis %>% mutate(stream_order_char = as.character(stream_order))
    
    n_stream_orders_plot <- n_distinct(cv_data_for_plot$stream_order)
    fig_width_box <- max(8, 2 + (n_stream_orders_plot * 0.8))
    fig_height_box <- 6
    
    box_png_file <- file.path(config$figure_output_dir, 
                              paste0("TributaryGroups_", config$name, "_CV_Boxplot", type_label, ".png"))
    png(box_png_file, width = fig_width_box, height = fig_height_box, units = "in", res = 300, bg = "white")
    
    par(bg = "#2d3a42", fg = "#ffffff", col.main = "#ffffff", col.lab = "#ffffff",
        col.axis = "#ffffff", mar = c(4, 4, 3, 2), mgp = c(3, 0.8, 0), family = "sans")
    
    boxplot(cv ~ stream_order_char, data = cv_data_for_plot,
            main = "CV by Stream Order", ylab = "Coefficient of Variation", xlab = "",
            ylim = c(0, max(cv_data_for_plot$cv, na.rm = TRUE) * 1.15),
            col = "#ff5555", border = "#ffffff", las = 1, cex.main = 1.2, cex.lab = 0.9,
            cex.axis = 0.85, outline = TRUE, pch = 19)
    
    abline(h = basin_cv, lty = 2, col = "#1dd4d4", lwd = 4)
    legend("topright", legend = paste("Basin CV =", round(basin_cv, 3)), 
           lty = 2, col = "#1dd4d4", bty = "n", cex = 0.8, text.col = "#ffffff")
    
    dev.off()
    cat(sprintf("  ✓ %s\n", basename(box_png_file)))
    cat(sprintf("  ✓ Dimensions: %.0f x %.0f inches\n\n", fig_width_box, fig_height_box))
    
    #==========================================================================
    # STEP 8: CREATE VIOLIN PLOT FIGURE
    #==========================================================================
    
    cat("Step 8: Creating violin plot figure...\n")
    
    unique_orders <- sort(unique(as.numeric(cv_data_for_plot$stream_order_char)))
    
    fig_width_vio <- max(8, 2 + (n_stream_orders_plot * 0.8))
    fig_height_vio <- 6
    
    vio_png_file <- file.path(config$figure_output_dir, 
                              paste0("TributaryGroups_", config$name, "_CV_ViolinPlot", type_label, ".png"))
    png(vio_png_file, width = fig_width_vio, height = fig_height_vio, units = "in", res = 300, bg = "white")
    
    par(bg = "#2d3a42", fg = "#ffffff", col.main = "#ffffff", col.lab = "#ffffff",
        col.axis = "#ffffff", mar = c(4, 4, 3, 2), mgp = c(3, 0.8, 0), family = "sans")
    
    # Create empty plot frame
    plot(range(0.5, length(unique_orders) + 0.5), 
         c(0, max(cv_data_for_plot$cv, na.rm = TRUE) * 1.15),
         type = "n",
         main = "CV by Stream Order",
         xlab = "",
         ylab = "Coefficient of Variation",
         axes = FALSE,
         xaxs = "i",
         yaxs = "i",
         cex.main = 1.2,
         cex.lab = 0.9)
    
    axis(1, at = 1:length(unique_orders), labels = unique_orders, 
         lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", cex.axis = 0.85)
    axis(2, lwd = 1.5, col = "#4a5f67", col.ticks = "#4a5f67", col.axis = "#ffffff", las = 1, cex.axis = 0.85)
    abline(h = axTicks(2), col = "#4a5f67", lwd = 0.5, lty = 1)
    
    # Draw violins for each stream order
    for (i in seq_along(unique_orders)) {
      so <- unique_orders[i]
      data_subset <- cv_data_for_plot %>% filter(stream_order == so) %>% pull(cv)
      
      if (length(data_subset) > 1) {
        # Calculate kernel density
        dens <- density(data_subset, na.rm = TRUE, adjust = 1.2)
        
        # Scale density for plotting (width of violin)
        dens_scaled <- dens$x
        dens_y <- dens$y
        dens_y_scaled <- dens_y / max(dens_y) * 0.35
        
        # Draw violin (filled polygon)
        polygon(c(i - dens_y_scaled, i + rev(dens_y_scaled)),
                c(dens_scaled, rev(dens_scaled)),
                col = "#ff5555", border = "#ffffff", lwd = 1.5)
        
        # Add mean line
        mean_val <- mean(data_subset, na.rm = TRUE)
        segments(i - 0.15, mean_val, i + 0.15, mean_val, 
                 col = "#1dd4d4", lwd = 3.5)
        
        # Add points
        points(rep(i, length(data_subset)), data_subset, 
               col = rgb(255, 255, 255, 100, maxColorValue = 255), 
               pch = 19, cex = 0.8)
      }
    }
    
    # Add basin CV reference line
    abline(h = basin_cv, lty = 2, col = "#1dd4d4", lwd = 4)
    
    # Add legend
    legend("topright", legend = c(paste("Basin CV =", round(basin_cv, 3)), "Mean"), 
           lty = c(2, 1), col = c("#1dd4d4", "#1dd4d4"), lwd = c(4, 3.5),
           bty = "n", cex = 0.8, text.col = "#ffffff")
    
    dev.off()
    cat(sprintf("  ✓ %s\n", basename(vio_png_file)))
    cat(sprintf("  ✓ Dimensions: %.0f x %.0f inches\n\n", fig_width_vio, fig_height_vio))
    
    #==========================================================================
    # STEP 9: ADD TO RESULTS SUMMARY
    #==========================================================================
    
    cat("Step 9: Adding to results summary...\n")
    
    results_summary <- rbind(results_summary, data.frame(
      scenario = config$name,
      status = "COMPLETE",
      years = paste(years_processed, collapse = ", "),
      n_groups = n_distinct(all_results$focal_reach),
      n_stream_orders = n_stream_orders,
      basin_cv = basin_cv
    ))
    
    cat("  ✓ Summary added\n\n")
    
  }, error = function(e) {
    cat(sprintf("✗ FAILED: %s\n\n", e$message))
    results_summary <<- rbind(results_summary, data.frame(
      scenario = config$name,
      status = "FAILED",
      years = "",
      n_groups = 0,
      n_stream_orders = 0,
      basin_cv = NA
    ))
  })
}

#==============================================================================
# FINAL SUMMARY
#==============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("ALL SCENARIOS COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
cat("Summary of Results:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
print(results_summary)
cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("Output locations:\n")
cat(" Data: /Data/UpstreamReaches/TribGroupProdByYear/[Scenario_Name]/\n")
cat(" Figures: /Figures/CVbyStrOrd/\n\n")

cat("✓ Analysis complete!\n\n")