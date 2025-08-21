# app.R
library(shiny)
library(shinydashboard)
library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(DT)
library(tidyr) # Added for pivot_longer function

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Watershed Analysis Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Basin Maps", tabName = "basin_maps", icon = icon("map")),
      menuItem("DOY Analysis", tabName = "doy_analysis", icon = icon("calendar")),
      menuItem("CPUE Analysis", tabName = "cpue_analysis", icon = icon("chart-line")),
      menuItem("Cumulative Analysis", tabName = "cumulative", icon = icon("layer-group")),
      menuItem("Comparison", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Watershed Analysis Explorer",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  "This app provides an interactive way to explore the results of salmon natal origin analysis for the Yukon and Kuskokwim watersheds.",
                  "Use the sidebar menu to navigate between different analysis types and views."
                )
              ),
              fluidRow(
                valueBox(
                  "Watersheds", 
                  "Yukon & Kuskokwim", 
                  icon = icon("water"),
                  color = "blue",
                  width = 4
                ),
                valueBox(
                  "Years", 
                  "2015-2016", 
                  icon = icon("calendar"),
                  color = "green",
                  width = 4
                ),
                valueBox(
                  "Analysis Types", 
                  "Basin, DOY, CPUE, Cumulative", 
                  icon = icon("chart-bar"),
                  color = "purple",
                  width = 4
                )
              ),
              fluidRow(
                box(
                  title = "Quick Stats",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("overviewPlot", height = "300px")
                )
              )
      ),
      
      # Basin Maps tab
      tabItem(tabName = "basin_maps",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  status = "primary",
                  selectInput("basin_watershed", "Watershed:",
                              c("Yukon" = "Yukon",
                                "Kuskokwim" = "Kusko")),
                  selectInput("basin_year", "Year:",
                              c("2015" = "2015",
                                "2016" = "2016")),
                  selectInput("basin_map_type", "Map Type:",
                              c("Production per km" = "per_km",
                                "Raw Production" = "raw_prod",
                                "Tributary Map" = "trib"))
                ),
                box(
                  title = "Basin Map",
                  width = 9,
                  status = "primary",
                  plotOutput("basinMapPlot", height = "600px")
                )
              )
      ),
      
      # DOY Analysis tab
      tabItem(tabName = "doy_analysis",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  status = "primary",
                  selectInput("doy_watershed", "Watershed:",
                              c("Yukon" = "Yukon",
                                "Kuskokwim" = "Kusko")),
                  selectInput("doy_year", "Year:",
                              c("2015" = "2015",
                                "2016" = "2016")),
                  selectInput("doy_quartile", "Quartile:",
                              c("Q1 (Early Run)" = "Q1",
                                "Q2" = "Q2",
                                "Q3" = "Q3",
                                "Q4 (Late Run)" = "Q4",
                                "All" = "All")),
                  selectInput("doy_map_type", "Map Type:",
                              c("Production per km" = "per_km",
                                "Raw Production" = "raw_prod",
                                "Tributary Map" = "trib"))
                ),
                box(
                  title = "DOY Quartile Analysis",
                  width = 9,
                  status = "primary",
                  plotOutput("doyMapPlot", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "DOY Distribution",
                  width = 12,
                  status = "info",
                  plotOutput("doyDistPlot", height = "300px")
                )
              )
      ),
      
      # CPUE Analysis tab
      tabItem(tabName = "cpue_analysis",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  status = "primary",
                  selectInput("cpue_watershed", "Watershed:",
                              c("Yukon" = "Yukon",
                                "Kuskokwim" = "Kusko")),
                  selectInput("cpue_year", "Year:",
                              c("2015" = "2015",
                                "2016" = "2016")),
                  selectInput("cpue_quartile", "Quartile:",
                              c("Q1 (First 25% of Catch)" = "Q1",
                                "Q2 (25-50% of Catch)" = "Q2",
                                "Q3 (50-75% of Catch)" = "Q3",
                                "Q4 (Last 25% of Catch)" = "Q4",
                                "All" = "All")),
                  selectInput("cpue_map_type", "Map Type:",
                              c("Production per km" = "per_km",
                                "Raw Production" = "raw_prod",
                                "Tributary Map" = "trib"))
                ),
                box(
                  title = "CPUE Quartile Analysis",
                  width = 9,
                  status = "primary",
                  plotOutput("cpueMapPlot", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "CPUE Distribution",
                  width = 12,
                  status = "info",
                  plotOutput("cpueDistPlot", height = "300px")
                )
              )
      ),
      
      # Cumulative Analysis tab
      tabItem(tabName = "cumulative",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  status = "primary",
                  selectInput("cum_watershed", "Watershed:",
                              c("Yukon" = "Yukon",
                                "Kuskokwim" = "Kusko")),
                  selectInput("cum_year", "Year:",
                              c("2015" = "2015",
                                "2016" = "2016")),
                  selectInput("cum_type", "Analysis Type:",
                              c("DOY Cumulative" = "DOY",
                                "CPUE Cumulative" = "CPUE")),
                  selectInput("cum_quartile", "Quartile:",
                              c("Q1 (25%)" = "Q1",
                                "Q2 (50%)" = "Q2",
                                "Q3 (75%)" = "Q3",
                                "Q4 (100%)" = "Q4")),
                  selectInput("cum_map_type", "Map Type:",
                              c("Production per km" = "per_km",
                                "Raw Production" = "raw_prod",
                                "Tributary Map" = "trib"))
                ),
                box(
                  title = "Cumulative Analysis",
                  width = 9,
                  status = "primary",
                  plotOutput("cumMapPlot", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Progression Dashboard",
                  width = 12, 
                  status = "info",
                  plotOutput("dashboardPlot", height = "400px")
                )
              )
      ),
      
      # Comparison tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  status = "primary",
                  selectInput("comp_watershed", "Watershed:",
                              c("Yukon" = "Yukon",
                                "Kuskokwim" = "Kusko")),
                  selectInput("comp_type", "Comparison Type:",
                              c("Annual Maps" = "annual",
                                "DOY Quartiles" = "doy",
                                "CPUE Quartiles" = "cpue",
                                "DOY Cumulative" = "doy_cumulative",
                                "CPUE Cumulative" = "cpue_cumulative")),
                  conditionalPanel(
                    condition = "input.comp_type == 'doy' || input.comp_type == 'cpue' || input.comp_type == 'doy_cumulative' || input.comp_type == 'cpue_cumulative'",
                    selectInput("comp_quartile", "Quartile:",
                                c("Q1" = "Q1",
                                  "Q2" = "Q2",
                                  "Q3" = "Q3",
                                  "Q4" = "Q4"))
                  ),
                  selectInput("comp_map_type", "Map Type:",
                              c("Production per km" = "per_km",
                                "Raw Production" = "raw_prod",
                                "Tributary Map" = "trib"))
                ),
                box(
                  title = "2015",
                  width = 4.5,
                  status = "primary",
                  plotOutput("compPlot2015", height = "500px")
                ),
                box(
                  title = "2016",
                  width = 4.5,
                  status = "primary",
                  plotOutput("compPlot2016", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Year-to-Year Change",
                  width = 12,
                  status = "info",
                  plotOutput("changePlot", height = "300px")
                )
              )
      ),
      
      # Data Table tab
      tabItem(tabName = "data_table",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  status = "primary",
                  selectInput("table_watershed", "Watershed:",
                              c("Yukon" = "Yukon",
                                "Kuskokwim" = "Kusko")),
                  selectInput("table_year", "Year:",
                              c("2015" = "2015",
                                "2016" = "2016")),
                  selectInput("table_type", "Data Type:",
                              c("HUC Summary" = "huc",
                                "Stream Summary" = "stream",
                                "Raw Sample Data" = "samples"))
                ),
                box(
                  title = "Data Table",
                  width = 9,
                  status = "primary",
                  DTOutput("dataTable")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Helper function to get image paths based on user selections
  get_image_path <- function(watershed, year, type, map_type, quartile = NULL, cumulative = FALSE) {
    # Set base directory
    base_dir <- here("Basin Maps")
    
    # Set stream order value based on watershed
    stream_order <- ifelse(watershed == "Yukon", "5", "3")
    
    # Generate file path based on selections
    if (type == "basin") {
      if (map_type == "per_km") {
        return(file.path(base_dir, "Annual_Maps/HUC", 
                         paste0(year, "_", watershed, "_HUC8_0.7_StrOrd", stream_order, "_.pdf")))
      } else if (map_type == "raw_prod") {
        return(file.path(base_dir, "Annual_Maps/HUC/RawProduction", 
                         paste0(year, "_", watershed, "_RawProd_HUC8_0.7_StrOrd", stream_order, "_.pdf")))
      } else if (map_type == "trib") {
        return(file.path(base_dir, "Annual_Maps/Tribs", 
                         paste0(year, "_", watershed, "_0.7_StrOrd", stream_order, "_.pdf")))
      }
    } else if (type == "doy") {
      if (quartile == "All") {
        # This would be a composite image in practice
        return(file.path(base_dir, "DOY_Quartile/Overview", 
                         paste0(year, "_", watershed, "_DOY_Overview.png")))
      } else {
        if (map_type == "per_km") {
          return(file.path(base_dir, "DOY_Quartile/HUC", 
                           paste0(watershed, "_", year, "_DOY_", quartile, "_HUC8_.pdf")))
        } else if (map_type == "raw_prod") {
          return(file.path(base_dir, "DOY_Quartile/HUC/RawProduction", 
                           paste0(watershed, "_", year, "_DOY_", quartile, "_RawProd_HUC8_.pdf")))
        } else if (map_type == "trib") {
          return(file.path(base_dir, "DOY_Quartile/Tribs", 
                           paste0(watershed, "_", year, "_DOY_", quartile, "_.pdf")))
        }
      }
    } else if (type == "cpue") {
      if (quartile == "All") {
        # This would be a composite image in practice
        return(file.path(base_dir, "Quartile_Maps/Overview", 
                         paste0(year, "_", watershed, "_CPUE_Overview.png")))
      } else {
        if (map_type == "per_km") {
          return(file.path(base_dir, "Quartile_Maps/HUC", 
                           paste0(watershed, "_", year, "_CPUE_", quartile, "_HUC8_.pdf")))
        } else if (map_type == "raw_prod") {
          return(file.path(base_dir, "Quartile_Maps/HUC/RawProduction", 
                           paste0(watershed, "_", year, "_CPUE_", quartile, "_RawProd_HUC8_.pdf")))
        } else if (map_type == "trib") {
          return(file.path(base_dir, "Quartile_Maps/Tribs", 
                           paste0(watershed, "_", year, "_CPUE_", quartile, "_.pdf")))
        }
      }
    } else if (type == "doy_cumulative") {
      if (map_type == "per_km") {
        return(file.path(base_dir, "DOY_Cumulative/HUC", 
                         paste0(watershed, "_", year, "_CumulativeDOY_", quartile, "_HUC8_.pdf")))
      } else if (map_type == "raw_prod") {
        return(file.path(base_dir, "DOY_Cumulative/HUC/RawProduction", 
                         paste0(watershed, "_", year, "_CumulativeDOY_", quartile, "_RawProd_HUC8_.pdf")))
      } else if (map_type == "trib") {
        return(file.path(base_dir, "DOY_Cumulative/Tribs", 
                         paste0(watershed, "_", year, "_CumulativeDOY_", quartile, "_.pdf")))
      }
    } else if (type == "cpue_cumulative") {
      if (map_type == "per_km") {
        return(file.path(base_dir, "CPUE_Cumulative/HUC", 
                         paste0(watershed, "_", year, "_CumulativeCPUE_", quartile, "_HUC8_.pdf")))
      } else if (map_type == "raw_prod") {
        return(file.path(base_dir, "CPUE_Cumulative/HUC/RawProduction", 
                         paste0(watershed, "_", year, "_CumulativeCPUE_", quartile, "_RawProd_HUC8_.pdf")))
      } else if (map_type == "trib") {
        return(file.path(base_dir, "CPUE_Cumulative/Tribs", 
                         paste0(watershed, "_", year, "_CumulativeCPUE_", quartile, "_.pdf")))
      }
    }
    
    # Default return if no match
    return(NULL)
  }
  
  # Create dummy data for sample visualization
  # In a real app, you would load actual data from files
  
  # Create synthetic HUC data
  create_huc_data <- function(watershed, year) {
    hucs <- c("18010101", "18010102", "18010103", "18010104", "18010105", 
              "18010106", "18010107", "18010108", "18010109", "18010110")
    names <- c("Upper Basin", "Middle Fork", "North Branch", "South Valley",
               "Western Ridge", "Eastern Plains", "Lower Basin", "Coastal Plain",
               "Delta Region", "Estuary Zone")
    
    # Add randomness based on watershed and year
    set.seed(ifelse(watershed == "Yukon", 1, 2) + ifelse(year == "2015", 0, 10))
    
    tibble(
      HUC8 = hucs,
      Name = names,
      total_production = runif(10, 0.5, 10),
      production_proportion = total_production / sum(total_production),
      total_stream_length = runif(10, 5000, 50000),
      production_per_meter = total_production / total_stream_length,
      production_per_meter_norm = production_per_meter / max(production_per_meter),
      year = year,
      watershed = watershed
    )
  }
  
  # Create synthetic sample data
  create_sample_data <- function(watershed, year) {
    # Add randomness based on watershed and year
    set.seed(ifelse(watershed == "Yukon", 1, 2) + ifelse(year == "2015", 0, 10))
    
    n <- 200
    
    tibble(
      sample_id = paste0(watershed, "_", year, "_", sprintf("%03d", 1:n)),
      DOY = sample(170:230, n, replace = TRUE),
      natal_iso = rnorm(n, mean = -25, sd = 3),
      dailyCPUEprop = runif(n, 0, 0.02),
      COratio = runif(n, 0.5, 1.5),
      year = year,
      watershed = watershed
    )
  }
  
  # Reactive expression to get the appropriate data based on user selections
  getData <- reactive({
    watershed <- input$table_watershed
    year <- input$table_year
    type <- input$table_type
    
    if (type == "huc") {
      return(create_huc_data(watershed, year))
    } else if (type == "samples") {
      return(create_sample_data(watershed, year))
    } else {
      # Stream data would be more complex in reality
      return(tibble(
        stream_id = paste0("S", 1:20),
        Str_Order = sample(3:8, 20, replace = TRUE),
        stream_length_m = runif(20, 1000, 20000),
        basin_assign_norm = runif(20, 0, 1),
        year = year,
        watershed = watershed
      ))
    }
  })
  
  # Overview plot
  output$overviewPlot <- renderPlot({
    # Create a summary of samples by watershed and year
    data <- bind_rows(
      create_sample_data("Yukon", "2015"),
      create_sample_data("Yukon", "2016"),
      create_sample_data("Kusko", "2015"),
      create_sample_data("Kusko", "2016")
    ) %>%
      group_by(watershed, year) %>%
      summarize(
        samples = n(),
        avg_cpue = mean(dailyCPUEprop),
        .groups = "drop"
      )
    
    ggplot(data, aes(x = watershed, y = samples, fill = year)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = samples), position = position_dodge(width = 0.9), vjust = -0.5) +
      labs(title = "Sample Counts by Watershed and Year",
           x = "Watershed", y = "Number of Samples") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  })
  
  # Basin Map plot
  output$basinMapPlot <- renderPlot({
    # In a real app, you would load the actual figure
    # For now, we'll create a placeholder
    path <- get_image_path(
      input$basin_watershed, 
      input$basin_year, 
      "basin", 
      input$basin_map_type
    )
    
    # Create a placeholder ggplot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Basin Map\n", 
                             "Watershed:", input$basin_watershed, "\n",
                             "Year:", input$basin_year, "\n",
                             "Map Type:", input$basin_map_type, "\n\n",
                             "File Path:", path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightblue", color = NA))
  })
  
  # DOY Map plot
  output$doyMapPlot <- renderPlot({
    path <- get_image_path(
      input$doy_watershed, 
      input$doy_year, 
      "doy", 
      input$doy_map_type,
      input$doy_quartile
    )
    
    # Create a placeholder ggplot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("DOY Quartile Map\n", 
                             "Watershed:", input$doy_watershed, "\n",
                             "Year:", input$doy_year, "\n",
                             "Quartile:", input$doy_quartile, "\n",
                             "Map Type:", input$doy_map_type, "\n\n",
                             "File Path:", path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightgreen", color = NA))
  })
  
  # DOY Distribution plot
  output$doyDistPlot <- renderPlot({
    # Get sample data for the selected watershed and year
    data <- create_sample_data(input$doy_watershed, input$doy_year)
    
    # Calculate quartile boundaries
    quartiles <- quantile(data$DOY, probs = c(0.25, 0.5, 0.75))
    
    # Plot the distribution with quartile lines
    ggplot(data, aes(x = DOY, y = dailyCPUEprop)) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = 0, ymax = dailyCPUEprop), alpha = 0.3, fill = "steelblue") +
      geom_vline(xintercept = quartiles, linetype = "dashed", color = "red") +
      geom_text(aes(x = quartiles, y = max(dailyCPUEprop) * 0.8, 
                    label = c("Q1|Q2", "Q2|Q3", "Q3|Q4")),
                nudge_x = 2, color = "red") +
      labs(title = paste("DOY Distribution for", input$doy_watershed, "River", input$doy_year),
           x = "Day of Year", y = "Daily CPUE Proportion") +
      theme_minimal()
  })
  
  # CPUE Map plot
  output$cpueMapPlot <- renderPlot({
    path <- get_image_path(
      input$cpue_watershed, 
      input$cpue_year, 
      "cpue", 
      input$cpue_map_type,
      input$cpue_quartile
    )
    
    # Create a placeholder ggplot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("CPUE Quartile Map\n", 
                             "Watershed:", input$cpue_watershed, "\n",
                             "Year:", input$cpue_year, "\n",
                             "Quartile:", input$cpue_quartile, "\n",
                             "Map Type:", input$cpue_map_type, "\n\n",
                             "File Path:", path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightyellow", color = NA))
  })
  
  # CPUE Distribution plot
  output$cpueDistPlot <- renderPlot({
    # Get sample data for the selected watershed and year
    data <- create_sample_data(input$cpue_watershed, input$cpue_year) %>%
      arrange(DOY) %>%
      mutate(cumulative_cpue = cumsum(dailyCPUEprop) / sum(dailyCPUEprop))
    
    # Find DOY values at CPUE quartiles
    cpue_quartile_doys <- approx(data$cumulative_cpue, data$DOY, 
                                 xout = c(0.25, 0.5, 0.75))$y
    
    # Plot the distribution with quartile lines
    ggplot() +
      geom_line(data = data, aes(x = DOY, y = dailyCPUEprop), size = 1.2) +
      geom_ribbon(data = data, aes(x = DOY, ymin = 0, ymax = dailyCPUEprop), 
                  alpha = 0.3, fill = "orange") +
      geom_line(data = data, aes(x = DOY, y = cumulative_cpue/5), 
                color = "blue", size = 1.2) +
      geom_vline(xintercept = cpue_quartile_doys, linetype = "dashed", color = "red") +
      geom_text(aes(x = cpue_quartile_doys, y = max(data$dailyCPUEprop) * 0.8, 
                    label = c("25%", "50%", "75%")),
                nudge_x = 2, color = "red") +
      scale_y_continuous(
        name = "Daily CPUE Proportion",
        sec.axis = sec_axis(~.*5, name = "Cumulative CPUE Proportion")
      ) +
      labs(title = paste("CPUE Distribution for", input$cpue_watershed, "River", input$cpue_year),
           x = "Day of Year") +
      theme_minimal() +
      theme(
        axis.title.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "blue")
      )
  })
  
  # Cumulative Map plot
  output$cumMapPlot <- renderPlot({
    cum_type <- ifelse(input$cum_type == "DOY", "doy_cumulative", "cpue_cumulative")
    
    path <- get_image_path(
      input$cum_watershed, 
      input$cum_year, 
      cum_type, 
      input$cum_map_type,
      input$cum_quartile
    )
    
    # Create a placeholder ggplot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Cumulative Analysis Map\n", 
                             "Watershed:", input$cum_watershed, "\n",
                             "Year:", input$cum_year, "\n",
                             "Analysis Type:", input$cum_type, "\n",
                             "Quartile:", input$cum_quartile, "\n",
                             "Map Type:", input$cum_map_type, "\n\n",
                             "File Path:", path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "lavender", color = NA))
  })
  
  # Dashboard plot
  output$dashboardPlot <- renderPlot({
    # Create a placeholder for the dashboard plot
    # In a real app, this would show a multi-panel visualization
    progression_path <- file.path(
      here("Basin Maps/Cumulative_Dashboards"),
      paste0(input$cum_watershed, "_", input$cum_year, "_Cumulative_Dashboard.pdf")
    )
    
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Cumulative Progression Dashboard\n", 
                             "Watershed:", input$cum_watershed, "\n",
                             "Year:", input$cum_year, "\n",
                             "Analysis Type:", input$cum_type, "\n\n",
                             "Dashboard Path:", progression_path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightpink", color = NA))
  })
  
  # Comparison plots
  output$compPlot2015 <- renderPlot({
    # Determine comparison type
    comp_type <- switch(input$comp_type,
                        "annual" = "basin",
                        "doy" = "doy",
                        "cpue" = "cpue",
                        "doy_cumulative" = "doy_cumulative",
                        "cpue_cumulative" = "cpue_cumulative")
    
    quartile <- if (input$comp_type %in% c("doy", "cpue", "doy_cumulative", "cpue_cumulative")) {
      input$comp_quartile
    } else {
      NULL
    }
    path <- get_image_path(
      input$comp_watershed, 
      "2015", 
      comp_type, 
      input$comp_map_type,
      quartile
    )
    
    # Create a placeholder ggplot for 2015 data
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("2015 Comparison Plot\n", 
                             "Watershed:", input$comp_watershed, "\n",
                             "Analysis Type:", input$comp_type, "\n",
                             if (!is.null(quartile)) paste("Quartile:", quartile, "\n") else "",
                             "Map Type:", input$comp_map_type, "\n\n",
                             "File Path:", path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightcyan", color = NA))
  })
  
  output$compPlot2016 <- renderPlot({
    # Determine comparison type
    comp_type <- switch(input$comp_type,
                        "annual" = "basin",
                        "doy" = "doy",
                        "cpue" = "cpue",
                        "doy_cumulative" = "doy_cumulative",
                        "cpue_cumulative" = "cpue_cumulative")
    
    quartile <- if (input$comp_type %in% c("doy", "cpue", "doy_cumulative", "cpue_cumulative")) {
      input$comp_quartile
    } else {
      NULL
    }
    
    path <- get_image_path(
      input$comp_watershed, 
      "2016", 
      comp_type, 
      input$comp_map_type,
      quartile
    )
    
    # Create a placeholder ggplot for 2016 data
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("2016 Comparison Plot\n", 
                             "Watershed:", input$comp_watershed, "\n",
                             "Analysis Type:", input$comp_type, "\n",
                             if (!is.null(quartile)) paste("Quartile:", quartile, "\n") else "",
                             "Map Type:", input$comp_map_type, "\n\n",
                             "File Path:", path),
               size = 6) +
      theme_void() +
      theme(panel.background = element_rect(fill = "mistyrose", color = NA))
  })
  
  # Change plot
  output$changePlot <- renderPlot({
    # In a real app, you would load HUC data for both years and calculate differences
    # For now, create synthetic data for demonstration
    
    huc_2015 <- create_huc_data(input$comp_watershed, "2015")
    huc_2016 <- create_huc_data(input$comp_watershed, "2016")
    
    # Calculate change metrics
    huc_change <- left_join(huc_2016, huc_2015, 
                            by = c("HUC8", "Name"),
                            suffix = c("_2016", "_2015")) %>%
      mutate(
        production_change = production_proportion_2016 - production_proportion_2015,
        intensity_change = production_per_meter_norm_2016 - production_per_meter_norm_2015
      ) %>%
      select(HUC8, Name, production_change, intensity_change) %>%
      pivot_longer(cols = c(production_change, intensity_change),
                   names_to = "metric", values_to = "change")
    
    # Plot the change
    ggplot(huc_change, aes(x = reorder(Name, change), y = change, fill = metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      scale_fill_brewer(palette = "Set1", 
                        labels = c("Production Proportion Change", "Production Intensity Change")) +
      labs(title = paste("Year-to-Year Change (2015-2016) for", input$comp_watershed, "River"),
           x = "HUC Name", y = "Change", fill = "Metric") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Data table
  output$dataTable <- renderDT({
    data <- getData()
    
    # Format columns based on data type
    if (input$table_type == "huc") {
      data <- data %>%
        mutate(
          production_proportion = scales::percent(production_proportion, accuracy = 0.1),
          production_per_meter_norm = scales::percent(production_per_meter_norm, accuracy = 0.1)
        )
    } else if (input$table_type == "samples") {
      data <- data %>%
        mutate(
          dailyCPUEprop = scales::percent(dailyCPUEprop, accuracy = 0.01)
        )
    } else {
      data <- data %>%
        mutate(
          basin_assign_norm = scales::percent(basin_assign_norm, accuracy = 0.1)
        )
    }
    
    # Create interactive table with searching, paging, etc.
    datatable(data, options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE
    ))
  })
}

# Run the app
shinyApp(ui, server)
