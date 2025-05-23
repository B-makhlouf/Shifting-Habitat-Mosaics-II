# combine_quartile_maps.R
# Script to combine all non-annual maps into 4-panel figures based on their split
# Includes improved organization with subfolders by type
# Updated to handle 5 maps in a 2x3 layout

library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(here)
library(dplyr)
library(stringr)
library(purrr)

#' Create 4-panel figures for quartile maps across years
#'
#' @param watershed Character: "Kusko" or "Yukon"
#' @param analysis_type Character: "DOY" or "CPUE"
#' @param quartile Character: "Q1", "Q2", "Q3", or "Q4"
#' @param map_type Character: "HUC", "RawProduction", or "Tribs"
#' @param cumulative Logical: whether to use cumulative maps
#' @param output_dir Base directory to save output files
#' @return Path to the created composite figure
create_quartile_composite <- function(watershed, 
                                      analysis_type = "DOY", 
                                      quartile = "Q1",
                                      map_type = "HUC",
                                      cumulative = FALSE,
                                      output_dir = here("Basin Maps/Composite_Figures")) {
  
  # Create specific output subdirectory based on categories
  subfolder <- paste0(
    watershed, "/",
    ifelse(cumulative, "Cumulative_", ""),
    analysis_type, "/",
    map_type
  )
  
  specific_output_dir <- file.path(output_dir, subfolder)
  dir.create(specific_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Determine base directory based on analysis type and cumulative flag
  if (cumulative) {
    if (analysis_type == "DOY") {
      base_dir <- here("Basin Maps/DOY_Cumulative")
    } else {
      base_dir <- here("Basin Maps/CPUE_Cumulative")
    }
    file_pattern <- paste0("^", watershed, "_\\d{4}_Cumulative", analysis_type, "_", quartile)
  } else {
    if (analysis_type == "DOY") {
      base_dir <- here("Basin Maps/DOY_Quartile")
    } else {
      base_dir <- here("Basin Maps/Quartile_Maps")
    }
    file_pattern <- paste0("^", watershed, "_\\d{4}_", analysis_type, "_", quartile)
  }
  
  # Determine specific subdirectory based on map type
  if (map_type == "HUC") {
    search_dir <- file.path(base_dir, "HUC")
  } else if (map_type == "RawProduction") {
    search_dir <- file.path(base_dir, "HUC/RawProduction")
  } else if (map_type == "Tribs") {
    search_dir <- file.path(base_dir, "Tribs")
  } else {
    stop("Invalid map_type: must be 'HUC', 'RawProduction', or 'Tribs'")
  }
  
  # Find all matching PNG files
  png_files <- list.files(search_dir, pattern = paste0(file_pattern, ".*\\.png$"), 
                          full.names = TRUE)
  
  # Extract years from filenames
  years <- png_files %>%
    basename() %>%
    str_extract("\\d{4}") %>%
    as.numeric() %>%
    sort()
  
  # Order files by year
  ordered_files <- character(0)
  for (year in years) {
    year_pattern <- paste0("_", year, "_")
    year_files <- png_files[grepl(year_pattern, png_files)]
    ordered_files <- c(ordered_files, year_files)
  }
  
  # Check if we have files to process
  if (length(ordered_files) == 0) {
    message(paste("No matching files found for", watershed, analysis_type, quartile, map_type))
    return(NULL)
  }
  
  message(paste("Found", length(ordered_files), "maps to combine:", 
                paste(basename(ordered_files), collapse=", ")))
  
  # Read PNG files
  png_list <- lapply(ordered_files, function(file) {
    tryCatch({
      img <- readPNG(file)
      grid::rasterGrob(img, interpolate = TRUE)
    }, error = function(e) {
      message(paste("Error reading file:", file, e$message))
      NULL
    })
  })
  
  # Filter out any NULL values (failed reads)
  png_list <- png_list[!sapply(png_list, is.null)]
  
  # Create the composite figure
  if (length(png_list) > 0) {
    # Determine the layout based on number of files
    # Special handling for 5 maps: 2x3 layout with 3 on top, 2 on bottom
    if (length(png_list) == 5) {
      ncols <- 3
      nrows <- 2
    } else if (length(png_list) <= 4) {
      ncols <- length(png_list)
      nrows <- 1
    } else {
      ncols <- 4
      nrows <- ceiling(length(png_list) / 4)
    }
    
    # Create output filename
    output_filename <- paste0(quartile, "_Composite.png")
    output_path <- file.path(specific_output_dir, output_filename)
    
    # Create composite figure
    png(output_path, width = 12 * ncols, height = 10 * nrows, units = "in", res = 150)
    
    grid.newpage()
    
    # Create a title
    title_text <- paste(watershed, "Watershed -", 
                        ifelse(cumulative, "Cumulative ", ""),
                        analysis_type, "Quartile", quartile, 
                        "-", map_type, "Maps Across Years")
    
    # Set up viewport with space for title
    pushViewport(viewport(layout = grid.layout(nrows + 1, ncols, heights = unit(c(0.05, rep(0.95/nrows, nrows)), "npc"))))
    
    # Add title
    grid.text(title_text, gp = gpar(fontsize = 24, fontface = "bold"), 
              vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncols))
    
    # Add individual maps
    for (i in 1:length(png_list)) {
      # Calculate row and column indices
      if (length(png_list) == 5) {
        # Special layout for 5 maps: 3 on top row, 2 on bottom
        if (i <= 3) {
          row_idx <- 1
          col_idx <- i
        } else {
          row_idx <- 2
          # Center the bottom 2 maps by starting at column 1 with offset
          col_idx <- i - 3
        }
      } else {
        # Standard layout for other numbers
        row_idx <- ceiling(i / ncols)
        col_idx <- ((i - 1) %% ncols) + 1
      }
      
      # Create a subtitle with year
      year_text <- paste("Year:", years[i])
      
      # Create a viewport for the map and its subtitle
      map_vp <- viewport(layout.pos.row = row_idx + 1, layout.pos.col = col_idx)
      pushViewport(map_vp)
      
      # Draw the map
      grid.draw(png_list[[i]])
      
      # Add year text at the top of each panel
      grid.text(year_text, y = 0.97, gp = gpar(fontsize = 18, fontface = "bold"))
      
      popViewport()
    }
    
    dev.off()
    
    message(paste("Created composite figure:", output_path))
    return(output_path)
  } else {
    message("No images could be successfully read")
    return(NULL)
  }
}

#' Process all combinations of quartile maps
#'
#' @param watersheds Vector of watersheds to process
#' @param analysis_types Vector of analysis types
#' @param quartiles Vector of quartiles to process
#' @param map_types Vector of map types to process
#' @param include_cumulative Whether to include cumulative maps
#' @return List of paths to created composite figures
process_all_quartile_maps <- function(watersheds = c("Kusko", "Yukon"),
                                      analysis_types = c("DOY", "CPUE"),
                                      quartiles = c("Q1", "Q2", "Q3", "Q4"),
                                      map_types = c("HUC", "RawProduction", "Tribs"),
                                      include_cumulative = TRUE) {
  
  # Create a list to store paths of created figures
  created_figures <- list()
  
  # Initialize counters for reporting
  total_attempts <- 0
  successful_creations <- 0
  
  # Process all combinations
  for (watershed in watersheds) {
    for (analysis_type in analysis_types) {
      for (quartile in quartiles) {
        for (map_type in map_types) {
          total_attempts <- total_attempts + 1
          
          # Process regular quartile maps
          figure_path <- create_quartile_composite(
            watershed = watershed,
            analysis_type = analysis_type,
            quartile = quartile,
            map_type = map_type,
            cumulative = FALSE
          )
          
          if (!is.null(figure_path)) {
            created_figures <- c(created_figures, figure_path)
            successful_creations <- successful_creations + 1
          }
          
          # Process cumulative maps if requested
          if (include_cumulative) {
            total_attempts <- total_attempts + 1
            
            figure_path <- create_quartile_composite(
              watershed = watershed,
              analysis_type = analysis_type,
              quartile = quartile,
              map_type = map_type,
              cumulative = TRUE
            )
            
            if (!is.null(figure_path)) {
              created_figures <- c(created_figures, figure_path)
              successful_creations <- successful_creations + 1
            }
          }
        }
      }
    }
  }
  
  message(paste("Created", successful_creations, "out of", total_attempts, "possible composite figures"))
  return(created_figures)
}

#' Create an index HTML file of all composite figures
#'
#' @param output_dir Base directory where composite figures are stored
#' @return Path to the created HTML file
create_composite_index <- function(output_dir = here("Basin Maps/Composite_Figures")) {
  # Get all PNG files in the directory and subdirectories
  all_pngs <- list.files(output_dir, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
  
  if (length(all_pngs) == 0) {
    message("No composite figures found to index")
    return(NULL)
  }
  
  # Create relative paths for the HTML file
  rel_paths <- gsub(paste0(output_dir, "/"), "", all_pngs)
  
  # Create HTML content
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <title>Watershed Analysis Composite Figures</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; }",
    "    h1, h2, h3 { color: #2c3e50; }",
    "    .figure-container { margin-bottom: 40px; }",
    "    .figure-link { display: block; margin: 10px 0; }",
    "    .watershed-section { margin-bottom: 30px; border-bottom: 1px solid #eee; padding-bottom: 20px; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <h1>Watershed Analysis Composite Figures</h1>"
  )
  
  # Organize by watershed
  watersheds <- c("Kusko", "Yukon")
  
  for (watershed in watersheds) {
    html_content <- c(html_content, 
                      paste0("  <div class='watershed-section'>"),
                      paste0("    <h2>", watershed, " Watershed</h2>"))
    
    # Regular analysis types
    for (analysis_type in c("DOY", "CPUE")) {
      html_content <- c(html_content,
                        paste0("    <h3>", analysis_type, " Quartiles</h3>"),
                        "    <div class='figure-container'>")
      
      # Filter figures for this watershed and analysis type
      pattern <- paste0("^", watershed, "/", analysis_type, "/")
      matching_paths <- rel_paths[grepl(pattern, rel_paths)]
      
      if (length(matching_paths) > 0) {
        for (path in matching_paths) {
          # Extract map type and quartile for a more descriptive link text
          map_type <- str_extract(path, "(HUC|RawProduction|Tribs)")
          quartile <- str_extract(path, "Q[1-4]")
          
          link_text <- paste0(map_type, " - ", quartile)
          html_content <- c(html_content,
                            paste0("      <a class='figure-link' href='", path, "'>", link_text, "</a>"))
        }
      } else {
        html_content <- c(html_content, "      <p>No figures available</p>")
      }
      
      html_content <- c(html_content, "    </div>")
    }
    
    # Cumulative analysis types
    for (analysis_type in c("DOY", "CPUE")) {
      html_content <- c(html_content,
                        paste0("    <h3>Cumulative ", analysis_type, " Quartiles</h3>"),
                        "    <div class='figure-container'>")
      
      # Filter figures for this watershed and cumulative analysis type
      pattern <- paste0("^", watershed, "/Cumulative_", analysis_type, "/")
      matching_paths <- rel_paths[grepl(pattern, rel_paths)]
      
      if (length(matching_paths) > 0) {
        for (path in matching_paths) {
          map_type <- str_extract(path, "(HUC|RawProduction|Tribs)")
          quartile <- str_extract(path, "Q[1-4]")
          
          link_text <- paste0(map_type, " - ", quartile)
          html_content <- c(html_content,
                            paste0("      <a class='figure-link' href='", path, "'>", link_text, "</a>"))
        }
      } else {
        html_content <- c(html_content, "      <p>No figures available</p>")
      }
      
      html_content <- c(html_content, "    </div>")
    }
    
    html_content <- c(html_content, "  </div>")
  }
  
  # Close HTML
  html_content <- c(html_content,
                    "</body>",
                    "</html>")
  
  # Write HTML file
  html_path <- file.path(output_dir, "index.html")
  writeLines(html_content, html_path)
  
  message(paste("Created index HTML file at", html_path))
  return(html_path)
}

# Example usage:
# Generate all composite figures
process_all_quartile_maps()
# create_composite_index()

# Or, generate specific composite figures
# create_quartile_composite("Kusko", "DOY", "Q1", "HUC")
# create_quartile_composite("Yukon", "CPUE", "Q4", "Tribs")
# create_quartile_composite("Kusko", "DOY", "Q2", "RawProduction", cumulative = TRUE)