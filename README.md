# Shifting Habitat Mosaics II

# Code 

## `R/`Yukon Map Fucntion

The `Yukon_map_function` R script generates basin-scale maps for the Yukon river basin, utilizing isoscape predictions and genetic data. To use the function, load Yukon river edges and basin shapefiles, then run `Yukon_map` with specified parameters like year, sensitivity threshold, and date filtering options. The function expects CSV files in the "Data" directory for genetics, natal origin, and CPUE weights. Output maps can be displayed interactively or saved as PDF files in the "Figures/Maps/Yukon" directory.

### Outputs

The `Yukon_map` script generates the following outputs:

1. **Basin scale production estimates:**
   - A CSV file containing basin-scale normalized probabilities is generated based on the specified parameters (year and sensitivity threshold). The file is saved in the "Data/Production/Yukon" directory.

3. **Map Output:**
   - Depending on the `plot_show` parameter:
     - If set to `TRUE`, an interactive map is displayed using base R graphics.
     - If set to `FALSE`, a PDF file is saved in the "Figures/Maps/Yukon" directory, featuring color-coded probabilities.


