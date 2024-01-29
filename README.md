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


## `R/`CPUE

The `CPUE` R script calculates weights for each fish in natal origin based on the day of the year they were caught and the proportional CPUE for that time strata. It processes CPUE values, summarizes fish catch on each day of the year, calculates daily and otolith proportions, and assigns weights to each stratum. The script provides a flexible way to analyze CPUE data for different years and locations.

### Inputs

- **CPUE Data:** The script expects CPUE data in CSV format with columns including "Date" and "CPUE" for each day.
- **Natal Values:** Natal origin data containing information about fish, including the day of the year they were caught.

### Outputs

1. **Weighting Vector CSV:** The script generates a CSV file containing weights assigned to each fish based on the calculated stratum weights.
    - *File Name:* `{identifier}_CPUE_weights.csv`
    - *Location:* `data/CPUE/CPUE_weights/`

2. **Expanded CPUE Data CSV:** The script exports the CPUE data with additional columns, including daily proportions, otolith proportions, and cumulative proportions.
    - *File Name:* `{identifier}_expanded_CPUE.csv`
    - *Location:* `data/CPUE/`

3. **Combined CPUE Data CSV:** The script combines expanded CPUE data from different years into a single CSV file.
    - *File Name:* `Yukon_all_exp_CPUE.csv`
    - *Location:* `data/CPUE/`

### Usage

1. Adjust the paths for CPUE and natal origin data for the desired year and location.
2. Run the script to calculate weights and generate output files.
3. Explore the generated CSV files for CPUE weights and expanded CPUE data.



