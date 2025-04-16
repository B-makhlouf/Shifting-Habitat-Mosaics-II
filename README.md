# Shifting Habitat Mosaics II 

## Introduction

Pacific salmon in Western Alaska watersheds are comprised of multiple unique stocks, many of which display unique life history strategies such as run timing and location of natal origin. Management of the fishery is predicated on adjustments to harvest at the mouth of major tributaries, which aims to maintain genetic and life history diversity while managing allocation between multiple stakeholders and across international boundaries. To do so requires an understanding of how salmon subpopulations are distributed across the landscape, and how these patterns change in space and time. To date, however, our understanding of these patterns are limited by the spatial resolutin of current methods; namely Genetic Stock identification. However, variation within current management units may be substantial, and management for this diverity requires a more detailed understanding of the spatial distribution of salmon subpopulations. Sr8786 in salmon otolith has been illustrated to be useful to this point, as the relative ratio Sr87/Sr86 varies across the landscape and is faithfully recorded into the structure of the otolith ( Kennedy et al., 1997, Brennan et al., 2019). In the Nushagak River Basin, these methods have been used to reveal substantial variation in the distribution of salmon subpopulations across the landscape, and how these patterns change in space and time. To date, this has not been done in Alaska's largest watersheds; the Yukon and Kuskokwim River Basins. This project aims to explore how patterns of production change at multiple spatial scales in these river basins using Otolith Sr8786. 

# Data Structure 

### Natal Origin Data for Analysis 

Natal Origin data has been extracted in an external repository from raw LA-ICPMS data. To these data, the proportion of dailyCPUE has been added as well as the proportion of otoliths collected per each day of the year and the relative ratio of the two.

# From Natal Origin to Basin Maps 

Basin maps can be created from the script *Code/Generate Basin Maps*. This script clears current PDF files, makes sure the current list of available datasets is updated, and loops through them to produce

 a) Full year maps      
        - Tributary     
        - HUC Polygon Chloropleth maps 

b) Maps split by Quartile       
        - Tributary     
        - Huc Polygon Chloropleth maps 


To do so, this script relies on several hefty helper scripts listed below. 

## `All_Map` Function

Generates spatial maps and analyses of salmon natal origin probabilities across watersheds (Kusko or Yukon).

### Features
- **Input Parameters**:
  - `year`: Analysis year
  - `sensitivity_threshold`: Probability threshold
  - `min_error`: Minimum error threshold  
  - `min_stream_order`: Minimum stream order to include
  - `HUC`: Hydrologic unit level (8 or 10)
  - `return_values`: Option to return data instead of plots

- **Outputs**:
  - PDF maps showing relative production probabilities
  - HUC-level summaries of production per river kilometer
  - Stream network visualizations colored by probability
  - Optionally returns spatial data frames

- **Processing Steps**:
  1. Assigns probabilities to watershed segments
  2. Calculates basin-scale values
  3. Joins spatial data with HUC polygons
  4. Normalizes and visualizes results

- **Customization Options**:
  - Adjustable stream order filtering
  - Different HUC-level analyses (8 or 10)
  - Color-coded probability ranges
  - Automatic filename generation


*** Importantly, this script works for BOTH Kuskowkim and Yukon. 
### Usage Example
```r
All_Map(year = 2019, 
        sensitivity_threshold = 0.8,
        min_error = 0.1,
        min_stream_order = 3,
        HUC = 8)
```


This script in itself calls two mapping specific functions. As opposed to previous iterations which contained different scripts for each watershed, this gaurentees consistant style between all maps produced 

## `HUC_MAP` Function

Creates HUC-level maps and visualizations of salmon production probabilities.

### Features
- **Input Parameters**:
  - `rescaled_values`: Normalized probability values
  - `identifier`: Unique identifier for output files
  - `edges`: Stream network spatial data
  - `basin`: Watershed boundary polygon
  - `HUC`: Hydrologic unit level (8 or 10, default=8)

- **Outputs**:
  - PDF containing:
    - Choropleth map of normalized production per river km
    - Bar graph of relative production by HUC
  - Processes and returns spatial data with:
    - Total production per HUC
    - Stream length metrics
    - Normalized production values

- **Key Operations**:
  1. Spatial joins between streams and HUC polygons
  2. Stream length calculations
  3. Production metrics aggregation
  4. Data normalization (0-1 range)
  5. Automatic plot styling and theming

### Visualization Features
- Color gradient: Reds palette (Brewer)
- Normalized scale (0-1)
- Clean cartographic styling:
  - White borders
  - Grey background
  - Professional legend
- Adaptive bar graph:
  - Hides y-axis labels for HUC10
  - Coordinated flip layout

### Usage Example
```r
HUC_MAP(
  rescaled_values = basin_assign_norm,
  identifier = "YK_2019",
  edges = yukon_streams,
  basin = yukon_basin,
  HUC = 8
)
```

## `TRIB_MAP` Function

Generates tributary-level maps of salmon production probabilities with stream network visualization.

### Features
- **Input Parameters**:
  - `basin_assign_norm`: Normalized probability values (0-1)
  - `identifier`: Unique identifier for output files
  - `edges`: Stream network spatial data
  - `basin`: Watershed boundary polygon

- **Outputs**:
  - PDF map showing:
    - Color-coded stream segments by probability
    - Stream order-based line widths
    - Informative legend
    - Watershed context

- **Key Operations**:
  1. Filters streams by minimum order (`min_stream_order`)
  2. Assigns color codes based on probability bins:
     - ≥0.9: Dark red
     - 0.8-0.9: Red
     - 0.7-0.8: Orange-red
     - 0.6-0.7: Light orange
     - <0.6: Cream
  3. Sets line widths by stream order (3-9)
  4. Generates automatic title with metadata

- **Visual Features**:
  - Color scheme: YlOrRd (Brewer palette)
  - Dynamic line widths (thicker for higher stream orders)
  - Gray basin background
  - Clear probability legend
  - Metadata-rich title

### Usage Example
```r
TRIB_MAP(
  basin_assign_norm = normalized_probs,
  identifier = "Kusko_2020_Q3",
  edges = kusko_streams,
  basin = kusko_boundary
)
```

