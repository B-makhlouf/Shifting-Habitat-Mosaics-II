# Shifting Habitat Mosaics II 

## Introduction

Pacific salmon in Western Alaska watersheds are comprised of multiple unique stocks, many of which display unique life history strategies such as run timing and location of natal origin. Management of the fishery is predicated on adjustments to harvest at the mouth of major tributaries, which aims to maintain genetic and life history diversity while managing allocation between multiple stakeholders and across international boundaries. To do so requires an understanding of how salmon subpopulations are distributed across the landscape, and how these patterns change in space and time. To date, however, our understanding of these patterns are limited by the spatial resolutin of current methods; namely Genetic Stock identification. However, variation within current management units may be substantial, and management for this diverity requires a more detailed understanding of the spatial distribution of salmon subpopulations. Sr8786 in salmon otolith has been illustrated to be useful to this point, as the relative ratio Sr87/Sr86 varies across the landscape and is faithfully recorded into the structure of the otolith ( Kennedy et al., 1997, Brennan et al., 2019). In the Nushagak River Basin, these methods have been used to reveal substantial variation in the distribution of salmon subpopulations across the landscape, and how these patterns change in space and time. To date, this has not been done in Alaska's largest watersheds; the Yukon and Kuskokwim River Basins. This project aims to explore how patterns of production change at multiple spatial scales in these river basins using Otolith Sr8786. 

# Watershed Analysis Tool


## Repository Structure
- **Main Scripts**
  - `main.R`: Primary execution script
  - `mapping.R`: Core mapping functionality
  - `assignment.R`: Bayesian assignment functions
  - `doy_analysis.R`: Day of Year quartile analysis
  - `cpue_analysis.R`: Catch Per Unit Effort analysis
  - `cumulative_quartile_analysis.R`: Cumulative analysis functions

- **Utilities**
  - `utils/spatial_utils.R`: Spatial data processing utilities
  - `utils/visualization.R`: Map and chart creation utilities

## Key Features
- Bayesian assignment based on isotope data
- HUC-based watershed mapping
- Temporal analysis by DOY and CPUE quartiles
- Cumulative run timing visualization
- Support for both Yukon and Kuskokwim watersheds

## Watershed Parameters
- **Yukon**: sensitivity=0.7, min_error=0.003, min_stream_order=5
- **Kuskokwim**: sensitivity=0.7, min_error=0.0006, min_stream_order=3

## Dependencies
`sf`, `dplyr`, `ggplot2`, `here`, `RColorBrewer`, `viridis`, `scales`, `grid`, `gridExtra`, `cowplot`