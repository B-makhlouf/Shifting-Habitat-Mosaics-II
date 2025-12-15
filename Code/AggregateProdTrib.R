# Load libraries
library(dplyr)
library(readr)

#------------------------------------------------------------------------------
# File paths
#------------------------------------------------------------------------------

tribcollect_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Data/UpstreamReaches/SameTrib/Kusko_UpstreamReaches_ByStreamOrder.csv"

prod_data_path <- "/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/AnnualProdData/Kusko/2017_Kusko_Assignment_Results.csv"

#------------------------------------------------------------------------------
# Read data
#------------------------------------------------------------------------------

tribcollect <- read_csv(tribcollect_path)
prod_data   <- read_csv(prod_data_path)

#------------------------------------------------------------------------------
# Attach tributary_group_id to each reach in production data
#------------------------------------------------------------------------------

prod_with_trib <- prod_data %>%
  left_join(
    tribcollect %>%
      select(
        upstream_reachid,
        tributary_group_id
      ),
    by = c("reachid" = "upstream_reachid")
  )

#------------------------------------------------------------------------------
# Aggregate production at the tributary level
#------------------------------------------------------------------------------

trib_production <- prod_with_trib %>%
  group_by(tributary_group_id) %>%
  summarise(
    trib_total_assignment_rescale = sum(assignment_rescale, na.rm = TRUE),
    trib_total_assignment_individuals = sum(assignment_individuals, na.rm = TRUE),
    n_reaches = n_distinct(reachid),
    .groups = "drop"
  )

#------------------------------------------------------------------------------
# Assign tributary-level production totals back to each reach
#------------------------------------------------------------------------------

prod_data_trib_level <- prod_with_trib %>%
  left_join(
    trib_production,
    by = "tributary_group_id"
  )








