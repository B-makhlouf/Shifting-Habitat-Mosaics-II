# Source both files
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Assignment.R")
source("/Users/benjaminmakhlouf/Research_repos/05_Shifting-Habitat-Mosaics-II/Code/Visualization.R")

# Run analysis for a single year
results <- run_annual_analysis(2017, "Kusko")

# Create map
create_annual_map(results, "/Users/benjaminmakhlouf/Desktop/Maps", 2017, "Kusko")

# Run for multiple years
for (year in c(2017, 2018, 2019, 2020, 2021, 2022)) {
  results <- run_annual_analysis(year, "Kusko")
  create_annual_map(results, "/Users/benjaminmakhlouf/Desktop/Maps", year, "Kusko")
}