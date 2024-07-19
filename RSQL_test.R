#install.packages(c("dbplyr", "RSQLite"))
library(dplyr)
library(dbplyr)
library(RSQLite)

SMH2 <- DBI::dbConnect(RSQLite::SQLite(), "/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/SHM2.db")

test<- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Outputs/Assignment Matrix/2017_Kuskokwim_0.7.csv")

dbWriteTable(SMH2, "production_matrices", test, overwrite = TRUE)

dbListTables(SMH2)

dbListFields(SMH2, "production_matrices")
