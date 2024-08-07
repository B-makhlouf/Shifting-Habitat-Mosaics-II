library(DBI)
library(RSQLite)

################## Raw otolith LA Reads 

# Define paths
csv_dir <- "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/03_Extracted Data/Natal Origins/Cleaned"
db_path <- "/Users/benjaminmakhlouf/Desktop/Databases/AYK_data.db"

# Create a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), db_path)

dbExecute(con, "DELETE FROM `natal origins`;")

# Get list of all CSV files in the directory
csv_files <- list.files(path = csv_dir, pattern = "\\.csv$", full.names = TRUE)

# Loop through each file
for (csv_path in csv_files) {
  # Read the CSV file, skipping the header
  data <- read.csv(csv_path)
  # Append data to the existing "natal origins" table
  dbWriteTable(con, "natal origins", data, append = TRUE, row.names = FALSE, col.names = FALSE)
}

# Disconnect from the database
dbDisconnect(con)


