# first, source in the code with the mapping functions 


if (T){

library(here)
source(here("Code/Map Functions Full.R"))
source(here("Code/Style_Map_Function.R"))
#source(here("Code/Check Dataset Completedness.R"))
source(here("Code/Map Functions QUARTILE.R"))

  
################################################################################
#Clear any pdfs from anywhere within Basin Maps

pdf_files <- list.files("/Users/benjaminmakhlouf/Research_repos/03_Shifting-Habitat-Mosaics-II/Basin Maps",
                        pattern = "\\.pdf$", 
                        recursive = TRUE,  # This looks in subfolders too
                        full.names = TRUE)

file.remove(pdf_files)

#### Then, make sure that the database is up to date. That is, the files in the repository for this manuscript 
# match the most recent version of these files in the database repository 

##############################################################################################################
source_dir <- ("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/03_Natal Origins Genetics CPUE")  # AYK database directory 
dest_dir <- here("Data/Natal Origin Data for analysis")   # This repo, location for the data for analysis 

csv_files <- list.files(source_dir, pattern = "\\.csv$", full.names = TRUE) # Get a list of all CSV files in the source directory
file.copy(from = csv_files, to = dest_dir, overwrite = TRUE) #copy over 

############################### 
## From that, list out all the datasets we have available for analysis. 
file_names <- list.files(dest_dir, pattern = "\\.csv$", full.names = FALSE)  # Get only file names
dataset_names <- unique(sub("^([a-zA-Z0-9]+_[a-zA-Z0-9]+)_.*\\.csv$", "\\1", file_names))  # Capture year and watershed before the second underscore

## remove 2017_Yukon, 2018_Yukon, and 2019_Yukon from the list of datasets. No complete coverage here... 
dataset_names <- dataset_names[!dataset_names %in% c("2017_Yukon", "2018_Yukon", "2019_Yukon")]


################################################################################################################
############ Here, we have a list of all the datasets available that we want to use 

### Go through each of them and map the full year! 

for (dataset in dataset_names) {
  
  parts <- strsplit(dataset, "_")[[1]]
  year <- parts[1]
  watershed <- parts[2]
  
  # plot_dataset_qc(year,watershed)
  
  if (watershed == "Yukon") {
    
    sensitivity_threshold <- 0.0001
    min_error <- 0.003
    min_stream_order <- 4
    filter_conditions <- NULL
    map_result <- All_Map(year, sensitivity_threshold, min_error, min_stream_order)
    map_result <- ALL_Map_func_Quartile(year, sensitivity_threshold, min_error, min_stream_order, filter_by = "DOY_Q", HUC = 8)
    

  } else if (watershed == "Kusko") {

    sensitivity_threshold <- 0.7
    min_error <- 0.0006
    min_stream_order <- 4
    filter_conditions <- NULL
    return_values<- FALSE
    map_result <- All_Map(year, sensitivity_threshold, min_error, min_stream_order)
    map_result <- ALL_Map_func_Quartile(year, sensitivity_threshold, min_error, min_stream_order, filter_by, HUC = 8)

  } 
  
  # remove sensitivity_threshold, min_error, min_stream_order, filter_conditions, return_values, map_result
  rm(sensitivity_threshold, min_error, min_stream_order, filter_conditions, return_values, map_result)
  
}


#################################################################################################################################################################

















# ################# 
# ## Now, do the same but make maps of the quartiles by CPUE and by DOY 
# 
# for (dataset in dataset_names) {
#   parts <- strsplit(dataset, "_")[[1]]
#   year <- parts[1]
#   watershed <- parts[2]
#   
#     if (watershed == "Yukon") {
#     # sensitivity_threshold <- 0.5
#     # min_error <- 0.003
#     # min_stream_order <- 4
#     # filter_conditions <- NULL
#     # map_result <- YK_Map_func_Quartile(year, sensitivity_threshold, min_error, min_stream_order)
# 
#   } else if (watershed == "Kusko") {
# 
#     sensitivity_threshold <- 0.7
#     min_error <- 0.0006
#     min_stream_order <- 4
#     filter_conditions <- NULL
#     map_result <- KK_Map_func_Quartile(year, sensitivity_threshold, min_error, min_stream_order, "DOY_Q") 
#   } 
# }
# 
# 
# ################################################################################# 
# # Generate a giant panel figure with all the Kusko years 
# 
# source(here("Code/4PanelConstruction.R"))

}








