#### Installing and loading required packages ####
rm(list=ls())

## Set the working directory to the path of where this script is 
## Ensure that the data and modules folders are also in the working directory
setwd("C:/Users/Joejyn/OneDrive/Camphora/Data_analysis/CT_processing_scripts_v1.0/")

source("modules/utils.R")
source("modules/extract_exif_timelapse.R")
install_load_packages(c("RSQLite", "tidyverse"))

#### Variable Control Panel ####
path_processed <- "C:/TempDataForSpeed/CT31_20230308/"

#### Copy and sort videos into Sorted based on Timelapse ID ####
cat("Sorting videos into Sorted folder based on Timelapse IDs.")

ddb_paths <- list.files(path_processed, recursive = T, 
                        pattern = "*.ddb", full.names = T)

ddb_paths <- ddb_paths[!grepl("Backups", ddb_paths)]

ddb_data <- lapply(ddb_paths, get_id_timelapse) %>%
  bind_rows() %>%
  mutate(SpeciesDir = paste(FolderSpeciesName, Quantity, sep = " "),
         SortedDir = file.path(FullStationPath, "Sorted", SpeciesDir, FileName))

copy_status <- list()
sorted_dir <- file.path(unique(ddb_data$FullStationPath), "Sorted")
if (!dir.exists(sorted_dir)) dir.create(sorted_dir)

pb <- txtProgressBar(min = 0, max = nrow(ddb_data), style = 3, char = "=") 

for (i in 1:nrow(ddb_data)){
  
  from <- ddb_data$FullPath[i]
  to <- ddb_data$SortedDir[i]
  
  if (!dir.exists(dirname(to))){
    dir.create(dirname(to))
  }
  
  copy_status[[i]] <- file.copy(from, to, copy.date = T)
  
  setTxtProgressBar(pb, i)
}
close(pb)

if (!any(unlist(copy_status))) {
  cat("WARNING: Some files fail to copy.")
} else
  cat("Sorting completed! :)")
