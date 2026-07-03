#### Installing and loading required packages ####
rm(list = ls())

## Set the working directory to the folder containing this script
setwd("C:/Users/Joejyn/OneDrive/Camphora/Data_analysis/CamphoraToolkitHub_Git/")

source("modules/utils.R")
source("CT_Step1_ExtractExif.R")
source("CT_Step2_MergeExifs.R")
source("CT_Step3_IndpDets.R")
source("modules/water_report.R")
source("modules/noise_report.R")

library(tools)


#### Fixed Variables ####
SPECIES_DB_PATH <- "data/Species_Database.xlsx"


#### CT Step 1: EXIF Extraction ####
## Uncomment and fill in paths before running
# path_processed <- "Z:/path/to/processed/station_folder"
# path_raw       <- "Z:/path/to/raw/station_folder"

extract_exif(
  path_processed        = path_processed,
  path_raw              = path_raw,
  path_species_database = SPECIES_DB_PATH
)


#### CT Step 2: Merge EXIFs ####
## Uncomment and fill in path before running
# path_exif_folder <- "Z:/path/to/folder/containing/exif_csvs"

merging_exifs(
  path_exif_folder      = path_exif_folder,
  path_species_database = SPECIES_DB_PATH,
  input_combined        = NA   # set to a file path string if using a manually edited combined CSV
)


#### CT Step 3: Independent Detections ####
## Uncomment and fill in path before running
# input_ct_file <- "Z:/path/to/combined_exif_all.csv"

indp_dets(
  input_ct_file         = input_ct_file,
  path_species_database = SPECIES_DB_PATH,
  indp_interval         = 3600,      # seconds; default 1 hour
  rm_stations           = NA         # e.g. c("S01", "S02") to exclude stations
)


#### Abiotic: Water Monitoring ####
## Uncomment and fill in path before running
# path_input <- "Z:/path/to/EXO2_export.csv"

time_threshold <- 2                          # minutes
date_format    <- "%d/%m/%Y %I:%M:%S %p"    # adjust if logger uses different format

in_situ(path_input, time_threshold, date_format)


#### Abiotic: Noise Monitoring ####
## Uncomment and fill in paths before running
# path_noise       <- "Z:/path/to/noise_data.csv"
# path_calibration <- "Z:/path/to/calibration.csv"

location      <- "Site Name"
monitoring_pt <- "N1"

noise_report(location, monitoring_pt, path_noise, path_calibration)
