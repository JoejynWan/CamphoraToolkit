#### Installing and loading required packages ####
rm(list = ls())

## Set the working directory to the folder containing this script
setwd("C:/Users/Joejyn/OneDrive/Camphora/Data_analysis/CamphoraToolkitHub_Git/")

source("apps/CameraTrapProcessing/modules/utils.R")
source("apps/CameraTrapProcessing/CT_Step1_ExtractExif.R")
source("apps/CameraTrapProcessing/CT_Step1.1_OffsetDateTime.R")
source("apps/CameraTrapProcessing/CT_Step2_MergeExifs.R")
source("apps/CameraTrapProcessing/CT_Step3_IndpDets.R")
source("apps/AbioticMonitoring/water_report.R")
source("apps/AbioticMonitoring/noise_report.R")
source("apps/ImpactAssessment/modules/utils.R")
source("apps/ImpactAssessment/impact_assessment.R")
source("apps/ArboReport/modules/utils.R")
source("apps/ArboReport/generate_report.R")
source("apps/ArboReport/resize_photos.R")

library(tools)


#### Fixed Variables ####
SPECIES_DB_PATH <- "apps/CameraTrapProcessing/data/Species_Database.xlsx"
IA_MATRIX_PATH  <- "apps/ImpactAssessment/data/ConsequenceSignificanceMatrix.xlsx"
ARBO_RMD_PATH   <- "apps/ArboReport/modules/arboreport_full.Rmd"


#### CT Step 1: EXIF Extraction ####
## Uncomment and fill in paths before running
# path_processed <- "Z:/path/to/processed/station_folder"
# path_raw       <- "Z:/path/to/raw/station_folder"

extract_exif(
  path_processed        = path_processed,
  path_raw              = path_raw,
  path_species_database = SPECIES_DB_PATH
)


#### CT Step 1.1: Offset DateTime ####
## Uncomment and fill in path before running
# exif_path <- "Z:/path/to/station_exif.csv"

## Either a number of hours (e.g. -12) or the correct DateTime of the first video
offset <- "2025-11-13 08:00:00"

offset_datetime(
  exif_path = exif_path,
  offset     = offset
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


#### Fauna Impact Assessment ####
## Uncomment and fill in paths before running
# path_species_list   <- "Z:/path/to/species_list.xlsx"
# path_fauna_database <- "Z:/path/to/Combined_Fauna_Database.xlsx"

output_path <- paste0(tools::file_path_sans_ext(path_species_list), "_output.xlsx")

run_impact_assessment(
  species_list_path   = path_species_list,
  fauna_database_path = path_fauna_database,
  matrix_path          = IA_MATRIX_PATH,
  output_path          = output_path
)


#### Arbo Report: Resize Photos ####
## Uncomment and fill in paths before running
# arbo_photo_dir          <- "Z:/path/to/original/photos"
# arbo_resized_photos_dir <- "Z:/path/to/resized/photos"

resize_arbo_photos(
  photo_dir          = arbo_photo_dir,
  resized_photos_dir = arbo_resized_photos_dir,
  photo_size          = 400
)


#### Arbo Report: Generate Report ####
path_arbo_biodata <- "C:/Users/Joejyn/Downloads/Holland_Arbo_v0_1-20.csv"

## [Optional] Set to NULL to generate the report without photos
arbo_resized_photos_dir <- NULL
arbo_photo_prefix <- "Holland Rd_Photos"

run_arbo_report(
  path_biodata        = path_arbo_biodata,
  rmd_path             = ARBO_RMD_PATH,
  output_dir           = "apps/ArboReport/results",
  resized_photos_dir   = arbo_resized_photos_dir,
  photo_prefix         = arbo_photo_prefix,
  report_size          = 100,      # trees per report
  select_ids           = NULL,     # e.g. c("12", "15", "20A") to select specific trees
  incl_crown_spread    = FALSE,
  sort_site            = FALSE,
  date_format          = "%d/%m/%Y"
)
