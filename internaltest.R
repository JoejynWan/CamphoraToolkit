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
source("apps/StreamInspection/stream_report.R")
source("apps/BatRecordingProcessing/modules/util.r")
source("apps/BatRecordingProcessing/modules/dup_rows.r")
source("apps/BatRecordingProcessing/modules/match_gps.r")
source("apps/BatRecordingProcessing/modules/sort_bat_data.r")
source("apps/BatRecordingProcessing/Step1_process_meta.R")
source("apps/BatRecordingProcessing/Step2_combine_meta.R")
source("apps/BatRecordingProcessing/subsample.R")
source("apps/BatRecordingProcessing/recover_meta.R")
source("apps/FloraPhotoFiling/modules/utils.R")
source("apps/FloraPhotoFiling/sort_photos.R")
source("apps/FloraPhotoFiling/resort_tag_dirs.R")
source("apps/CAGPhotoRenaming/rename_photos.R")

install_load_packages(c(
  "shiny", "shinyFiles", "fs", "bslib", "bsicons",
  "tidyverse", "openxlsx", "tools",
  "exifr", "zip", "batch", "vegan", "RSQLite", "parallel", "camtrapR",
  "rlang", "knitr", "rmarkdown", "magick", "pbapply"
))


#### Fixed Variables ####
SPECIES_DB_PATH     <- "apps/CameraTrapProcessing/data/Species_Database.xlsx"
IA_MATRIX_PATH      <- "apps/ImpactAssessment/data/ConsequenceSignificanceMatrix.xlsx"
ARBO_RMD_PATH       <- "apps/ArboReport/modules/arboreport_full.Rmd"
BAT_SPECIES_DB_PATH <- "apps/BatRecordingProcessing/data/Species_Database_Bats.csv"


#### CT Step 1: EXIF Extraction ####
## Uncomment and fill in paths before running
path_processed <- "G:/Shared drives/01_Current_Projects_A-D/CR205 EMMP_CCCC/02_Camera_Trapping/Camera_Trap_Data/02 Processed/Forest Monitoring/20260713/"
path_raw       <- "G:/Shared drives/01_Current_Projects_A-D/CR205 EMMP_CCCC/02_Camera_Trapping/Camera_Trap_Data/01 Raw/Forest Monitoring/20260713/"

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
path_exif_folder <- "C:/Users/joejyn/OneDrive/Camphora/Projects/CR202_Obayashi/EngNeo/Data/CT"

merging_exifs(
  path_exif_folder      = path_exif_folder,
  path_species_database = SPECIES_DB_PATH,
  input_combined        = NA   # set to a file path string if using a manually edited combined CSV
)


#### CT Step 3: Independent Detections ####
## Uncomment and fill in path before running
input_ct_file <- "C:/Users/joejyn/OneDrive/Camphora/Projects/CR202_Obayashi/EngNeo/Data/CT/combined_exif_all.csv"

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
path_biodata <- "C:/Users/Joejyn/Downloads/Holland_Arbo_v0_1-20.csv"

## [Optional] Set to NULL to generate the report without photos
arbo_resized_photos_dir <- NULL
arbo_photo_prefix <- "Holland Rd_Photos"

run_arbo_report(
  path_biodata        = path_biodata,
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


#### Stream Inspection Report ####
## Uncomment and fill in paths before running
# si_path_fauna  <- "Z:/path/to/CR202 Fauna Monitoring Data.xlsx"
# si_photos_dir  <- "Z:/path/to/Stream Inspection/Windsor"

## Inspection date(s), YYYY-MM-DD. Single: "2025-11-25"; multiple: c("2025-11-25", "2025-11-26")
si_dates <- c("2025-11-25", "2025-11-26")

stream_report(
  path_fauna_data = si_path_fauna,
  path_photos_dir = si_photos_dir,
  inspection_date = si_dates,
  output_dir      = "apps/StreamInspection/results"
)


#### Bat Recording: Step 1 Process Meta ####
## Uncomment and fill in paths before running
bat_meta_file <- "F:/Shared drives/01_Current_Projects_A-D/CR202 EMMP_Obayashi/10_Bat_Recordings/02_Processed/Eng Neo/20260303/meta.csv"
bat_gps_file  <- "F:/Shared drives/01_Current_Projects_A-D/CR202 EMMP_Obayashi/10_Bat_Recordings/02_Processed/Eng Neo/20260303/tracks.csv"   # set to NA to skip GPS matching
bat_wav_dir   <- NA   # set to NA to skip sorting

process_bat_meta(
  meta_file         = bat_meta_file,
  species_db_path   = BAT_SPECIES_DB_PATH,
  delimiter         = "_",
  wav_folder        = NA,          # or bat_wav_dir to sort .wav files
  handheld_gps_file = NA,          # or bat_gps_file to match GPS
  output_dir        = dirname(bat_meta_file)
)


#### Bat Recording: Step 2 Combine Meta ####
## Uncomment and fill in path before running
bat_meta_folder <- "C:/Users/Joejyn/OneDrive/Camphora/Projects/CR202_Obayashi/EngNeo/Data/Bat Recordings/"

combine_bat_meta(
  meta_folder = bat_meta_folder,
  output_dir  = bat_meta_folder
)


#### Bat Recording: Sub-sample Files ####
## Uncomment and fill in path before running
# bat_raw_dir <- "Z:/path/to/raw/wav_folder"

## Keep 5 minutes out of every 30-minute block
bat_subsample_mins <- c(0, 1, 2, 3, 4, 30, 31, 32, 33, 34)

subsample_bat_files(
  path_raw       = bat_raw_dir,
  subsample_mins = bat_subsample_mins
)


#### Bat Recording: Recover Meta ####
## Uncomment and fill in paths before running
# bat_proc_dir <- "C:/TempDataForSpeed/20240930/Processed/Bat4_20240930/"
# bat_raw_dir2 <- "C:/TempDataForSpeed/20240930/Raw/Bat4_20240930/"

recover_bat_meta(
  path_processed = bat_proc_dir,
  path_raw       = bat_raw_dir2,
  output_dir     = bat_proc_dir
)


#### Flora Photo Filing: Sort Photos ####
## Uncomment and fill in paths before running
# flora_datasheet_path <- "Z:/path/to/BTNR_Master data_v106_JM.xlsx"
# flora_photos_dir     <- "Z:/path/to/BTNR_Interim Report 3_Photos"
# flora_sorted_dir     <- "Z:/path/to/Flora_and_Arboriculture_Batch3.2"

flora_status_to_sort <- c("Batch 3.1", "Batch 3.2")

sort_flora_photos(
  datasheet_path = flora_datasheet_path,
  photos_dir     = flora_photos_dir,
  sorted_dir     = flora_sorted_dir,
  status_to_sort = flora_status_to_sort,
  sheet_name     = "Photo Filing (For JO)"
)


#### Flora Photo Filing: Re-sort Tag Folders ####
## Uncomment and fill in paths before running
# flora_resort_src_dir  <- "Z:/path/to/Flora_and_Arboriculture_Batch2.1"
# flora_resort_dest_dir <- "Z:/path/to/Flora_and_Arboriculture_Batch2.1_Updated"

resort_flora_tag_dirs(
  sorted_dir  = flora_resort_src_dir,
  updated_dir = flora_resort_dest_dir
)


#### CAG Photo Renaming ####
## Uncomment and fill in paths before running
# cag_excel_path <- "C:/Users/joejyn/Downloads/CAG 14.xlsx"
# cag_photo_dir  <- "C:/Users/joejyn/Downloads/OneDrive_2026-07-23/Picture 2026 07 08/"

cag_sheet <- "T1 20260708"
cag_mode  <- "dry_run"   # "dry_run" -> preview only | "copy" -> safe | "rename" -> in place

rename_photos_from_excel(
  excel_path = cag_excel_path,
  sheet      = cag_sheet,
  photo_dir  = cag_photo_dir,
  mode       = cag_mode,
  id_col     = "Tree ID",
  photo_col  = "Photo"
)
