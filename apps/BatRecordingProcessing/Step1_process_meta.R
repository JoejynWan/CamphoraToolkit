## Step1_process_meta.R
## Core logic for cleaning a Kaleidoscope bat meta.csv (one species per row,
## corrected scientific names), optionally matching handheld GPS tracks, and
## optionally sorting the .wav files into species folders. Called by app.R —
## do not run this file directly.
##
## Adapted from ../BatRecordingProcessing_v1.5/Step1_process_meta.R (a top-level
## script with a VARIABLE CONTROL PANEL of hardcoded paths) into a single entry
## point, process_bat_meta(), that takes an output directory and a log function.
## Depends on modules/util.r, modules/dup_rows.r, modules/match_gps.r,
## modules/sort_bat_data.r.


#### Main function ####

#' Clean (and optionally GPS-match / sort) a Kaleidoscope bat meta.csv.
#'
#' @param meta_file          Path to the meta.csv exported from Kaleidoscope.
#' @param species_db_path    Path to Species_Database_Bats.csv.
#' @param delimiter          Delimiter separating multiple species in MANUAL.ID.
#' @param wav_folder         Folder of .wav files to sort, or NA to skip sorting.
#' @param handheld_gps_file  Handheld GPS tracks CSV, or NA to skip GPS matching.
#' @param output_dir         Directory to write the cleaned/matched CSV (and, if
#'                           sorting, the "out" folder) into.
#' @param log                Progress message function (default message).
#'
#' @return Invisibly returns the path to the cleaned/matched meta CSV.
process_bat_meta <- function(meta_file,
                             species_db_path,
                             delimiter         = "_",
                             wav_folder        = NA,
                             handheld_gps_file = NA,
                             output_dir        = dirname(meta_file),
                             log               = message){

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  #### Prepping output ####
  ## Duplicate each row for each unique species
  log("Expanding meta rows by species ID...")
  meta_expanded <- dup_rows_by_ids(meta_file, delimiter) %>%
    mutate(FolderSpeciesName = MANUAL.ID)

  check_for_duplicates(meta_expanded)

  ## Replace MANUAL.ID with correct scientific names
  log("Correcting species names against the bat species database...")
  species_database <- read.csv(species_db_path) %>%
    select(FolderSpeciesName, ScientificName)

  meta_species <- correct_sp_names(meta_expanded, species_database) %>%
    mutate(MANUAL.ID = ScientificName) %>%
    select(-ScientificName)

  ## Match time to handheld GPS to update lat long
  if (is.na(handheld_gps_file)){

    out_path <- file.path(output_dir, "meta_cleaned.csv")
    write.csv(meta_species, out_path, row.names = FALSE)
    log(paste("Checkpoint 1: No handheld GPS file given, so meta.csv was only cleaned. Saved at", out_path))

  } else {

    log("Matching bat call times to handheld GPS tracks...")
    matched_csv <- match_gps_data(meta_species, handheld_gps_file)

    out_path <- file.path(output_dir, "meta_matched.csv")
    write.csv(matched_csv, out_path, row.names = FALSE)
    log(paste("Checkpoint 1: GPS data matched with meta.csv. Saved at", out_path))
  }

  #### Sorting wav files ####
  if (is.na(wav_folder)){
    log("Checkpoint 2: Skipping the sorting of .wav files.")
  } else {
    log("Checkpoint 2: Sorting .wav files now...")
    sorted_out <- file.path(output_dir, "out")
    sort_wav_files(meta_species, wav_folder, sorted_out)
    log(paste("Checkpoint 2: .wav files sorted based on manual IDs, into", sorted_out))
  }

  log("All completed! :)")
  invisible(out_path)
}
