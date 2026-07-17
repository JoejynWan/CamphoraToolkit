## sort_photos.R
## Core logic for filing flora survey photos into Family/Species/Tag folders, driven by the photo
## filing sheet of the project master datasheet.
## Called by app.R — do not run this file directly.
##
## Adapted from ../FloraPhotoFiling/ImgSort.R (a top-level script with no callable function) into
## a single entry point, sort_flora_photos().


#### Main function ####

#' File flora survey photos into Family/Species/Tag folders.
#'
#' Reads the photo filing sheet, resolves each tag's ZOOM and FS camera photo numbers into real
#' photo paths, removes any previously sorted copies whose family/species has since changed, then
#' copies photos into <sorted_dir>/<Family>/<Species>/<Tag>/<Tag>_<original name>.
#'
#' @param datasheet_path  Path to the master datasheet (.xlsx).
#' @param photos_dir      Folder containing the raw per-session photo folders.
#' @param sorted_dir      Destination folder for the sorted photos.
#' @param status_to_sort  Character vector of STATUS values to sort, e.g.
#'                        c("Batch 3.1", "Batch 3.2").
#' @param sheet_name      Name of the photo filing sheet in the datasheet.
#' @param log             A function used for progress messages, e.g. message (default) or a Shiny
#'                        logger.
#'
#' @return Invisibly, a data frame of photos copied per Family/Species/Tag.
sort_flora_photos <- function(datasheet_path,
                              photos_dir,
                              sorted_dir,
                              status_to_sort,
                              sheet_name = "Photo Filing (For JO)",
                              log        = message){

  #### Read and clean data ####

  log(paste("Reading datasheet sheet:", sheet_name))

  data <- read.xlsx(datasheet_path, detectDates = TRUE, sheet = sheet_name)

  required_cols <- c("STATUS", "DATE_MEASURED_2025", "TAG_2025", "Species", "Family",
                     "ZOOM_CAM", "ZOOM_PHOTO_ID", "FS_CAM", "FS_PHOTO_ID")
  missing_cols  <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Sheet '", sheet_name, "' is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         "\nFound: ", paste(names(data), collapse = ", "))
  }

  ## NOTE: %in%, not ==, so that multiple statuses can be sorted in one run.
  data_clean <- data %>%
    mutate(across(where(is.character), trimws)) %>%
    filter(STATUS %in% status_to_sort)

  if (nrow(data_clean) == 0) {
    stop("No rows in '", sheet_name, "' have STATUS matching: ",
         paste(status_to_sort, collapse = ", "))
  }

  log(paste("Found", nrow(data_clean), "tagged specimens to file."))


  #### Resolve photo folders ####

  ## Stack the ZOOM and FS camera columns so both are handled identically
  data_folder <- bind_rows(
      select(data_clean, DATE_MEASURED_2025, TAG_2025, Species, Family,
             Folder = ZOOM_CAM, PhotoID = ZOOM_PHOTO_ID),
      select(data_clean, DATE_MEASURED_2025, TAG_2025, Species, Family,
             Folder = FS_CAM,   PhotoID = FS_PHOTO_ID)
    ) %>%
    separate_longer_delim(cols = c(Folder, PhotoID), ";") %>%
    mutate(across(c(Folder, PhotoID), trimws)) %>%
    mutate(PhotoFolder = case_when(!is.na(Folder) ~ file.path(photos_dir, Folder),
                                   .default = NA))

  ## Check if there are missing or misnamed photo folders
  unique_folders     <- na.omit(unique(data_folder$PhotoFolder))
  missing_folders_idx <- !dir.exists(unique_folders)

  if (any(missing_folders_idx)){
    missing_folders <- unique_folders[missing_folders_idx]

    folders_with_issues <- data_folder %>%
      filter(PhotoFolder %in% missing_folders) %>%
      select(TAG_2025, PhotoFolder, PhotoID) %>%
      distinct()

    stop("There are ", length(missing_folders), " missing/misnamed photo folders.\n",
         "Affected tags: ", paste(unique(folders_with_issues$TAG_2025), collapse = ", "), "\n",
         "Missing folders:\n", paste(missing_folders, collapse = "\n"))
  }


  #### Resolve photo paths ####

  log("Resolving photo numbers into photo paths...")

  data_photos <- data_folder %>%
    mutate(PhotosFrom = get_flora_photo_paths(PhotoFolder, PhotoID)) %>%
    unnest(PhotosFrom) %>%
    filter(!is.na(PhotosFrom)) %>%
    select(PhotosFrom, Family, Species, TAG_2025) %>%
    mutate(PhotosToName = paste(TAG_2025, basename(PhotosFrom), sep = "_"),
           Family       = gsub("\\.+$", "", Family),
           Species      = gsub("\\.+$", "", Species),
           PhotosTo     = file.path(sorted_dir, Family, Species, TAG_2025, PhotosToName))

  if (nrow(data_photos) == 0) {
    stop("No photos resolved from the datasheet. Check the photo numbers and folders.")
  }

  ## Catch tags whose photo number resolved to nothing, leaving a bare extension
  photo_number_issues <- data_photos %>%
    filter(str_ends(PhotosFrom, "/.JPG")) %>%
    select(PhotosFrom, TAG_2025)

  if (nrow(photo_number_issues) != 0){
    stop("There are issues with the photo numbers of ", nrow(photo_number_issues),
         " photos.\nAffected tags: ",
         paste(unique(photo_number_issues$TAG_2025), collapse = ", "))
  }

  log(paste("Resolved", nrow(data_photos), "photos across",
            length(unique(data_photos$TAG_2025)), "tags."))


  #### Remove previously sorted photos whose family/species changed ####

  old_photos <- list.files(sorted_dir, recursive = TRUE, full.names = TRUE)

  if (length(old_photos) != 0){
    old_photos_df <- data.frame(PhotosOld = old_photos) %>%
      mutate(PhotosOldName = basename(PhotosOld)) %>%
      merge(data_photos, by.x = "PhotosOldName", by.y = "PhotosToName") %>%
      filter(PhotosOld != PhotosTo)

    if (nrow(old_photos_df) != 0){
      file.remove(old_photos_df$PhotosOld)
      log(paste("Removed", nrow(old_photos_df),
                "previously sorted photos whose family/species changed."))
    }

    for (species_dir in list.dirs(sorted_dir, recursive = TRUE)){
      if (dir.exists(species_dir) && length(dir(species_dir)) == 0) {
        unlink(species_dir, recursive = TRUE, force = TRUE)
        log(paste("Deleted empty folder:", species_dir))
      }
    }
  }


  #### Sort photos into Family/Species/Tag folders ####

  ## recursive = TRUE so the tree is built even on a brand new sorted_dir
  for (tag_dir in unique(dirname(data_photos$PhotosTo))) {
    if (!dir.exists(tag_dir)) dir.create(tag_dir, recursive = TRUE)
  }

  log(paste("Copying", nrow(data_photos), "photos into:", sorted_dir))

  failed_copies  <- character(0)
  skipped_copies <- character(0)

  for (i in 1:nrow(data_photos)){
    status <- file.copy(data_photos$PhotosFrom[i], data_photos$PhotosTo[i],
                        overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
    if (!status) {
      if (file.exists(data_photos$PhotosTo[i])) {
        skipped_copies <- c(skipped_copies, data_photos$PhotosTo[i])
      } else {
        failed_copies  <- c(failed_copies, data_photos$PhotosTo[i])
      }
    }
  }

  n_copied <- nrow(data_photos) - length(failed_copies) - length(skipped_copies)

  log(paste0("Copy complete. ", n_copied, " copied, ",
             length(skipped_copies), " skipped (already present), ",
             length(failed_copies), " failed."))

  if (length(failed_copies) > 0) {
    log("Failed copies:")
    for (f in failed_copies) log(paste(" -", f))
  }


  #### Summary of photos filed ####

  summary_df <- data_photos %>%
    group_by(Family, Species, TAG_2025) %>%
    summarise(Photos = n(), .groups = "drop") %>%
    arrange(Family, Species, TAG_2025)

  invisible(summary_df)
}
