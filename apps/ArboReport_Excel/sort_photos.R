## sort_photos.R
## Core logic for filing arbo photos into the ArboReportPhotos/<Tree.ID>/ folder that accompanies
## the Excel arbo report, driven by the Photos.by and Photo.no. columns of the arbo report working
## copy.
## Called by app.R — do not run this file directly.
##
## UNDER CONSTRUCTION: this will later be merged with the Excel clean-up into a single script that
## generates the full Excel arbo report.


#### Helper functions ####

#' Coerce a Date column read by read.xlsx() into "YYYY-MM-DD" strings.
#'
#' detectDates = TRUE gives a Date for proper Excel dates, but a column with any text in it comes
#' back as character, holding either Excel serial numbers or typed dates.
#'
#' @param x A Date, numeric or character vector.
#'
#' @return A character vector of "YYYY-MM-DD" dates, NA where the value could not be read.
format_arbo_xl_date <- function(x){
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))

  x   <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))

  is_serial      <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
  out[is_serial] <- format(convertToDate(as.numeric(x[is_serial])), "%Y-%m-%d")

  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%d/%m/%y", "%d-%m-%Y", "%d.%m.%Y")) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    parsed    <- as.Date(x[todo], format = fmt)
    out[todo] <- ifelse(is.na(parsed), NA_character_, format(parsed, "%Y-%m-%d"))
  }

  out
}


#### Main function ####

#' File arbo photos into ArboReportPhotos/<Tree.ID>/<Tree.ID>_<original name>.
#'
#' Reads the arbo report working copy, resolves each tree's photo numbers into photos in
#' <photos_dir>/<photo_prefix>_<YYYY-MM-DD>_<Photos.by>, optionally removes previously filed photos
#' that the datasheet no longer refers to, then copies the photos across.
#'
#' @param datasheet_path  Path to the arbo report working copy (.xlsx).
#' @param photos_dir      Folder containing the raw per-inspection photo folders.
#' @param photo_prefix    Prefix used in photo folder names, e.g. "Seletar East_Photos" for
#'                        "Seletar East_Photos_2026-05-25_SK".
#' @param output_dir      Folder to create the ArboReportPhotos/ folder in.
#' @param sheet_name      Sheet to read; NULL reads the first sheet.
#' @param remove_stale    Delete photos in ArboReportPhotos/ that the datasheet no longer refers
#'                        to, e.g. after a photo number or Tree.ID was corrected.
#' @param log             A function used for progress messages, e.g. message (default) or a Shiny
#'                        logger.
#'
#' @return Invisibly, a data frame of photos expected and found per tree.
sort_arbo_xl_photos <- function(datasheet_path,
                                photos_dir,
                                photo_prefix,
                                output_dir,
                                sheet_name   = NULL,
                                remove_stale = TRUE,
                                log          = message){

  ## Paths are compared against list.files() output, so strip any trailing separator first
  photos_dir <- normalise_dir_path(photos_dir)
  sorted_dir <- file.path(normalise_dir_path(output_dir), "ArboReportPhotos")

  #### Read and clean data ####

  if (is.null(sheet_name) || !nzchar(trimws(sheet_name))) sheet_name <- 1

  log(paste("Reading datasheet sheet:", sheet_name))

  data <- read.xlsx(datasheet_path, detectDates = TRUE, sheet = sheet_name)

  required_cols <- c("Tree.ID", "Date", "Photos.by", "Photo.no.")
  missing_cols  <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Sheet '", sheet_name, "' is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         "\nFound: ", paste(names(data), collapse = ", "))
  }

  data_clean <- data %>%
    select(all_of(required_cols)) %>%
    mutate(across(c(Tree.ID, Photos.by, Photo.no.), ~ trimws(as.character(.x))),
           Date_chr = format_arbo_xl_date(Date))

  no_photos <- data_clean %>% filter(is.na(Photo.no.) | !nzchar(Photo.no.))

  if (nrow(no_photos) > 0) {
    log(paste0("Skipping ", nrow(no_photos), " trees with no photo number: ",
               paste(no_photos$Tree.ID, collapse = ", ")))
  }

  data_clean <- data_clean %>% filter(!is.na(Photo.no.) & nzchar(Photo.no.))

  if (nrow(data_clean) == 0) stop("No trees in the datasheet have a photo number.")

  bad_rows <- data_clean %>% filter(is.na(Tree.ID) | is.na(Date_chr) | is.na(Photos.by))

  if (nrow(bad_rows) > 0) {
    stop("There are ", nrow(bad_rows), " rows with a missing Tree.ID, Photos.by or unreadable ",
         "Date.\nAffected rows (Tree.ID / Date / Photos.by):\n",
         paste0(" - ", bad_rows$Tree.ID, " / ", bad_rows$Date, " / ", bad_rows$Photos.by,
                collapse = "\n"))
  }

  log(paste("Found", nrow(data_clean), "trees with photos to file."))


  #### Resolve photo folders ####

  data_folder <- data_clean %>%
    mutate(PhotoFolder = file.path(photos_dir,
                                   paste(photo_prefix, Date_chr, Photos.by, sep = "_")))

  unique_folders      <- unique(data_folder$PhotoFolder)
  missing_folders_idx <- !dir.exists(unique_folders)

  if (any(missing_folders_idx)){
    missing_folders <- unique_folders[missing_folders_idx]
    affected_tags   <- data_folder$Tree.ID[data_folder$PhotoFolder %in% missing_folders]

    stop("There are ", length(missing_folders), " missing/misnamed photo folders.\n",
         "Affected trees: ", paste(unique(affected_tags), collapse = ", "), "\n",
         "Missing folders:\n", paste(missing_folders, collapse = "\n"))
  }


  #### Resolve photo paths ####

  log("Resolving photo numbers into photo paths...")

  resolved <- get_arbo_xl_photo_paths(data_folder$PhotoFolder, data_folder$Photo.no.,
                                      data_folder$Tree.ID)

  data_folder <- data_folder %>%
    mutate(PhotosFrom = lapply(resolved, `[[`, "paths"),
           Expected   = sapply(resolved, `[[`, "expected"),
           Found      = lengths(PhotosFrom))

  no_match <- data_folder %>% filter(Found == 0)

  if (nrow(no_match) > 0) {
    stop("No photos found for ", nrow(no_match), " trees. Check their photo numbers and ",
         "folders.\nAffected trees (Tree.ID: Photo.no. in folder):\n",
         paste0(" - ", no_match$Tree.ID, ": '", no_match$Photo.no., "' in ",
                basename(no_match$PhotoFolder), collapse = "\n"))
  }

  mismatch <- data_folder %>% filter(Found != Expected)

  if (nrow(mismatch) > 0) {
    log(paste("WARNING:", nrow(mismatch), "trees have a different number of photos found than",
              "their photo numbers suggest:"))
    for (i in seq_len(nrow(mismatch))) {
      log(paste0(" - ", mismatch$Tree.ID[i], ": '", mismatch$Photo.no.[i], "' — expected ",
                 mismatch$Expected[i], ", found ", mismatch$Found[i]))
    }
  }

  data_photos <- data_folder %>%
    select(Tree.ID, PhotosFrom) %>%
    unnest(PhotosFrom) %>%
    mutate(PhotosToName = paste(Tree.ID, basename(PhotosFrom), sep = "_"),
           PhotosTo     = file.path(sorted_dir, Tree.ID, PhotosToName))

  log(paste("Resolved", nrow(data_photos), "photos across",
            length(unique(data_photos$Tree.ID)), "trees."))


  #### Remove previously filed photos no longer in the datasheet ####

  old_photos <- list.files(sorted_dir, recursive = TRUE, full.names = TRUE)

  if (remove_stale && length(old_photos) != 0){
    stale_photos <- setdiff(old_photos, data_photos$PhotosTo)

    if (length(stale_photos) != 0){
      file.remove(stale_photos)
      log(paste("Removed", length(stale_photos),
                "previously filed photos no longer referred to by the datasheet."))
    }

    ## setdiff() so sorted_dir itself is never deleted, even when it ends up empty
    for (tag_dir in setdiff(list.dirs(sorted_dir, recursive = TRUE), sorted_dir)){
      if (dir.exists(tag_dir) && length(dir(tag_dir, all.files = TRUE, no.. = TRUE)) == 0) {
        unlink(tag_dir, recursive = TRUE, force = TRUE)
        log(paste("Deleted empty folder:", tag_dir))
      }
    }
  }


  #### Copy photos into ArboReportPhotos/<Tree.ID> folders ####

  ## recursive = TRUE so the tree is built even on a brand new output_dir
  for (tag_dir in unique(dirname(data_photos$PhotosTo))) {
    if (!dir.exists(tag_dir)) dir.create(tag_dir, recursive = TRUE)
  }

  log(paste("Copying", nrow(data_photos), "photos into:", sorted_dir))

  failed_copies  <- character(0)
  skipped_copies <- character(0)

  for (i in seq_len(nrow(data_photos))){
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

  summary_df <- data_folder %>%
    transmute(Tree.ID, Date = Date_chr, Photos.by, Photo.no., Expected, Found)

  invisible(summary_df)
}
