## resort_tag_dirs.R
## Core logic for restructuring an already-sorted photo folder that is missing its per-tag
## subfolder level.
## Called by app.R — do not run this file directly.
##
## Adapted from ../FloraPhotoFiling/ReSortTagDirs.R (a top-level script with no callable function)
## into a single entry point, resort_flora_tag_dirs().


#### Main function ####

#' Re-file an existing Family/Species photo tree into Family/Species/Tag.
#'
#' Use this on batches sorted before per-tag subfolders were introduced. The tag is read from the
#' leading portion of each file name, up to the first "_".
#'
#' @param sorted_dir  Existing sorted folder, structured Family/Species/photo.jpg.
#' @param updated_dir Destination folder for the restructured tree.
#' @param log         A function used for progress messages, e.g. message (default) or a Shiny
#'                    logger.
#'
#' @return Invisibly, a data frame of photos copied per Family/Species/Tag.
resort_flora_tag_dirs <- function(sorted_dir, updated_dir, log = message){

  old_photos <- list.files(sorted_dir, recursive = TRUE, full.names = TRUE)

  if (length(old_photos) == 0) stop("No photos found in: ", sorted_dir)

  log(paste("Found", length(old_photos), "photos to restructure."))

  data_photos <- data.frame(PhotosFrom = old_photos) %>%
    mutate(PhotosOldName = basename(PhotosFrom),
           Species       = basename(dirname(PhotosFrom)),
           Family        = basename(dirname(dirname(PhotosFrom)))) %>%
    separate(PhotosOldName, into = c("TAG_2025", "PhotoName"),
             sep = "_", extra = "merge", remove = FALSE) %>%
    mutate(PhotosTo = file.path(updated_dir, Family, Species, TAG_2025, PhotosOldName))

  ## recursive = TRUE so the tree is built even on a brand new updated_dir
  for (tag_dir in unique(dirname(data_photos$PhotosTo))) {
    if (!dir.exists(tag_dir)) dir.create(tag_dir, recursive = TRUE)
  }

  log(paste("Copying", nrow(data_photos), "photos into:", updated_dir))

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

  summary_df <- data_photos %>%
    group_by(Family, Species, TAG_2025) %>%
    summarise(Photos = n(), .groups = "drop") %>%
    arrange(Family, Species, TAG_2025)

  invisible(summary_df)
}
