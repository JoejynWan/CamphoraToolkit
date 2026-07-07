## subsample.R
## Core logic for sub-sampling raw bat call .wav files by the minute-of-recording
## before sorting (e.g. keep 5 minutes out of every 30). Called by app.R — do not
## run this file directly.
##
## Adapted from ../BatRecordingProcessing_v1.5/subsample.R (a top-level script
## with a Variable Control Panel) into a single entry point, subsample_bat_files().


#### Main function ####

#' Copy raw bat call .wav files whose recording minute falls within a set.
#'
#' File names are expected to follow Project_Date_Time (e.g. E_HT_20250522_003012.wav),
#' where the minute is characters 3-4 of the time component.
#'
#' @param path_raw        Folder containing the raw .wav files.
#' @param subsample_mins  Integer vector of minutes to keep, e.g. c(0:4, 30:34)
#'                        keeps 5 minutes out of every 30-minute block.
#' @param path_out        Destination folder (default: <path_raw>_subsampled next
#'                        to path_raw).
#' @param log             Progress message function (default message).
#'
#' @return Invisibly returns the destination folder path.
subsample_bat_files <- function(path_raw, subsample_mins, path_out = NULL, log = message){

  if (is.null(path_out))
    path_out <- file.path(dirname(path_raw), paste0(basename(path_raw), "_subsampled"))
  if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)

  ## Conduct sub-sampling
  files_subsampled <- data.frame(fullpath_from = list.files(path_raw, full.names = TRUE)) %>%
    mutate(filename = basename(fullpath_from),
           filename_noext = file_path_sans_ext(filename)) %>%
    separate_wider_delim(filename_noext, "_", names = c("Project", "Date", "Time")) %>%
    separate_wider_position(Time, widths = c(Hour = 2, Minute = 2, Second = 2), cols_remove = FALSE,
                            too_many = "drop") %>%
    mutate(Minute = as.integer(Minute)) %>%
    filter(Minute %in% subsample_mins) %>%
    mutate(fullpath_to = file.path(path_out, filename))

  if (nrow(files_subsampled) == 0)
    stop("No files matched the requested minutes in: ", path_raw)

  log(paste("Copying", nrow(files_subsampled), "sub-sampled file(s)..."))

  ## Copy files
  status <- file.copy(from = files_subsampled$fullpath_from, to = files_subsampled$fullpath_to,
                      copy.mode = TRUE, copy.date = TRUE)

  if (!all(status)) {
    ## try again for files with error
    errorfiles_from <- files_subsampled$fullpath_from[!status]
    errorfiles_to   <- files_subsampled$fullpath_to[!status]
    status2 <- file.copy(from = errorfiles_from, to = errorfiles_to,
                         copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)

    if (!all(status2))
      stop("Error in copying these files:\n",
           paste(basename(errorfiles_from[!status2]), collapse = "\n"))
  }

  log(paste("Complete! Sub-sampled files are saved in", path_out))
  invisible(path_out)
}
