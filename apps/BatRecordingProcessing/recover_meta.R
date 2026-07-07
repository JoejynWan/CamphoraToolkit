## recover_meta.R
## Core logic to reverse-engineer a meta.csv from already-sorted species folders
## plus the raw .wav files' EXIF timestamps. A last resort for when the meta.csv
## has been lost but the files have already been sorted. Called by app.R — do not
## run this file directly. Note: the reverse-engineered meta.csv may not be
## readable by Kaleidoscope.
##
## Adapted from ../BatRecordingProcessing_v1.5/recover_meta.R (a top-level script
## with hardcoded paths) into a single entry point, recover_bat_meta().


#### Helper ####

read_exif_parallel <- function(dir_path){

  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, library(exifr))

  wav_files <- list.files(dir_path, recursive = TRUE, pattern = "*.wav", full.names = TRUE)
  if (length(wav_files) < num_cores){
    file_batches <- msplit(wav_files, length(wav_files))
  } else {
    file_batches <- msplit(wav_files, num_cores)
  }

  exif_dat_list <- parLapply(cl, file_batches, read_exif)
  exif_dat <- bind_rows(exif_dat_list)

  stopCluster(cl)

  return(exif_dat)
}


#### Main function ####

#' Reverse-engineer a meta.csv from sorted species folders and raw .wav EXIFs.
#'
#' @param path_processed  Folder of sorted .wav files (one subfolder per species).
#' @param path_raw        Folder of the original raw .wav files.
#' @param output_dir      Directory to write meta_reverse.csv into (default:
#'                        path_processed).
#' @param log             Progress message function (default message).
#'
#' @return Invisibly returns the path to meta_reverse.csv.
recover_bat_meta <- function(path_processed, path_raw, output_dir = path_processed,
                             log = message){

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  log("Reading sorted (processed) folder structure...")
  data_processed <- data.frame(
    files_processed = list.files(path_processed, recursive = TRUE, pattern = "*.wav", full.names = TRUE)) %>%
    mutate(INDIR = dirname(dirname(files_processed)),
           IN.FILE = basename(files_processed),
           MANUAL.ID = basename(dirname(files_processed)))

  log("Reading raw .wav EXIF timestamps (this may take a while)...")
  data_raw <- read_exif_parallel(path_raw) %>%
    select(FileName, FileModifyDate, Duration) %>%
    mutate(DateTime = as.POSIXct(FileModifyDate, format = "%Y:%m:%d %H:%M:%S", tz = "Singapore"),
           DATE = format(DateTime, "%d/%m/%Y"),
           TIME = format(DateTime, "%H:%M:%S"),
           HOUR = format(DateTime, "%H"),
           DATE.12 = format(DateTime, "%d/%m/%Y"),
           TIME.12 = format(DateTime, "%I:%M:%S %p"),
           HOUR.12 = format(DateTime, "%I")) %>%
    rename(DURATION = Duration)

  meta_reverse <- merge(data_processed, data_raw, by.x = "IN.FILE", by.y = "FileName") %>%
    mutate(LATITUDE = "",
           LONGITUDE = "",
           MODEL = "",
           SERIAL.NO = "",
           FIRMWARE = "",
           PREFIX = "",
           FILES = "",
           INPATHMD5 = "") %>%
    select(INDIR, IN.FILE, DURATION, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, LATITUDE,
           LONGITUDE, MODEL, SERIAL.NO, FIRMWARE, PREFIX, FILES, MANUAL.ID, INPATHMD5)

  path_out <- file.path(output_dir, "meta_reverse.csv")
  write.csv(meta_reverse, path_out, row.names = FALSE)

  log(paste("Completed. meta_reverse.csv saved at", path_out))
  invisible(path_out)
}
