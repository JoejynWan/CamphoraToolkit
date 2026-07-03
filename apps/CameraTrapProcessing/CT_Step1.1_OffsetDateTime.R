#------------------------------------------------#
#### Offset DateTime in an already-generated exif ####
#------------------------------------------------#
## Corrects FileModifyDate/Date/Time in a *_exif.csv when the camera's clock
## was wrong at the time of recording, either by a fixed number of hours or by
## anchoring the first video to its actual DateTime.


#### Main function ####
offset_datetime <- function(exif_path, offset, log = message){

  offset_numeric <- suppressWarnings(as.numeric(offset))

  if (!is.na(offset_numeric)){
    ## offset is a number of hours (e.g. -12, or "12" typed into a text box)
    offset_sec <- offset_numeric * 60 * 60
    log(paste("Applying a", offset_numeric, "hour offset..."))

  } else {
    ## offset is the correct DateTime of the first video (e.g. "2025-11-13 08:00:00")
    exif_raw <- read.csv(exif_path)
    first_datetime <- min(exif_raw$FileModifyDate)
    offset_sec <- difftime(offset, first_datetime, units = "secs")
    log(paste("Anchoring first video to", offset, "..."))
  }

  exif <- read.csv(exif_path) %>%
    mutate(FileModifyDate = as.POSIXct(FileModifyDate),
           FileModifyDate = FileModifyDate + offset_sec,
           Date = as.Date(FileModifyDate, tz = "Singapore"),
           Time = format(as.POSIXlt(FileModifyDate),
                        format = "%H:%M:%S", tz = "Singapore"))

  exif_output <- paste0(tools::file_path_sans_ext(exif_path), "_offset_exif.csv")
  write.csv(exif, exif_output, row.names = FALSE)

  log(paste("Done! Offset exif saved at:", exif_output))
  invisible(exif_output)
}
