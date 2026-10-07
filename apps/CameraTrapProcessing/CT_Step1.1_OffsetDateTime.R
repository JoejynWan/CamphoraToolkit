#------------------------------------------------#
#### Offset DateTime in an already-generated exif ####
#------------------------------------------------#
## Corrects FileModifyDate/Date/Time in a *_exif.csv when the camera's clock was wrong at the
## time of recording, either by a fixed number of hours or by anchoring a video to its actual
## DateTime.
## By default the whole exif is corrected and the first video is the anchor. If the clock only
## went wrong partway through (e.g. from IMG_0008.AVI onwards), pass that video name to
## `from_video`: it becomes the anchor and every earlier video is left untouched.


#### Main function ####
offset_datetime <- function(exif_path, offset, from_video = NA, log = message){

  context  <- basename(exif_path)
  required <- c("SamplingDate", "FileModifyDate", "Date", "Time", "FileName")

  raw <- read_exif_csv(exif_path, required, context)

  ## DateTimes are parsed rather than read as plain text, so that an exif that has been opened
  ## and re-saved in Excel (22/02/2023 20:54, AM/PM times, a dropped seconds field) still works.
  ## Every row comes back as ISO; only the rows selected below have their value changed.
  exif <- read_exif_datetimes(raw, context = context, log_fn = log)

  ## Put Quantity back to a number, as long as every value is one
  quantity <- suppressWarnings(as.numeric(trimws(exif$Quantity)))
  if (!any(is.na(quantity) & !is.na(exif$Quantity) & !trimws(exif$Quantity) %in% c("", "NA"))){
    exif$Quantity <- quantity
  }


  #### Deciding which videos to correct ####
  from_video <- if (length(from_video) == 0) NA_character_ else trimws(as.character(from_video)[1])

  if (is.na(from_video) || from_video == ""){
    rows_to_fix <- rep(TRUE, nrow(exif))

  } else {
    ## Videos are corrected from `from_video` onwards in file-name order, not in DateTime
    ## order, as the recorded DateTimes are the unreliable part here
    file_names <- sort(unique(exif$FileName))
    name_match <- file_names[tolower(file_names) == tolower(from_video)]

    if (length(name_match) == 0){
      stop("Cannot find a video named \"", from_video, "\" in the exif. Please check the ",
           "spelling and file extension (e.g. IMG_0008.AVI).")
    }

    from_video  <- name_match[1]
    rows_to_fix <- match(exif$FileName, file_names) >= match(from_video, file_names)

    log(paste0("Correcting from ", from_video, " onwards (", sum(rows_to_fix), " of ",
               nrow(exif), " rows); earlier videos are left untouched."))
  }

  if (!any(rows_to_fix)) stop("No videos left to correct.")


  #### Working out the offset ####
  offset_numeric <- suppressWarnings(as.numeric(offset))

  if (!is.na(offset_numeric)){
    ## offset is a number of hours (e.g. -12, or "12" typed into a text box)
    offset_sec <- offset_numeric * 60 * 60
    log(paste("Applying a", offset_numeric, "hour offset..."))

  } else {
    ## offset is the correct DateTime of the anchor video (e.g. "2025-11-13 08:00:00")
    target <- parse_one_datetime(offset)

    if (is.na(target)){
      stop("Cannot read \"", offset, "\" as a number of hours or as a DateTime. Please enter ",
           "either an hour offset (e.g. -12) or the correct DateTime of the anchor video ",
           "(e.g. 2025-11-13 08:00:00).")
    }

    if (is.na(from_video) || from_video == ""){
      anchor_label    <- "first video"
      anchor_datetime <- min(exif$FileModifyDate)
    } else {
      anchor_label    <- from_video
      anchor_datetime <- min(exif$FileModifyDate[exif$FileName == from_video])
    }

    offset_sec <- as.numeric(difftime(target, anchor_datetime, units = "secs"))
    log(paste("Anchoring", anchor_label, "to",
              format(target, format = "%Y-%m-%d %H:%M:%S"), "..."))
  }


  #### Applying the offset ####
  datetime <- exif$FileModifyDate
  datetime[rows_to_fix] <- datetime[rows_to_fix] + offset_sec

  ## Formatted rather than written as POSIXct, so that midnight timings are printed
  exif$FileModifyDate <- format(datetime, format = "%Y-%m-%d %H:%M:%S")
  exif$Date           <- format(datetime, format = "%Y-%m-%d")
  exif$Time           <- format(datetime, format = "%H:%M:%S")

  exif_output <- paste0(tools::file_path_sans_ext(exif_path), "_offset_exif.csv")
  write.csv(exif, exif_output, row.names = FALSE)

  log(paste("Done! Offset exif saved at:", exif_output))
  invisible(exif_output)
}
