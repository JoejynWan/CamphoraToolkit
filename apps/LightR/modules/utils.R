## utils.R
## Helper for reading a single light logger (.xlsx) export into a tidy data frame.
## Sourced by process_light.R — do not run this file directly.
##
## NOTE: named read_light_file() rather than read_light() because app.R sources every module into
## the same global environment, so helper names are kept app-specific.


#### Helper functions ####

## Column names differ between logger export versions — e.g. "Light , lux" (newer) vs
## "Ch:2 - Light   (lux)" (older) — so columns are matched by keyword rather than exact name.
## read.xlsx() also substitutes "." for the blanks in those names, hence the loose matching.
find_light_col <- function(col_names, pattern, light_file){

  hits <- col_names[str_detect(tolower(col_names), pattern)]

  if (length(hits) == 0) {
    stop("Could not find a column matching '", pattern, "' in: ", basename(light_file),
         ". Columns found: ", paste(col_names, collapse = ", "))
  }

  hits[1]
}


## The loggers write the date-time cells in a custom format that read.xlsx() does not recognise as
## a date, so they come back as Excel serial numbers (days since 1899-12-30) even with
## detectDates = TRUE. Serials are converted here; anything already parsed is passed through.
as_light_datetime <- function(x){

  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date"))    return(as.POSIXct(format(x), tz = "UTC"))

  ## Rounded to the nearest second — the serial-to-seconds multiplication otherwise leaves values a
  ## fraction short, which truncates to the previous second when formatted.
  if (is.numeric(x)) return(as.POSIXct(round(x * 86400), origin = "1899-12-30", tz = "UTC"))

  as.POSIXct(as.character(x), tz = "UTC")
}


#' Read one light logger export into a tidy data frame.
#'
#' The station name is taken as the first token of the file name, split on a space or an
#' underscore — e.g. "L2 2026-07-02 09_51_33 SGT (Data SGT).xlsx" and "L2_20260702.xlsx" both
#' give station "L2".
#'
#' @param light_file  Path to a logger export (.xlsx) containing a "Data" sheet.
#'
#' @return A data frame with FileName, SamplingDate, Station, DateTime, Date, Time, Hour,
#'         Light and Temperature columns.
read_light_file <- function(light_file){

  station <- str_split_1(basename(light_file), pattern = "[ _]")[1]

  raw <- read.xlsx(light_file, sheet = "Data")

  col_datetime <- find_light_col(names(raw), "date.?.?time", light_file)
  col_light    <- find_light_col(names(raw), "light",        light_file)
  col_temp     <- find_light_col(names(raw), "temp",         light_file)

  data <- raw %>%
    select(DateTime    = all_of(col_datetime),
           Light       = all_of(col_light),
           Temperature = all_of(col_temp)) %>%
    mutate(DateTime     = as_light_datetime(DateTime),
           FileName     = basename(light_file),
           SamplingDate = basename(dirname(light_file)),
           Station      = station,
           Date         = format(DateTime, "%Y-%m-%d"),
           Time         = format(DateTime, "%H:%M:%S"),
           Hour         = as.integer(format(DateTime, "%H"))) %>%
    select(FileName, SamplingDate, Station, DateTime, Date, Time, Hour, Light, Temperature)

  return(data)
}
