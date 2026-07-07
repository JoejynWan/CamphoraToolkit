######################################################################
# (\__/)
# (>'.'<)   Functions to match GPS data
# (")_(")
######################################################################

# Format time data in mobile gps data row to POSIXct type
format_mobile_time <- function(mobile_gps) {
  # Parse the date in the data row
  parsed_date <- parse_date_time(mobile_gps['DATE'], orders=c("dmy", "ymd"))

  # Combine date and time columns in mobile gps into a datetime column
  datetime <- paste(parsed_date, mobile_gps['TIME'])

  # Set timezone to Singapore
  mobile_gps_time <- ymd_hms(datetime, tz="Asia/Singapore")
}

# Find index in handheld gps data with the closest time to the current
# mobile gps data row
find_closest_idx <- function(mobile_gps_row, handheld_gps) {
  mobile_gps_time <- ymd_hms(mobile_gps_row['DateTime'], tz="Asia/Singapore")
  time_diff <- abs(mobile_gps_time - handheld_gps$CLEANED.DATETIME)
  min_diff_idx <- which.min(time_diff)
}

# Format time data in handheld gps data to POSIXct type
format_handheld_time <- function(handheld_gps) {

  # Check for data in the time column
  if (all(is.na(handheld_gps$time))) {
    stop("There is missing time data in the HANDHELD_GPS_FILE")
  }

  # Convert time to Singapore timezone
  handheld_gps$CLEANED.DATETIME <- ymd_hms(handheld_gps$time, tz="UTC")
  handheld_gps$CLEANED.DATETIME <- with_tz(handheld_gps$CLEANED.DATETIME, tzone="Asia/Singapore")

  return(handheld_gps)
}


read_gps <- function(handheld_gps_file){
  ## Find where the actual data starts and remove the trash at the top
  handheld_gps = readr::read_lines(handheld_gps_file, skip_empty_rows = F)
  blank_row_last <- tail(which(handheld_gps == "" | !grepl("[^,]", handheld_gps)), 1)

  if (length(blank_row_last) == 0){
    # GPS file does not have any trash
    handheld_gps <- read.csv(handheld_gps_file)
  } else {
    # Remove all trash using the last empty row as a marker
    handheld_gps <- read.csv(handheld_gps_file, skip = blank_row_last+1)
  }

  return(handheld_gps)
}


## Process gps data files
match_gps_data <- function(mobile_gps, handheld_gps_file) {

  # Read csv file as data frame
  handheld_gps = read_gps(handheld_gps_file)

  # Format time
  handheld_gps = format_handheld_time(handheld_gps)

  # Process each row in mobile gps data
  mobile_gps2 <- mobile_gps %>%
    mutate(DATE = parse_date_time(DATE, orders=c("dmy", "ymd", "mdy")),
           DateTime = ymd_hms(paste(DATE, TIME), tz="Asia/Singapore")) %>%
    arrange(DateTime)

  closest_idx <- apply(mobile_gps2, 1, find_closest_idx, handheld_gps=handheld_gps)

  # Replace latitudes and longitudes based on closest indices
  mobile_gps3 <- mobile_gps2 %>%
    mutate(LATITUDE = handheld_gps$lat[closest_idx],
           LONGITUDE = handheld_gps$lon[closest_idx]) %>%
    select(-DateTime)

  return(mobile_gps3)
}
