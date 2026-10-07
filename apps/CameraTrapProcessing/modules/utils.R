install_load_packages <- function(packages){
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(new.packages)) install.packages(unlist(new.packages))
  
  lapply(packages, require, character.only = T)
}


check_missing_sp <- function(data_lower, species_database_lower){
  
  species_database_names <- unique(species_database_lower$FolderSpeciesName)
  species_present <- unique(data_lower$FolderSpeciesName)
  missing_species_log <- !species_present %in% species_database_names
  missing_species <- species_present[missing_species_log]
  
  data_missingspp <- data_lower %>%
    filter(FolderSpeciesName %in% missing_species) %>%
    select(FolderSpeciesName, Station, SamplingDate) %>%
    unique() %>%
    mutate(MissingSpp = paste(FolderSpeciesName, " (", Station, ", ", SamplingDate, ")", sep = ""))
  
  if (any(missing_species_log)){
    missing_species_names <- paste(sort(data_missingspp$MissingSpp), collapse = ", ")
    
    stop("There are species missing in the Species_Database.csv: ", missing_species_names, 
         ". Please add them in.")
  }
}

  
correct_sp_names <- function(data, species_database){
  ## Import corrected species names without taxonomic information
  ## Column name of species to be corrected in data should be FolderSpeciesName
  
  data_lower <- data %>%
    mutate(FolderSpeciesName = trimws(tolower(FolderSpeciesName)))
  
  species_database_lower <- species_database %>%
    mutate(FolderSpeciesName = trimws(tolower(FolderSpeciesName))) %>%
    unique()
  
  ## Check for missing species in Species Database
  check_missing_sp(data_lower, species_database_lower)
  
  ## Correct for species name if there are no missing species
  data_correct <- merge(data_lower, species_database_lower, by = "FolderSpeciesName", all.x = T) %>%
    select(-FolderSpeciesName)
  
  ## Check that all the rows are present and/or not duplicated
  if (nrow(data_correct) != nrow(data)) {
    stop("There are some missing or duplicated rows.")
  }
  
  return(data_correct)
}


read_exif_parallel <- function(dir_path){
  
  num_cores <- detectCores()-1
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, library(exifr))
  
  vid_files <- list.files(dir_path, recursive = T,
                          pattern = "(*.AVI|*.MP4|*.MOV|*.avi|*.JPG|*.jpg)", full.names = T)
  if (length(vid_files) < num_cores){
    file_batches <- msplit(vid_files, length(vid_files))
  } else {
    file_batches <- msplit(vid_files, num_cores)
  }
  
  exif_dat_list <- parLapply(cl, file_batches, read_exif) %>%
    lapply(., function(df) {
      if ("ShutterSpeedValue" %in% names(df)) {
        df$ShutterSpeedValue <- as.character(df$ShutterSpeedValue)
      }
      if ("Compression" %in% names(df)) {
        df$Compression <- as.character(df$Compression)
      }
      return(df)
    })
  exif_dat <- bind_rows(exif_dat_list)
  
  stopCluster(cl)
  
  return(exif_dat)
}


check_animal_captures_dir <- function(manual_data){
  animal_captures <- manual_data %>%
    filter(FolderSpeciesName %in% "Animal captures") %>%
    select(Station_SampleDate) %>%
    unique()
  
  if (nrow(animal_captures) != 0){
    animal_captures_location <- paste(animal_captures$Station_SampleDate, collapse = ", ")
    
    stop('There are "Animal captures" folders still present after manual sorting here: ',
         animal_captures_location, '. Please check and delete if the videos have been sorted.')
  }
}


check_speciesdatabase <- function(species_database){
  species_database_sim <- species_database %>%
    select(-FolderSpeciesName) %>%
    unique()
  
  num_spp <- length(unique(species_database_sim$ScientificName))
  if (nrow(species_database_sim) != num_spp){
    inconsistent_spp <- species_database_sim %>%
      dplyr::group_by(ScientificName) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1L) 
    
    species_database_wrong <- species_database_sim %>%
      filter(ScientificName %in% inconsistent_spp$ScientificName)
    
    stop("These species in the Species Database have inconsistent data: ", 
         paste(inconsistent_spp$ScientificName, collapse = ", "), 
         ". Please check the data entered in all columns for these species.")
  }
}



#### Reading DateTimes out of exif CSVs ####
## Exif CSVs straight out of Step 1 always hold ISO DateTimes ("2023-02-22 08:54:09"), but
## opening one in Excel and saving it rewrites them in the machine's locale: "22/02/2023 20:54",
## "2/22/2023 8:54:09 AM", seconds dropped, years shortened, or even a raw Excel serial number.
## Handing that mixture to lubridate::parse_date_time() is unsafe — given several `orders` it
## matches greedily, so "22/02/2023 20:54" comes back as 2022-02-20 23:20:54 instead of failing,
## and "8:54 PM" silently loses its PM. These helpers pull each value apart with explicit
## patterns instead, settle one day/month convention per file, and stop with a message naming
## the file when a value genuinely cannot be read.

MONTH_ABB <- c("jan", "feb", "mar", "apr", "may", "jun",
               "jul", "aug", "sep", "oct", "nov", "dec")

## Excel stores DateTimes as days since 1899-12-30
EXCEL_ORIGIN <- as.Date("1899-12-30")


## Reads an exif CSV with every column as text, so that its DateTimes can be parsed
## deliberately. Excel's "CSV UTF-8" save starts the file with a byte-order mark, which would
## otherwise be glued onto the first column name ("X.U.FEFF.Station"). A plain Excel CSV save
## is Windows-1252 rather than UTF-8, and left unconverted its accented characters (e.g. in
## Remarks) are written out as bytes that Step 3 cannot read.
read_exif_csv <- function(path, required, context = basename(path)){
  bytes    <- readBin(path, "raw", file.size(path))
  encoding <- if (identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) "UTF-8-BOM"
              else if (validUTF8(rawToChar(bytes))) "UTF-8"
              else "windows-1252"

  raw <- tryCatch(read.csv(path, colClasses = "character", fileEncoding = encoding),
                  error = function(e) stop("Cannot read ", context, ": ",
                                           conditionMessage(e)))

  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols) > 0){
    stop(context, " is missing the column(s) ", paste(missing_cols, collapse = ", "),
         ". Please check that this is an exif CSV from Step 1 or Step 2.")
  }

  raw
}


## Puts Quantity back to a number, stopping on any value that is not one
parse_quantity <- function(exif, context){
  quantity <- suppressWarnings(as.numeric(trimws(exif$Quantity)))
  bad_qty  <- is.na(quantity) & !is.na(exif$Quantity) & !trimws(exif$Quantity) %in% c("", "NA")
  if (any(bad_qty)){
    stop("Quantity is not a number in ", context, " at: ",
         paste(unique(exif$FileName[bad_qty]), collapse = ", "), ".")
  }
  exif$Quantity <- quantity

  exif
}


## Turns 2-digit years into 4-digit ones; camera traps all post-date 2000
expand_year <- function(y){
  y <- as.integer(y)
  ifelse(is.na(y) | y >= 100, y, ifelse(y < 70, y + 2000L, y + 1900L))
}


## Splits "<date> <time>" values into their two halves
split_datetime <- function(x){
  x    <- trimws(as.character(x))
  x    <- sub("^(\\S+)[T ]+", "\\1 ", x)
  date <- sub(" .*$", "", x)
  time <- ifelse(grepl(" ", x), trimws(sub("^\\S+ ", "", x)), NA_character_)
  list(date = date, time = time)
}


## Breaks date strings into their components without committing to a day/month order.
## `kind` records how each value was recognised: "num" values are the ambiguous ones
## (e.g. 5/2/2023), whose day and month stay in `a` and `b` until the order is settled.
parse_date_parts <- function(x){
  x <- trimws(as.character(x))
  n <- length(x)

  parts <- list(kind = rep("bad", n), y = rep(NA_integer_, n), mo = rep(NA_integer_, n),
                d = rep(NA_integer_, n), a = rep(NA_real_, n), b = rep(NA_real_, n))
  if (n == 0) return(parts)

  re_iso    <- "^(\\d{4})[-/.](\\d{1,2})[-/.](\\d{1,2})$"
  re_num    <- "^(\\d{1,2})[-/.](\\d{1,2})[-/.](\\d{2}|\\d{4})$"
  re_dmon   <- "^(\\d{1,2})[-/ ]+([A-Za-z]{3,9})\\.?[-/ ,]+(\\d{2}|\\d{4})$"
  re_mond   <- "^([A-Za-z]{3,9})\\.?[-/ ,]+(\\d{1,2}),?[-/ ]+(\\d{2}|\\d{4})$"
  re_serial <- "^\\d{4,6}(\\.\\d+)?$"

  month_no <- function(nm) match(substr(tolower(nm), 1, 3), MONTH_ABB)
  take     <- function(mask, re, i) sub(re, paste0("\\", i), x[mask])

  parts$kind[is.na(x) | x %in% c("", "NA")] <- "blank"

  m <- parts$kind == "bad" & grepl(re_iso, x)
  if (any(m)){
    parts$y[m]    <- as.integer(take(m, re_iso, 1))
    parts$mo[m]   <- as.integer(take(m, re_iso, 2))
    parts$d[m]    <- as.integer(take(m, re_iso, 3))
    parts$kind[m] <- "iso"
  }

  m <- parts$kind == "bad" & grepl(re_dmon, x)
  if (any(m)){
    parts$d[m]    <- as.integer(take(m, re_dmon, 1))
    parts$mo[m]   <- month_no(take(m, re_dmon, 2))
    parts$y[m]    <- expand_year(take(m, re_dmon, 3))
    parts$kind[m] <- "mon"
  }

  m <- parts$kind == "bad" & grepl(re_mond, x)
  if (any(m)){
    parts$mo[m]   <- month_no(take(m, re_mond, 1))
    parts$d[m]    <- as.integer(take(m, re_mond, 2))
    parts$y[m]    <- expand_year(take(m, re_mond, 3))
    parts$kind[m] <- "mon"
  }

  m <- parts$kind == "bad" & grepl(re_num, x)
  if (any(m)){
    parts$a[m]    <- as.integer(take(m, re_num, 1))
    parts$b[m]    <- as.integer(take(m, re_num, 2))
    parts$y[m]    <- expand_year(take(m, re_num, 3))
    parts$kind[m] <- "num"
  }

  m <- parts$kind == "bad" & grepl(re_serial, x)
  if (any(m)){
    parts$a[m]    <- as.numeric(x[m])
    parts$kind[m] <- "serial"
  }

  ## Month names that do not match a real month are no better than unreadable
  parts$kind[parts$kind == "mon" & is.na(parts$mo)] <- "bad"

  parts
}


## Which way round the ambiguous "num" values must be read, judged only on the values that
## cannot be read both ways. Returns "day", "month", "conflict", or NA when nothing says.
day_first_evidence <- function(parts){
  num <- parts$kind == "num"
  if (!any(num)) return(NA_character_)

  needs_day   <- any(parts$a[num] > 12, na.rm = TRUE)   # 1st field too big to be a month
  needs_month <- any(parts$b[num] > 12, na.rm = TRUE)   # 2nd field too big to be a month

  if (needs_day && needs_month) return("conflict")
  if (needs_day)   return("day")
  if (needs_month) return("month")
  NA_character_
}


## Assembles the parts into Dates, reading ambiguous values day-first or month-first
date_from_parts <- function(parts, day_first = TRUE){
  d  <- parts$d
  mo <- parts$mo

  num <- parts$kind == "num"
  if (any(num)){
    d[num]  <- as.integer(if (day_first) parts$a[num] else parts$b[num])
    mo[num] <- as.integer(if (day_first) parts$b[num] else parts$a[num])
  }

  out <- rep(as.Date(NA), length(parts$kind))

  ok <- !is.na(d) & !is.na(mo) & !is.na(parts$y) & mo %in% 1:12 & d %in% 1:31
  if (any(ok)){
    ## as.Date returns NA for impossible dates such as 30 February
    out[ok] <- as.Date(sprintf("%04d-%02d-%02d", parts$y[ok], mo[ok], d[ok]),
                       format = "%Y-%m-%d")
  }

  ser <- parts$kind == "serial" & !is.na(parts$a)
  if (any(ser)) out[ser] <- EXCEL_ORIGIN + floor(parts$a[ser])

  out
}


## Breaks time strings into seconds past midnight, honouring any AM/PM marker.
## `has_seconds` flags the values that actually carried a seconds field.
parse_time_parts <- function(x){
  x <- toupper(trimws(as.character(x)))
  n <- length(x)

  out <- list(secs = rep(NA_real_, n), has_seconds = rep(FALSE, n), kind = rep("bad", n))
  if (n == 0) return(out)

  re_clock <- "^(\\d{1,2}):(\\d{1,2})(:(\\d{1,2})(\\.\\d+)?)?\\s*(([AP])\\.?M\\.?)?$"

  out$kind[is.na(x) | x %in% c("", "NA")] <- "blank"

  m <- out$kind == "bad" & grepl(re_clock, x)
  if (any(m)){
    hh  <- as.integer(sub(re_clock, "\\1", x[m]))
    mi  <- as.integer(sub(re_clock, "\\2", x[m]))
    ss  <- sub(re_clock, "\\4", x[m])
    mer <- sub(re_clock, "\\7", x[m])

    has_ss <- ss != ""
    ss     <- ifelse(ss == "", 0L, suppressWarnings(as.integer(ss)))

    ## 12-hour clock: 12 AM is midnight and 12 PM is noon
    hh <- ifelse(mer == "A" & hh == 12, 0L, hh)
    hh <- ifelse(mer == "P" & hh < 12, hh + 12L, hh)

    valid <- !is.na(hh) & !is.na(mi) & !is.na(ss) & hh < 24 & mi < 60 & ss < 60
    secs  <- ifelse(valid, hh * 3600 + mi * 60 + ss, NA_real_)

    out$secs[m]        <- secs
    out$kind[m]        <- ifelse(valid, "clock", "bad")
    out$has_seconds[m] <- has_ss & valid
  }

  ## A time cell left as General in Excel comes through as a fraction of a day
  m <- out$kind == "bad" & grepl("^0?\\.\\d+$", x)
  if (any(m)){
    out$secs[m]        <- round(as.numeric(x[m]) * 86400)
    out$has_seconds[m] <- TRUE
    out$kind[m]        <- "fraction"
  }

  out
}


## Picks the one day/month convention for a whole exif file. A file is written out by a single
## Excel save, so every ambiguous value in it reads the same way round. Values that can only be
## read one way settle it; failing that the SamplingDate does, since videos are recorded in the
## weeks leading up to the day the cards were collected.
settle_day_first <- function(p_fmd, p_date, sampling_date, context, log_fn = message){

  ambiguous <- any(p_fmd$kind == "num") || any(p_date$kind == "num")
  if (!ambiguous) return(TRUE)   # nothing to settle; both readings are identical

  evidence <- c(day_first_evidence(p_fmd), day_first_evidence(p_date))
  evidence <- unique(evidence[!is.na(evidence)])

  if ("conflict" %in% evidence || length(evidence) > 1){
    stop(context, " mixes day-first and month-first dates (e.g. both 22/02/2023 and ",
         "02/22/2023), so they cannot be read reliably. Please make the DateTime columns ",
         "consistent, or re-generate this exif with Step 1.")
  }

  if (length(evidence) == 1) return(evidence == "day")

  ## Every ambiguous value reads both ways (all days fall on the 12th or earlier), so test
  ## each reading against the SamplingDate
  sampling <- rep(as.Date(NA), length(p_fmd$kind))
  eight    <- grepl("^\\d{8}$", trimws(as.character(sampling_date)))
  if (any(eight)){
    sampling[eight] <- as.Date(trimws(as.character(sampling_date))[eight], format = "%Y%m%d")
  }

  fits <- function(day_first){
    cand <- c(date_from_parts(p_fmd, day_first), date_from_parts(p_date, day_first))
    samp <- rep(sampling, 2)
    ok   <- !is.na(cand) & !is.na(samp)
    if (!any(ok)) return(FALSE)
    all(cand[ok] <= samp[ok] & cand[ok] >= samp[ok] - 120)
  }

  day_fits   <- fits(TRUE)
  month_fits <- fits(FALSE)

  if (day_fits && !month_fits)  return(TRUE)
  if (month_fits && !day_fits)  return(FALSE)

  log_fn(paste0("WARNING: every date in ", context, " could be read either day-first or ",
                "month-first, and the SamplingDate does not settle it. Read as day-first ",
                "(DD/MM/YYYY) — please check this file's dates in the output."))
  TRUE
}


## Reads the Station/SamplingDate/FileModifyDate/Date/Time columns of one exif data frame into
## proper types. FileModifyDate is the column to trust, as Step 1 derives Date and Time from it,
## but Excel drops its seconds while leaving them in Time, so they are taken back from there
## when the two agree to the minute.
read_exif_datetimes <- function(exif, context, log_fn = message){

  halves <- split_datetime(exif$FileModifyDate)
  p_fmd  <- parse_date_parts(halves$date)
  p_date <- parse_date_parts(exif$Date)
  t_fmd  <- parse_time_parts(halves$time)
  t_own  <- parse_time_parts(exif$Time)

  day_first <- settle_day_first(p_fmd, p_date, exif$SamplingDate, context, log_fn)

  fmd_date <- date_from_parts(p_fmd, day_first)
  own_date <- date_from_parts(p_date, day_first)

  ## A whole DateTime left as General in Excel arrives as one serial number, its time of day
  ## in the fraction
  fmd_secs <- t_fmd$secs
  fmd_has  <- t_fmd$has_seconds
  ser <- p_fmd$kind == "serial" & !is.na(p_fmd$a)
  if (any(ser)){
    fmd_secs[ser] <- round((p_fmd$a[ser] - floor(p_fmd$a[ser])) * 86400)
    fmd_has[ser]  <- TRUE
  }

  ## A DateTime with no time of day at all is midnight, as Step 1's own output records it
  no_time <- !is.na(fmd_date) & is.na(fmd_secs) & t_fmd$kind == "blank"
  fmd_secs[no_time] <- 0
  fmd_has[no_time]  <- FALSE

  ## Excel writes "22/2/2023 8:54" but leaves Time as "8:54:09", so put the seconds back
  recover <- !is.na(fmd_secs) & !fmd_has & t_own$has_seconds & !is.na(t_own$secs) &
    floor(fmd_secs / 60) == floor(t_own$secs / 60)
  fmd_secs[recover] <- t_own$secs[recover]

  ## Fall back to the Date and Time columns for rows whose FileModifyDate is unreadable
  usable_fmd <- !is.na(fmd_date) & !is.na(fmd_secs)
  usable_own <- !is.na(own_date) & !is.na(t_own$secs)
  fallback   <- !usable_fmd & usable_own

  final_date <- ifelse(fallback, own_date, fmd_date)
  final_date <- as.Date(final_date, origin = "1970-01-01")
  final_secs <- ifelse(fallback, t_own$secs, fmd_secs)

  unreadable <- !usable_fmd & !usable_own
  if (any(unreadable)){
    shown <- unique(paste0(exif$FileName[unreadable], " (FileModifyDate '",
                           exif$FileModifyDate[unreadable], "', Date '", exif$Date[unreadable],
                           "', Time '", exif$Time[unreadable], "')"))
    stop("Cannot read the DateTime of ", sum(unreadable), " row(s) in ", context, ":\n",
         paste(head(shown, 10), collapse = "\n"),
         if (length(shown) > 10) paste0("\n...and ", length(shown) - 10, " more") else "",
         "\nPlease correct these in the exif, or re-generate it with Step 1.")
  }

  if (any(fallback)){
    log_fn(paste0("WARNING: FileModifyDate was unreadable for ", sum(fallback), " row(s) in ",
                  context, " — used their Date and Time columns instead."))
  }

  ## Flag rows whose own Date column disagrees, which usually means the CSV was hand-edited
  ## in one column only
  disagrees <- !fallback & !is.na(own_date) & own_date != final_date
  if (any(disagrees)){
    log_fn(paste0("WARNING: the Date column disagrees with FileModifyDate for ",
                  sum(disagrees), " row(s) in ", context, " (e.g. ",
                  paste(head(unique(exif$FileName[disagrees]), 5), collapse = ", "),
                  "). FileModifyDate was used."))
  }

  ## Keep SamplingDate as the YYYYMMDD string the later steps expect, even if Excel
  ## reformatted it into a date
  samp <- trimws(as.character(exif$SamplingDate))
  odd  <- !grepl("^\\d{8}$", samp) & !samp %in% c("", "NA")
  if (any(odd)){
    reparsed <- date_from_parts(parse_date_parts(samp), day_first)
    samp[odd] <- ifelse(is.na(reparsed[odd]), samp[odd], format(reparsed[odd], "%Y%m%d"))
  }

  exif$SamplingDate   <- samp
  exif$FileModifyDate <- datetime_from_parts(final_date, final_secs)
  exif$Date <- final_date
  exif$Time <- secs_to_clock(final_secs)

  exif
}


## Seconds past midnight as "%H:%M:%S"
secs_to_clock <- function(secs){
  secs <- as.integer(round(secs))
  ifelse(is.na(secs), NA_character_,
         sprintf("%02d:%02d:%02d", secs %/% 3600L, (secs %% 3600L) %/% 60L, secs %% 60L))
}


## Builds POSIXct values from a Date and seconds past midnight. Assembled from the wall-clock
## components rather than from a seconds count so that the DateTime reads the same in Singapore
## time wherever the app is running, which is not the case on the shinyapps.io servers (UTC).
datetime_from_parts <- function(dates, secs, tz = "Singapore"){
  clock <- secs_to_clock(secs)
  ok    <- !is.na(dates) & !is.na(clock)

  out <- rep(as.POSIXct(NA, tz = tz), length(clock))
  if (any(ok)){
    out[ok] <- as.POSIXct(paste(format(dates[ok], "%Y-%m-%d"), clock[ok]),
                          format = "%Y-%m-%d %H:%M:%S", tz = tz)
  }
  out
}


## Reads a single DateTime typed in by hand, in ISO ("2025-11-13 08:00:00") or in the local
## format ("13/11/2025 8:00 AM"). A bare date means midnight. Returns NA if it cannot be read.
parse_one_datetime <- function(x, day_first = TRUE, tz = "Singapore"){
  halves <- split_datetime(x)
  dates  <- date_from_parts(parse_date_parts(halves$date), day_first)
  times  <- parse_time_parts(halves$time)

  secs <- times$secs
  secs[is.na(secs) & times$kind == "blank"] <- 0

  datetime_from_parts(dates, secs, tz = tz)
}
