library(tools)
library(openxlsx)
library(tidyverse)


#### Fixed variables ####
## A standard column can arrive under more than one raw header name, because Kor renames columns
## between software versions and labels them with whichever unit the sonde was configured for. List
## every accepted raw name against the standard name; raw names are written as make.names() mangles
## them, i.e. every non-alphanumeric character becomes a dot.
WATER_COLUMN_RENAME_MAP <- list(
  Date            = c("DATE..M.d.yyyy.", "DATE..dd.MM.yyyy."),
  Time            = c("TIME..h.mm.ss.tt.", "TIME..HH.mm.ss."),
  Depth           = c("DEPTH.M"),
  Conductivity    = c("SPCOND.S.CM"),
  DissolvedOxygen = c("ODO.MG.L"),
  pH              = c("PH"),
  Salinity        = c("SAL.PSU"),
  Temperature     = c("TEMP.C"),
  Turbidity       = c("TURBIDITY.NTU", "TURBIDITY.FNU")
)

## Turbidity is left without its unit here because the sonde reports either NTU or FNU; in_situ()
## fills in whichever unit the source file used.
WATER_COLUMN_OUTPUT_NAMES <- c(
  PointNo = "Point No.",
  Date = "Measurement Date",
  Time = "Measurement Time", 
  Depth = "Measurement Depth", 
  Weather = "Weather Condition", 
  Conductivity = "Conductivity (µS/cm)", 
  DissolvedOxygen = "Dissolved Oxygen (mg/L)", 
  pH = "pH Value", 
  Salinity = "Salinity (PSU)", 
  Temperature = "Temperature (°C)", 
  Turbidity = "Turbidity"
)

WATER_DEPTH_THRES <- 2

## Kor writes its date/time pattern into the header itself, e.g. "DATE (dd/MM/yyyy)". These tokens
## translate that pattern into the strptime format that as.POSIXct() wants, so a failed parse can
## tell the user exactly which date_format to pass. Longest tokens first, matched in one pass.
WATER_DATE_TOKEN_MAP <- c(
  "yyyy" = "%Y", "yy"   = "%y", 
  "MMMM" = "%B", "MMM"  = "%b", "MM" = "%m", "M" = "%m", 
  "dddd" = "%A", "ddd"  = "%a", "dd" = "%d", "d" = "%d", 
  "HH"   = "%H", "hh"   = "%I", "H"  = "%H", "h" = "%I", 
  "mm"   = "%M", "m"    = "%M", 
  "ss"   = "%S", "s"    = "%S", 
  "tt"   = "%p", "t"    = "%p"
)


#### Helper functions ####
## Newer Kor exports are UTF-16 with a byte-order mark, older ones are plain ANSI. readLines() reads
## the former as one-character lines unless the connection is told the encoding, so sniff the BOM.
water_read_export_lines <- function(path_input){
  bom <- readBin(path_input, "raw", n = 2)
  enc <- if (identical(bom, as.raw(c(0xFF, 0xFE)))) "UTF-16LE" 
         else if (identical(bom, as.raw(c(0xFE, 0xFF)))) "UTF-16BE" 
         else "native.enc"

  con <- file(path_input, encoding = enc)
  on.exit(close(con))

  readLines(con, warn = enc == "native.enc") %>%
    iconv("latin1", "ASCII", sub = "")
}

## rename(any_of()) wants a c(new_name = raw_name) vector; duplicated new names are fine because
## only one of the accepted raw names is ever present in a given file.
water_column_rename_vector <- function(){
  setNames(unlist(WATER_COLUMN_RENAME_MAP, use.names = FALSE), 
           rep(names(WATER_COLUMN_RENAME_MAP), lengths(WATER_COLUMN_RENAME_MAP)))
}

## Attached to every error raised once the file has been read: the header row(s) exactly as they
## appear in the export, plus the names R imported them as (make.names() turns every non-
## alphanumeric character into a dot). A header the rename map does not cover can then be spotted
## from the error alone, without opening the file.
water_header_report <- function(header_lines, imported_names){
  c("  Header row(s) in the file:", 
    paste0("    Block ", seq_along(header_lines), ": ", str_trim(header_lines)), 
    "  Column names as R imported them:", 
    paste0("    ", paste(imported_names, collapse = ", "))) %>%
    paste(collapse = "\n")
}

water_kor_to_r_format <- function(pattern){
  if (is.na(pattern)) return(NA_character_)
  str_replace_all(pattern, paste(names(WATER_DATE_TOKEN_MAP), collapse = "|"), 
                  function(tokens) unname(WATER_DATE_TOKEN_MAP[tokens]))
}

## Built when no row's date/time could be parsed: shows what the file actually contains and, where
## the header declares a pattern that works, the exact date_format argument to use instead.
water_date_format_message <- function(header_line, data, date_format){
  declared_date <- str_match(header_line, "DATE\\s*\\(([^)]*)\\)")[, 2]
  declared_time <- str_match(header_line, "TIME\\s*\\(([^)]*)\\)")[, 2]
  suggested     <- str_trim(paste(water_kor_to_r_format(declared_date),
                                  water_kor_to_r_format(declared_time)))
  example       <- paste(data$Date[1], data$Time[1])
  
  msg <- c(
    paste0("Could not read any date/time using date_format = \"", date_format, "\"."), 
    paste0("  Date format declared in the file header: ", 
           if (is.na(declared_date)) "not stated" else declared_date), 
    paste0("  Time format declared in the file header: ", 
           if (is.na(declared_time)) "not stated" else declared_time), 
    paste0("  First date/time value in the file:       ", example)
  )
  
  works <- !str_detect(suggested, "NA") && !is.na(as.POSIXct(example, format = suggested))
  c(msg, if (works) paste0("  Re-run with date_format = \"", suggested, "\"") 
         else       "  Adjust the date_format argument to match the value shown above.") %>%
    paste(collapse = "\n")
}


#### Main Function ####
in_situ <- function(path_input, time_threshold, date_format = "%d/%m/%Y %I:%M:%S %p"){
  
  raw_lines <- water_read_export_lines(path_input)
  header_positions <- which(str_detect(raw_lines, "FILE NAME"))
  
  if (length(header_positions) == 0)
    stop("No column header row (one containing \"FILE NAME\") found in ", basename(path_input), 
         ". Is this a Kor/EXO measurement file export?", call. = FALSE)
  
  ## A block is one header row plus every measurement row under it, down to the next header. Lines
  ## not starting with a digit are export metadata (MEAN VALUE:, SENSOR SERIAL NUMBER:, blanks)
  ## rather than measurements, so drop them; the time column always starts the row with a digit.
  process_block <- function(i){
    start <- header_positions[i]
    end   <- if (i < length(header_positions)) header_positions[i+1] - 1 else length(raw_lines)
    body  <- if (end > start) raw_lines[(start+1):end] else character(0)
    
    read.table(text = c(raw_lines[start], body[str_detect(body, "^\\s*\\d")]), 
               header = T, stringsAsFactors = F, sep = ',')
  }
  
  all_blocks <- lapply(seq_along(header_positions), process_block) %>%
    bind_rows() %>%
    select(where(~ !all(is.na(.) | str_trim(as.character(.)) == ""))) %>%
    mutate(across(where(is.character), str_trim))

  ## Read the turbidity unit off the raw header before renaming loses it: the sonde reports NTU or
  ## FNU depending on the sensor fitted, and the output column is labelled with whichever was used.
  turbidity_unit <- intersect(WATER_COLUMN_RENAME_MAP$Turbidity, names(all_blocks))[1] %>%
    str_remove("^TURBIDITY[.]")

  ## Captured before rename(): these are the raw names the rename map has to match, and the ones
  ## worth showing when anything downstream cannot find a standard column.
  header_report <- water_header_report(raw_lines[header_positions], names(all_blocks))

  ## Everything past this point works on the renamed standard columns, so an unexpected failure
  ## (typically "object 'Time' not found" when a header slipped past the rename map) only makes
  ## sense with the file's own headers attached.
  with_header_context <- function(expr){
    tryCatch(expr, error = function(e) 
      stop(conditionMessage(e), "\n", header_report, call. = FALSE))
  }

  all_blocks <- with_header_context(
    all_blocks %>%
      rename(any_of(water_column_rename_vector()))
  )

  if (nrow(all_blocks) == 0)
    stop("No measurement rows found in ", basename(path_input), ".", call. = FALSE)
  
  missing_cols <- setdiff(names(WATER_COLUMN_RENAME_MAP), names(all_blocks))
  if (length(missing_cols) > 0)
    stop("Column(s) not found in ", basename(path_input), ": ", 
         paste(missing_cols, collapse = ", "), "\n", 
         header_report, "\n", 
         "  Add each missing column's name in this file to WATER_COLUMN_RENAME_MAP in ",
         "water_report.R.",
         call. = FALSE)
  
  all_blocks <- with_header_context(
    all_blocks %>%
      mutate(DateTime = paste(Date, Time, sep = " "), 
             DateTime = as.POSIXct(DateTime, format = date_format)) %>%
      arrange(DateTime)
  )
  
  if (all(is.na(all_blocks$DateTime)))
    stop(water_date_format_message(raw_lines[header_positions[1]], all_blocks, date_format), 
         call. = FALSE)
  
  output_names <- WATER_COLUMN_OUTPUT_NAMES
  output_names["Turbidity"] <- paste0(output_names["Turbidity"], " (", turbidity_unit, ")")

  output_data <- with_header_context(
    all_blocks %>%
      select(DateTime, Date, Time, Depth, Conductivity, DissolvedOxygen, pH, Salinity,
             Temperature, Turbidity) %>%
      mutate(time_diff = as.numeric(difftime(DateTime, lag(DateTime), units = "secs")), 
             group_id = cumsum(is.na(time_diff) | time_diff >= time_threshold*60)) %>%
      group_by(group_id) %>%
      summarise(
        DateTime = first(DateTime),
        Date = first(Date), 
        Time = first(Time), 
        across(c(Depth, Conductivity, DissolvedOxygen, pH, Salinity, Temperature, Turbidity),
               ~ signif(mean(.x, na.rm = TRUE), digits = 3)),
        .groups = "drop"
      ) %>%
      filter(!is.nan(Depth)) %>%
      mutate(PointNo = row_number(), 
             Weather = "", 
             Depth = case_when(Depth < WATER_DEPTH_THRES ~ "Near Water Surface", 
                               .default = as.character(Depth))) %>%
      select(PointNo, Date, Time, Depth, Weather, Conductivity, DissolvedOxygen, pH, Salinity, 
             Temperature, Turbidity) %>%
      rename(any_of(setNames(names(output_names), output_names)))
  )
  
  
  #### Save out data as workbook ####
  ## Styles 
  title_style <- createStyle(fontName = "Aptos Narrow", fontSize = 14, fontColour = "#000000", 
                             textDecoration = "bold", halign = "center", valign = "center")
  
  meta_label_style <- createStyle(fontName = "Aptos Narrow", fontSize = 11, textDecoration = "bold")
  
  header_style <- createStyle(fontName = "Aptos Narrow", fontSize = 11, textDecoration = "bold",
                              halign = "center", valign = "center", wrapText = TRUE, fgFill="#D9D9D9",
                              border = "TopBottomLeftRight", borderColour = "#000000")
  
  data_style_1dp <- createStyle(fontName = "Aptos Narrow", fontSize = 11, 
                                halign = "center", valign = "center",
                                border = "TopBottomLeftRight", borderColour = "#000000", 
                                numFmt = "0.0")
  
  data_style_2dp <- createStyle(fontName = "Aptos Narrow", fontSize = 11, 
                                halign = "center", valign = "center",
                                border = "TopBottomLeftRight", borderColour = "#000000", 
                                numFmt = "0.00")
  
  data_style_text <- createStyle(fontName = "Aptos Narrow", fontSize = 11, 
                                 halign = "center", valign = "center",
                                 border = "TopBottomLeftRight", borderColour = "#000000")
  
  note_label_style <- createStyle(fontName = "Aptos Narrow", fontSize = 11, textDecoration = "bold")
  
  note_text_style <- createStyle(fontName = "Aptos Narrow", fontSize = 11, wrapText = TRUE)
  
  ## Create workbook
  wb <- createWorkbook()
  ws <- "In-Situ Measurements"
  addWorksheet(wb, ws)
  
  ## Add title
  mergeCells(wb, ws, cols = 1:11, rows = 1)
  writeData(wb, ws, "In-Situ Measurement Worksheet", startCol = 1, startRow = 1)
  addStyle(wb, ws, title_style, rows = 1, cols = 1)
  setRowHeights(wb, ws, rows = 1, heights = 30)
  
  ## Add metadata
  meta_layout <- list(
    list(row = 3, label1 = "Project Name:", label2  = "Staff Name:", label3 = "Equipment Name:"),
    list(row = 4, label1 = "Project Number:", label2  = "Email:", label3 = "Equipment Model:"),
    list(row = 5, label1 = "Site Location:",  label2  = "Phone Number:", label3 = "Calibration Date:")
  )
  
  for (m in meta_layout){
    writeData(wb, ws, m$label1, startCol = 1, startRow = m$row)
    writeData(wb, ws, m$label2, startCol = 5, startRow = m$row)
    writeData(wb, ws, m$label3, startCol = 9, startRow = m$row)
    
    addStyle(wb, ws, meta_label_style, rows = m$row, cols = c(1,5,9))
  }
  
  ## Add data
  DATA_START_ROW = 8
  writeData(wb, ws, output_data, startRow = DATA_START_ROW-1, startCol = 1,
            headerStyle = header_style, borders = "all", borderStyle = "thin")
  
  text_cols <- c(1, 2, 3, 5)              # point no, date, time, weather
  numeric_cols_1dp <- c(6, 10)  # condal, temp
  numeric_cols_2dp <- c(4, 7, 8, 9, 11)  # depth, DO, pH, sal, turb
  
  addStyle(wb, ws, data_style_1dp, rows = DATA_START_ROW:(DATA_START_ROW + nrow(output_data) - 1),
           cols = numeric_cols_1dp, gridExpand = TRUE)
  addStyle(wb, ws, data_style_2dp, rows = DATA_START_ROW:(DATA_START_ROW + nrow(output_data) - 1),
           cols = numeric_cols_2dp, gridExpand = TRUE)
  addStyle(wb, ws, data_style_text, rows = DATA_START_ROW:(DATA_START_ROW + nrow(output_data) - 1),
           cols = text_cols, gridExpand = TRUE)
  
  ## Add note
  NOTE_ROW = DATA_START_ROW + nrow(output_data) + 2
  NOTE_TEXT <- paste0(
    "1. Weather condition (described \"Dry\" or \"Wet\"). Dry weather conditions are defined as ",
    "after a continuous 48-hour period of no-rain, and wet weather conditions are defined as a ",
    "rainfall event having more than 10 mm of rainfall, with samples to be collected within 3 hours ", 
    "after the rain stops."
  )
  
  writeData(wb, ws, "Note", startCol = 1, startRow = NOTE_ROW)
  addStyle(wb, ws, note_label_style, rows = NOTE_ROW, cols = 1)
  
  mergeCells(wb, ws, cols = 1:11, rows = NOTE_ROW + 1)
  writeData(wb, ws, NOTE_TEXT, startCol = 1, startRow = NOTE_ROW + 1)
  addStyle(wb, ws, note_text_style, rows = NOTE_ROW + 1, cols = 1)
  setRowHeights(wb, ws, rows = NOTE_ROW + 1, heights = 40)
  setColWidths(wb, ws, cols = 1:11, widths = 20)
  
  ## Export and save
  path_output <- paste0(file_path_sans_ext(path_input), "_clean.xlsx")
  saveWorkbook(wb, path_output, overwrite = TRUE)
  cat("Completed! In situ data cleaned and saved at:", path_output)

}
