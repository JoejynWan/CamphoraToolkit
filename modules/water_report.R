library(tools)
library(openxlsx)
library(tidyverse)


#### Fixed variables ####
COLUMN_RENAME_MAP <- c(
  "DATE..M.d.yyyy." = "Date", 
  "TIME..h.mm.ss.tt." = "Time", 
  "DEPTH.M" = "Depth", 
  "SPCOND.S.CM" = "Conductivity", 
  "ODO.MG.L" = "DissolvedOxygen",
  "PH" = "pH",
  "SAL.PSU" = "Salinity", 
  "TEMP.C" = "Temperature", 
  "TURBIDITY.NTU" = "Turbidity"
)

COLUMN_OUTPUT_NAMES <- c(
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
  Turbidity = "Turbidity (NTU)"
)

DEPTH_THRES <- 2


#### Main Function ####
in_situ <- function(path_input, time_threshold, date_format = "%d/%m/%Y %I:%M:%S %p"){
  
  raw_lines <- readLines(path_input, warn = T) %>%
    iconv("latin1", "ASCII", sub = "")
  header_positions <- which(str_detect(raw_lines, "FILE NAME"))
  
  process_block <- function(header_pos){
    block <- raw_lines[header_pos:(header_pos+1)] %>%
      read.table(text = ., header = T, stringsAsFactors = F, sep = ',')
  }
  
  all_blocks <- lapply(header_positions, process_block) %>%
    bind_rows() %>%
    select(where(~ !all(is.na(.) | str_trim(as.character(.)) == ""))) %>%
    mutate(across(where(is.character), str_trim)) %>%
    rename(any_of(setNames(names(COLUMN_RENAME_MAP), COLUMN_RENAME_MAP))) %>%
    mutate(DateTime = paste(Date, Time, sep = " "), 
           DateTime = as.POSIXct(DateTime, format = date_format)) %>%
    arrange(DateTime)
  
  output_data <- all_blocks %>%
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
           Depth = case_when(Depth < DEPTH_THRES ~ "Near Water Surface", 
                             .default = as.character(Depth))) %>%
    select(PointNo, Date, Time, Depth, Weather, Conductivity, DissolvedOxygen, pH, Salinity, 
           Temperature, Turbidity) %>%
    rename(any_of(setNames(names(COLUMN_OUTPUT_NAMES), COLUMN_OUTPUT_NAMES)))
  
  
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
