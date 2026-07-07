## stream_report.R
## Core logic for generating the Stream Inspection Report from aquatic fauna
## survey data and site photos. Called by app.R — do not run this file directly.
##
## Adapted from ../StreamInspection_v1.2/app.R (a top-level script with a
## VARIABLE CONTROL PANEL of hardcoded paths and a hardcoded output location)
## into a single entry point, stream_report(), that takes an output directory
## and a log function.


#### Main function ####

#' Generate a Stream Inspection Report workbook from fauna data and site photos.
#'
#' The fauna datasheet must contain sheets '01 Log' and '02 DataList' with the
#' sampling point names in the Transect column. The photo folder must follow the
#' structure: path_photos_dir / YYYYMMDD / SamplingPoint_YYYYMMDD / photo.jpg,
#' where the sampling point names match those in the fauna datasheet.
#'
#' @param path_fauna_data  Path to the fauna datasheet (.xlsx).
#' @param path_photos_dir  Root photo folder (see structure above).
#' @param inspection_date  Character vector of inspection date(s), YYYY-MM-DD.
#' @param output_dir       Directory to write StreamInspection.xlsx into.
#' @param log              A function used for progress messages, e.g. message
#'                         (default) or a Shiny logger.
#'
#' @return Invisibly returns the path to the generated .xlsx file.
stream_report <- function(path_fauna_data, path_photos_dir, inspection_date,
                          output_dir, log = message){

  #### Load and clean fauna data ####
  log("Reading fauna data...")
  fauna_data <- read.xlsx(path_fauna_data, sheet = "02 DataList") %>%
    rename(Date = `Date.(DD-MM-YY)`,
           Time = `Time.(24h.-.HHMM)`,
           Cycle = `Cycle.(1/2/3)`,
           SurveyType = `Survey.type.(AM.BHM/AM.OBB/Aquatic.etc)`) %>%
    mutate(Date = convertToDate(Date))

  fauna_log <- read.xlsx(path_fauna_data, sheet = "01 Log") %>%
    mutate(Date = convertToDate(Date),
           Activity = tolower(Activity)) %>%
    filter(Date %in% as.Date(inspection_date)) %>%
    filter(grepl(Activity, pattern = "aquatic")) %>%
    mutate(Time.in = sprintf("%04d", as.numeric(Time.in)),
           Time.out = sprintf("%04d", as.numeric(Time.out)),
           TimeInOut = paste0(Time.in, "-", Time.out),
           Date = format(as.Date(Date, format = "%Y%m%d"), "%d-%b-%Y")) %>%
    arrange(Date, Time.in)
  date_col <- paste(fauna_log$Date, collapse = ", ")
  time_col <- paste(fauna_log$TimeInOut, collapse = ", ")

  aquatic <- fauna_data %>%
    filter(SurveyType %in% c("AM Aquatic", "PM Aquatic")) %>%
    filter(Date %in% inspection_date) %>%
    mutate(Transect = str_replace_all(Transect, "_", ""),
           Quantity = case_when(Quantity %in% c("TNTC", "TMTC", "tmtc") ~ "100",
                                is.na(Quantity) ~ "1",
                                .default = Quantity),
           Quantity = as.numeric(Quantity)) %>%
    group_by(Transect, Scientific.name, Common.name) %>%
    summarise(Quantity = sum(Quantity), .groups = "drop") %>%
    mutate(FaunaLine = paste0(Common.name, " (", Scientific.name, ")", " x ", Quantity)) %>%
    group_by(Transect) %>%
    summarise(FaunaLine = paste(FaunaLine, collapse = ",\n"))

  #### Locate photos ####
  log("Locating site photos...")
  get_photo_paths <- function(date){
    photo_dir <- file.path(path_photos_dir, str_replace_all(date, "-", ""))
    data.frame(Paths = list.files(photo_dir, pattern = ".jpg|.jpeg|.png|.JPG|.JPEG|.PNG",
                                  full.names = T, recursive = T))
  }

  photo_data <- lapply(inspection_date, get_photo_paths) %>%
    bind_rows() %>%
    mutate(Dir = basename(dirname(Paths))) %>%
    separate(Dir, into = c("Transect", "Date"), sep = "_") %>%
    merge(aquatic, by = "Transect", all.x = T, all.y = T) %>%
    mutate(Date = date_col,
           Time = time_col,
           Remarks = "None",
           Photos = NA) %>%
    group_by(Transect) %>%
    mutate(PhotoNum = row_number())

  table <- photo_data %>%
    mutate(FaunaLine = case_when(is.na(FaunaLine) ~ "None", .default = FaunaLine)) %>%
    select(Transect, Date, Time, FaunaLine, Photos) %>%
    unique() %>%
    rename("Time (h)" = Time,
           "Fauna observed" = FaunaLine,
           "Sampling point" = Transect) %>%
    t() %>%
    as.data.frame() %>%
    bind_rows(as.data.frame(matrix(data = NA, nrow = max(photo_data$PhotoNum), ncol = ncol(.))))
  rownames(table) <- c(head(rownames(table), -1), "Remarks")

  #### Build the workbook ####
  log(paste("Building report with", nrow(photo_data), "photos across",
            length(unique(photo_data$Transect)), "sampling point(s)..."))
  wb <- createWorkbook()
  addWorksheet(wb, 1)

  unique_surveyor = unique(unlist(strsplit(fauna_log$Surveyor, " ")))

  writeData(wb, 1, "STREAM INSPECTION", startRow = 1)
  writeData(wb, 1, "Inspection round:", startRow = 3)
  writeData(wb, 1, paste("Date(s) of inspection: ", paste(unique(fauna_log$Date), collapse = ", ")),
            startRow = 4)
  writeData(wb, 1, "Project title:", startRow = 5)
  writeData(wb, 1,
            paste("Conducted by:", paste(unique_surveyor, collapse = ", ")),
            startRow = 6)
  writeData(wb, 1, paste("Weather conditions:", paste(fauna_log$Weather, collapse = "; ")),
            startRow = 7)

  writeData(wb, 1, table, rowNames = T, colNames = F, , startRow = 9)

  for (i in 1:nrow(photo_data)){

    path <- photo_data$Paths[i]

    if (is.na(path)) next

    transect <- photo_data$Transect[i]
    photo_num <- photo_data$PhotoNum[i]
    startcol <- match(transect, table[1,]) + 1

    # Check and convert to landscape picture
    img <- image_read(path)
    info <- image_info(img)
    if (info$width > info$height){
      img_landscape <- image_rotate(img, 90)
      image_write(img_landscape, path)
    }

    # Scale the width to a fixed height
    img <- image_read(path)
    info <- image_info(img)
    scaled_width <- 2/info$width*info$height

    # Insert image in excel sheet
    insertImage(wb, 1, path, startRow = 12+photo_num, startCol = startcol,
                height = 2, width = scaled_width, units = "in")
  }


  ## Format the excel sheet
  num_col <- ncol(table) + 1
  num_row <- nrow(table)

  setColWidths(wb, 1, cols = 1, widths = "auto")
  setColWidths(wb, 1, cols = 2:num_col, widths = 55)
  setRowHeights(wb, 1, rows = 13:(13+max(photo_data$PhotoNum)-1), heights = 150)

  for (i in 1:num_col){
    mergeCells(wb, 1, cols = i, rows = 13:(13+max(photo_data$PhotoNum)-1))
  }

  addStyle(wb, 1, rows = 9:(8+num_row), cols = 1:num_col, gridExpand = T,
           style = createStyle(valign = "center", wrapText = T, border = "TopBottomLeftRight"))
  addStyle(wb, 1, rows = c(9:(8+num_row)), cols = 1,
           style = createStyle(valign = "center", textDecoration="Bold", border="TopBottomLeftRight"))
  addStyle(wb, 1, rows = 9, cols = 2:num_col,
           style = createStyle(halign = "center", textDecoration="Bold", border="TopBottomLeftRight"))
  addStyle(wb, 1, rows = 10:11, cols = 2:num_col, gridExpand = T,
           style = createStyle(halign = "center", border = "TopBottomLeftRight"))
  addStyle(wb, 1, rows = 1, cols = 1,
           style = createStyle(textDecoration = "Bold"))

  #### Save ####
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  path_report <- file.path(output_dir, "StreamInspection.xlsx")
  saveWorkbook(wb, path_report, overwrite = T)

  log(paste("Completed. Stream report saved in", path_report))
  invisible(path_report)
}
