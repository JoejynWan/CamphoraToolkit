library(openxlsx)
library(tidyverse)


#### Main function for Generating the noise report ####
noise_report <- function(location, monitoring_pt, path_noise, path_calibration) {

  ## Clean and calculate noise for Leq 1h and Leq 24h
  leq5min <- read.csv(path_noise, check.names = FALSE) %>%
    rename(StartTime = `Start Time`,
           EndTime = `End Time`, 
           LEQ5MIN = `Leq (LAeq)`) %>%
    mutate(DateTime = as.POSIXct(StartTime, format="%d/%m/%Y %I:%M:%S %p", tz="Asia/Singapore")) %>%
    mutate(LEQ5MIN = case_when(LEQ5MIN == 0 ~ NA, .default = LEQ5MIN))
  
  leq1h <- leq5min %>%
    group_by(DateHour = floor_date(DateTime, '1 hour')) %>% 
    summarize(LEQ1H = round(10*log10(mean(10^(LEQ5MIN/10), na.rm = TRUE)), 1)) %>%
    mutate(LEQ1H = case_when(is.nan(LEQ1H) ~ NA, .default = LEQ1H))
  
  leq12h <- leq5min %>%
    group_by(DateHour = floor_date(DateTime - hours(7), '12 hours') + hours(7)) %>% 
    summarize(LEQ12H = round(10*log10(mean(10^(LEQ5MIN/10), na.rm = TRUE)), 1)) %>%
    mutate(LEQ12H = case_when(is.nan(LEQ12H) ~ NA, .default = LEQ12H))
  
  leq_all <- leq5min %>%
    mutate(DateHour = as.POSIXct(format(DateTime, format = "%Y-%m-%d %H:00")), 
           Minute = format(DateTime, format = "%M")) %>%
    select(DateHour, Minute, LEQ5MIN) %>%
    pivot_wider(id_cols = DateHour, names_from = Minute, values_from = LEQ5MIN) %>%
    select(DateHour, "00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55") %>%
    merge(leq1h, by = "DateHour", all.x = T) %>%
    merge(leq12h, by = "DateHour", all.x = T) %>%
    mutate(Date = format(DateHour, "%d-%b-%Y"), 
           Time = format(DateHour, "%H:%M")) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~replace_na(., ""))) %>%
    select(Date, Time, everything()) %>%
    arrange(DateHour) %>%
    select(-DateHour) %>%
    rename("LAeq 1 hour" = LEQ1H, 
           "LAeq 12 hour" = LEQ12H)
    
  ## Get calibration info
  calibration <- read.csv(path_calibration)
  
  meter_num <- calibration[which(calibration[,1] == "Instrument"), 2]
  cali_set1_time <- calibration[which(calibration[,1] == "Time")[1], 2]
  cali_set1_level <- calibration[which(calibration[,1] == "Level")[1], 2]
  cali_set1_offset <- calibration[which(calibration[,1] == "Offset")[1], 2]
  cali_set2_time <- calibration[which(calibration[,1] == "Time")[2], 2]
  cali_set2_level <- calibration[which(calibration[,1] == "Level")[2], 2]
  cali_set2_offset <- calibration[which(calibration[,1] == "Offset")[2], 2]
  
  if (as.POSIXct(cali_set1_time, format = "%d/%m/%Y") < min(leq5min$DateTime)){
    cali_before_time <- cali_set1_time
    cali_before_level_offset <- paste0(cali_set1_level, ", ", cali_set1_offset)
    cali_after_time <- cali_set2_time
    cali_after_level_offset <- paste0(cali_set2_level, ", ", cali_set2_offset)
    
  } else {
    cali_before_time <- ""
    cali_before_level_offset <- ""
    cali_after_time <- cali_set1_time
    cali_after_level_offset <- paste0(cali_set1_level, ", ", cali_set1_offset)
  }
  
  info <- matrix(
    c("Location:", location,
      "Monitoring Point:", monitoring_pt, 
      "Meter Serial Number:", meter_num,
      "Calibration Before Survey:", cali_before_time, 
      "Calibration Level and Offset:", cali_before_level_offset,
      "Calibration After Survey:", cali_after_time, 
      "Calibration Level and Offset:", cali_after_level_offset), 
    nrow = 7, ncol = 2, byrow = T)
  
  ## Generate the max values for summary table
  leq5min_max <- leq5min %>%
    mutate(Period = case_when(hour(DateTime) >= 7 & hour(DateTime) < 19 ~ "7am - 7pm",
                              hour(DateTime) >= 19 & hour(DateTime) < 22 ~ "7pm - 10pm",
                              TRUE ~ "10pm - 7am"),
           PeriodDate = if_else(hour(DateTime) < 7,
                                as.Date(DateTime, tz = "Asia/Singapore") - days(1),
                                as.Date(DateTime, tz = "Asia/Singapore"))) %>%
    group_by(PeriodDate, Period) %>%
    summarise(LEQ5MIN_MAX = max(LEQ5MIN, na.rm = T), .groups = "drop") %>%
    select(PeriodDate, Period, LEQ5MIN_MAX)

  leq1h_max <- leq1h %>%
    mutate(Period = case_when(hour(DateHour) >= 7 & hour(DateHour) < 19 ~ "7am - 7pm",
                              hour(DateHour) >= 19 & hour(DateHour) < 22 ~ "7pm - 10pm",
                              TRUE ~ "10pm - 7am"),
           PeriodDate = if_else(hour(DateHour) < 7,
                                as.Date(DateHour, tz = "Asia/Singapore") - days(1),
                                as.Date(DateHour, tz = "Asia/Singapore"))) %>%
    group_by(PeriodDate, Period) %>%
    summarise(LEQ1H_MAX = max(LEQ1H, na.rm = T), .groups = "drop")%>%
    select(PeriodDate, Period, LEQ1H_MAX)

  leq12h_periods <- leq12h %>%
    mutate(Period = case_when(hour(DateHour) == 7  ~ "7am - 7pm",
                              hour(DateHour) == 19 ~ "7pm - 10pm"),
           PeriodDate = as.Date(DateHour, tz = "Asia/Singapore"))
  
  leq12h_periods_all <- leq12h_periods %>%
    bind_rows(
      leq12h_periods %>%
        filter(Period == "7pm - 10pm") %>%
        mutate(Period = "10pm - 7am")
    ) %>%
    arrange(PeriodDate, Period) %>%
    select(PeriodDate, Period, LEQ12H)
  
  summ_table <- merge(leq5min_max, leq1h_max, by = c("PeriodDate", "Period")) %>%
    merge(leq12h_periods_all, by = c("PeriodDate", "Period")) %>% 
    pivot_longer(cols = c(LEQ5MIN_MAX:LEQ12H), names_to = "Metric", values_to = "Values") %>%
    pivot_wider(id_cols = c(PeriodDate, Metric), names_from = Period, values_from = Values) %>%
    rename(Date = PeriodDate) %>%
    mutate(across(everything(), ~replace(., is.infinite(.), NA))) %>%
    mutate(Metric = case_when(Metric == "LEQ5MIN_MAX" ~ "LAeq 5 min", 
                              Metric == "LEQ1H_MAX" ~ "LAeq 1 hour", 
                              Metric == "LEQ12H" ~ "LAeq 12 hour"), 
           Day = format(Date, "%A")) %>%
    select(Day, Date, Metric, `7am - 7pm`, `7pm - 10pm`, `10pm - 7am`) 
    
  
  #### Export as xlsx ####
  wb <- createWorkbook()
  addWorksheet(wb, "Noise Data")
  addWorksheet(wb, "Summary Table")
  
  ## Write Noise Data Tab
  writeData(wb, sheet = "Noise Data", x = info, startRow = 1, startCol = 1, colNames = F)
  
  start_row_leq <- nrow(info) + 2
  writeData(wb, sheet = "Noise Data", x = leq_all, startRow = start_row_leq, startCol = 1)
  
  # Merge cells in leq12h
  row_nums_start <- c(1, which(!leq_all$`LAeq 12 hour`=="")) + start_row_leq
  row_nums_end <- c(which(!leq_all$`LAeq 12 hour`=="")-1, nrow(leq_all)) + start_row_leq
  row_nums <- data.frame(start_row = row_nums_start, 
                         end_row = row_nums_end)
  
  leq12h_col <- which(names(leq_all) == "LAeq 12 hour")
                      
  for (i in 1:nrow(row_nums)) {
    if (row_nums$start_row[i] < row_nums$end_row[i]) {
      mergeCells(wb, sheet = "Noise Data", 
                 cols = leq12h_col, 
                 rows = row_nums$start_row[i]:row_nums$end_row[i])
    }
  }
  
  # Add styles
  header_style <- createStyle(textDecoration = "bold")
  border_style <- createStyle(border = "TopBottomLeftRight", halign = "center", valign = "center")
  
  addStyle(wb, sheet = "Noise Data", style = header_style, 
           cols = 1:ncol(leq_all), rows = start_row_leq, gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = "Noise Data", style = header_style, 
           cols = 1, rows = 1:nrow(info), gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = "Noise Data", style = border_style, cols = 1:ncol(leq_all), 
           rows = start_row_leq:(nrow(leq_all) + start_row_leq), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet = "Noise Data", cols = c(1:2, leq12h_col), widths = "auto")
  
  ## Write Summary Table Tab
  writeData(wb, sheet = "Summary Table", x = summ_table, startRow = 1, startCol = 1)
  
  # Merge Day and Date cells
  nrow_summ <- nrow(summ_table)
  
  date_grouped <- summ_table %>%
    mutate(row_num = row_number() + 1) %>%
    mutate(group = cumsum(Day != lag(Day, default = ""))) %>%
    summarise(start_row = min(row_num), end_row = max(row_num), .by = c(group, Day))
  
  for (i in 1:nrow(date_grouped)) {
    if (date_grouped$start_row[i] < date_grouped$end_row[i]) {
      mergeCells(wb, sheet = "Summary Table", 
                 cols = 1, 
                 rows = date_grouped$start_row[i]:date_grouped$end_row[i])
    }
      
    if (date_grouped$start_row[i] < date_grouped$end_row[i]) {
      mergeCells(wb, sheet = "Summary Table", 
                 cols = 2, 
                 rows = date_grouped$start_row[i]:date_grouped$end_row[i])
    }
  }
  
  # Add styles
  addStyle(wb, sheet = "Summary Table", style = header_style, 
           cols = 1:ncol(summ_table), rows = 1, gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = "Summary Table", style = border_style, cols = 1:ncol(summ_table), 
           rows = 1:(nrow(summ_table)+1), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet = "Summary Table", cols = 3, widths = "auto")
  
  ## Save out workbook
  path_out <- file.path(dirname(path_noise),
                        paste0("NoiseReport_", location, "_", monitoring_pt, ".xlsx"))
  saveWorkbook(wb, path_out, overwrite = TRUE)
  cat("Completed! File saved at", path_out)
  
}
