## process_light.R
## Core logic for the Light Monitoring report — summarises baseline light levels per station and
## flags monitoring phase hours that exceed them. Called by app.R — do not run this file directly.
##
## Adapted from ../LightR/process_light.R (a top-level script with a VARIABLE CONTROL PANEL of
## hardcoded folder paths, printed plots and a hardcoded output location) into a single entry
## point, run_light_report(), that takes an output directory and a log function.


#### Helper functions ####

## Excel writes a "~$name.xlsx" lock file next to any workbook that is currently open — those are
## not logger exports and must never be read.
list_light_files <- function(path_dir){
  files <- list.files(path_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.xlsx$")
  files[!str_detect(basename(files), "^~\\$")]
}

## Mean, SD and +/- 1 SD bounds of Light, grouped by the columns passed through `...`.
## The lower bound is clamped at 0 because light levels cannot be negative.
summarise_light <- function(data, ...){
  data %>%
    group_by(...) %>%
    summarise(Light_Mean = mean(Light),
              Light_SD   = sd(Light),
              .groups    = "drop") %>%
    mutate(Light_LB = pmax(Light_Mean - Light_SD, 0),
           Light_UB = Light_Mean + Light_SD)
}


#### Main function ####

#' Summarise baseline and monitoring phase light logger data.
#'
#' Both folders are searched recursively for logger exports (.xlsx). Only the night window is
#' analysed: hours from `time_from` (inclusive) through to `time_to` (exclusive), crossing
#' midnight. Baseline light levels are the mean +/- 1 SD per station across that window, and every
#' monitoring hour is flagged against both the baseline mean and its upper bound.
#'
#' @param path_baseline    Folder of baseline phase logger exports (.xlsx, searched recursively).
#' @param path_monitoring  Folder of monitoring phase logger exports (.xlsx, searched recursively).
#' @param output_dir       Directory to write the CSV and plot outputs into.
#' @param time_from        Hour (0-23) the night window starts, e.g. 18 for 6pm. Default 18.
#' @param time_to          Hour (0-23) the night window ends, e.g. 7 for 7am. Default 7.
#' @param excl_dates       Character vector of baseline dates (YYYY-MM-DD) to exclude, or NULL.
#' @param log              A function used for progress messages, e.g. message (default) or a
#'                         Shiny logger.
#'
#' @return Invisibly, a list with: csv_paths, plot_paths, baseline_values, monitoring_results,
#'         plots (named list of ggplot objects) and summary (a one-paragraph exceedance summary).
run_light_report <- function(path_baseline, path_monitoring, output_dir,
                             time_from = 18, time_to = 7, excl_dates = NULL,
                             log = message){

  if (time_from <= time_to) {
    stop("The night window must cross midnight — 'time_from' (", time_from,
         ") must be later than 'time_to' (", time_to, ").")
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ## Night hours are plotted on one continuous axis by pushing post-midnight hours onto a second
  ## dummy day, so 6pm-7am reads left to right instead of wrapping around.
  hour_to_plot_time <- function(hour){
    case_when(
      hour >= time_from ~ as.POSIXct(paste0("2000-01-01 ", hour, ":00:00")),
      hour <  time_to   ~ as.POSIXct(paste0("2000-01-02 ", hour, ":00:00"))
    )
  }


  # ── Baseline light levels ───────────────────────────────────────────────────────────────────────

  baseline_files <- list_light_files(path_baseline)
  if (length(baseline_files) == 0) {
    stop("No .xlsx logger exports found in the baseline folder: ", path_baseline)
  }

  log(paste("Reading", length(baseline_files), "baseline file(s)..."))
  baseline_raw <- bind_rows(lapply(baseline_files, read_light_file)) %>%
    filter(Hour >= time_from | Hour < time_to) %>%
    filter(!Date %in% excl_dates)

  if (nrow(baseline_raw) == 0) {
    stop("No baseline records left after filtering to the ", time_from, ":00-", time_to,
         ":00 window and excluding the dates given.")
  }
  log(paste0("Baseline: ", nrow(baseline_raw), " records across ",
             length(unique(baseline_raw$Station)), " station(s) and ",
             length(unique(baseline_raw$Date)), " date(s)."))

  ## Daily light levels at each station, to check which hours are stable enough to use
  baseline_daily <- baseline_raw %>%
    summarise_light(Station, Date, Hour) %>%
    mutate(HourPlot = hour_to_plot_time(Hour))

  daily_plot <- ggplot(baseline_daily, aes(x = HourPlot, y = Light_Mean, colour = Date)) +
    geom_line() +
    geom_errorbar(aes(ymin = Light_LB, ymax = Light_UB), width = 700) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    facet_wrap(~Station) +
    theme_classic() +
    ggtitle("Daily baseline light levels") +
    xlab("Time") +
    ylab("Light (lux)")

  ## Light levels at each hour, pooled across the whole baseline period
  hourly_values <- baseline_raw %>%
    summarise_light(Station, Hour) %>%
    mutate(HourPlot = hour_to_plot_time(Hour))

  hourly_plot <- ggplot(hourly_values, aes(x = HourPlot, y = Light_Mean)) +
    geom_line() +
    geom_errorbar(aes(ymin = Light_LB, ymax = Light_UB), width = 700) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    facet_wrap(~Station) +
    theme_classic() +
    ggtitle("Baseline light levels at each hour") +
    xlab("Time") +
    ylab("Light (lux)")

  ## Baseline light level per station, across every hour of the night window
  baseline_values <- baseline_raw %>%
    summarise_light(Station) %>%
    rename(Baseline_Mean = Light_Mean, Baseline_SD = Light_SD,
           Baseline_LB   = Light_LB,   Baseline_UB = Light_UB)

  baseline_plot <- ggplot(baseline_values, aes(x = Station, y = Baseline_Mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = Baseline_LB, ymax = Baseline_UB), width = 0.2,
                  position = position_dodge(0.05)) +
    theme_classic() +
    ggtitle("Baseline light levels at each station (across all hours)") +
    ylab("Light (lux)")


  # ── Monitoring phase light levels ───────────────────────────────────────────────────────────────

  monitoring_files <- list_light_files(path_monitoring)
  if (length(monitoring_files) == 0) {
    stop("No .xlsx logger exports found in the monitoring folder: ", path_monitoring)
  }

  log(paste("Reading", length(monitoring_files), "monitoring file(s)..."))
  monitoring_raw <- bind_rows(lapply(monitoring_files, read_light_file)) %>%
    filter(Hour >= time_from | Hour < time_to)

  if (nrow(monitoring_raw) == 0) {
    stop("No monitoring records left after filtering to the ", time_from, ":00-", time_to,
         ":00 window.")
  }

  new_stations <- setdiff(unique(monitoring_raw$Station), unique(baseline_raw$Station))
  if (length(new_stations) > 0) {
    log(paste("WARNING: no baseline data for station(s):", paste(new_stations, collapse = ", "),
              "- their exceedance columns will be blank."))
  }

  log("Comparing monitoring hours against baseline...")
  monitoring_hour <- monitoring_raw %>%
    summarise_light(Station, Date, Hour) %>%
    merge(baseline_values, by = "Station", all.x = TRUE) %>%
    mutate(Exceed_Mean = Light_Mean > Baseline_Mean,
           Exceed_UB   = Light_Mean > Baseline_UB,
           HourPlot    = hour_to_plot_time(Hour))

  monitoring_plot <- ggplot(monitoring_hour) +
    geom_line(aes(x = HourPlot, y = Light_Mean, colour = Date)) +
    geom_errorbar(aes(x = HourPlot, y = Light_Mean, colour = Date,
                      ymin = Light_LB, ymax = Light_UB),
                  width = 700) +
    geom_line(aes(x = HourPlot, y = Baseline_Mean), colour = "black") +
    geom_errorbar(aes(x = HourPlot, y = Baseline_Mean,
                      ymin = Baseline_LB, ymax = Baseline_UB),
                  width = 700, colour = "black") +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    facet_wrap(~Station) +
    theme_classic() +
    ggtitle("Daily light levels (coloured lines) against baseline (black line)") +
    xlab("Time") +
    ylab("Light (lux)")


  # ── Outputs ─────────────────────────────────────────────────────────────────────────────────────

  monitoring_results <- monitoring_hour %>%
    select(Station, Date, Hour, Baseline_Mean, Baseline_SD, Light_Mean, Light_SD,
           Exceed_Mean, Exceed_UB)

  baseline_csv   <- file.path(output_dir, "light_baseline_values.csv")
  monitoring_csv <- file.path(output_dir, "light_monitoring_results.csv")
  write.csv(baseline_values,   baseline_csv,   row.names = FALSE)
  write.csv(monitoring_results, monitoring_csv, row.names = FALSE)

  plots <- list(
    baseline_daily   = daily_plot,
    baseline_hourly  = hourly_plot,
    baseline_station = baseline_plot,
    monitoring_daily = monitoring_plot
  )

  plot_paths <- vapply(names(plots), function(nm){
    out <- file.path(output_dir, paste0("light_", nm, ".png"))
    ggsave(out, plots[[nm]], width = 10, height = 7, dpi = 150)
    out
  }, character(1), USE.NAMES = FALSE)

  ## Hours exceeding baseline, per station and date
  exceed_mean_summ <- monitoring_hour %>%
    filter(Exceed_Mean) %>%
    group_by(Station, Date) %>%
    summarise(Num_Hours = n(), .groups = "drop")

  exceed_ub_summ <- monitoring_hour %>%
    filter(Exceed_UB) %>%
    group_by(Station, Date) %>%
    summarise(Num_Hours = n(), .groups = "drop")

  summary_text <- paste0(
    "From ", min(monitoring_raw$Date), " to ", max(monitoring_raw$Date),
    ", there are a total of ", sum(exceed_mean_summ$Num_Hours),
    " hours exceeding the mean baseline light levels, while there are ",
    sum(exceed_ub_summ$Num_Hours), " hours exceeding the upper bound baseline light levels."
  )

  log(summary_text)
  log(paste("Completed! Outputs saved at", output_dir))

  invisible(list(
    csv_paths          = c(baseline_csv, monitoring_csv),
    plot_paths         = plot_paths,
    baseline_values    = baseline_values,
    monitoring_results = monitoring_results,
    plots              = plots,
    summary            = summary_text
  ))
}
