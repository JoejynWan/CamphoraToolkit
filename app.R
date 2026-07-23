## app.R
## Camphora Toolkit Hub — unified Shiny front-end.
## Integrates Camera Trap Processing, Abiotic Monitoring, Fauna Impact Assessment, Arbo Report,
## Stream Inspection, Bat Recording Processing, and Flora Photo Filing alongside the project
## directory hub.
##
## Runs locally via launcher: shiny::runGitHub("CamphoraToolkit", "JoejynWan")
## Or directly:               shiny::runApp(".")

library(fs)
library(zip)
library(exifr)
library(batch)
library(tools)
library(shiny)
library(bslib)
library(vegan)
library(magick)
library(rlang)
library(knitr)
library(pbapply)
library(RSQLite)
library(bsicons)
library(parallel)
library(openxlsx)
library(camtrapR)
library(lubridate)
library(rmarkdown)
library(tidyverse)
library(shinyFiles)

source("apps/CameraTrapProcessing/modules/utils.R")
source("apps/CameraTrapProcessing/CT_Step1_ExtractExif.R")
source("apps/CameraTrapProcessing/CT_Step1.1_OffsetDateTime.R")
source("apps/CameraTrapProcessing/CT_Step2_MergeExifs.R")
source("apps/CameraTrapProcessing/CT_Step3_IndpDets.R")
source("apps/AbioticMonitoring/water_report.R")
source("apps/AbioticMonitoring/noise_report.R")
source("apps/ImpactAssessment/modules/utils.R")
source("apps/ImpactAssessment/impact_assessment.R")
source("apps/ArboReport/modules/utils.R")
source("apps/ArboReport/generate_report.R")
source("apps/ArboReport/resize_photos.R")
source("apps/StreamInspection/stream_report.R")
source("apps/BatRecordingProcessing/modules/util.r")
source("apps/BatRecordingProcessing/modules/dup_rows.r")
source("apps/BatRecordingProcessing/modules/match_gps.r")
source("apps/BatRecordingProcessing/modules/sort_bat_data.r")
source("apps/BatRecordingProcessing/Step1_process_meta.R")
source("apps/BatRecordingProcessing/Step2_combine_meta.R")
source("apps/BatRecordingProcessing/subsample.R")
source("apps/BatRecordingProcessing/recover_meta.R")
source("apps/FloraPhotoFiling/modules/utils.R")
source("apps/FloraPhotoFiling/sort_photos.R")
source("apps/FloraPhotoFiling/resort_tag_dirs.R")
source("apps/CAGPhotoRenaming/rename_photos.R")

SPECIES_DB_PATH     <- "apps/CameraTrapProcessing/data/Species_Database.xlsx"
IA_MATRIX_PATH      <- "apps/ImpactAssessment/data/ConsequenceSignificanceMatrix.xlsx"
ARBO_RMD_PATH       <- "apps/ArboReport/modules/arboreport_full.Rmd"
BAT_SPECIES_DB_PATH <- "apps/BatRecordingProcessing/data/Species_Database_Bats.csv"
VERSION         <- "v2.7"
UPDATE_DATE     <- "2026-07-23"


# ── Project Registry ──────────────────────────────────────────────────────────────────────────────
# nav_target: value of a nav_panel in this app; NULL = external url or disabled. version/updated:
# per-app tracking only (independent of the Hub's own VERSION above) — bump these whenever that
# app's own logic/tabs change.
PROJECTS <- list(

  list(
    title       = "Fauna IA Toolkit",
    description = "Converts recorded and probable species lists into formatted
                   Excel impact assessment templates.",
    url         = NULL,
    nav_target  = "impact_assessment",
    icon        = "bug",
    category    = "Fauna",
    status      = "live",
    version     = "v2.2",
    updated     = "2026-07-03"
  ),

  list(
    title       = "Abiotic Monitoring Toolkit",
    description = "Processes raw water quality logger and noise meter exports
                   into structured field report workbooks.",
    url         = NULL,
    nav_target  = "water",
    icon        = "moisture",
    category    = "Abiotic",
    status      = "live",
    version     = "v1.1",
    updated     = "2026-04-08"
  ),

  list(
    title       = "Camera Trap Processing",
    description = "Generates EXIFs after camera trap sorting and calculates
                   independent detections and other metrics for reports.",
    url         = NULL,
    nav_target  = "ct_step1",
    icon        = "camera",
    category    = "Fauna",
    status      = "live",
    version     = "v2.1",
    updated     = "2026-07-03"
  ),

  list(
    title       = "Arbo Report",
    description = "Generates the Arboriculture report for each specimen complete
                   with photos from site.",
    url         = NULL,
    nav_target  = "arbo_report",
    icon        = "tree",
    category    = "Flora",
    status      = "live",
    version     = "v2.4",
    updated     = "2026-07-03"
  ),

  list(
    title       = "Stream Inspection Report",
    description = "Processes fauna data and stream photos into a standardised
                   stream inspection report.",
    url         = NULL,
    nav_target  = "stream_report",
    icon        = "water",
    category    = "Fauna",
    status      = "live",
    version     = "v1.2",
    updated     = "2026-07-07"
  ),

  list(
    title       = "Bat Recording Processing",
    description = "Cleans Kaleidoscope bat meta.csv exports, matches handheld GPS,
                   sorts .wav files, sub-samples and combines datasheets.",
    url         = NULL,
    nav_target  = "bat_step1",
    icon        = "soundwave",
    category    = "Fauna",
    status      = "live",
    version     = "v1.5",
    updated     = "2026-07-07"
  ),

  list(
    title       = "BTNR Flora Photo Filing",
    description = "Files flora survey photos into Family/Species/Tag folders
                   using the photo filing sheet of the master datasheet.",
    url         = NULL,
    nav_target  = "flora_sort",
    icon        = "folder",
    category    = "Flora",
    status      = "beta",
    version     = "v1.0",
    updated     = "2026-07-17"
  ), 
  
  list(
    title       = "CAG Photo Renaming",
    description = "Renames flora/arbo photos based on Tree ID in an excel datasheet.",
    url         = NULL,
    nav_target  = "flora_rename",
    icon        = "pencil-square",
    category    = "Flora",
    status      = "beta",
    version     = "v1.0",
    updated     = "2026-07-23"
  )

  ## ── Paste new entries above this line ──────────────────────────────────────────────────────────
  #
  # list(
  #   title       = "My New Tool",
  #   description = "Short description.",
  #   url         = NULL,
  #   nav_target  = "my_tab_value",   # or url = "https://..." if external
  #   icon        = "bar-chart",
  #   category    = "Category",
  #   status      = "live",
  #   version     = "v1.0",
  #   updated     = "YYYY-MM-DD"
  # ),

)


#### Helper Functions ####
## IMPORTANT: icon_text() is called inside page_navbar() — it must be defined before ui <-, or R
## throws "could not find function" on startup.

icon_text <- function(label, icon_name) {
  tagList(bsicons::bs_icon(icon_name), " ", label)
}

make_logger <- function(rv) {
  function(msg) {
    timestamp <- format(Sys.time(), "[%H:%M:%S]")
    rv$log_lines <- c(rv$log_lines, paste(timestamp, msg))
  }
}

# ── Hub helpers ───────────────────────────────────────────────────────────────────────────────────

all_categories <- function() sort(unique(sapply(PROJECTS, `[[`, "category")))

## Looks up a project's own version/updated metadata by its nav_target, for display on Hub cards
## and in the About page. Returns NULL if not found.
get_project_meta <- function(nav_target) {
  match <- Filter(function(p) identical(p$nav_target, nav_target), PROJECTS)
  if (length(match) == 0) NULL else match[[1]]
}

## "v1.0 · 2026-07-03", or "" if version/updated are NA (e.g. coming soon)
project_version_label <- function(p) {
  if (is.null(p$version) || is.na(p$version)) return("")
  paste0(p$version, if (!is.null(p$updated) && !is.na(p$updated)) paste0(" · ", p$updated) else "")
}

## Panel title for the About page's accordion — app name + icon + version, so adding a new app only
## means adding one more accordion_panel, not rewriting a flat card grid.
about_panel_title <- function(app_title, nav_target, icon_name) {
  meta  <- get_project_meta(nav_target)
  label <- if (!is.null(meta)) project_version_label(meta) else ""
  tagList(
    bsicons::bs_icon(icon_name), " ", app_title,
    if (nzchar(label)) tags$span(class = "text-muted small fw-normal ms-2", label)
  )
}

status_badge <- function(status) {
  cfg <- switch(status,
    "live"        = list(class = "badge-live",        label = "Live"),
    "beta"        = list(class = "badge-beta",        label = "Beta"),
    "coming soon" = list(class = "badge-coming-soon", label = "Coming Soon"),
                    list(class = "badge-live",        label = status)
  )
  tags$span(class = paste("proj-badge", cfg$class), cfg$label)
}

project_card <- function(p) {
  is_disabled <- p$status == "coming soon"
  has_nav     <- !is.null(p$nav_target)
  has_url     <- !is.null(p$url)

  card(
    class = paste("proj-card", if (is_disabled) "proj-card--disabled"),
    `data-category` = p$category,

    card_body(
      div(class = "proj-card-top",
        div(class = "proj-icon-wrap", bsicons::bs_icon(p$icon, size = "1.6rem")),
        status_badge(p$status)
      ),
      div(class = "proj-category", p$category),
      h5(class  = "proj-title",    p$title),
      p(class   = "proj-desc",     p$description),
      if (nzchar(project_version_label(p)))
        p(class = "proj-version text-muted small mb-0", project_version_label(p))
    ),

    card_footer(
      class = "proj-footer",
      if (!is_disabled) {
        if (has_nav) {
          tags$button(
            class   = "proj-launch-btn",
            onclick = sprintf(
              "Shiny.setInputValue('hub_nav_to', '%s', {priority: 'event'})",
              p$nav_target
            ),
            bsicons::bs_icon("box-arrow-in-right"), " Open"
          )
        } else if (has_url) {
          tags$a(href = p$url, target = "_blank", class = "proj-launch-btn",
                 bsicons::bs_icon("box-arrow-up-right"), " Launch")
        }
      } else {
        tags$span(class = "proj-launch-btn proj-launch-btn--disabled",
                  bsicons::bs_icon("clock"), " Coming Soon")
      }
    )
  )
}


#### User Interface ####

ui <- page_navbar(
  title = tagList(bsicons::bs_icon("grid-3x3-gap-fill"), " Camphora Toolkit Hub"),
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#1a6496",
    base_font    = font_google("Source Sans 3"),
    heading_font = font_google("DM Serif Display")
  ),
  id     = "main_navbar",
  header = tags$head(tags$style(HTML("

    body { background: #f0f4f7; }

    /* ── Hub header ── */
    .hub-header {
      background: linear-gradient(135deg, #0d3d56 0%, #1a6496 60%, #2a9d8f 100%);
      color: #fff;
      padding: 3rem 2.5rem 2.5rem;
      border-radius: 0 0 1.5rem 1.5rem;
      margin-bottom: 2rem;
      position: relative; overflow: hidden;
    }
    .hub-header::before {
      content: '';
      position: absolute; inset: 0;
      background-image: radial-gradient(circle, rgba(255,255,255,0.08) 1px, transparent 1px);
      background-size: 22px 22px; pointer-events: none;
    }
    .hub-header-inner { position: relative; }
    .hub-title        { font-size: 2.4rem; font-weight: 400; letter-spacing: -0.5px; margin-bottom: 0.35rem; line-height: 1.15; }
    .hub-subtitle     { font-size: 1rem; opacity: 0.80; margin-bottom: 0; font-family: 'Source Sans 3', sans-serif; }
    .hub-meta         { font-size: 0.78rem; opacity: 0.6; margin-top: 1.5rem; font-family: 'Source Sans 3', sans-serif; }

    /* ── Filter bar ── */
    .filter-bar { display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 1.5rem; padding: 0 0.25rem; }
    .filter-btn {
      background: #fff; border: 1.5px solid #d0dce6; border-radius: 2rem;
      padding: 0.3rem 1rem; font-size: 0.85rem; color: #3a5a70;
      cursor: pointer; transition: all 0.18s ease; font-family: 'Source Sans 3', sans-serif;
    }
    .filter-btn:hover, .filter-btn.active { background: #1a6496; border-color: #1a6496; color: #fff; }

    /* ── Project cards ── */
    .proj-card {
      background: #fff; border: 1px solid #dde6ee; border-radius: 1rem;
      box-shadow: 0 2px 8px rgba(26,100,150,0.06);
      transition: transform 0.18s ease, box-shadow 0.18s ease; height: 100%;
    }
    .proj-card:hover        { transform: translateY(-3px); box-shadow: 0 8px 24px rgba(26,100,150,0.13); }
    .proj-card--disabled    { opacity: 0.55; pointer-events: none; }
    .proj-card-top          { display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 1rem; }
    .proj-icon-wrap         {
      background: linear-gradient(135deg, #e8f4fb, #d0eaf7); color: #1a6496;
      width: 3rem; height: 3rem; border-radius: 0.75rem;
      display: flex; align-items: center; justify-content: center;
    }
    .proj-category { font-size: 0.72rem; font-weight: 600; text-transform: uppercase; letter-spacing: 0.08em; color: #2a9d8f; margin-bottom: 0.3rem; font-family: 'Source Sans 3', sans-serif; }
    .proj-title    { font-size: 1.05rem; font-weight: 600; color: #0d3d56; margin-bottom: 0.5rem; line-height: 1.3; }
    .proj-desc     { font-size: 0.88rem; color: #5a7a8a; line-height: 1.55; margin-bottom: 0; font-family: 'Source Sans 3', sans-serif; }
    .proj-footer   { background: transparent; border-top: 1px solid #eef2f6; padding-top: 0.85rem; }

    /* ── Badges ── */
    .proj-badge        { font-size: 0.68rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em; padding: 0.22rem 0.6rem; border-radius: 2rem; font-family: 'Source Sans 3', sans-serif; }
    .badge-live        { background: #d4f0e8; color: #1a7a5e; }
    .badge-beta        { background: #fff3cd; color: #856404; }
    .badge-coming-soon { background: #e9ecef; color: #6c757d; }

    /* ── Launch button (shared by <a> and <button>) ── */
    .proj-launch-btn {
      display: inline-flex; align-items: center; gap: 0.35rem;
      font-size: 0.85rem; font-weight: 600; color: #1a6496;
      text-decoration: none; transition: color 0.15s;
      font-family: 'Source Sans 3', sans-serif;
      background: none; border: none; padding: 0; cursor: pointer;
    }
    .proj-launch-btn:hover        { color: #0d3d56; text-decoration: none; }
    .proj-launch-btn--disabled    { color: #aab5be; cursor: default; }

    /* ── Empty state ── */
    .empty-state { text-align: center; padding: 4rem 2rem; color: #8aa0b0; font-family: 'Source Sans 3', sans-serif; }

    /* ── Card grid ── */
    .card-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 1.25rem; }
    .card-grid .card { margin: 0; }

  "))),


  # ── Hub Tab ─────────────────────────────────────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("Hub", "grid-3x3-gap-fill"),
    value = "hub",

    div(class = "hub-header",
      div(class = "hub-header-inner",
        div(style = "display:flex; align-items:center; gap:0.75rem; margin-bottom:0.5rem;",
          bsicons::bs_icon("grid-3x3-gap-fill", size = "1.8rem"),
          h1(class = "hub-title mb-0", "Camphora Toolkit Hub")
        ),
        p(class = "hub-subtitle",
          "A central directory of internal analytical tools. Click any card to open."),
        p(class = "hub-meta",
          paste0(length(PROJECTS), " tools  \u00b7  ", VERSION, "  \u00b7  Last updated ", UPDATE_DATE))
      )
    ),

    div(style = "padding: 0 0.5rem 3rem;",
      uiOutput("filter_bar"),
      uiOutput("project_grid")
    )
  ),


  # ── Fauna Impact Assessment ─────────────────────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("Impact Assessment", "bug"),
    value = "impact_assessment",

    layout_sidebar(
      fillable = TRUE,

      sidebar = sidebar(
        width = 320,

        h5("Input files", class = "fw-bold mt-1"),

        fileInput(
          "ia_species_list_file",
          label = tooltip(
            span("Species list (.xlsx)", bsicons::bs_icon("info-circle")),
            "Must contain the following columns: 'Scientific.Name' column and match the expected format."
          ),
          accept   = ".xlsx",
          multiple = FALSE
        ),

        fileInput(
          "ia_fauna_db_file",
          label = tooltip(
            span("Fauna database (.xlsx)", bsicons::bs_icon("info-circle")),
            "Combined fauna database containing the 'CS species impact intensity' sheet."
          ),
          accept   = ".xlsx",
          multiple = FALSE
        ),

        hr(),
        actionButton("ia_run_btn",
                     label = tagList(bsicons::bs_icon("play-fill"), " Run Assessment"),
                     class = "btn-primary w-100"),
        hr(),
        uiOutput("ia_download_ui")
      ),

      layout_column_wrap(
        width = 1,
        card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
             verbatimTextOutput("ia_log_output"), height = 200),
        card(card_header(tagList(bsicons::bs_icon("table"), " Output preview (first 50 rows)")),
             div(style = "overflow-x: auto;", tableOutput("ia_preview_table")))
      )
    )
  ),


  # ── Camera Traps ────────────────────────────────────────────────────────────────────────────────
  nav_menu(
    title = icon_text("Camera Traps", "camera"),

    # Step 1: EXIF Extraction
    nav_panel(
      title = icon_text("Step 1: EXIF Extraction", "camera-video"),
      value = "ct_step1",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input folders", class = "fw-bold mt-1"),

          p(class = "mb-1",
            tooltip(span("Processed data folder", bsicons::bs_icon("info-circle")),
                    "Folder containing Timelapse .ddb files or manually sorted species folders.")),
          shinyDirButton("s1_path_processed", label = "Browse...",
                         title = "Choose processed data directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("s1_processed_path_display", placeholder = TRUE),

          p(class = "mb-1 mt-2",
            tooltip(span("Raw data folder", bsicons::bs_icon("info-circle")),
                    "Folder containing the original raw video files.")),
          shinyDirButton("s1_path_raw", label = "Browse...",
                         title = "Choose raw data directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("s1_raw_path_display", placeholder = TRUE),

          hr(),
          actionButton("s1_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Run EXIF Extraction"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("s1_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("s1_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("s1_preview_table")))
        )
      )
    ),

    # Step 1.1: Offset DateTime
    nav_panel(
      title = icon_text("Step 1.1: Offset DateTime", "clock-history"),
      value = "ct_step1_1",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input file", class = "fw-bold mt-1"),
          fileInput("s1a_exif_file",
                    label = tooltip(
                      span("Exif CSV (from Step 1)", bsicons::bs_icon("info-circle")),
                      "The *_exif.csv output from Step 1, when FileModifyDate is wrong because the camera's clock was set incorrectly."
                    ),
                    accept = ".csv", multiple = FALSE),

          hr(),
          h5("Offset", class = "fw-bold mt-1"),
          textInput("s1a_offset",
                    label = tooltip(
                      span("Hours to offset, or correct first-video DateTime", bsicons::bs_icon("info-circle")),
                      "Enter a number of hours (e.g. -12 or 5), OR the actual correct DateTime of the first video (e.g. 2025-11-13 08:00:00)."
                    ),
                    placeholder = "e.g. -12  or  2025-11-13 08:00:00"),

          hr(),
          actionButton("s1a_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Apply Offset"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("s1a_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("s1a_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("s1a_preview_table")))
        )
      )
    ),

    # Step 2: Merge EXIFs
    nav_panel(
      title = icon_text("Step 2: Merge EXIFs", "files"),
      value = "ct_step2",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input folder", class = "fw-bold mt-1"),

          p(class = "mb-1",
            tooltip(span("EXIF folder", bsicons::bs_icon("info-circle")),
                    "Folder containing all *_exif.csv files output by Step 1. Sub-folders are also searched.")),
          shinyDirButton("s2_exif_folder", label = "Browse...",
                         title = "Choose folder containing EXIF CSV files",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("s2_folder_display", placeholder = TRUE),

          hr(),
          fileInput("s2_edited_combined",
                    label = tooltip(
                      span("Edited combined CSV (optional)", bsicons::bs_icon("info-circle")),
                      "Upload a manually corrected combined_exif_all.csv to use for the mammals-only filter instead of re-merging the raw files."
                    ),
                    accept = ".csv", multiple = FALSE),

          hr(),
          actionButton("s2_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Run Merge"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("s2_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("s2_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview — combined_exif_all (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("s2_preview_table")))
        )
      )
    ),

    # Step 3: Independent Detections
    nav_panel(
      title = icon_text("Step 3: Independent Detections", "activity"),
      value = "ct_step3",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input file", class = "fw-bold mt-1"),
          fileInput("s3_ct_file",
                    label = tooltip(
                      span("Combined EXIF CSV", bsicons::bs_icon("info-circle")),
                      "The combined_exif_all.csv produced by Step 2."
                    ),
                    accept = ".csv"),

          hr(),
          h5("Parameters", class = "fw-bold mt-1"),

          numericInput("s3_indp_interval",
                       label = tooltip(
                         span("Independence interval (seconds)", bsicons::bs_icon("info-circle")),
                         "Records of the same species at the same station separated by less than this interval are grouped into one independent detection. Default: 3600 s (1 hour)."
                       ),
                       value = 3600, min = 1, step = 1),

          textAreaInput("s3_rm_stations",
                        label = tooltip(
                          span("Stations to exclude (optional)", bsicons::bs_icon("info-circle")),
                          "Comma-separated list of station names to exclude from analysis."
                        ),
                        placeholder = "e.g. S01, S02", rows = 2),

          hr(),
          actionButton("s3_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Run Analysis"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("s3_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("s3_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview — species summary (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("s3_preview_table")))
        )
      )
    )
  ),


  # ── Abiotic Monitoring ──────────────────────────────────────────────────────────────────────────
  nav_menu(
    title = icon_text("Abiotic Monitoring", "moisture"),

    # Water Monitoring
    nav_panel(
      title = icon_text("Water Monitoring", "droplet-half"),
      value = "water",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 320,

          h5("Input files", class = "fw-bold mt-1"),
          fileInput("water_data_file",
                    label = tooltip(
                      span("Raw water monitoring data (.csv)", bsicons::bs_icon("info-circle")),
                      "CSV file after downloading data from EXO2 logger to KOR."
                    ),
                    accept = ".csv", multiple = FALSE),

          hr(),
          h5("Parameters", class = "fw-bold mt-1"),

          numericInput("water_time_threshold",
                       label = tooltip(
                         span("Time threshold (mins)", bsicons::bs_icon("info-circle")),
                         "Measurements within this window are averaged into a single record."
                       ),
                       value = 2, min = 1, step = 1),

          hr(),
          textInput("date_format",
                    label = tooltip(
                      span("Date format", bsicons::bs_icon("info-circle")),
                      "Date format of your CSV. Default '%d/%m/%Y %I:%M:%S %p' means Day/Month/Year Hour:Minute:Second am/pm."
                    ),
                    value = "%d/%m/%Y %I:%M:%S %p"),

          hr(),
          actionButton("water_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Run Report"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("water_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("water_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("water_preview_table")))
        )
      )
    ),

    # Noise Monitoring
    nav_panel(
      title = icon_text("Noise Monitoring", "soundwave"),
      value = "noise",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 320,

          h5("Input files", class = "fw-bold mt-1"),
          fileInput("noise_data_file",
                    label = tooltip(
                      span("Noise data (.csv)", bsicons::bs_icon("info-circle")),
                      "CSV export from the sound level meter. Must contain 'Start.Time' and 'Leq..LAeq.' columns."
                    ),
                    accept = ".csv", multiple = FALSE),

          fileInput("noise_calibration_file",
                    label = tooltip(
                      span("Calibration data (.csv)", bsicons::bs_icon("info-circle")),
                      "CSV containing calibration metadata rows for Instrument, Time, Level, and Offset."
                    ),
                    accept = ".csv", multiple = FALSE),

          hr(),
          h5("Metadata", class = "fw-bold mt-1"),

          textInput("noise_location",
                    label = "Site location",
                    placeholder = "e.g. Mandai OALC"),

          textInput("noise_monitoring_pt",
                    label = "Monitoring point",
                    placeholder = "e.g. N2"),

          hr(),
          actionButton("noise_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Run Report"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("noise_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("noise_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview — Noise Data (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("noise_preview_table")))
        )
      )
    )
  ),


  # ── Arbo Report ─────────────────────────────────────────────────────────────────────────────────
  nav_menu(
    title = icon_text("Arbo Report", "tree"),

    # Generate Report
    nav_panel(
      title = icon_text("Generate Report", "file-earmark-word"),
      value = "arbo_report",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input file", class = "fw-bold mt-1"),
          fileInput("arbo_biodata_file",
                    label = tooltip(
                      span("Tree biodata (.csv)", bsicons::bs_icon("info-circle")),
                      "Combined arbo assessment export. Must contain Tree.ID, Date, Species, and other assessment columns."
                    ),
                    accept = ".csv", multiple = FALSE),

          hr(),
          h5("Photos", class = "fw-bold mt-1"),
          checkboxInput("arbo_incl_photos", "Include photos in report", value = TRUE),
          conditionalPanel(
            condition = "input.arbo_incl_photos == true",
            p(class = "mb-1",
              tooltip(span("Resized photos folder", bsicons::bs_icon("info-circle")),
                      "Folder containing per-inspection photo subfolders, already resized (see 'Resize Photos' tab).")),
            shinyDirButton("arbo_photos_dir", label = "Browse...",
                           title = "Choose resized photos directory",
                           class = "btn-outline-secondary w-100 mb-1"),
            verbatimTextOutput("arbo_photos_dir_display", placeholder = TRUE),

            textInput("arbo_photo_prefix",
                      label = tooltip(
                        span("Photo folder prefix", bsicons::bs_icon("info-circle")),
                        "Prefix used in photo folder names, e.g. 'UWCSEA_Photos' for 'UWCSEA_Photos_2026-01-13_EL'."
                      ),
                      placeholder = "e.g. UWCSEA_Photos")
          ),

          hr(),
          h5("Parameters", class = "fw-bold mt-1"),
          numericInput("arbo_report_size",
                       label = tooltip(
                         span("Trees per report", bsicons::bs_icon("info-circle")),
                         "Number of trees included in each generated Word document."
                       ),
                       value = 100, min = 1, step = 1),

          textInput("arbo_select_ids",
                    label = tooltip(
                      span("Select Tree IDs (optional)", bsicons::bs_icon("info-circle")),
                      "Comma-separated Tree.ID values to include. Leave blank to include all trees."
                    ),
                    placeholder = "e.g. 12, 15, 20A"),

          checkboxInput("arbo_incl_crown_spread", "Include crown spread", value = FALSE),
          checkboxInput("arbo_sort_site", "Sort by site", value = FALSE),

          textInput("arbo_date_format",
                    label = tooltip(
                      span("Date format", bsicons::bs_icon("info-circle")),
                      "Date format of the Date column in your CSV."
                    ),
                    value = "%d/%m/%Y"),

          hr(),
          actionButton("arbo_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Generate Report"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("arbo_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("arbo_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("file-earmark-word"), " Generated reports")),
               div(style = "overflow-x: auto;", tableOutput("arbo_preview_table")))
        )
      )
    ),

    # Resize Photos
    nav_panel(
      title = icon_text("Resize Photos", "image"),
      value = "arbo_resize",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input/output folders", class = "fw-bold mt-1"),

          p(class = "mb-1",
            tooltip(span("Original photos folder", bsicons::bs_icon("info-circle")),
                    "Folder containing the original full-size site photos (searched recursively).")),
          shinyDirButton("arbophoto_source_dir", label = "Browse...",
                         title = "Choose original photos directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("arbophoto_source_dir_display", placeholder = TRUE),

          p(class = "mb-1 mt-2",
            tooltip(span("Destination folder", bsicons::bs_icon("info-circle")),
                    "Folder to save the resized photos into. Use this as the 'Resized photos folder' in Generate Report.")),
          shinyDirButton("arbophoto_dest_dir", label = "Browse...",
                         title = "Choose destination directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("arbophoto_dest_dir_display", placeholder = TRUE),

          hr(),
          numericInput("arbophoto_size",
                       label = tooltip(
                         span("Photo size (px)", bsicons::bs_icon("info-circle")),
                         "Target width/height in pixels after resizing."
                       ),
                       value = 400, min = 50, step = 50),

          hr(),
          actionButton("arbophoto_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Resize Photos"),
                       class = "btn-primary w-100")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("arbophoto_log_output"), height = 300)
        )
      )
    )
  ),


  # ── Stream Inspection Report ────────────────────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("Stream Inspection", "water"),
    value = "stream_report",

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 340,

        h5("Input file", class = "fw-bold mt-1"),
        fileInput("si_fauna_file",
                  label = tooltip(
                    span("Fauna datasheet (.xlsx)", bsicons::bs_icon("info-circle")),
                    "Must contain sheets '01 Log' and '02 DataList', with the sampling point names in the Transect column."
                  ),
                  accept = ".xlsx", multiple = FALSE),

        hr(),
        h5("Photos", class = "fw-bold mt-1"),
        p(class = "mb-1",
          tooltip(span("Root photo folder", bsicons::bs_icon("info-circle")),
                  "Folder structured as: root / YYYYMMDD / SamplingPoint_YYYYMMDD / photo.jpg. Sampling point names must match the Transect column.")),
        shinyDirButton("si_photos_dir", label = "Browse...",
                       title = "Choose root photo directory",
                       class = "btn-outline-secondary w-100 mb-1"),
        verbatimTextOutput("si_photos_dir_display", placeholder = TRUE),

        hr(),
        h5("Survey parameters", class = "fw-bold mt-1"),
        textAreaInput("si_dates",
                      label = tooltip(
                        span("Inspection date(s)", bsicons::bs_icon("info-circle")),
                        "One date per line or comma-separated. Format: YYYY-MM-DD."
                      ),
                      placeholder = "e.g.\n2025-11-25\n2025-11-26", rows = 3),

        hr(),
        actionButton("si_run_btn",
                     label = tagList(bsicons::bs_icon("play-fill"), " Generate Report"),
                     class = "btn-primary w-100"),
        hr(),
        uiOutput("si_download_ui")
      ),

      layout_column_wrap(
        width = 1,
        card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
             verbatimTextOutput("si_log_output"), height = 200),
        card(card_header(tagList(bsicons::bs_icon("table"), " Report preview")),
             div(style = "overflow-x: auto;", tableOutput("si_preview_table")))
      )
    )
  ),


  # ── Bat Recording Processing ────────────────────────────────────────────────────────────────────
  nav_menu(
    title = icon_text("Bat Recordings", "soundwave"),

    # Step 1: Process Meta
    nav_panel(
      title = icon_text("Step 1: Process Meta", "file-earmark-check"),
      value = "bat_step1",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input file", class = "fw-bold mt-1"),
          fileInput("bat1_meta_file",
                    label = tooltip(
                      span("Kaleidoscope meta.csv", bsicons::bs_icon("info-circle")),
                      "The meta.csv exported from Kaleidoscope, with a MANUAL.ID column."
                    ),
                    accept = ".csv", multiple = FALSE),

          textInput("bat1_delimiter",
                    label = tooltip(
                      span("Species delimiter", bsicons::bs_icon("info-circle")),
                      "Character separating multiple species within one MANUAL.ID cell."
                    ),
                    value = "_"),

          hr(),
          checkboxInput("bat1_match_gps", "Match handheld GPS tracks", value = FALSE),
          conditionalPanel(
            condition = "input.bat1_match_gps == true",
            fileInput("bat1_gps_file",
                      label = tooltip(
                        span("Handheld GPS tracks (.csv)", bsicons::bs_icon("info-circle")),
                        "Tracks CSV with time, lat, lon columns. Each bat call is matched to the closest track time."
                      ),
                      accept = ".csv", multiple = FALSE)
          ),

          checkboxInput("bat1_sort_wav", "Sort .wav files into species folders", value = FALSE),
          conditionalPanel(
            condition = "input.bat1_sort_wav == true",
            p(class = "mb-1",
              tooltip(span("Folder of .wav files", bsicons::bs_icon("info-circle")),
                      "Folder containing the .wav files named in the meta.csv IN.FILE column.")),
            shinyDirButton("bat1_wav_dir", label = "Browse...",
                           title = "Choose folder of .wav files",
                           class = "btn-outline-secondary w-100 mb-1"),
            verbatimTextOutput("bat1_wav_dir_display", placeholder = TRUE)
          ),

          hr(),
          actionButton("bat1_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Process Meta"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("bat1_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("bat1_log_output"), height = 220),
          card(card_header(tagList(bsicons::bs_icon("table"), " Output preview (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("bat1_preview_table")))
        )
      )
    ),

    # Step 2: Combine Meta
    nav_panel(
      title = icon_text("Step 2: Combine Meta", "collection"),
      value = "bat_step2",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input folder", class = "fw-bold mt-1"),
          p(class = "mb-1",
            tooltip(span("Folder of cleaned/matched CSVs", bsicons::bs_icon("info-circle")),
                    "Folder containing the meta_cleaned.csv / meta_matched.csv outputs from Step 1 (searched recursively).")),
          shinyDirButton("bat2_meta_dir", label = "Browse...",
                         title = "Choose folder of cleaned/matched CSVs",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("bat2_meta_dir_display", placeholder = TRUE),

          hr(),
          actionButton("bat2_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Combine"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("bat2_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("bat2_log_output"), height = 200),
          card(card_header(tagList(bsicons::bs_icon("table"), " Combined preview (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("bat2_preview_table")))
        )
      )
    ),

    # Sub-sample Files
    nav_panel(
      title = icon_text("Sub-sample Files", "funnel"),
      value = "bat_subsample",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input folder", class = "fw-bold mt-1"),
          p(class = "mb-1",
            tooltip(span("Folder of raw .wav files", bsicons::bs_icon("info-circle")),
                    "Raw .wav files named Project_Date_Time (e.g. E_HT_20250522_003012.wav). A '<folder>_subsampled' copy is created next to it.")),
          shinyDirButton("bat_sub_raw_dir", label = "Browse...",
                         title = "Choose folder of raw .wav files",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("bat_sub_raw_dir_display", placeholder = TRUE),

          hr(),
          textInput("bat_sub_mins",
                    label = tooltip(
                      span("Minutes to keep", bsicons::bs_icon("info-circle")),
                      "Comma-separated minutes of the hour to keep. Default keeps 5 minutes out of every 30-minute block."
                    ),
                    value = "0,1,2,3,4,30,31,32,33,34"),

          hr(),
          actionButton("bat_sub_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Sub-sample"),
                       class = "btn-primary w-100")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("bat_sub_log_output"), height = 300)
        )
      )
    ),

    # Recover Meta
    nav_panel(
      title = icon_text("Recover Meta", "arrow-counterclockwise"),
      value = "bat_recover",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          div(class = "alert alert-warning small",
              bsicons::bs_icon("exclamation-triangle"), " ",
              "Last resort only — use when the meta.csv is lost but files are already sorted. The output may not be readable by Kaleidoscope."),

          h5("Input folders", class = "fw-bold mt-1"),
          p(class = "mb-1",
            tooltip(span("Sorted (processed) folder", bsicons::bs_icon("info-circle")),
                    "Folder of already-sorted .wav files, one subfolder per species.")),
          shinyDirButton("bat_rec_proc_dir", label = "Browse...",
                         title = "Choose sorted (processed) folder",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("bat_rec_proc_dir_display", placeholder = TRUE),

          p(class = "mb-1 mt-2",
            tooltip(span("Raw folder", bsicons::bs_icon("info-circle")),
                    "Folder of the original raw .wav files (read for EXIF timestamps).")),
          shinyDirButton("bat_rec_raw_dir", label = "Browse...",
                         title = "Choose raw folder",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("bat_rec_raw_dir_display", placeholder = TRUE),

          hr(),
          actionButton("bat_rec_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Recover Meta"),
                       class = "btn-primary w-100"),
          hr(),
          uiOutput("bat_rec_download_ui")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("bat_rec_log_output"), height = 220),
          card(card_header(tagList(bsicons::bs_icon("table"), " meta_reverse preview (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("bat_rec_preview_table")))
        )
      )
    )
  ),


  # ── Flora Photo Filing ──────────────────────────────────────────────────────────────────────────
  nav_menu(
    title = icon_text("Flora Photos", "images"),

    # Sort Photos
    nav_panel(
      title = icon_text("Sort Photos", "folder-symlink"),
      value = "flora_sort",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input file", class = "fw-bold mt-1"),
          fileInput("flora_datasheet_file",
                    label = tooltip(
                      span("Master datasheet (.xlsx)", bsicons::bs_icon("info-circle")),
                      "Must contain the photo filing sheet, with STATUS, TAG_2025, Family, Species, ZOOM_CAM, ZOOM_PHOTO_ID, FS_CAM and FS_PHOTO_ID columns."
                    ),
                    accept = ".xlsx", multiple = FALSE),

          textInput("flora_sheet_name",
                    label = tooltip(
                      span("Photo filing sheet name", bsicons::bs_icon("info-circle")),
                      "Name of the sheet in the datasheet holding the photo filing table."
                    ),
                    value = "Photo Filing (For JO)"),

          hr(),
          h5("Input/output folders", class = "fw-bold mt-1"),

          p(class = "mb-1",
            tooltip(span("Raw photos folder", bsicons::bs_icon("info-circle")),
                    "Folder containing the per-session photo folders named in the ZOOM_CAM and FS_CAM columns.")),
          shinyDirButton("flora_photos_dir", label = "Browse...",
                         title = "Choose raw photos directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("flora_photos_dir_display", placeholder = TRUE),

          p(class = "mb-1 mt-2",
            tooltip(span("Sorted photos folder", bsicons::bs_icon("info-circle")),
                    "Destination folder. Photos are filed as Family/Species/Tag/Tag_photo.jpg.")),
          shinyDirButton("flora_sorted_dir", label = "Browse...",
                         title = "Choose sorted photos directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("flora_sorted_dir_display", placeholder = TRUE),

          hr(),
          h5("Parameters", class = "fw-bold mt-1"),
          textInput("flora_status",
                    label = tooltip(
                      span("STATUS to sort", bsicons::bs_icon("info-circle")),
                      "Comma-separated STATUS values to file, e.g. Batch 3.1, Batch 3.2."
                    ),
                    placeholder = "e.g. Batch 3.1, Batch 3.2"),

          hr(),
          actionButton("flora_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Sort Photos"),
                       class = "btn-primary w-100")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("flora_log_output"), height = 220),
          card(card_header(tagList(bsicons::bs_icon("table"), " Photos filed per tag (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("flora_preview_table")))
        )
      )
    ),

    # Re-sort Tag Folders
    nav_panel(
      title = icon_text("Re-sort Tag Folders", "diagram-3"),
      value = "flora_resort",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          div(class = "alert alert-warning small",
              bsicons::bs_icon("exclamation-triangle"), " ",
              "Only needed for older batches sorted as Family/Species, before per-tag subfolders were introduced."),

          h5("Input/output folders", class = "fw-bold mt-1"),

          p(class = "mb-1",
            tooltip(span("Existing sorted folder", bsicons::bs_icon("info-circle")),
                    "Folder structured as Family/Species/Tag_photo.jpg. The tag is read from the file name, up to the first underscore.")),
          shinyDirButton("flora_resort_src_dir", label = "Browse...",
                         title = "Choose existing sorted directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("flora_resort_src_dir_display", placeholder = TRUE),

          p(class = "mb-1 mt-2",
            tooltip(span("Updated folder", bsicons::bs_icon("info-circle")),
                    "Destination folder for the restructured Family/Species/Tag tree.")),
          shinyDirButton("flora_resort_dest_dir", label = "Browse...",
                         title = "Choose updated directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("flora_resort_dest_dir_display", placeholder = TRUE),

          hr(),
          actionButton("flora_resort_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Re-sort Folders"),
                       class = "btn-primary w-100")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("flora_resort_log_output"), height = 220),
          card(card_header(tagList(bsicons::bs_icon("table"), " Photos re-sorted per tag (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("flora_resort_preview_table")))
        )
      )
    ),

    # Rename Photos (CAG)
    nav_panel(
      title = icon_text("Rename Photos", "pencil-square"),
      value = "flora_rename",

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 340,

          h5("Input file", class = "fw-bold mt-1"),
          fileInput("cag_excel_file",
                    label = tooltip(
                      span("Datasheet (.xlsx)", bsicons::bs_icon("info-circle")),
                      "Workbook holding the Tree ID <-> Photo-number mapping, e.g. CAG_14.xlsx."
                    ),
                    accept = ".xlsx", multiple = FALSE),

          textInput("cag_sheet",
                    label = tooltip(
                      span("Sheet name", bsicons::bs_icon("info-circle")),
                      "Name of the sheet holding the mapping table, e.g. T1 20260708."
                    ),
                    placeholder = "e.g. T1 20260708"),

          hr(),
          h5("Photos folder", class = "fw-bold mt-1"),
          p(class = "mb-1",
            tooltip(span("Photo folder", bsicons::bs_icon("info-circle")),
                    "Folder holding the photos to rename. Copy mode writes to a 'renamed' subfolder; Rename mode renames in place.")),
          shinyDirButton("cag_photo_dir", label = "Browse...",
                         title = "Choose photo directory",
                         class = "btn-outline-secondary w-100 mb-1"),
          verbatimTextOutput("cag_photo_dir_display", placeholder = TRUE),

          hr(),
          h5("Columns", class = "fw-bold mt-1"),
          textInput("cag_id_col",
                    label = tooltip(
                      span("Tree ID column", bsicons::bs_icon("info-circle")),
                      "Column holding the new name for each photo. Matched ignoring case and spaces."
                    ),
                    value = "Tree ID"),
          textInput("cag_photo_col",
                    label = tooltip(
                      span("Photo column", bsicons::bs_icon("info-circle")),
                      "Column holding the photo numbers, e.g. 9105-07, 9198-9201, 9230."
                    ),
                    value = "Photo"),

          hr(),
          h5("Mode", class = "fw-bold mt-1"),
          radioButtons("cag_mode",
                       label = NULL,
                       choices = c("Preview only (no changes)" = "dry_run",
                                   "Copy into 'renamed' subfolder" = "copy",
                                   "Rename in place" = "rename"),
                       selected = "dry_run"),

          hr(),
          actionButton("cag_run_btn",
                       label = tagList(bsicons::bs_icon("play-fill"), " Run"),
                       class = "btn-primary w-100")
        ),

        layout_column_wrap(
          width = 1,
          card(card_header(tagList(bsicons::bs_icon("terminal"), " Log")),
               verbatimTextOutput("cag_log_output"), height = 220),
          card(card_header(tagList(bsicons::bs_icon("table"), " Rename plan (first 50 rows)")),
               div(style = "overflow-x: auto;", tableOutput("cag_preview_table")))
        )
      )
    )
  ),


  # ── About Tab ───────────────────────────────────────────────────────────────────────────────────
  # One accordion_panel per app, grouping that app's cards — adding a new app means adding one more
  # accordion_panel(), not editing a flat card grid.
  nav_panel(
    title = icon_text("About", "info-circle"),
    value = "about",

    accordion(
      id   = "about_accordion",
      open = FALSE,

      accordion_panel(
        value = "about_ia",
        title = about_panel_title("Fauna IA Toolkit", "impact_assessment", "bug"),

        card(
          card_header("Impact Assessment"),
          card_body(
            p("Converts a list of recorded and/or probable fauna species into a formatted Excel impact assessment template."),
            tags$ol(
              tags$li("Upload the ", strong("species list"), " (.xlsx) — must contain a ", code("Scientific Name"), " column. Note that this is case sensitive."),
              tags$li("Upload the ", strong("fauna database"), " (.xlsx) containing the ", em("CS species impact intensity"), " sheet."),
              tags$li("Click ", strong("Run Assessment"), "."),
              tags$li("Download the formatted output workbook.")
            ),
            hr(),
            p(strong("Output:")),
            tags$ul(tags$li("Receptor sheet — sensitivity, impact intensity, consequence, likelihood, significance, and residual impact columns per species/phase."))
          )
        )
      ),

      accordion_panel(
        value = "about_ct",
        title = about_panel_title("Camera Trap Processing", "ct_step1", "camera"),

        layout_column_wrap(
          width = 1 / 2,

          card(
            card_header("Step 1: EXIF Extraction"),
            card_body(
              p("Extracts and merges metadata from processed (Timelapse / manually sorted) and raw video folders."),
              tags$ol(
                tags$li("Select the ", strong("processed"), " folder (.ddb files or sorted species folders)."),
                tags$li("Select the ", strong("raw"), " folder (original video files)."),
                tags$li("Click ", strong("Run"), "."),
                tags$li("Download the ", code("*_exif.csv"), " output.")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li("One CSV per station — Station, SamplingDate, FileModifyDate, Date, Time, FileName, Genus, Species, ScientificName, Quantity, Remarks."))
            )
          ),

          card(
            card_header("Step 1.1: Offset DateTime"),
            card_body(
              p("Corrects FileModifyDate/Date/Time in a Step 1 exif.csv when the camera's clock was wrong at the time of recording."),
              tags$ol(
                tags$li("Upload the ", code("*_exif.csv"), " from Step 1."),
                tags$li("Enter either an ", strong("hour offset"), " (e.g. -12) or the ", strong("correct DateTime of the first video"), " (e.g. 2025-11-13 08:00:00)."),
                tags$li("Click ", strong("Apply Offset"), "."),
                tags$li("Download the corrected ", code("*_offset_exif.csv"), " output.")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li("Same CSV as Step 1, with FileModifyDate/Date/Time shifted by the offset."))
            )
          ),

          card(
            card_header("Step 2: Merge EXIFs"),
            card_body(
              p("Combines multiple station EXIF CSVs into a single dataset and filters to target mammals."),
              tags$ol(
                tags$li("Select the folder containing all ", code("*_exif.csv"), " files from Step 1."),
                tags$li("Optionally upload a manually corrected ", code("combined_exif_all.csv"), "."),
                tags$li("Click ", strong("Run Merge"), "."),
                tags$li("Download outputs.")
              ),
              hr(),
              p(strong("Outputs:")),
              tags$ul(
                tags$li(code("combined_exif_all.csv"), " — all species combined"),
                tags$li(code("combined_exif_mammals_only.csv"), " — target mammals only")
              )
            )
          ),

          card(
            card_header("Step 3: Independent Detections"),
            card_body(
              p("Groups records into independent detections by station and species, then generates summary tables."),
              tags$ol(
                tags$li("Upload ", code("combined_exif_all.csv"), " from Step 2."),
                tags$li("Set the ", strong("independence interval"), " (default: 3600 s = 1 hour)."),
                tags$li("Optionally list stations to exclude."),
                tags$li("Click ", strong("Run Analysis"), ".")
              ),
              hr(),
              p(strong("Outputs (zipped):")),
              tags$ul(
                tags$li(code("ct_indp_det_full.csv")),
                tags$li(code("ct_indp_det.csv")),
                tags$li(code("ct_indp_det_species_summary.csv")),
                tags$li(code("ct_indp_det_station_summary.csv")),
                tags$li(code("ct_species_detection.xlsx")),
                tags$li(code("ct_indp_det_wildboar_summary.csv"), " (if Sus scrofa present)"),
                tags$li(code("ct_arboreal.xlsx"), " (if crossing remarks present)")
              )
            )
          )
        )
      ),

      accordion_panel(
        value = "about_abiotic",
        title = about_panel_title("Abiotic Monitoring Toolkit", "water", "moisture"),

        layout_column_wrap(
          width = 1 / 2,

          card(
            card_header("Water Monitoring"),
            card_body(
              p("Processes raw CSV exports from EXO2 field water quality loggers into a formatted Excel worksheet."),
              tags$ol(
                tags$li("Upload the ", strong("raw in-situ CSV"), " from the logger."),
                tags$li("Set the ", strong("time threshold"), " (default: 2 mins)."),
                tags$li("Click ", strong("Run Report"), ".")
              ),
              hr(),
              p(strong("Output sheet:"), " In-Situ Measurements"),
              tags$ul(
                tags$li("Metadata header (project name, staff, equipment)"),
                tags$li("Point No., Date, Time, Depth, Weather Condition"),
                tags$li("Conductivity, Dissolved Oxygen, pH, Salinity, Temperature, Turbidity")
              )
            )
          ),

          card(
            card_header("Noise Monitoring"),
            card_body(
              p("Calculates LAeq 1h, LAeq 12h and per-period maxima from raw 5-minute noise data."),
              tags$ol(
                tags$li("Upload the ", strong("noise CSV"), " (5-min Leq readings)."),
                tags$li("Upload the ", strong("calibration CSV"), "."),
                tags$li("Enter ", strong("site location"), " and ", strong("monitoring point"), "."),
                tags$li("Click ", strong("Run Report"), ".")
              ),
              hr(),
              p(strong("Output sheets:")),
              tags$ul(
                tags$li(strong("Noise Data"), " — calibration info + LAeq 5min/1h/12h table"),
                tags$li(strong("Summary Table"), " — daily max LAeq by time period (7am–7pm, 7pm–10pm, 10pm–7am)")
              )
            )
          )
        )
      ),

      accordion_panel(
        value = "about_arbo",
        title = about_panel_title("Arbo Report", "arbo_report", "tree"),

        layout_column_wrap(
          width = 1 / 2,

          card(
            card_header("Generate Report"),
            card_body(
              p("Converts tree assessment biodata into formatted Word arboriculture reports, one per batch of trees, complete with site photos."),
              tags$ol(
                tags$li("Upload the ", strong("tree biodata"), " (.csv) — must contain ", code("Tree.ID"), ", ", code("Date"), ", ", code("Species"), " and other assessment columns."),
                tags$li("Optionally select the ", strong("resized photos folder"), " and enter the photo folder prefix (see Resize Photos)."),
                tags$li("Set parameters (trees per report, crown spread, sort by site, date format)."),
                tags$li("Click ", strong("Generate Report"), "."),
                tags$li("Download the zipped Word document(s).")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li("One .docx per batch of trees — summary table, observations, photos, assessment, and recommendations per tree."))
            )
          ),

          card(
            card_header("Resize Photos"),
            card_body(
              p("Resizes and pads site photos so Word reports stay a manageable size."),
              tags$ol(
                tags$li("Select the ", strong("original photos"), " folder (searched recursively)."),
                tags$li("Select a ", strong("destination"), " folder for the resized copies."),
                tags$li("Set the target photo size (default: 400px)."),
                tags$li("Click ", strong("Resize Photos"), ".")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li("Resized copies saved into the destination folder, preserving the original per-inspection subfolder structure."))
            )
          )
        )
      ),

      accordion_panel(
        value = "about_stream",
        title = about_panel_title("Stream Inspection Report", "stream_report", "water"),

        card(
          card_header("Stream Inspection Report"),
          card_body(
            p("Collates aquatic fauna survey data and field photos into a formatted Excel stream inspection report."),
            tags$ol(
              tags$li("Upload the ", strong("fauna datasheet"), " (.xlsx) — must contain sheets ", code("01 Log"), " and ", code("02 DataList"), "."),
              tags$li("Select the ", strong("root photo folder"), ", structured as ", code("root / YYYYMMDD / SamplingPoint_YYYYMMDD / photo.jpg"), " — sampling point names must match the Transect column."),
              tags$li("Enter the ", strong("inspection date(s)"), " in YYYY-MM-DD format, one per line."),
              tags$li("Click ", strong("Generate Report"), "."),
              tags$li("Download the output workbook.")
            ),
            hr(),
            p(strong("Output:")),
            tags$ul(tags$li("Excel workbook — header block (inspection round, dates, surveyors, weather) plus a transposed table per sampling point with date, time, fauna observed, and embedded field photos."))
          )
        )
      ),

      accordion_panel(
        value = "about_bat",
        title = about_panel_title("Bat Recording Processing", "bat_step1", "soundwave"),

        layout_column_wrap(
          width = 1 / 2,

          card(
            card_header("Step 1: Process Meta"),
            card_body(
              p("Cleans a Kaleidoscope ", code("meta.csv"), " — one species per row with corrected scientific names — and optionally matches handheld GPS tracks and sorts the .wav files into species folders."),
              tags$ol(
                tags$li("Upload the ", strong("meta.csv"), " and set the ", strong("species delimiter"), " (default ", code("_"), ")."),
                tags$li("Optionally tick ", strong("Match handheld GPS"), " and upload the tracks CSV."),
                tags$li("Optionally tick ", strong("Sort .wav files"), " and select the folder of .wav files."),
                tags$li("Click ", strong("Process Meta"), " and download the result.")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(
                tags$li(code("meta_cleaned.csv"), " (no GPS) or ", code("meta_matched.csv"), " (GPS matched)"),
                tags$li("If sorting: .wav files copied into per-species subfolders, zipped for download.")
              )
            )
          ),

          card(
            card_header("Step 2: Combine Meta"),
            card_body(
              p("Combines multiple cleaned/matched CSVs from Step 1 into a single dataset."),
              tags$ol(
                tags$li("Select the folder containing the Step 1 CSVs."),
                tags$li("Click ", strong("Combine"), " and download ", code("meta_combined.csv"), ".")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li(code("meta_combined.csv"), " — all rows from every CSV in the folder."))
            )
          ),

          card(
            card_header("Sub-sample Files"),
            card_body(
              p("Copies a subset of raw .wav files by their recording minute, before manual ID-ing (e.g. keep 5 minutes out of every 30)."),
              tags$ol(
                tags$li("Select the folder of raw .wav files (named ", code("Project_Date_Time"), ")."),
                tags$li("Set the ", strong("minutes to keep"), "."),
                tags$li("Click ", strong("Sub-sample"), ".")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li("A ", code("<folder>_subsampled"), " folder created next to the input folder, containing the matched files."))
            )
          ),

          card(
            card_header("Recover Meta"),
            card_body(
              p("Last resort: reverse-engineers a ", code("meta.csv"), " from already-sorted species folders plus the raw .wav EXIF timestamps."),
              tags$ol(
                tags$li("Select the ", strong("sorted (processed)"), " folder (one subfolder per species)."),
                tags$li("Select the ", strong("raw"), " folder."),
                tags$li("Click ", strong("Recover Meta"), " and download the result.")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li(code("meta_reverse.csv"), " — may not be readable by Kaleidoscope."))
            )
          )
        )
      ),

      accordion_panel(
        value = "about_flora",
        title = about_panel_title("BTNR Flora Photo Filing", "flora_sort", "images"),

        layout_column_wrap(
          width = 1 / 2,

          card(
            card_header("Sort Photos"),
            card_body(
              p("Files raw flora survey photos into ", code("Family/Species/Tag"), " folders, using the photo filing sheet of the project master datasheet to look up which photos belong to which tagged specimen."),
              tags$ol(
                tags$li("Upload the ", strong("master datasheet"), " (.xlsx) and confirm the photo filing sheet name."),
                tags$li("Select the ", strong("raw photos folder"), " — the parent of the per-session folders named in ", code("ZOOM_CAM"), " and ", code("FS_CAM"), "."),
                tags$li("Select the ", strong("sorted photos folder"), " to file into."),
                tags$li("Enter the ", strong("STATUS"), " value(s) to sort, e.g. ", code("Batch 3.1, Batch 3.2"), "."),
                tags$li("Click ", strong("Sort Photos"), ".")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(
                tags$li("Photos copied to ", code("Family/Species/Tag/Tag_photo.jpg"), " — existing copies are skipped, so re-runs are safe."),
                tags$li("Photos whose family or species changed since the last run are removed from their old location, and empty folders cleaned up.")
              ),
              hr(),
              p(class = "mb-0",
                strong("Note: "),
                "the run stops with an error if any photo folder named in the datasheet is missing or misnamed, or if a photo number resolves to no file. Fix the datasheet and re-run.")
            )
          ),

          card(
            card_header("Re-sort Tag Folders"),
            card_body(
              p("Restructures an older ", code("Family/Species"), " photo tree into ", code("Family/Species/Tag"), ". Only needed for batches sorted before per-tag subfolders were introduced."),
              tags$ol(
                tags$li("Select the ", strong("existing sorted folder"), "."),
                tags$li("Select an ", strong("updated folder"), " for the restructured copy."),
                tags$li("Click ", strong("Re-sort Folders"), ".")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(tags$li("A copy of the tree with a per-tag level inserted, where the tag is read from each file name up to the first underscore."))
            )
          )
        )
      ),

      accordion_panel(
        value = "about_cag",
        title = about_panel_title("CAG Photo Renaming", "flora_rename", "pencil-square"),

        layout_column_wrap(
          width = 1 / 2,

          card(
            card_header("Rename Photos"),
            card_body(
              p("Renames CAG field photos using a ", strong("Tree ID"), " to ", strong("Photo-number"), " mapping held in an Excel sheet. Each tree's photos are named ", code("<TreeID>_01"), ", ", code("<TreeID>_02"), ", and so on, matched by the trailing number in each file (e.g. ", code("IMG_9380.jpg"), " -> ", code("P342_01.jpg"), ")."),
              tags$ol(
                tags$li("Upload the ", strong("datasheet"), " (.xlsx) and enter the ", strong("sheet name"), ", e.g. ", code("T1 20260708"), "."),
                tags$li("Select the ", strong("photo folder"), " holding the files to rename."),
                tags$li("Confirm the ", strong("Tree ID"), " and ", strong("Photo"), " column names (defaults ", code("Tree ID"), " and ", code("Photo"), ")."),
                tags$li("Choose a ", strong("mode"), " and click ", strong("Run"), ".")
              ),
              hr(),
              p(strong("Photo column formats:")),
              tags$ul(
                tags$li(code("9230"), " — a single photo."),
                tags$li(code("9105-07"), ", ", code("9108-110"), ", ", code("9198-9201"), " — ranges, with abbreviated ends filled in."),
                tags$li(code("9230-32, 9240"), " — several comma/semicolon-separated tokens.")
              )
            )
          ),

          card(
            card_header("Modes & output"),
            card_body(
              p(strong("Modes:")),
              tags$ul(
                tags$li(strong("Preview only"), " — writes nothing; shows the full rename plan so you can check it first."),
                tags$li(strong("Copy"), " — copies renamed photos into a ", code("renamed"), " subfolder, leaving the originals untouched."),
                tags$li(strong("Rename in place"), " — renames the originals directly (two-phase, so a new name never overwrites a file still waiting to be renamed).")
              ),
              hr(),
              p(strong("Output:")),
              tags$ul(
                tags$li("The rename plan is shown below the log, with a ", code("status"), " column flagging ", code("MISSING_FILE"), " and ", code("NAME_CLASH"), " rows that are skipped."),
                tags$li("Copy and Rename modes also write a timestamped ", code("rename_log_*.csv"), " audit trail into the photo folder.")
              )
            )
          )
        )
      )
    ),

    div(
      class = "text-muted text-center small mt-2",
      paste0("Camphora Toolkit Hub — ", VERSION, "  |  Last updated: ", UPDATE_DATE)
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(href = "https://github.com/JoejynWan/CamphoraToolkit",
           target = "_blank",
           bsicons::bs_icon("github"), " GitHub")
  )
)


#### Server ####

server <- function(input, output, session) {

  # ── Shared: shinyFiles volumes (all local drives including Google Drive G:) ─────────────────────
  volumes <- c(Home = fs::path_home(), getVolumes()())

  shinyDirChoose(input, "s1_path_processed",     roots = volumes, session = session)
  shinyDirChoose(input, "s1_path_raw",           roots = volumes, session = session)
  shinyDirChoose(input, "s2_exif_folder",        roots = volumes, session = session)
  shinyDirChoose(input, "arbo_photos_dir",       roots = volumes, session = session)
  shinyDirChoose(input, "arbophoto_source_dir",  roots = volumes, session = session)
  shinyDirChoose(input, "arbophoto_dest_dir",    roots = volumes, session = session)
  shinyDirChoose(input, "si_photos_dir",         roots = volumes, session = session)
  shinyDirChoose(input, "bat1_wav_dir",          roots = volumes, session = session)
  shinyDirChoose(input, "bat2_meta_dir",         roots = volumes, session = session)
  shinyDirChoose(input, "bat_sub_raw_dir",       roots = volumes, session = session)
  shinyDirChoose(input, "bat_rec_proc_dir",      roots = volumes, session = session)
  shinyDirChoose(input, "bat_rec_raw_dir",       roots = volumes, session = session)
  shinyDirChoose(input, "flora_photos_dir",      roots = volumes, session = session)
  shinyDirChoose(input, "flora_sorted_dir",      roots = volumes, session = session)
  shinyDirChoose(input, "flora_resort_src_dir",  roots = volumes, session = session)
  shinyDirChoose(input, "flora_resort_dest_dir", roots = volumes, session = session)
  shinyDirChoose(input, "cag_photo_dir",         roots = volumes, session = session)


  # ── Hub: filter bar + card grid + in-app navigation ─────────────────────────────────────────────
  selected_category <- reactiveVal("All")

  output$filter_bar <- renderUI({
    cats <- c("All", all_categories())
    div(class = "filter-bar",
      lapply(cats, function(cat) {
        tags$button(
          class   = paste("filter-btn", if (cat == selected_category()) "active"),
          onclick = sprintf("Shiny.setInputValue('filter_cat', '%s', {priority: 'event'})", cat),
          cat
        )
      })
    )
  })

  observeEvent(input$filter_cat, selected_category(input$filter_cat))

  output$project_grid <- renderUI({
    cat_filter <- selected_category()
    visible    <- if (cat_filter == "All") PROJECTS else
                  Filter(function(p) p$category == cat_filter, PROJECTS)

    if (length(visible) == 0) {
      return(div(class = "empty-state",
        bsicons::bs_icon("inbox", size = "2.5rem"),
        p("No tools in this category yet.")))
    }
    div(class = "card-grid", lapply(visible, project_card))
  })

  observeEvent(input$hub_nav_to, {
    nav_select("main_navbar", input$hub_nav_to)
  })


  # ── Fauna Impact Assessment ─────────────────────────────────────────────────────────────────────
  ia_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  ia_log <- make_logger(ia_rv)

  observeEvent(input$ia_run_btn, {

    ia_rv$log_lines    <- character(0)
    ia_rv$output_path  <- NULL
    ia_rv$preview_data <- NULL

    if (is.null(input$ia_species_list_file)) { ia_log("ERROR: No species list uploaded."); return() }
    if (is.null(input$ia_fauna_db_file))      { ia_log("ERROR: No fauna database uploaded."); return() }
    if (!file.exists(IA_MATRIX_PATH))         { ia_log(paste("ERROR: Matrix file not found at", IA_MATRIX_PATH)); return() }

    in_path  <- file_path_sans_ext(input$ia_species_list_file$datapath)
    out_path <- paste0(in_path, "_output.xlsx")

    withProgress(message = "Running impact assessment...", value = 0, {
      tryCatch({

        run_impact_assessment(
          species_list_path   = input$ia_species_list_file$datapath,
          fauna_database_path = input$ia_fauna_db_file$datapath,
          matrix_path          = IA_MATRIX_PATH,
          output_path          = out_path,
          log = function(msg) {
            ia_log(msg)
            incProgress(1 / 6)   # 6 major steps in run_impact_assessment()
          }
        )

        ia_rv$output_path  <- out_path
        ia_rv$preview_data <- tryCatch(
          read.xlsx(out_path, sheet = "Receptor"),
          error = function(e) NULL
        )
        ia_log("Done! Click 'Download output' to save the file.")

      }, error = function(e) {
        ia_log(paste("ERROR:", conditionMessage(e)))
      })
    })
  })

  output$ia_log_output <- renderText({
    if (length(ia_rv$log_lines) == 0) "No output yet. Upload files and click Run."
    else paste(ia_rv$log_lines, collapse = "\n")
  })

  output$ia_preview_table <- renderTable({
    req(ia_rv$preview_data)
    head(ia_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$ia_download_ui <- renderUI({
    req(ia_rv$output_path)
    downloadButton("ia_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download output (.xlsx)"),
                   class = "btn-success w-100")
  })

  output$ia_download_btn <- downloadHandler(
    filename = function() paste0("IA_output_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) { req(ia_rv$output_path); file.copy(ia_rv$output_path, file) }
  )


  # ── CT Step 1: EXIF Extraction ──────────────────────────────────────────────────────────────────
  s1_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  s1_log <- make_logger(s1_rv)

  s1_processed_dir <- reactive({
    req(input$s1_path_processed)
    parseDirPath(volumes, input$s1_path_processed)
  })

  s1_raw_dir <- reactive({
    req(input$s1_path_raw)
    parseDirPath(volumes, input$s1_path_raw)
  })

  output$s1_processed_path_display <- renderText({
    d <- tryCatch(s1_processed_dir(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  output$s1_raw_path_display <- renderText({
    d <- tryCatch(s1_raw_dir(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  observeEvent(input$s1_run_btn, {
    s1_rv$log_lines    <- character(0)
    s1_rv$output_path  <- NULL
    s1_rv$preview_data <- NULL

    proc <- tryCatch(s1_processed_dir(), error = function(e) "")
    raw  <- tryCatch(s1_raw_dir(),       error = function(e) "")

    if (length(proc) == 0 || proc == "") { s1_log("ERROR: Please select a processed data folder."); return() }
    if (length(raw)  == 0 || raw  == "") { s1_log("ERROR: Please select a raw data folder.");       return() }
    if (!file.exists(SPECIES_DB_PATH))   { s1_log(paste("ERROR: Species Database not found at:", SPECIES_DB_PATH)); return() }

    withProgress(message = "Extracting EXIF data...", value = 0, {
      tryCatch({
        s1_log("Running EXIF extraction — this may take a while while raw videos are read...")
        incProgress(0.1)

        extract_exif(
          path_processed        = proc,
          path_raw              = raw,
          path_species_database = SPECIES_DB_PATH,
          log_fn                = s1_log
        )

        out_name           <- paste(basename(proc), "exif.csv", sep = "_")
        out_path           <- file.path(proc, out_name)
        s1_rv$output_path  <- out_path
        s1_rv$preview_data <- read.csv(out_path)

        incProgress(0.9)
        s1_log(paste("Done! Output saved to:", out_path))
        s1_log(paste("Total records:", nrow(s1_rv$preview_data)))

      }, error = function(e) s1_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$s1_log_output <- renderText({
    if (length(s1_rv$log_lines) == 0) "No output yet. Select folders and click Run."
    else paste(s1_rv$log_lines, collapse = "\n")
  })

  output$s1_preview_table <- renderTable({
    req(s1_rv$preview_data)
    head(s1_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$s1_download_ui <- renderUI({
    req(s1_rv$output_path)
    downloadButton("s1_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download EXIF CSV"),
                   class = "btn-success w-100")
  })

  output$s1_download_btn <- downloadHandler(
    filename = function() paste(basename(tryCatch(s1_processed_dir(), error = function(e) "station")), "exif.csv", sep = "_"),
    content  = function(file) { req(s1_rv$output_path); file.copy(s1_rv$output_path, file) }
  )


  # ── CT Step 1.1: Offset DateTime ────────────────────────────────────────────────────────────────
  s1a_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  s1a_log <- make_logger(s1a_rv)

  observeEvent(input$s1a_run_btn, {
    s1a_rv$log_lines    <- character(0)
    s1a_rv$output_path  <- NULL
    s1a_rv$preview_data <- NULL

    if (is.null(input$s1a_exif_file))          { s1a_log("ERROR: No exif CSV uploaded."); return() }
    if (trimws(input$s1a_offset) == "")        { s1a_log("ERROR: Please enter an hour offset or the correct first-video DateTime."); return() }

    withProgress(message = "Applying DateTime offset...", value = 0, {
      tryCatch({
        incProgress(0.2)

        out_path <- offset_datetime(
          exif_path = input$s1a_exif_file$datapath,
          offset     = trimws(input$s1a_offset),
          log        = s1a_log
        )

        s1a_rv$output_path  <- out_path
        s1a_rv$preview_data <- read.csv(out_path)
        incProgress(0.8)

      }, error = function(e) s1a_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$s1a_log_output <- renderText({
    if (length(s1a_rv$log_lines) == 0) "No output yet. Upload an exif CSV and click Apply Offset."
    else paste(s1a_rv$log_lines, collapse = "\n")
  })

  output$s1a_preview_table <- renderTable({
    req(s1a_rv$preview_data)
    head(s1a_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$s1a_download_ui <- renderUI({
    req(s1a_rv$output_path)
    downloadButton("s1a_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download output (.csv)"),
                   class = "btn-success w-100")
  })

  output$s1a_download_btn <- downloadHandler(
    filename = function() paste0(file_path_sans_ext(input$s1a_exif_file$name), "_offset_exif.csv"),
    content  = function(file) { req(s1a_rv$output_path); file.copy(s1a_rv$output_path, file) }
  )


  # ── CT Step 2: Merge EXIFs ──────────────────────────────────────────────────────────────────────
  s2_rv <- reactiveValues(
    log_lines    = character(0),
    path_all     = NULL,
    path_mammals = NULL,
    preview_data = NULL
  )
  s2_log <- make_logger(s2_rv)

  s2_exif_dir <- reactive({
    req(input$s2_exif_folder)
    parseDirPath(volumes, input$s2_exif_folder)
  })

  output$s2_folder_display <- renderText({
    d <- tryCatch(s2_exif_dir(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  observeEvent(input$s2_run_btn, {
    s2_rv$log_lines    <- character(0)
    s2_rv$path_all     <- NULL
    s2_rv$path_mammals <- NULL
    s2_rv$preview_data <- NULL

    exif_dir <- tryCatch(s2_exif_dir(), error = function(e) "")
    if (length(exif_dir) == 0 || exif_dir == "") { s2_log("ERROR: Please select the folder containing EXIF CSV files."); return() }
    if (!file.exists(SPECIES_DB_PATH))            { s2_log(paste("ERROR: Species Database not found at:", SPECIES_DB_PATH)); return() }

    withProgress(message = "Merging EXIF files...", value = 0, {
      tryCatch({
        input_combined_path <- if (!is.null(input$s2_edited_combined))
          input$s2_edited_combined$datapath else NA

        incProgress(0.1)
        merging_exifs(
          path_exif_folder      = exif_dir,
          path_species_database = SPECIES_DB_PATH,
          input_combined        = input_combined_path,
          log_fn                = s2_log
        )

        s2_rv$path_all     <- file.path(exif_dir, "combined_exif_all.csv")
        s2_rv$path_mammals <- file.path(exif_dir, "combined_exif_mammals_only.csv")
        s2_rv$preview_data <- read.csv(s2_rv$path_all)

        incProgress(0.9)
        s2_log(paste("Done!", nrow(s2_rv$preview_data), "total records. Click 'Download' to save."))

      }, error = function(e) s2_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$s2_log_output <- renderText({
    if (length(s2_rv$log_lines) == 0) "No output yet. Select a folder and click Run."
    else paste(s2_rv$log_lines, collapse = "\n")
  })

  output$s2_preview_table <- renderTable({
    req(s2_rv$preview_data)
    head(s2_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$s2_download_ui <- renderUI({
    req(s2_rv$path_all)
    tagList(
      downloadButton("s2_download_all",
                     label = tagList(bsicons::bs_icon("download"), " Download all species CSV"),
                     class = "btn-success w-100 mb-2"),
      downloadButton("s2_download_mammals",
                     label = tagList(bsicons::bs_icon("download"), " Download mammals only CSV"),
                     class = "btn-success w-100")
    )
  })

  output$s2_download_all <- downloadHandler(
    filename = function() "combined_exif_all.csv",
    content  = function(file) { req(s2_rv$path_all); file.copy(s2_rv$path_all, file) }
  )

  output$s2_download_mammals <- downloadHandler(
    filename = function() "combined_exif_mammals_only.csv",
    content  = function(file) { req(s2_rv$path_mammals); file.copy(s2_rv$path_mammals, file) }
  )


  # ── CT Step 3: Independent Detections ───────────────────────────────────────────────────────────
  s3_rv <- reactiveValues(
    log_lines    = character(0),
    zip_path     = NULL,
    preview_data = NULL
  )
  s3_log <- make_logger(s3_rv)

  observeEvent(input$s3_run_btn, {
    s3_rv$log_lines    <- character(0)
    s3_rv$zip_path     <- NULL
    s3_rv$preview_data <- NULL

    if (is.null(input$s3_ct_file))    { s3_log("ERROR: No combined EXIF CSV uploaded."); return() }
    if (!file.exists(SPECIES_DB_PATH)) { s3_log(paste("ERROR: Species Database not found at:", SPECIES_DB_PATH)); return() }

    withProgress(message = "Calculating independent detections...", value = 0, {
      tryCatch({
        rm_stations_raw <- trimws(input$s3_rm_stations)
        rm_stations     <- if (nchar(rm_stations_raw) == 0) NA else
                           trimws(strsplit(rm_stations_raw, ",")[[1]])

        incProgress(0.1)
        indp_dets(
          input_ct_file         = input$s3_ct_file$datapath,
          path_species_database = SPECIES_DB_PATH,
          indp_interval         = input$s3_indp_interval,
          rm_stations           = rm_stations,
          log_fn                = s3_log
        )

        output_dir        <- dirname(input$s3_ct_file$datapath)
        species_summ_path <- file.path(output_dir, "ct_indp_det_species_summary.csv")
        if (file.exists(species_summ_path))
          s3_rv$preview_data <- read.csv(species_summ_path)

        output_files <- list.files(output_dir, pattern = "^ct_", full.names = TRUE)
        zip_path     <- file.path(tempdir(), "ct_independent_detections.zip")
        zip::zip(zip_path, files = output_files, mode = "cherry-pick")
        s3_rv$zip_path <- zip_path

        incProgress(0.9)
        s3_log("Done! All output files zipped. Click 'Download' to save.")

      }, error = function(e) s3_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$s3_log_output <- renderText({
    if (length(s3_rv$log_lines) == 0) "No output yet. Upload a combined EXIF CSV and click Run."
    else paste(s3_rv$log_lines, collapse = "\n")
  })

  output$s3_preview_table <- renderTable({
    req(s3_rv$preview_data)
    head(s3_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$s3_download_ui <- renderUI({
    req(s3_rv$zip_path)
    downloadButton("s3_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download all outputs (.zip)"),
                   class = "btn-success w-100")
  })

  output$s3_download_btn <- downloadHandler(
    filename = function() "ct_independent_detections.zip",
    content  = function(file) { req(s3_rv$zip_path); file.copy(s3_rv$zip_path, file) }
  )


  # ── Abiotic: Water Monitoring ───────────────────────────────────────────────────────────────────
  water_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  water_log <- make_logger(water_rv)

  observeEvent(input$water_run_btn, {
    water_rv$log_lines    <- character(0)
    water_rv$output_path  <- NULL
    water_rv$preview_data <- NULL

    if (is.null(input$water_data_file)) { water_log("ERROR: No in-situ data file uploaded."); return() }

    withProgress(message = "Processing in-situ data...", value = 0, {
      tryCatch({
        water_log("Reading and cleaning raw data...")
        incProgress(0.3)

        in_situ(
          path_input     = input$water_data_file$datapath,
          time_threshold = input$water_time_threshold,
          date_format    = input$date_format
        )

        out_path              <- paste0(file_path_sans_ext(input$water_data_file$datapath), "_clean.xlsx")
        water_rv$output_path  <- out_path
        incProgress(0.5)
        water_log("Loading output preview...")

        water_rv$preview_data <- tryCatch(
          read.xlsx(out_path, sheet = "In-Situ Measurements"),
          error = function(e) NULL
        )
        incProgress(0.2)
        water_log("Done! Click 'Download output' to save the file.")

      }, error = function(e) water_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$water_log_output <- renderText({
    if (length(water_rv$log_lines) == 0) "No output yet. Upload a file and click Run."
    else paste(water_rv$log_lines, collapse = "\n")
  })

  output$water_preview_table <- renderTable({
    req(water_rv$preview_data)
    head(water_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$water_download_ui <- renderUI({
    req(water_rv$output_path)
    downloadButton("water_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download output (.xlsx)"),
                   class = "btn-success w-100")
  })

  output$water_download_btn <- downloadHandler(
    filename = function() paste0(file_path_sans_ext(input$water_data_file$name), "_clean.xlsx"),
    content  = function(file) { req(water_rv$output_path); file.copy(water_rv$output_path, file) }
  )


  # ── Abiotic: Noise Monitoring ───────────────────────────────────────────────────────────────────
  noise_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  noise_log <- make_logger(noise_rv)

  observeEvent(input$noise_run_btn, {
    noise_rv$log_lines    <- character(0)
    noise_rv$output_path  <- NULL
    noise_rv$preview_data <- NULL

    if (is.null(input$noise_data_file))         { noise_log("ERROR: No noise data file uploaded.");    return() }
    if (is.null(input$noise_calibration_file))  { noise_log("ERROR: No calibration file uploaded.");   return() }
    if (trimws(input$noise_location)    == "")  { noise_log("ERROR: Site location is required.");      return() }
    if (trimws(input$noise_monitoring_pt) == "") { noise_log("ERROR: Monitoring point is required."); return() }

    withProgress(message = "Processing noise data...", value = 0, {
      tryCatch({
        noise_log("Reading noise and calibration data...")
        incProgress(0.3)

        noise_report(
          location         = trimws(input$noise_location),
          monitoring_pt    = trimws(input$noise_monitoring_pt),
          path_noise       = input$noise_data_file$datapath,
          path_calibration = input$noise_calibration_file$datapath
        )

        out_path <- file.path(
          dirname(input$noise_data_file$datapath),
          paste0("NoiseReport_", trimws(input$noise_location), "_",
                 trimws(input$noise_monitoring_pt), ".xlsx")
        )
        noise_rv$output_path <- out_path
        incProgress(0.5)
        noise_log("Loading output preview...")

        noise_rv$preview_data <- tryCatch(
          read.xlsx(out_path, sheet = "Noise Data"),
          error = function(e) NULL
        )
        incProgress(0.2)
        noise_log("Done! Click 'Download output' to save the file.")

      }, error = function(e) noise_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$noise_log_output <- renderText({
    if (length(noise_rv$log_lines) == 0) "No output yet. Upload files and click Run."
    else paste(noise_rv$log_lines, collapse = "\n")
  })

  output$noise_preview_table <- renderTable({
    req(noise_rv$preview_data)
    head(noise_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$noise_download_ui <- renderUI({
    req(noise_rv$output_path)
    downloadButton("noise_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download output (.xlsx)"),
                   class = "btn-success w-100")
  })

  output$noise_download_btn <- downloadHandler(
    filename = function() paste0("NoiseReport_", trimws(input$noise_location), "_",
                                 trimws(input$noise_monitoring_pt), ".xlsx"),
    content  = function(file) { req(noise_rv$output_path); file.copy(noise_rv$output_path, file) }
  )


  # ── Arbo Report: Generate Report ────────────────────────────────────────────────────────────────
  arbo_photos_dir_sel <- reactive({
    req(input$arbo_photos_dir)
    parseDirPath(volumes, input$arbo_photos_dir)
  })

  output$arbo_photos_dir_display <- renderText({
    d <- tryCatch(arbo_photos_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  arbo_rv <- reactiveValues(
    log_lines    = character(0),
    zip_path     = NULL,
    preview_data = NULL
  )
  arbo_log <- make_logger(arbo_rv)

  observeEvent(input$arbo_run_btn, {
    arbo_rv$log_lines    <- character(0)
    arbo_rv$zip_path     <- NULL
    arbo_rv$preview_data <- NULL

    if (is.null(input$arbo_biodata_file)) { arbo_log("ERROR: No tree biodata CSV uploaded."); return() }

    photos_dir <- NULL
    if (isTRUE(input$arbo_incl_photos)) {
      photos_dir <- tryCatch(arbo_photos_dir_sel(), error = function(e) "")
      if (length(photos_dir) == 0 || photos_dir == "") { arbo_log("ERROR: Please select a resized photos folder, or uncheck 'Include photos in report'."); return() }
      if (trimws(input$arbo_photo_prefix) == "")        { arbo_log("ERROR: Please enter the photo folder prefix."); return() }
    }

    select_ids_raw <- trimws(input$arbo_select_ids)
    select_ids     <- if (nchar(select_ids_raw) == 0) NULL else
                      trimws(strsplit(select_ids_raw, ",")[[1]])

    withProgress(message = "Generating Arbo report(s)...", value = 0, {
      tryCatch({

        incProgress(0.1)
        output_paths <- run_arbo_report(
          path_biodata        = input$arbo_biodata_file$datapath,
          rmd_path             = ARBO_RMD_PATH,
          output_dir           = file.path(tempdir(), "arbo_reports"),
          resized_photos_dir   = photos_dir,
          photo_prefix         = trimws(input$arbo_photo_prefix),
          report_size          = input$arbo_report_size,
          select_ids           = select_ids,
          incl_crown_spread    = isTRUE(input$arbo_incl_crown_spread),
          sort_site            = isTRUE(input$arbo_sort_site),
          date_format          = input$arbo_date_format,
          log                  = arbo_log
        )

        arbo_rv$preview_data <- data.frame(`Report file` = basename(output_paths), check.names = FALSE)

        zip_path <- file.path(tempdir(), "arbo_reports.zip")
        zip::zip(zip_path, files = output_paths, mode = "cherry-pick")
        arbo_rv$zip_path <- zip_path

        incProgress(0.9)
        arbo_log("Done! All reports zipped. Click 'Download' to save.")

      }, error = function(e) arbo_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$arbo_log_output <- renderText({
    if (length(arbo_rv$log_lines) == 0) "No output yet. Upload biodata and click Generate Report."
    else paste(arbo_rv$log_lines, collapse = "\n")
  })

  output$arbo_preview_table <- renderTable({
    req(arbo_rv$preview_data)
    arbo_rv$preview_data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$arbo_download_ui <- renderUI({
    req(arbo_rv$zip_path)
    downloadButton("arbo_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download reports (.zip)"),
                   class = "btn-success w-100")
  })

  output$arbo_download_btn <- downloadHandler(
    filename = function() "arbo_reports.zip",
    content  = function(file) { req(arbo_rv$zip_path); file.copy(arbo_rv$zip_path, file) }
  )


  # ── Arbo Report: Resize Photos ──────────────────────────────────────────────────────────────────
  arbophoto_source_dir_sel <- reactive({
    req(input$arbophoto_source_dir)
    parseDirPath(volumes, input$arbophoto_source_dir)
  })

  arbophoto_dest_dir_sel <- reactive({
    req(input$arbophoto_dest_dir)
    parseDirPath(volumes, input$arbophoto_dest_dir)
  })

  output$arbophoto_source_dir_display <- renderText({
    d <- tryCatch(arbophoto_source_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  output$arbophoto_dest_dir_display <- renderText({
    d <- tryCatch(arbophoto_dest_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  arbophoto_rv <- reactiveValues(log_lines = character(0))
  arbophoto_log <- make_logger(arbophoto_rv)

  observeEvent(input$arbophoto_run_btn, {
    arbophoto_rv$log_lines <- character(0)

    source_dir <- tryCatch(arbophoto_source_dir_sel(), error = function(e) "")
    dest_dir   <- tryCatch(arbophoto_dest_dir_sel(),   error = function(e) "")

    if (length(source_dir) == 0 || source_dir == "") { arbophoto_log("ERROR: Please select the original photos folder."); return() }
    if (length(dest_dir)   == 0 || dest_dir   == "") { arbophoto_log("ERROR: Please select a destination folder.");        return() }

    withProgress(message = "Resizing photos...", value = 0, {
      tryCatch({
        incProgress(0.1)
        resize_arbo_photos(
          photo_dir          = source_dir,
          resized_photos_dir = dest_dir,
          photo_size          = input$arbophoto_size,
          log                 = arbophoto_log
        )
        incProgress(0.9)

      }, error = function(e) arbophoto_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$arbophoto_log_output <- renderText({
    if (length(arbophoto_rv$log_lines) == 0) "No output yet. Select folders and click Resize Photos."
    else paste(arbophoto_rv$log_lines, collapse = "\n")
  })


  # ── Stream Inspection Report ────────────────────────────────────────────────────────────────────
  si_photos_dir_sel <- reactive({
    req(input$si_photos_dir)
    parseDirPath(volumes, input$si_photos_dir)
  })

  output$si_photos_dir_display <- renderText({
    d <- tryCatch(si_photos_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  si_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  si_log <- make_logger(si_rv)

  observeEvent(input$si_run_btn, {
    si_rv$log_lines    <- character(0)
    si_rv$output_path  <- NULL
    si_rv$preview_data <- NULL

    if (is.null(input$si_fauna_file)) { si_log("ERROR: No fauna datasheet uploaded."); return() }

    photos_dir <- tryCatch(si_photos_dir_sel(), error = function(e) "")
    if (length(photos_dir) == 0 || photos_dir == "") { si_log("ERROR: Please select the root photo folder."); return() }

    dates_raw <- trimws(unlist(strsplit(input$si_dates, "[,\n]")))
    dates     <- dates_raw[nchar(dates_raw) > 0]
    dates     <- dates[!is.na(suppressWarnings(as.Date(dates, format = "%Y-%m-%d")))]
    if (length(dates) == 0) { si_log("ERROR: No valid inspection dates entered. Use YYYY-MM-DD format."); return() }

    withProgress(message = "Generating stream report...", value = 0, {
      tryCatch({
        incProgress(0.1)
        out_path <- stream_report(
          path_fauna_data = input$si_fauna_file$datapath,
          path_photos_dir = photos_dir,
          inspection_date = dates,
          output_dir      = file.path(tempdir(), "stream_report"),
          log             = si_log
        )
        si_rv$output_path <- out_path
        incProgress(0.8)

        # Preview: read back the transposed table rows (header + first photo row)
        si_rv$preview_data <- tryCatch({
          raw <- read.xlsx(out_path, sheet = 1, colNames = FALSE, skipEmptyRows = FALSE)
          raw[9:min(13, nrow(raw)), ]
        }, error = function(e) NULL)
        incProgress(0.1)

      }, error = function(e) si_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$si_log_output <- renderText({
    if (length(si_rv$log_lines) == 0) "No output yet. Upload a datasheet, select photos, and click Generate Report."
    else paste(si_rv$log_lines, collapse = "\n")
  })

  output$si_preview_table <- renderTable({
    req(si_rv$preview_data)
    si_rv$preview_data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "", colnames = FALSE)

  output$si_download_ui <- renderUI({
    req(si_rv$output_path)
    downloadButton("si_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download report (.xlsx)"),
                   class = "btn-success w-100")
  })

  output$si_download_btn <- downloadHandler(
    filename = function() paste0("StreamInspection_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) { req(si_rv$output_path); file.copy(si_rv$output_path, file) }
  )


  # ── Bat: Step 1 Process Meta ────────────────────────────────────────────────────────────────────
  bat1_wav_dir_sel <- reactive({
    req(input$bat1_wav_dir)
    parseDirPath(volumes, input$bat1_wav_dir)
  })

  output$bat1_wav_dir_display <- renderText({
    d <- tryCatch(bat1_wav_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  bat1_rv <- reactiveValues(
    log_lines    = character(0),
    meta_path    = NULL,
    zip_path     = NULL,
    preview_data = NULL
  )
  bat1_log <- make_logger(bat1_rv)

  observeEvent(input$bat1_run_btn, {
    bat1_rv$log_lines    <- character(0)
    bat1_rv$meta_path    <- NULL
    bat1_rv$zip_path     <- NULL
    bat1_rv$preview_data <- NULL

    if (is.null(input$bat1_meta_file)) { bat1_log("ERROR: No meta.csv uploaded."); return() }
    if (!file.exists(BAT_SPECIES_DB_PATH)) { bat1_log(paste("ERROR: Bat species database not found at:", BAT_SPECIES_DB_PATH)); return() }

    delimiter <- input$bat1_delimiter
    if (is.null(delimiter) || delimiter == "") delimiter <- "_"

    gps_file <- NA
    if (isTRUE(input$bat1_match_gps)) {
      if (is.null(input$bat1_gps_file)) { bat1_log("ERROR: 'Match handheld GPS' is ticked but no GPS tracks CSV was uploaded."); return() }
      gps_file <- input$bat1_gps_file$datapath
    }

    wav_folder <- NA
    if (isTRUE(input$bat1_sort_wav)) {
      wav_folder <- tryCatch(bat1_wav_dir_sel(), error = function(e) "")
      if (length(wav_folder) == 0 || wav_folder == "") { bat1_log("ERROR: 'Sort .wav files' is ticked but no folder was selected."); return() }
    }

    ## Fresh unique output dir (sort_wav_files requires an empty 'out' folder)
    out_dir <- file.path(tempdir(), paste0("bat_step1_", as.integer(Sys.time())))

    withProgress(message = "Processing bat meta...", value = 0, {
      tryCatch({
        incProgress(0.1)
        meta_path <- process_bat_meta(
          meta_file         = input$bat1_meta_file$datapath,
          species_db_path   = BAT_SPECIES_DB_PATH,
          delimiter         = delimiter,
          wav_folder        = wav_folder,
          handheld_gps_file = gps_file,
          output_dir        = out_dir,
          log               = bat1_log
        )
        bat1_rv$meta_path    <- meta_path
        bat1_rv$preview_data <- tryCatch(read.csv(meta_path), error = function(e) NULL)
        incProgress(0.7)

        ## Zip the sorted .wav folders for download, if any
        sorted_out <- file.path(out_dir, "out")
        if (dir.exists(sorted_out)) {
          zip_path <- file.path(tempdir(), "bat_sorted_wav.zip")
          if (file.exists(zip_path)) unlink(zip_path)
          zip::zip(zip_path, files = list.files(sorted_out, recursive = TRUE, full.names = TRUE),
                   mode = "cherry-pick")
          bat1_rv$zip_path <- zip_path
          bat1_log("Sorted .wav files zipped for download.")
        }
        incProgress(0.2)

      }, error = function(e) bat1_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$bat1_log_output <- renderText({
    if (length(bat1_rv$log_lines) == 0) "No output yet. Upload a meta.csv and click Process Meta."
    else paste(bat1_rv$log_lines, collapse = "\n")
  })

  output$bat1_preview_table <- renderTable({
    req(bat1_rv$preview_data)
    head(bat1_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$bat1_download_ui <- renderUI({
    req(bat1_rv$meta_path)
    tagList(
      downloadButton("bat1_download_meta",
                     label = tagList(bsicons::bs_icon("download"), " Download cleaned meta (.csv)"),
                     class = "btn-success w-100 mb-2"),
      if (!is.null(bat1_rv$zip_path))
        downloadButton("bat1_download_wav",
                       label = tagList(bsicons::bs_icon("download"), " Download sorted .wav (.zip)"),
                       class = "btn-success w-100")
    )
  })

  output$bat1_download_meta <- downloadHandler(
    filename = function() basename(bat1_rv$meta_path),
    content  = function(file) { req(bat1_rv$meta_path); file.copy(bat1_rv$meta_path, file) }
  )

  output$bat1_download_wav <- downloadHandler(
    filename = function() "bat_sorted_wav.zip",
    content  = function(file) { req(bat1_rv$zip_path); file.copy(bat1_rv$zip_path, file) }
  )


  # ── Bat: Step 2 Combine Meta ────────────────────────────────────────────────────────────────────
  bat2_meta_dir_sel <- reactive({
    req(input$bat2_meta_dir)
    parseDirPath(volumes, input$bat2_meta_dir)
  })

  output$bat2_meta_dir_display <- renderText({
    d <- tryCatch(bat2_meta_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  bat2_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  bat2_log <- make_logger(bat2_rv)

  observeEvent(input$bat2_run_btn, {
    bat2_rv$log_lines    <- character(0)
    bat2_rv$output_path  <- NULL
    bat2_rv$preview_data <- NULL

    meta_dir <- tryCatch(bat2_meta_dir_sel(), error = function(e) "")
    if (length(meta_dir) == 0 || meta_dir == "") { bat2_log("ERROR: Please select the folder of cleaned/matched CSVs."); return() }

    withProgress(message = "Combining meta files...", value = 0, {
      tryCatch({
        incProgress(0.2)
        out_path <- combine_bat_meta(
          meta_folder = meta_dir,
          output_dir  = file.path(tempdir(), "bat_combine"),
          log         = bat2_log
        )
        bat2_rv$output_path  <- out_path
        bat2_rv$preview_data <- tryCatch(read.csv(out_path), error = function(e) NULL)
        incProgress(0.8)

      }, error = function(e) bat2_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$bat2_log_output <- renderText({
    if (length(bat2_rv$log_lines) == 0) "No output yet. Select a folder and click Combine."
    else paste(bat2_rv$log_lines, collapse = "\n")
  })

  output$bat2_preview_table <- renderTable({
    req(bat2_rv$preview_data)
    head(bat2_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$bat2_download_ui <- renderUI({
    req(bat2_rv$output_path)
    downloadButton("bat2_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download combined meta (.csv)"),
                   class = "btn-success w-100")
  })

  output$bat2_download_btn <- downloadHandler(
    filename = function() "meta_combined.csv",
    content  = function(file) { req(bat2_rv$output_path); file.copy(bat2_rv$output_path, file) }
  )


  # ── Bat: Sub-sample Files ───────────────────────────────────────────────────────────────────────
  bat_sub_raw_dir_sel <- reactive({
    req(input$bat_sub_raw_dir)
    parseDirPath(volumes, input$bat_sub_raw_dir)
  })

  output$bat_sub_raw_dir_display <- renderText({
    d <- tryCatch(bat_sub_raw_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  bat_sub_rv <- reactiveValues(log_lines = character(0))
  bat_sub_log <- make_logger(bat_sub_rv)

  observeEvent(input$bat_sub_run_btn, {
    bat_sub_rv$log_lines <- character(0)

    raw_dir <- tryCatch(bat_sub_raw_dir_sel(), error = function(e) "")
    if (length(raw_dir) == 0 || raw_dir == "") { bat_sub_log("ERROR: Please select the folder of raw .wav files."); return() }

    mins <- suppressWarnings(as.integer(trimws(strsplit(input$bat_sub_mins, ",")[[1]])))
    mins <- mins[!is.na(mins)]
    if (length(mins) == 0) { bat_sub_log("ERROR: No valid minutes entered. Enter comma-separated integers, e.g. 0,1,2,3,4."); return() }

    withProgress(message = "Sub-sampling files...", value = 0, {
      tryCatch({
        incProgress(0.2)
        path_out <- subsample_bat_files(
          path_raw       = raw_dir,
          subsample_mins = mins,
          log            = bat_sub_log
        )
        incProgress(0.8)

      }, error = function(e) bat_sub_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$bat_sub_log_output <- renderText({
    if (length(bat_sub_rv$log_lines) == 0) "No output yet. Select a folder and click Sub-sample."
    else paste(bat_sub_rv$log_lines, collapse = "\n")
  })


  # ── Bat: Recover Meta ───────────────────────────────────────────────────────────────────────────
  bat_rec_proc_dir_sel <- reactive({
    req(input$bat_rec_proc_dir)
    parseDirPath(volumes, input$bat_rec_proc_dir)
  })

  bat_rec_raw_dir_sel <- reactive({
    req(input$bat_rec_raw_dir)
    parseDirPath(volumes, input$bat_rec_raw_dir)
  })

  output$bat_rec_proc_dir_display <- renderText({
    d <- tryCatch(bat_rec_proc_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  output$bat_rec_raw_dir_display <- renderText({
    d <- tryCatch(bat_rec_raw_dir_sel(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  bat_rec_rv <- reactiveValues(
    log_lines    = character(0),
    output_path  = NULL,
    preview_data = NULL
  )
  bat_rec_log <- make_logger(bat_rec_rv)

  observeEvent(input$bat_rec_run_btn, {
    bat_rec_rv$log_lines    <- character(0)
    bat_rec_rv$output_path  <- NULL
    bat_rec_rv$preview_data <- NULL

    proc_dir <- tryCatch(bat_rec_proc_dir_sel(), error = function(e) "")
    raw_dir  <- tryCatch(bat_rec_raw_dir_sel(),  error = function(e) "")
    if (length(proc_dir) == 0 || proc_dir == "") { bat_rec_log("ERROR: Please select the sorted (processed) folder."); return() }
    if (length(raw_dir)  == 0 || raw_dir  == "") { bat_rec_log("ERROR: Please select the raw folder.");               return() }

    withProgress(message = "Recovering meta.csv...", value = 0, {
      tryCatch({
        incProgress(0.1)
        out_path <- recover_bat_meta(
          path_processed = proc_dir,
          path_raw       = raw_dir,
          output_dir     = file.path(tempdir(), "bat_recover"),
          log            = bat_rec_log
        )
        bat_rec_rv$output_path  <- out_path
        bat_rec_rv$preview_data <- tryCatch(read.csv(out_path), error = function(e) NULL)
        incProgress(0.9)

      }, error = function(e) bat_rec_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$bat_rec_log_output <- renderText({
    if (length(bat_rec_rv$log_lines) == 0) "No output yet. Select the sorted and raw folders and click Recover Meta."
    else paste(bat_rec_rv$log_lines, collapse = "\n")
  })

  output$bat_rec_preview_table <- renderTable({
    req(bat_rec_rv$preview_data)
    head(bat_rec_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  output$bat_rec_download_ui <- renderUI({
    req(bat_rec_rv$output_path)
    downloadButton("bat_rec_download_btn",
                   label = tagList(bsicons::bs_icon("download"), " Download meta_reverse (.csv)"),
                   class = "btn-success w-100")
  })

  output$bat_rec_download_btn <- downloadHandler(
    filename = function() "meta_reverse.csv",
    content  = function(file) { req(bat_rec_rv$output_path); file.copy(bat_rec_rv$output_path, file) }
  )


  # ── Flora Photo Filing: Sort Photos ─────────────────────────────────────────────────────────────
  flora_rv <- reactiveValues(
    log_lines    = character(0),
    preview_data = NULL
  )
  flora_log <- make_logger(flora_rv)

  flora_photos_dir_path <- reactive({
    req(input$flora_photos_dir)
    parseDirPath(volumes, input$flora_photos_dir)
  })

  flora_sorted_dir_path <- reactive({
    req(input$flora_sorted_dir)
    parseDirPath(volumes, input$flora_sorted_dir)
  })

  output$flora_photos_dir_display <- renderText({
    d <- tryCatch(flora_photos_dir_path(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  output$flora_sorted_dir_display <- renderText({
    d <- tryCatch(flora_sorted_dir_path(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  observeEvent(input$flora_run_btn, {

    flora_rv$log_lines    <- character(0)
    flora_rv$preview_data <- NULL

    photos_dir <- tryCatch(flora_photos_dir_path(), error = function(e) "")
    sorted_dir <- tryCatch(flora_sorted_dir_path(), error = function(e) "")

    if (is.null(input$flora_datasheet_file))            { flora_log("ERROR: No datasheet uploaded."); return() }
    if (length(photos_dir) == 0 || photos_dir == "")    { flora_log("ERROR: Please select a raw photos folder."); return() }
    if (length(sorted_dir) == 0 || sorted_dir == "")    { flora_log("ERROR: Please select a sorted photos folder."); return() }
    if (trimws(input$flora_sheet_name) == "")           { flora_log("ERROR: Please enter the photo filing sheet name."); return() }
    if (trimws(input$flora_status) == "")               { flora_log("ERROR: Please enter at least one STATUS to sort."); return() }

    status_to_sort <- trimws(unlist(strsplit(input$flora_status, ",")))
    status_to_sort <- status_to_sort[nzchar(status_to_sort)]

    withProgress(message = "Filing photos...", value = 0, {
      tryCatch({

        incProgress(0.1)

        summary_df <- sort_flora_photos(
          datasheet_path = input$flora_datasheet_file$datapath,
          photos_dir     = photos_dir,
          sorted_dir     = sorted_dir,
          status_to_sort = status_to_sort,
          sheet_name     = trimws(input$flora_sheet_name),
          log            = flora_log
        )

        incProgress(0.8)
        flora_rv$preview_data <- summary_df
        flora_log(paste("Done! Photos filed into:", sorted_dir))

      }, error = function(e) flora_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$flora_log_output <- renderText({
    if (length(flora_rv$log_lines) == 0)
      "No output yet. Upload a datasheet, select folders and click Sort Photos."
    else paste(flora_rv$log_lines, collapse = "\n")
  })

  output$flora_preview_table <- renderTable({
    req(flora_rv$preview_data)
    head(flora_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")


  # ── Flora Photo Filing: Re-sort Tag Folders ─────────────────────────────────────────────────────
  flora_resort_rv <- reactiveValues(
    log_lines    = character(0),
    preview_data = NULL
  )
  flora_resort_log <- make_logger(flora_resort_rv)

  flora_resort_src_path <- reactive({
    req(input$flora_resort_src_dir)
    parseDirPath(volumes, input$flora_resort_src_dir)
  })

  flora_resort_dest_path <- reactive({
    req(input$flora_resort_dest_dir)
    parseDirPath(volumes, input$flora_resort_dest_dir)
  })

  output$flora_resort_src_dir_display <- renderText({
    d <- tryCatch(flora_resort_src_path(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  output$flora_resort_dest_dir_display <- renderText({
    d <- tryCatch(flora_resort_dest_path(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  observeEvent(input$flora_resort_run_btn, {

    flora_resort_rv$log_lines    <- character(0)
    flora_resort_rv$preview_data <- NULL

    src_dir  <- tryCatch(flora_resort_src_path(),  error = function(e) "")
    dest_dir <- tryCatch(flora_resort_dest_path(), error = function(e) "")

    if (length(src_dir)  == 0 || src_dir  == "") { flora_resort_log("ERROR: Please select an existing sorted folder."); return() }
    if (length(dest_dir) == 0 || dest_dir == "") { flora_resort_log("ERROR: Please select an updated folder."); return() }

    withProgress(message = "Re-sorting folders...", value = 0, {
      tryCatch({

        incProgress(0.1)

        summary_df <- resort_flora_tag_dirs(
          sorted_dir  = src_dir,
          updated_dir = dest_dir,
          log         = flora_resort_log
        )

        incProgress(0.8)
        flora_resort_rv$preview_data <- summary_df
        flora_resort_log(paste("Done! Photos re-sorted into:", dest_dir))

      }, error = function(e) flora_resort_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$flora_resort_log_output <- renderText({
    if (length(flora_resort_rv$log_lines) == 0)
      "No output yet. Select folders and click Re-sort Folders."
    else paste(flora_resort_rv$log_lines, collapse = "\n")
  })

  output$flora_resort_preview_table <- renderTable({
    req(flora_resort_rv$preview_data)
    head(flora_resort_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")


  # ── CAG Photo Renaming ──────────────────────────────────────────────────────────────────────────
  cag_rv <- reactiveValues(
    log_lines    = character(0),
    preview_data = NULL
  )
  cag_log <- make_logger(cag_rv)

  cag_photo_dir_path <- reactive({
    req(input$cag_photo_dir)
    parseDirPath(volumes, input$cag_photo_dir)
  })

  output$cag_photo_dir_display <- renderText({
    d <- tryCatch(cag_photo_dir_path(), error = function(e) "")
    if (length(d) == 0 || d == "") "No folder selected." else d
  })

  observeEvent(input$cag_run_btn, {

    cag_rv$log_lines    <- character(0)
    cag_rv$preview_data <- NULL

    photo_dir <- tryCatch(cag_photo_dir_path(), error = function(e) "")

    if (is.null(input$cag_excel_file))               { cag_log("ERROR: No datasheet uploaded."); return() }
    if (trimws(input$cag_sheet) == "")               { cag_log("ERROR: Please enter the sheet name."); return() }
    if (length(photo_dir) == 0 || photo_dir == "")   { cag_log("ERROR: Please select a photo folder."); return() }
    if (trimws(input$cag_id_col) == "")              { cag_log("ERROR: Please enter the Tree ID column name."); return() }
    if (trimws(input$cag_photo_col) == "")           { cag_log("ERROR: Please enter the Photo column name."); return() }

    withProgress(message = "Renaming photos...", value = 0.3, {
      tryCatch({

        plan <- rename_photos_from_excel(
          excel_path = input$cag_excel_file$datapath,
          sheet      = trimws(input$cag_sheet),
          photo_dir  = photo_dir,
          mode       = input$cag_mode,
          id_col     = trimws(input$cag_id_col),
          photo_col  = trimws(input$cag_photo_col),
          log        = cag_log
        )

        incProgress(0.6)

        cag_rv$preview_data <- plan[, c("tree_id", "photo_no", "file", "new_name", "status")]

        if (input$cag_mode == "dry_run") {
          cag_log("Done! This was a preview — re-run as Copy or Rename to apply.")
        } else {
          cag_log("Done! See the plan below for the status of each photo.")
        }

      }, error = function(e) cag_log(paste("ERROR:", conditionMessage(e))))
    })
  })

  output$cag_log_output <- renderText({
    if (length(cag_rv$log_lines) == 0)
      "No output yet. Fill in the inputs and click Run."
    else paste(cag_rv$log_lines, collapse = "\n")
  })

  output$cag_preview_table <- renderTable({
    req(cag_rv$preview_data)
    head(cag_rv$preview_data, 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")
}


#### Launch ####
shinyApp(ui, server)
