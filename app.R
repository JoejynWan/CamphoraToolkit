## app.R
## Camphora Toolkit Hub — unified Shiny front-end.
## Integrates Camera Trap Processing, Abiotic Monitoring, Fauna Impact
## Assessment, and Arbo Report alongside the project directory hub.
##
## File layout expected:
##   app.R
##   apps/CameraTrapProcessing/CT_Step1_ExtractExif.R             <- CT Step 1 logic (sources modules/ internally)
##   apps/CameraTrapProcessing/CT_Step2_MergeExifs.R              <- CT Step 2 logic
##   apps/CameraTrapProcessing/CT_Step3_IndpDets.R                <- CT Step 3 logic
##   apps/CameraTrapProcessing/modules/utils.R                    <- CT shared utilities
##   apps/CameraTrapProcessing/modules/extract_exif_manual.R      <- CT manual-sorted extraction
##   apps/CameraTrapProcessing/modules/extract_exif_timelapse.R   <- CT timelapse extraction
##   apps/CameraTrapProcessing/data/Species_Database.xlsx         <- CT species lookup table
##   apps/AbioticMonitoring/water_report.R                        <- Abiotic water quality
##   apps/AbioticMonitoring/noise_report.R                        <- Abiotic noise monitoring
##   apps/ImpactAssessment/modules/impact_assessment.R            <- Fauna IA core logic
##   apps/ImpactAssessment/modules/utils.R                        <- Fauna IA shared utilities
##   apps/ImpactAssessment/data/ConsequenceSignificanceMatrix.xlsx <- Fauna IA matrix (bundled)
##   apps/ArboReport/modules/generate_report.R                    <- Arbo Report core logic (renders Word docs)
##   apps/ArboReport/modules/resize_photos.R                      <- Arbo Report photo resizing
##   apps/ArboReport/modules/utils.R                               <- Arbo Report shared utilities
##   apps/ArboReport/arboreport_full.Rmd, arboreport_onetree.Rmd  <- Arbo Report Word templates
##   apps/ArboReport/data/arboreport_template.docx                <- Arbo Report Word reference style (bundled)
##   apps/ArboReport/data/Arboriculture_phrases_to_automate.csv   <- Arbo Report phrase lookup (bundled)
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
library(rmarkdown)
library(tidyverse)
library(shinyFiles)

source("apps/CameraTrapProcessing/modules/utils.R")
source("apps/CameraTrapProcessing/CT_Step1_ExtractExif.R")
source("apps/CameraTrapProcessing/CT_Step2_MergeExifs.R")
source("apps/CameraTrapProcessing/CT_Step3_IndpDets.R")
source("apps/AbioticMonitoring/water_report.R")
source("apps/AbioticMonitoring/noise_report.R")
source("apps/ImpactAssessment/modules/utils.R")
source("apps/ImpactAssessment/modules/impact_assessment.R")
source("apps/ArboReport/modules/utils.R")
source("apps/ArboReport/modules/generate_report.R")
source("apps/ArboReport/modules/resize_photos.R")

SPECIES_DB_PATH <- "apps/CameraTrapProcessing/data/Species_Database.xlsx"
IA_MATRIX_PATH  <- "apps/ImpactAssessment/data/ConsequenceSignificanceMatrix.xlsx"
ARBO_RMD_PATH   <- "apps/ArboReport/arboreport_full.Rmd"
VERSION         <- "v2.2"
UPDATE_DATE     <- "2026-07-03"


# ── Project Registry ────────────────────────────────────────────────────────
# nav_target: value of a nav_panel in this app; NULL = external url or disabled
PROJECTS <- list(

  list(
    title       = "Fauna IA Toolkit",
    description = "Converts recorded and probable species lists into formatted
                   Excel impact assessment templates.",
    url         = NULL,
    nav_target  = "impact_assessment",
    icon        = "bug",
    category    = "Fauna",
    status      = "live"
  ),

  list(
    title       = "Abiotic Monitoring Toolkit",
    description = "Processes raw water quality logger and noise meter exports
                   into structured field report workbooks.",
    url         = NULL,
    nav_target  = "water",
    icon        = "moisture",
    category    = "Abiotic",
    status      = "live"
  ),

  list(
    title       = "Camera Trap Processing",
    description = "Generates EXIFs after camera trap sorting and calculates
                   independent detections and other metrics for reports.",
    url         = NULL,
    nav_target  = "ct_step1",
    icon        = "camera",
    category    = "Fauna",
    status      = "beta"
  ),

  list(
    title       = "Arbo Report",
    description = "Generates the Arboriculture report for each specimen complete
                   with photos from site.",
    url         = NULL,
    nav_target  = "arbo_report",
    icon        = "tree",
    category    = "Flora",
    status      = "beta"
  ),

  list(
    title       = "Stream Inspection Report",
    description = "Processes fauna data and stream photos into a standardised
                   stream inspection report.",
    url         = NULL,
    nav_target  = NULL,
    icon        = "water",
    category    = "Fauna",
    status      = "coming soon"
  )

  ## ── Paste new entries below this line ──────────────────────────────────────
  #
  # list(
  #   title       = "My New Tool",
  #   description = "Short description.",
  #   url         = NULL,
  #   nav_target  = "my_tab_value",   # or url = "https://..." if external
  #   icon        = "bar-chart",
  #   category    = "Category",
  #   status      = "live"
  # ),

)


#### Helper Functions ####
## IMPORTANT: icon_text() is called inside page_navbar() — must be defined before ui <-

icon_text <- function(label, icon_name) {
  tagList(bsicons::bs_icon(icon_name), " ", label)
}

make_logger <- function(rv) {
  function(msg) {
    timestamp <- format(Sys.time(), "[%H:%M:%S]")
    rv$log_lines <- c(rv$log_lines, paste(timestamp, msg))
  }
}

# ── Hub helpers ──────────────────────────────────────────────────────────────

all_categories <- function() sort(unique(sapply(PROJECTS, `[[`, "category")))

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
      p(class   = "proj-desc",     p$description)
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


  # ── Hub Tab ───────────────────────────────────────────────────────────────
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


  # ── Fauna Impact Assessment ──────────────────────────────────────────────
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


  # ── Camera Traps ──────────────────────────────────────────────────────────
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


  # ── Abiotic Monitoring ────────────────────────────────────────────────────
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


  # ── Arbo Report ───────────────────────────────────────────────────────────
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


  # ── About Tab ─────────────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("About", "info-circle"),
    value = "about",

    layout_column_wrap(
      width = 1 / 3,

      card(
        card_header("Fauna Impact Assessment"),
        card_body(
          p("Converts a list of recorded and/or probable fauna species into a formatted Excel impact assessment template."),
          tags$ol(
            tags$li("Upload the ", strong("species list"), " (.xlsx) — must contain a ", code("Scientific Name"), " column."),
            tags$li("Upload the ", strong("fauna database"), " (.xlsx) containing the ", em("CS species impact intensity"), " sheet."),
            tags$li("Click ", strong("Run Assessment"), "."),
            tags$li("Download the formatted output workbook.")
          ),
          hr(),
          p(strong("Output:")),
          tags$ul(tags$li("Receptor sheet — sensitivity, impact intensity, consequence, likelihood, significance, and residual impact columns per species/phase."))
        )
      ),

      card(
        card_header("Camera Trap — Step 1: EXIF Extraction"),
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
        card_header("Camera Trap — Step 2: Merge EXIFs"),
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
        card_header("Camera Trap — Step 3: Independent Detections"),
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
      ),

      card(
        card_header("Abiotic — Water Monitoring"),
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
        card_header("Abiotic — Noise Monitoring"),
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
      ),

      card(
        card_header("Arbo Report — Generate Report"),
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
        card_header("Arbo Report — Resize Photos"),
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

  # ── Shared: shinyFiles volumes (all local drives including Google Drive G:) ──
  volumes <- c(Home = fs::path_home(), getVolumes()())

  shinyDirChoose(input, "s1_path_processed",     roots = volumes, session = session)
  shinyDirChoose(input, "s1_path_raw",           roots = volumes, session = session)
  shinyDirChoose(input, "s2_exif_folder",        roots = volumes, session = session)
  shinyDirChoose(input, "arbo_photos_dir",       roots = volumes, session = session)
  shinyDirChoose(input, "arbophoto_source_dir",  roots = volumes, session = session)
  shinyDirChoose(input, "arbophoto_dest_dir",    roots = volumes, session = session)


  # ── Hub: filter bar + card grid + in-app navigation ──────────────────────
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


  # ── Fauna Impact Assessment ──────────────────────────────────────────────
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


  # ── CT Step 1: EXIF Extraction ────────────────────────────────────────────
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


  # ── CT Step 2: Merge EXIFs ────────────────────────────────────────────────
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


  # ── CT Step 3: Independent Detections ────────────────────────────────────
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


  # ── Abiotic: Water Monitoring ─────────────────────────────────────────────
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


  # ── Abiotic: Noise Monitoring ─────────────────────────────────────────────
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


  # ── Arbo Report: Generate Report ─────────────────────────────────────────
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


  # ── Arbo Report: Resize Photos ───────────────────────────────────────────
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
}


#### Launch ####
shinyApp(ui, server)
