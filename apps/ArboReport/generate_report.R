## generate_report.R
## Core logic for generating Arboriculture (Arbo) Word reports from tree
## assessment biodata. Called by app.R — do not run this file directly.
##
## Adapted from ../ArboReport_v2.4/generate_report.R (a top-level script with
## no callable function) into a single entry point, run_arbo_report().


#### Main function ####

#' Generate one or more Arbo Word report(s) from tree assessment biodata.
#'
#' @param path_biodata        Path to the biodata CSV export.
#' @param rmd_path            Path to arboreport_full.Rmd (bundled in the modules/ subfolder).
#' @param output_dir          Directory to write the generated .docx file(s) into.
#' @param resized_photos_dir  Directory containing resized photo folders, or NULL to omit photos.
#' @param photo_prefix        Prefix used in photo folder names, e.g. "UWCSEA_Photos". Required if resized_photos_dir is set.
#' @param report_size         Number of trees per report file.
#' @param select_ids          Optional character vector of Tree.ID values to include; NULL = all.
#' @param incl_crown_spread   Include the Crown Spread column/field in the report.
#' @param sort_site           Sort trees by Site then Tree.ID number, instead of Tree.ID number alone.
#' @param date_format         Date format string used to parse the Date column.
#' @param log                 A function used for progress messages, e.g. message (default) or a Shiny logger.
#'
#' @return Invisibly returns a character vector of generated .docx file paths.
run_arbo_report <- function(path_biodata,
                            rmd_path,
                            output_dir,
                            resized_photos_dir = NULL,
                            photo_prefix        = NULL,
                            report_size         = 100,
                            select_ids          = NULL,
                            incl_crown_spread   = FALSE,
                            sort_site           = FALSE,
                            date_format         = "%d/%m/%Y",
                            log = message){

  #### Load and clean biodata ####
  log("Reading biodata...")
  biodata_full <- read.csv(path_biodata, na.strings = c("NA", "-")) %>%
    mutate(Date = as.character(Date),
           Date = as.Date(Date, format = date_format)) %>%
    arrange(Tree.ID)

  if (is.na(biodata_full$Date[1])) {
    stop("Date is not loaded in properly. Most likely due to date format.")
  }

  if (incl_crown_spread){
    col_names <- c("Tree.ID", "Girth..spread..m.", "Crown.Spread", "Height..m.", "Date", "Site",
                   "Habit", "Species", "Health", "Form", "Crown.form", "Lean", "Observations",
                   "Other.observations", "Photos.by", "Photo.no.", "Origin", "Status", "SULE.Rating",
                   "Tree.AZ.Rating", "Retention.Value", "Additional.assessment", "Recommendation",
                   "Affected.by.construction")
  } else {
    col_names <- c("Tree.ID", "Girth..spread..m.", "Height..m.", "Date", "Site",
                   "Habit", "Species", "Health", "Form", "Crown.form", "Lean", "Observations",
                   "Other.observations", "Photos.by", "Photo.no.", "Origin", "Status", "SULE.Rating",
                   "Tree.AZ.Rating", "Retention.Value", "Additional.assessment", "Recommendation",
                   "Affected.by.construction")
  }

  if (!all(col_names %in% names(biodata_full))) {
    missing_cols <- col_names[!col_names %in% names(biodata_full)]
    extra_cols <- names(biodata_full)[!names(biodata_full) %in% col_names]
    stop("There are missing columns, please add them in or check for spelling and formatting: ",
         paste(missing_cols, collapse = ", "), 
         "\nYour extra columns are: ", paste(extra_cols, collapse = ", "))
  }

  ## Combine tree IDs for additional locations (A, B etc.) into one cell
  extra_gps_rows <- biodata_full %>%
    filter(grepl("[A-Za-z]$", Tree.ID)) %>%
    mutate(Tree.ID.origin = substr(Tree.ID, 1, nchar(Tree.ID)-1)) %>%
    group_by(Tree.ID.origin) %>%
    summarise(Tree.ID.full = paste(Tree.ID, collapse = ", "), .groups = "drop") %>%
    mutate(Tree.ID.full = paste(Tree.ID.origin, Tree.ID.full, sep = ", "))

  biodata_clean <- biodata_full %>%
    filter(!grepl("[A-Za-z]$", Tree.ID)) %>%
    merge(extra_gps_rows, by.x = "Tree.ID", by.y = "Tree.ID.origin", all.x = TRUE) %>%
    mutate(Recommendation = as.character(Recommendation),
           Affected.by.construction = as.character(Affected.by.construction),
           Retention.Value = as.character(Retention.Value),
           Tree.ID = case_when(!is.na(Tree.ID.full) ~ Tree.ID.full,
                               .default = Tree.ID)) %>%
    select(-Tree.ID.full) %>%
    ## Remove rows marked to not be in the report
    mutate(Remove = case_when(tolower(Remove) %in% c("yes", "t") ~ TRUE,
                              .default = FALSE),
           Remove = as.logical(Remove),
           Remove = case_when(is.na(Remove) ~ FALSE,
                              .default = Remove)) %>%
    filter(Remove != TRUE)

  if (sort_site) {
    ## Sort based on Site and numbers in Tree.ID
    biodata_clean <- biodata_clean %>%
      mutate(Tree.ID.First = unlist(strsplit(Tree.ID, ","))[1],
             Tree.ID.Nums = as.integer(str_replace_all(Tree.ID.First, "[[:alpha:]]", ""))) %>%
      arrange(Site, Tree.ID.Nums, Tree.ID) %>%
      select(-Tree.ID.Nums, -Tree.ID.First)
  } else {
    ## Sort based on numbers in Tree.ID only
    biodata_clean <- biodata_clean %>%
      mutate(Tree.ID.First = unlist(strsplit(Tree.ID, ","))[1],
             Tree.ID.Nums = as.integer(str_replace_all(Tree.ID.First, "[[:alpha:]]", ""))) %>%
      arrange(Tree.ID.Nums, Tree.ID) %>%
      select(-Tree.ID.Nums, -Tree.ID.First)
  }

  if (!is.null(select_ids)){
    biodata_clean <- biodata_clean %>%
      filter(Tree.ID %in% select_ids)
  }

  #### Check photo folders/numbers (only if photos are included) ####
  if (!is.null(resized_photos_dir)){

    biodata_folders <- biodata_clean %>%
      mutate(Date_chr = format(Date, "%Y-%m-%d"),
             PhotoFolder = paste(photo_prefix, Date_chr, Photos.by, sep = "_"),
             PhotoFolder = file.path(resized_photos_dir, PhotoFolder))

    missing_folders_idx <- !dir.exists(unique(biodata_folders$PhotoFolder))
    if (any(missing_folders_idx)){
      missing_folders <- unique(biodata_folders$PhotoFolder)[missing_folders_idx]
      stop("There are missing/misnamed photo folders:\n",
           paste(missing_folders, collapse = "\n"))
    }

    log("Checking for issues with photo numbers...")
    biodata_photos <- biodata_folders %>%
      mutate(PhotoPath = get_photo_paths(Photos.by, Photo.no., Date, resized_photos_dir, photo_prefix))

    photo_number_issues <- biodata_photos %>%
      unnest(PhotoPath) %>%
      filter(str_ends(PhotoPath, "/.JPG")) %>%
      select(PhotoPath, Tree.ID, Date, Photos.by, Photo.no.)

    if (nrow(photo_number_issues) != 0){
      stop("There are issues with photo numbers for the following trees:\n",
           paste(photo_number_issues$Tree.ID, collapse = "\n"))
    }
    log("No photo issues found.")
  }

  #### Generate the report(s) ####
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  render_report_chunk <- function(biodata_chunk){
    first_tree <- min(biodata_chunk$Tree.ID)
    last_tree  <- max(biodata_chunk$Tree.ID)
    filename   <- paste0("ArboReport_", first_tree, "_", last_tree, ".docx")

    ## arboreport_full.Rmd reads `biodata`, `resized_photos_dir`, `photo_prefix`,
    ## and `incl_crown_spread` as plain variables (not via `params$...`), so they
    ## must be bound directly in the rendering environment, not just passed as
    ## rmarkdown params.
    render_env <- new.env(parent = globalenv())
    assign("biodata",            biodata_chunk,       envir = render_env)
    assign("resized_photos_dir", resized_photos_dir,  envir = render_env)
    assign("photo_prefix",       photo_prefix,         envir = render_env)
    assign("incl_crown_spread",  incl_crown_spread,    envir = render_env)

    rmarkdown::render(rmd_path,
                      output_format = "word_document",
                      output_file   = filename,
                      output_dir    = output_dir,
                      params        = list(biodata = biodata_chunk,
                                          resized_photos_dir = resized_photos_dir,
                                          photo_prefix        = photo_prefix,
                                          incl_crown_spread   = incl_crown_spread),
                      envir = render_env,
                      quiet = TRUE)

    file.path(output_dir, filename)
  }

  biodata_split <- split(biodata_clean, (seq(nrow(biodata_clean)) - 1) %/% report_size)

  log(paste("Generating", length(biodata_split), "report(s), each with up to", report_size, "trees..."))

  output_paths <- vapply(biodata_split, render_report_chunk, character(1))

  log("Done! Report(s) generated.")
  invisible(output_paths)
}
