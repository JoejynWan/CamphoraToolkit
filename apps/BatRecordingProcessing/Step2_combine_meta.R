## Step2_combine_meta.R
## Core logic for combining multiple cleaned/matched bat meta CSVs (from Step 1)
## into a single meta_combined.csv. Called by app.R — do not run this file
## directly.
##
## Adapted from ../BatRecordingProcessing_v1.5/Step2_combine_meta.R (a top-level
## script with a hardcoded folder path) into a single entry point,
## combine_bat_meta(), that takes an output directory and a log function.


#### Main function ####

#' Combine all cleaned/matched bat meta CSVs in a folder into one CSV.
#'
#' @param meta_folder  Folder containing the cleaned/matched CSVs (searched
#'                     recursively).
#' @param output_dir   Directory to write meta_combined.csv into.
#' @param log          Progress message function (default message).
#'
#' @return Invisibly returns the path to the combined CSV.
combine_bat_meta <- function(meta_folder, output_dir = meta_folder, log = message){

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  meta_filepaths <- list.files(meta_folder, full.names = TRUE, recursive = TRUE,
                               pattern = "\\.csv$")
  ## Never re-consume a previous combined output
  meta_filepaths <- meta_filepaths[basename(meta_filepaths) != "meta_combined.csv"]

  if (length(meta_filepaths) == 0) stop("No CSV files found in: ", meta_folder)

  log(paste("Combining", length(meta_filepaths), "CSV file(s)..."))
  meta_files <- lapply(meta_filepaths, read.csv) %>%
    bind_rows()

  out_path <- file.path(output_dir, "meta_combined.csv")
  write.csv(meta_files, out_path, row.names = FALSE)

  log(paste("Completed! Combined meta file (", nrow(meta_files), " rows) saved at ", out_path, sep = ""))
  invisible(out_path)
}
