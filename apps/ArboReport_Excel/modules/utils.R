## utils.R
## Helpers for resolving arbo photo number ranges into full photo paths.
## Sourced by sort_photos.R — do not run this file directly.
##
## NOTE: depends on parse_flora_photo_nums() and normalise_dir_path() from
## apps/FloraPhotoFiling/modules/utils.R, which must be sourced first. Named get_arbo_xl_*() so it
## does not clash with get_photo_paths() (ArboReport) or get_flora_photo_paths() (FloraPhotoFiling),
## as app.R sources every module into the same global environment.


#### Helper functions ####

#' Resolve one photo folder + photo number cell into full photo paths.
#'
#' Unlike the ArboReport/Flora lookups, the zero-padded number must sit at the END of the file name,
#' e.g. "140" matches "P1150140.JPG" but not "P1014099.JPG", which an unanchored "0140" would.
#'
#' @param photo_folder_input A single photo folder path.
#' @param photo_num_input    A single Photo.no. cell, e.g. "144-47".
#'
#' @return A list of `paths` (character vector of matched photos) and `expected` (number of photo
#'         numbers in the cell).
get_arbo_xl_photo_paths_row <- function(photo_folder_input, photo_num_input){

  photo_nums_all <- parse_flora_photo_nums(photo_num_input)$nums

  if (length(photo_nums_all) == 0 || is.na(photo_folder_input)) {
    return(list(paths = character(0), expected = length(photo_nums_all)))
  }

  photo_all_files <- list.files(photo_folder_input)
  photo_all       <- tools::file_path_sans_ext(photo_all_files)
  photo_pattern   <- paste0("(", paste(sprintf("%04d", photo_nums_all), collapse = "|"), ")$")
  photo_names_ext <- photo_all_files[grepl(photo_pattern, photo_all)]

  list(paths    = file.path(photo_folder_input, photo_names_ext),
       expected = length(photo_nums_all))
}


#### Main function ####

#' Resolve photo folder + photo number columns into lists of full photo paths.
#'
#' Every photo number is parsed first, so all malformed cells are reported in one go along with
#' their tags, instead of erroring part-way through.
#'
#' @param photo_folder_col Character vector of photo folder paths.
#' @param photo_num_col    Character vector of photo numbers, e.g. "140-143".
#' @param tag_col          Character vector of tags, used to name offending rows in the error.
#'
#' @return A list, one element per row, each a list of `paths` and `expected`.
get_arbo_xl_photo_paths <- function(photo_folder_col, photo_num_col, tag_col){

  issues <- character(0)

  for (i in seq_along(photo_num_col)) {
    bad_parts <- parse_flora_photo_nums(photo_num_col[i])$issues

    if (length(bad_parts) > 0) {
      issues <- c(issues, paste0(" - Tree ", tag_col[i], ": '", photo_num_col[i],
                                 "' — cannot read: ", paste(bad_parts, collapse = ", ")))
    }
  }

  if (length(issues) > 0) {
    stop("There are issues with the photo numbers of ", length(issues), " trees.\n",
         "Expected numbers or number ranges, e.g. '140-143, 150'.\n",
         paste(unique(issues), collapse = "\n"))
  }

  mapply(get_arbo_xl_photo_paths_row, photo_folder_col, photo_num_col,
         SIMPLIFY = FALSE, USE.NAMES = FALSE)
}
