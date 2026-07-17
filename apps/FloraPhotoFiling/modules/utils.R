## utils.R
## Helpers for resolving photo number ranges into full photo paths.
## Sourced by sort_photos.R — do not run this file directly.
##
## NOTE: named get_flora_photo_paths*() rather than get_photo_paths*() because
## apps/ArboReport/modules/utils.R already defines get_photo_paths() with a different signature, and
## app.R sources every module into the same global environment.


#### Helper function ####

get_flora_photo_paths_row <- function(photo_folder_input, photo_num_input){

  # Return NA if photo number is missing
  if (is.na(photo_num_input)) {
    return(NA)
  }

  # Get the full list of photo numbers
  photo_nums <- unlist(strsplit(photo_num_input, ",|, "))

  photo_nums_all <- c()
  for (photo_num in photo_nums) {
    if (grepl("-", photo_num)) {
      photo_from <- trimws(unlist(strsplit(photo_num, "-"))[1])
      photo_to   <- trimws(unlist(strsplit(photo_num, "-"))[2])

      # Fill in missing digits if applicable, e.g. "6807-12" -> "6807-6812"
      photo_from_digits <- nchar(photo_from)
      photo_to_digits   <- nchar(photo_to)

      if (photo_from_digits != photo_to_digits) {
        num_missing_digits <- photo_from_digits - photo_to_digits
        missing_digits     <- substr(photo_from, 1, num_missing_digits)
        photo_to           <- paste0(missing_digits, photo_to)
      }

      # Get range of all photo numbers
      photo_range    <- seq(as.integer(photo_from), as.integer(photo_to))
      photo_nums_all <- as.integer(c(photo_nums_all, photo_range))

    } else {
      photo_nums_all <- as.integer(c(photo_nums_all, photo_num))
    }
  }

  ## Get the photo paths
  photo_all_files <- list.files(photo_folder_input)
  photo_all       <- tools::file_path_sans_ext(photo_all_files)
  photo_pattern   <- paste(sprintf("%04d", photo_nums_all), collapse = "|")
  photo_idx       <- grep(photo_pattern, photo_all)
  photo_names_ext <- photo_all_files[photo_idx]
  photo_paths     <- file.path(photo_folder_input, photo_names_ext)

  return(list(photo_paths))
}


#### Main function ####

#' Resolve photo folder + photo number columns into lists of full photo paths.
#'
#' @param photo_folder_col Character vector of photo folder paths.
#' @param photo_num_col    Character vector of photo numbers, e.g. "6807-12, 6820".
#'
#' @return A list, one element per row, of full photo paths (or NA).
get_flora_photo_paths <- function(photo_folder_col, photo_num_col){

  photo_paths <- mapply(get_flora_photo_paths_row, photo_folder_col, photo_num_col,
                        SIMPLIFY = TRUE, USE.NAMES = FALSE)

  return(photo_paths)
}
