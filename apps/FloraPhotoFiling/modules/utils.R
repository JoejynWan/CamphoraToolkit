## utils.R
## Helpers for resolving photo number ranges into full photo paths.
## Sourced by sort_photos.R — do not run this file directly.
##
## NOTE: named get_flora_photo_paths*() rather than get_photo_paths*() because
## apps/ArboReport/modules/utils.R already defines get_photo_paths() with a different signature, and
## app.R sources every module into the same global environment.


#### Helper functions ####

#' Parse one PhotoID cell, e.g. "6807-12, 6820", into the photo numbers it refers to.
#'
#' Anything that cannot be read as a number or a number range is reported back rather than being
#' silently dropped or passed to seq(), which errors with an unhelpful "'to' must be a finite
#' number" when the datasheet cell is malformed.
#'
#' @param photo_num_input A single PhotoID cell.
#'
#' @return A list of `nums` (integer vector of photo numbers) and `issues` (character vector of the
#'         parts that could not be read).
parse_flora_photo_nums <- function(photo_num_input){

  if (is.na(photo_num_input) || !nzchar(trimws(photo_num_input))) {
    return(list(nums = integer(0), issues = character(0)))
  }

  ## Normalise en/em dashes to "-" so "6807 – 12" reads the same as "6807-12"
  photo_num_input <- gsub("[\u2010-\u2015]", "-", photo_num_input)

  photo_nums <- trimws(unlist(strsplit(photo_num_input, "[,;]")))
  photo_nums <- photo_nums[nzchar(photo_nums)]

  photo_nums_all <- integer(0)
  issues         <- character(0)

  for (photo_num in photo_nums) {
    if (grepl("-", photo_num)) {
      ## strsplit() drops a trailing empty part, so "6807-" gives one part and is caught here
      parts <- trimws(unlist(strsplit(photo_num, "-")))

      if (length(parts) != 2 || !all(grepl("^[0-9]+$", parts))) {
        issues <- c(issues, photo_num)
        next
      }

      photo_from <- parts[1]
      photo_to   <- parts[2]

      # Fill in missing digits if applicable, e.g. "6807-12" -> "6807-6812"
      if (nchar(photo_from) > nchar(photo_to)) {
        num_missing_digits <- nchar(photo_from) - nchar(photo_to)
        missing_digits     <- substr(photo_from, 1, num_missing_digits)
        photo_to           <- paste0(missing_digits, photo_to)
      }

      # Get range of all photo numbers
      if (as.integer(photo_to) < as.integer(photo_from)) {
        issues <- c(issues, photo_num)
        next
      }

      photo_range    <- seq(as.integer(photo_from), as.integer(photo_to))
      photo_nums_all <- c(photo_nums_all, photo_range)

    } else {
      if (!grepl("^[0-9]+$", photo_num)) {
        issues <- c(issues, photo_num)
        next
      }

      photo_nums_all <- c(photo_nums_all, as.integer(photo_num))
    }
  }

  return(list(nums = unique(photo_nums_all), issues = issues))
}


get_flora_photo_paths_row <- function(photo_folder_input, photo_num_input){

  # Return NA if photo number is missing
  if (is.na(photo_num_input)) {
    return(NA)
  }

  # Get the full list of photo numbers
  photo_nums_all <- parse_flora_photo_nums(photo_num_input)$nums

  if (length(photo_nums_all) == 0) {
    return(NA)
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
#' Every photo number is parsed first, so a malformed cell reports its tag and value instead of
#' erroring part-way through, and all bad cells are listed in one go.
#'
#' @param photo_folder_col Character vector of photo folder paths.
#' @param photo_num_col    Character vector of photo numbers, e.g. "6807-12, 6820".
#' @param tag_col          Optional character vector of tags, used to name offending rows in the
#'                         error message.
#'
#' @return A list, one element per row, of full photo paths (or NA).
get_flora_photo_paths <- function(photo_folder_col, photo_num_col, tag_col = NULL){

  ## Check every photo number up front so one bad cell doesn't hide the others
  if (is.null(tag_col)) tag_col <- rep(NA_character_, length(photo_num_col))

  issues <- character(0)

  for (i in seq_along(photo_num_col)) {
    bad_parts <- parse_flora_photo_nums(photo_num_col[i])$issues

    if (length(bad_parts) > 0) {
      issues <- c(issues, paste0(" - Tag ", ifelse(is.na(tag_col[i]), "(unknown)", tag_col[i]),
                                 ": '", photo_num_col[i], "' — cannot read: ",
                                 paste(bad_parts, collapse = ", ")))
    }
  }

  if (length(issues) > 0) {
    stop("There are issues with the photo numbers of ", length(issues), " entries.\n",
         "Expected numbers or number ranges, e.g. '6807-12, 6820'.\n",
         paste(unique(issues), collapse = "\n"))
  }

  photo_paths <- mapply(get_flora_photo_paths_row, photo_folder_col, photo_num_col,
                        SIMPLIFY = TRUE, USE.NAMES = FALSE)

  return(photo_paths)
}
