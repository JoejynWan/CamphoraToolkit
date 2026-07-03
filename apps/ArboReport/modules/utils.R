get_photo_paths_row <- function(photo_by, photo_num_input, insp_date, photos_dir, photo_prefix){
  
  # Return NA if photo number is missing
  if (is.na(photo_num_input)) {
    return(NA)
  }
  
  # Get the full list of photo numbers
  photo_nums <- unlist(strsplit(photo_num_input, ",|;|; |, "))
  
  photo_nums_all <- c()
  for (photo_num in photo_nums) {
    if (grepl("-", photo_num)) {
      photo_from <- trimws(unlist(strsplit(photo_num, "-"))[1])
      photo_to <- trimws(unlist(strsplit(photo_num, "-"))[2])
      
      # Fill in missing digits if applicable
      photo_from_digits <- nchar(photo_from)
      photo_to_digits <- nchar(photo_to)
      
      if (photo_from_digits != photo_to_digits) {
        num_missing_digits <- photo_from_digits - photo_to_digits
        missing_digits <- substr(photo_from, 1, num_missing_digits)
        photo_to <- paste0(missing_digits, photo_to)
      }
      
      # Get range of all photo numbers
      photo_range <- seq(as.integer(photo_from), as.integer(photo_to))
      photo_nums_all <- as.integer(c(photo_nums_all, photo_range))
      
    } else {
      photo_nums_all <- as.integer(c(photo_nums_all, photo_num))
    }
  }
  
  ## Get the photo paths
  photo_folder <- paste(photo_prefix, as.character(insp_date), photo_by, sep = "_")
  photo_folder_full <- file.path(photos_dir, photo_folder)
  
  if (!dir.exists(photo_folder_full)) {
    warning("Folder does not exist: ", photo_folder_full)
    return(NA)
  }
  
  photo_all <- list.files(photo_folder_full)
  photo_pattern <- paste(sprintf("%04d", photo_nums_all), collapse = "|")
  photo_names <- grep(photo_pattern, photo_all, value = T)
  photo_paths <- file.path(photo_folder_full, photo_names)
  
  return(list(photo_paths))
}


get_photo_paths <- function(photo_by, photo_num_input, insp_date, photos_dir, photo_prefix){
  
  photo_paths <- mapply(get_photo_paths_row, photo_by, photo_num_input, insp_date, photos_dir, 
                        photo_prefix, SIMPLIFY = T, USE.NAMES = F)
  
  return(photo_paths)
}

