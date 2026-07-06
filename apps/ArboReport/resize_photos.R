## resize_photos.R
## Core logic for resizing Arbo site photos before generating Word reports.
## Called by app.R — do not run this file directly.
##
## Adapted from ../ArboReport_v2.4/resize_imgs.R (a top-level script with no
## callable function) into a single entry point, resize_arbo_photos().


#### Helper function ####

resize_arbo_photo <- function(img_idx, photo_info, photo_size){

  from_path <- photo_info$From[img_idx]
  img_dir   <- photo_info$ImgDir[img_idx]
  to_path   <- photo_info$To[img_idx]

  if (!file.exists(to_path)){

    im <- magick::image_orient(magick::image_read(from_path))
    im_info <- magick::image_info(im)
    photo_length <- photo_size - 10

    if (im_info$width > im_info$height){ # landscape image

      im_resized <- magick::image_resize(im, paste0(photo_length, "x"))
      im_resized_height <- magick::image_info(im_resized)$height
      border_geometry <- paste0("5x", round((photo_size - im_resized_height) / 2))
      im_border <- magick::image_border(im_resized, "white", border_geometry)

    } else { # portrait image

      im_resized <- magick::image_resize(im, paste0("x", photo_length))
      im_resized_width <- magick::image_info(im_resized)$width
      border_geometry <- paste0(round((photo_size - im_resized_width) / 2), "x5")
      im_border <- magick::image_border(im_resized, "white", border_geometry)
    }

    if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)

    magick::image_write(im_border, path = to_path)
    gc()
  }

  to_path
}


#### Main function ####

#' Resize all photos in a folder (recursively) for use in Arbo Word reports.
#'
#' @param photo_dir          Folder containing the original (full-size) photos.
#' @param resized_photos_dir Destination folder for the resized photos.
#' @param photo_size         Target photo size in pixels (default 400).
#' @param log                A function used for progress messages, e.g. message (default) or a Shiny logger.
#'
#' @return Invisibly returns a character vector of resized photo paths.
resize_arbo_photos <- function(photo_dir, resized_photos_dir, photo_size = 400, log = message){

  photo_info <- data.frame(From = list.files(photo_dir,
                                             pattern = ".JPG|.jpg|.jpeg|.png|.PNG",
                                             recursive = TRUE, full.names = TRUE)) %>%
    mutate(From    = as.character(From),
           ImgName = basename(From),
           ImgDir  = file.path(resized_photos_dir, basename(dirname(From))),
           To      = file.path(ImgDir, ImgName))

  if (nrow(photo_info) == 0) stop("No photos found in: ", photo_dir)

  log(paste("Resizing", nrow(photo_info), "photos..."))

  resized_paths <- vapply(seq_len(nrow(photo_info)), resize_arbo_photo,
                          character(1), photo_info = photo_info, photo_size = photo_size)

  log(paste("Resizing complete. Saved in:", resized_photos_dir))
  invisible(resized_paths)
}
