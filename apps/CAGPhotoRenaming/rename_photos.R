## rename_photos.R
## Core logic for renaming CAG field photos using a Tree ID <-> Photo-number mapping held in an
## Excel sheet (e.g. the 'T1 20260708' sheet of CAG_14.xlsx).
## Called by app.R — do not run this file directly.
##
## Adapted from ../CAG_RenamePhotos.R (an RStudio "edit settings and Source" script) into a single
## entry point, rename_photos_from_excel(), with a log() hook for the Shiny console.
##
##   Photo column formats supported:  9105-07  9108-110  9198-9201  9230  9230-32, 9240
##   Output names:                    P342_01.jpeg, P342_02.jpeg, P342_03.jpeg
##
## Helpers are prefixed cag_ because app.R sources every module into the same global environment.


#### Helper functions ####

# Expand one token from the Photo column into a vector of photo numbers.
# Handles a bare number ("9230") and a range with an abbreviated end ("9105-07" -> 9105:9107,
# "9108-110" -> 9108:9110, "9198-9201" -> 9198:9201, "9198-01" -> 9198:9201 via roll-over).
cag_expand_photo_token <- function(tok) {
  tok <- trimws(tok)
  tok <- sub("\\.0+$", "", tok)          # a plain number read from Excel may arrive as "9230.0"
  if (!nzchar(tok)) return(integer(0))
  if (grepl("^[0-9]+$", tok)) return(as.integer(tok))

  m <- regmatches(tok, regexec("^([0-9]+)\\s*[-–—to ]+\\s*([0-9]+)$", tok))[[1]]
  if (length(m) != 3L) {
    warning("Could not parse photo token: '", tok, "'", call. = FALSE)
    return(integer(0))
  }

  start_chr <- m[2]; end_chr <- m[3]
  start <- as.integer(start_chr)

  if (nchar(end_chr) >= nchar(start_chr)) {
    end <- as.integer(end_chr)
  } else {
    # abbreviated end: overwrite the last nchar(end_chr) digits of the start number
    keep <- substr(start_chr, 1, nchar(start_chr) - nchar(end_chr))
    end  <- as.integer(paste0(keep, end_chr))
    if (end < start) end <- end + 10L^nchar(end_chr)   # e.g. 9198-01 -> 9201
  }

  if (end < start) {
    warning("Range ends before it starts: '", tok, "'", call. = FALSE)
    return(integer(0))
  }
  if (end - start > 200L) {
    warning("Suspiciously long range (", end - start + 1L, " photos): '", tok, "'", call. = FALSE)
  }
  seq.int(start, end)
}

# Expand a whole Photo cell, which may hold several comma/semicolon separated tokens.
cag_expand_photo_cell <- function(cell) {
  if (is.na(cell) || !nzchar(trimws(cell))) return(integer(0))
  toks <- unlist(strsplit(as.character(cell), "[,;&/]|\\band\\b"))
  unique(unlist(lapply(toks, cag_expand_photo_token)))
}

# Make a Tree ID safe for use in a Windows/macOS filename ("T235 / P" -> "T235_P").
cag_sanitise_id <- function(x) {
  y <- trimws(as.character(x))
  y <- gsub("[\\\\/:*?\"<>|]", " ", y)   # illegal filename characters
  y <- gsub("\\s+", "_", y)              # collapse whitespace to a single underscore
  y <- gsub("_+", "_", y)
  gsub("^_|_$", "", y)
}

# Trailing digit block of a filename stem: "IMG_9380" -> 9380, "DSC09380" -> 9380.
cag_stem_number <- function(stem) {
  m <- regmatches(stem, regexpr("[0-9]+$", stem))
  if (length(m) == 0L) return(NA_integer_)
  as.integer(m)
}


#### Main function ####

#' Rename CAG field photos from a Tree ID <-> Photo-number Excel mapping.
#'
#' @param excel_path    Path to the .xlsx workbook.
#' @param sheet         Sheet name, e.g. "T1 20260708".
#' @param photo_dir     Folder holding the photos.
#' @param mode          "dry_run" (default, writes nothing), "rename" (in place) or "copy".
#' @param out_dir       Destination for mode = "copy". Default: <photo_dir>/renamed.
#' @param id_col        Column holding the new name. Default "Tree ID".
#' @param photo_col     Column holding the photo numbers. Default "Photo".
#' @param extensions    File extensions to consider (case-insensitive).
#' @param suffix_single If TRUE, a tree with one photo still gets "_01". Default FALSE.
#' @param pad           Digits in the sequence suffix. Default 2 -> _01, _02.
#' @param log_csv       Path for the audit log. Default: <photo_dir>/rename_log_<timestamp>.csv.
#' @param log           A function used for progress messages, e.g. message (default) or a Shiny
#'                      logger.
#' @return A data.frame of the rename plan (invisibly), with a `status` column.
rename_photos_from_excel <- function(excel_path,
                                     sheet,
                                     photo_dir,
                                     mode          = c("dry_run", "rename", "copy"),
                                     out_dir       = NULL,
                                     id_col        = "Tree ID",
                                     photo_col     = "Photo",
                                     extensions    = c("jpg", "jpeg", "png", "heic", "heif",
                                                       "dng", "cr2", "nef", "arw", "tif", "tiff"),
                                     suffix_single = FALSE,
                                     pad           = 2L,
                                     log_csv       = NULL,
                                     log           = message) {

  mode <- match.arg(mode)
  stopifnot(file.exists(excel_path), dir.exists(photo_dir))

  # 1. read the sheet (Photo cells such as "9105-07" are stored as text and stay text) ----------
  sheets <- openxlsx::getSheetNames(excel_path)
  if (!sheet %in% sheets) {
    stop("Sheet '", sheet, "' not found. Sheets in this workbook: ",
         paste(sheets, collapse = ", "))
  }
  dat <- openxlsx::read.xlsx(excel_path, sheet = sheet, colNames = TRUE,
                             detectDates = FALSE, skipEmptyRows = TRUE)

  # openxlsx substitutes blanks in headers ("Tree ID" -> "Tree.ID"), so match on a normalised
  # form: lower case with every non-alphanumeric character dropped.
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  find_col <- function(want) {
    hit <- which(norm(names(dat)) == norm(want))
    if (!length(hit)) {
      stop("Column '", want, "' not found in sheet '", sheet, "'.\nColumns present: ",
           paste(names(dat), collapse = ", "))
    }
    names(dat)[hit[1]]
  }
  id_col    <- find_col(id_col)
  photo_col <- find_col(photo_col)

  # read.xlsx types each column, so force both to character before any parsing
  ids    <- as.character(dat[[id_col]])
  photos <- as.character(dat[[photo_col]])
  keep   <- !is.na(ids) & nzchar(trimws(ids))
  ids    <- ids[keep]; photos <- photos[keep]

  # 2. build the sheet-side plan: one row per photo number --------------------------------------
  plan <- do.call(rbind, lapply(seq_along(ids), function(i) {
    nums <- cag_expand_photo_cell(photos[i])
    if (!length(nums)) return(NULL)
    data.frame(tree_id  = ids[i],
               safe_id  = cag_sanitise_id(ids[i]),
               photo_no = nums,
               seq_no   = seq_along(nums),
               n_photos = length(nums),
               stringsAsFactors = FALSE)
  }))

  no_photo <- ids[vapply(photos, function(p) length(cag_expand_photo_cell(p)) == 0L, logical(1))]
  if (is.null(plan)) stop("No photo numbers could be read from column '", photo_col, "'.")

  dup_no <- plan$photo_no[duplicated(plan$photo_no)]
  if (length(dup_no)) {
    warning("Photo number(s) claimed by more than one Tree ID: ",
            paste(sort(unique(dup_no)), collapse = ", "), call. = FALSE)
  }

  # 3. index the folder -------------------------------------------------------------------------
  pat   <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
  files <- list.files(photo_dir, pattern = pat, ignore.case = TRUE, full.names = FALSE)
  if (!length(files)) stop("No photo files found in: ", photo_dir)

  idx <- data.frame(file = files,
                    ext  = tolower(tools::file_ext(files)),
                    num  = vapply(tools::file_path_sans_ext(files), cag_stem_number, integer(1),
                                  USE.NAMES = FALSE),
                    stringsAsFactors = FALSE)
  idx <- idx[!is.na(idx$num), , drop = FALSE]

  # 4. join sheet plan to files (a number may match several files, e.g. RAW + JPEG) --------------
  plan <- merge(plan, idx, by.x = "photo_no", by.y = "num", all.x = TRUE)
  plan <- plan[order(plan$safe_id, plan$seq_no, plan$ext), , drop = FALSE]

  suffix <- ifelse(plan$n_photos > 1L | suffix_single,
                   paste0("_", formatC(plan$seq_no, width = pad, flag = "0")), "")
  plan$new_name <- ifelse(is.na(plan$file), NA_character_,
                          paste0(plan$safe_id, suffix, ".", plan$ext))

  plan$status <- ifelse(is.na(plan$file), "MISSING_FILE", "ok")

  clash <- plan$new_name[!is.na(plan$new_name) & duplicated(plan$new_name)]
  if (length(clash)) {
    plan$status[plan$new_name %in% clash] <- "NAME_CLASH"
    warning("Target name(s) used more than once, these will be skipped: ",
            paste(unique(clash), collapse = ", "), call. = FALSE)
  }

  unreferenced <- setdiff(idx$file, plan$file[!is.na(plan$file)])

  # 5. report ------------------------------------------------------------------------------------
  log(sprintf("Sheet '%s': %d Tree IDs, %d photo numbers.", sheet,
              length(unique(plan$tree_id)), length(unique(plan$photo_no))))
  log(sprintf("Folder: %d matching files, %d to be renamed, %d missing, %d not in sheet.",
              nrow(idx), sum(plan$status == "ok"), sum(plan$status == "MISSING_FILE"),
              length(unreferenced)))
  if (length(no_photo)) {
    log(paste("Tree IDs with a blank Photo cell:", paste(no_photo, collapse = ", ")))
  }
  if (sum(plan$status == "MISSING_FILE")) {
    miss <- plan$photo_no[plan$status == "MISSING_FILE"]
    log(paste("Photo numbers with no file:", paste(sort(unique(miss)), collapse = ", ")))
  }
  if (length(unreferenced)) {
    log(paste("Files not referenced by the sheet (left untouched):",
              paste(head(unreferenced, 20), collapse = ", "),
              if (length(unreferenced) > 20) sprintf(" ... (+%d more)",
                                                     length(unreferenced) - 20) else ""))
  }

  todo <- plan[plan$status == "ok", , drop = FALSE]

  # 6. act ---------------------------------------------------------------------------------------
  if (mode == "dry_run") {
    log(paste0("DRY RUN: nothing written. ", nrow(todo),
               " file(s) would be renamed. Review the preview, then re-run as Copy or Rename."))
  } else if (mode == "copy") {
    if (is.null(out_dir)) out_dir <- file.path(photo_dir, "renamed")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    exists_already <- file.exists(file.path(out_dir, todo$new_name))
    if (any(exists_already)) {
      stop(sum(exists_already), " target file(s) already exist in ", out_dir,
           " - clear the folder or pick another out_dir.")
    }
    okc <- file.copy(file.path(photo_dir, todo$file), file.path(out_dir, todo$new_name))
    todo$status <- ifelse(okc, "copied", "COPY_FAILED")
    log(paste0("Copied ", sum(okc), " of ", nrow(todo), " file(s) to ", out_dir))
  } else {
    # two-phase rename so a new name can never overwrite a file still waiting to be renamed
    blockers <- setdiff(intersect(todo$new_name, files), todo$file)
    if (length(blockers)) {
      stop("These existing files would be overwritten: ", paste(blockers, collapse = ", "))
    }
    tmp  <- paste0(todo$file, ".renaming_tmp")
    ok1  <- file.rename(file.path(photo_dir, todo$file), file.path(photo_dir, tmp))
    ok2  <- rep(FALSE, nrow(todo))
    ok2[ok1] <- file.rename(file.path(photo_dir, tmp[ok1]),
                            file.path(photo_dir, todo$new_name[ok1]))
    if (any(ok1 & !ok2)) {   # roll the failures back to their original names
      bad <- which(ok1 & !ok2)
      file.rename(file.path(photo_dir, tmp[bad]), file.path(photo_dir, todo$file[bad]))
    }
    todo$status <- ifelse(ok2, "renamed", "RENAME_FAILED")
    log(paste0("Renamed ", sum(ok2), " of ", nrow(todo), " file(s) in ", photo_dir))
  }

  key <- function(d) paste(d$file, d$new_name, sep = "")
  plan$status[match(key(todo), key(plan))] <- todo$status

  # 7. log ---------------------------------------------------------------------------------------
  # if (mode != "dry_run") {
  #   if (is.null(log_csv)) {
  #     log_csv <- file.path(photo_dir,
  #                          format(Sys.time(), "rename_log_%Y%m%d_%H%M%S.csv"))
  #   }
  #   utils::write.csv(plan[, c("tree_id", "photo_no", "seq_no", "file", "new_name", "status")],
  #                    log_csv, row.names = FALSE, na = "")
  #   log(paste("Log written to", log_csv))
  # }

  invisible(plan)
}
