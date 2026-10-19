## To be used only when videos from the camera trap were stored in multiple folders (e.g.
## 100RECNX, 101RECNX), which reuse the same video names. This renames and merges all videos
## into the Station_SampleDate folder itself, numbered on in folder order, so that each video
## has a unique name before ID-ing.


#### Main function ####
## mode: "dry_run" -> preview the new names only | "copy" -> keep the original folders |
##       "move" -> move the videos out, then remove the emptied folders
## progress: optional function(i, n) called after each video, e.g. for a progress bar
merge_ct_videos <- function(station_dir, mode = c("dry_run", "copy", "move"),
                            log_fn = message, progress = NULL){

  mode        <- match.arg(mode)
  station_dir <- sub("[/\\\\]+$", "", station_dir)

  if (!dir.exists(station_dir)) stop("Folder not found: ", station_dir)

  vid_pattern <- "\\.(avi|mp4|mov|jpg)$"

  ## Videos already sitting in the station folder would clash with the new names, and usually
  ## mean the folder has been merged before
  top_vids <- list.files(station_dir, pattern = vid_pattern, ignore.case = T)
  top_vids <- top_vids[!dir.exists(file.path(station_dir, top_vids))]
  if (length(top_vids) > 0) {
    stop("There are already ", length(top_vids), " videos/images directly in ",
         basename(station_dir), " (e.g. ", top_vids[1], "). Has it been merged before? ",
         "Only videos inside subfolders are merged.")
  }

  rel_paths <- list.files(station_dir, pattern = vid_pattern, ignore.case = T, recursive = T)
  if (length(rel_paths) == 0) {
    stop("No videos/images found in the subfolders of ", basename(station_dir), ".")
  }

  if (!grepl(STATION_DIR_PATTERN, basename(station_dir))) {
    log_fn(paste0("WARNING: ", basename(station_dir), " is not named like Station_YYYYMMDD. ",
                  "Please check that this is the Station_SampleDate folder."))
  }

  ## Keep the camera's own prefix (e.g. RCNX) if all videos share one
  prefixes <- unique(sub("[0-9]+$", "", tools::file_path_sans_ext(basename(rel_paths))))
  prefix   <- if (length(prefixes) == 1 && prefixes != "") prefixes else "RCNX"

  vids <- data.frame(OriginalFile = rel_paths) %>%
    mutate(Dir = dirname(OriginalFile),
           Vid = basename(OriginalFile),
           Ext = tools::file_ext(OriginalFile)) %>%
    arrange(Dir, Vid, .locale = "C") %>%
    mutate(NewFile = paste0(prefix,
                            str_pad(row_number(), width = max(4, nchar(n())), pad = "0"),
                            ".", Ext),
           Status = "Preview") %>%
    select(OriginalFile, NewFile, Status)

  folder_counts <- count(vids, Folder = dirname(OriginalFile))
  log_fn(paste0("Found ", nrow(vids), " videos/images in ", nrow(folder_counts), " folder(s): ",
                paste0(folder_counts$Folder, " (", folder_counts$n, ")", collapse = ", ")))

  if (mode == "dry_run") {
    log_fn(paste0("Preview only — videos would be renamed ", vids$NewFile[1], " to ",
                  tail(vids$NewFile, 1), ". Nothing was changed."))
    return(vids)
  }


  #### Copying/moving videos ####
  log_fn(paste0(if (mode == "copy") "Copying" else "Moving", " videos into ",
                basename(station_dir), ". This might take a while..."))

  for (i in seq_len(nrow(vids))) {
    from <- file.path(station_dir, vids$OriginalFile[i])
    to   <- file.path(station_dir, vids$NewFile[i])

    from_size  <- file.size(from)
    from_mtime <- file.mtime(from)

    ok <- if (mode == "copy") {
      file.copy(from, to, copy.date = T)
    } else {
      file.rename(from, to)
    }

    ## Step 1 takes each video's date and time from its modified date, so it must survive
    if (!ok) {
      vids$Status[i] <- "Failed"
    } else if (!isTRUE(file.size(to) == from_size)) {
      vids$Status[i] <- "Failed: size differs"
    } else if (!isTRUE(abs(as.numeric(difftime(file.mtime(to), from_mtime, units = "secs"))) <= 2)) {
      vids$Status[i] <- "Failed: modified date changed"
    } else {
      vids$Status[i] <- if (mode == "copy") "Copied" else "Moved"
    }

    if (!is.null(progress)) progress(i, nrow(vids))
  }

  ## Remove camera folders that moving has emptied
  if (mode == "move") {
    sub_dirs <- list.dirs(station_dir, full.names = T, recursive = T)[-1]
    for (d in rev(sub_dirs[order(nchar(sub_dirs))])) {
      if (length(list.files(d, all.files = T, no.. = T)) == 0) unlink(d, recursive = T)
    }
  }

  ## Record of which original video became which, kept with the videos
  log_path <- file.path(station_dir, paste0(basename(station_dir), "_merged_videos.csv"))
  write.csv(vids, log_path, row.names = F)
  log_fn(paste("Record of the renaming saved in:", log_path))

  failed <- vids %>% filter(grepl("^Failed", Status))
  if (nrow(failed) > 0) {
    stop(nrow(failed), " videos failed to ", mode, ":\n",
         paste0(head(failed$OriginalFile, 10), " (", head(failed$Status, 10), ")",
                collapse = "\n"),
         if (nrow(failed) > 10) paste0("\n...and ", nrow(failed) - 10, " more") else "",
         "\nPlease check these before ID-ing.")
  }

  log_fn(paste0("All ", nrow(vids), " videos ", tolower(vids$Status[1]), " successfully."))

  return(vids)
}
