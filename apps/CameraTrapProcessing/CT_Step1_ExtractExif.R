#---------------------------------------------------#
#### Extract exifs from manual sorting/timelapse ####
#---------------------------------------------------#
## Date and Time info will be extracted from raw videos, as DateTime info may be inaccurate after 
## usage of SpeciesClassifier AI and/or manual copy and pasting of videos. 


#### Installing and loading required packages ####
source("apps/CameraTrapProcessing/modules/extract_exif_manual.R")
source("apps/CameraTrapProcessing/modules/extract_exif_timelapse.R")


#### Main function ####
extract_exif <- function(path_processed, path_raw, path_species_database, log_fn = message){
  
  #### Load and check Species Database ####
  species_database <- read.xlsx(path_species_database, sheet = "Species_Database")
  check_speciesdatabase(species_database)
  
  
  #### Extracting processed IDs from Timelapse and Manual ID-ing ####
  log_fn("Extracting sorted species info from processed videos/images...")
  
  ## IDs from Timelapse
  ddb_paths <- list.files(path_processed, recursive = T, pattern = "*.ddb", full.names = T)
  
  ddb_paths <- ddb_paths[!grepl("Backups", ddb_paths)]

  if (length(ddb_paths) == 0){
    no_timelapse_sorts <- T

  } else {
    no_timelapse_sorts <- F

    ddb_data <- lapply(ddb_paths, get_id_timelapse, log = log_fn) %>%
      bind_rows() %>%
      filter(!grepl("unreadable_file", RelativePath))
  }

  ## IDs from manual method: every video not in a folder with a Timelapse ddb
  vid_paths <- list.files(path_processed, recursive = T,
                          pattern = "(*.AVI|*.MP4|*.MOV|*.avi|*.JPG|*.jpg)", full.names = T)
  in_timelapse_dir <- vapply(vid_paths, function(p) {
    any(startsWith(p, paste0(dirname(ddb_paths), "/")))
  }, logical(1), USE.NAMES = F)
  manual_vid_paths <- vid_paths[!in_timelapse_dir]
  
  if (length(manual_vid_paths) == 0){
    no_manual_sorts <- T
    
  } else {
    no_manual_sorts <- F
    
    manual_data <- get_id_manual(manual_vid_paths)
    
    # Checking that all "Animal Captures" folders have been removed
    check_animal_captures_dir(manual_data)
  }
  
  ## IDs of all stations
  if (no_timelapse_sorts & no_manual_sorts){
    stop("There is no data. Please check your path to the processed folder. ")
  } else if (no_timelapse_sorts) {
    dat_processed <- manual_data
  } else if (no_manual_sorts){
    dat_processed <- ddb_data
  } else {
    dat_processed <- bind_rows(ddb_data, manual_data)
  }
  
  
  #### Conducting checks before combining with Raw data ####
  ## Correcting Species Names 
  species_check <- correct_sp_names(dat_processed, species_database) %>%
    mutate(Quantity = case_when(is.na(Quantity) ~ "1", .default = Quantity),
           Species_Dir = basename(dirname(FullPath)), 
           File_Path = file.path(Station_SampleDate, Species_Dir)) %>%
    mutate(Quantity = as.numeric(Quantity)) %>%
    filter(is.na(Quantity))
  
  if (nrow(species_check) != 0) {
    stop("Missing quantity at:\n", paste(unique(species_check$File_Path), collapse = "\n"))
  }
  
  dat_processed_spp <- correct_sp_names(dat_processed, species_database) %>%
    mutate(Quantity = as.numeric(Quantity)) 
  
  ## Checking that all species have quantity 
  qty_check <- function(x){
    sp_remove <- c("Non targeted", "False trigger", "Corrupted vids")
    dat_animal <- filter(x, !ScientificName %in% sp_remove)
    return(any(is.na(dat_animal$Quantity)))
  }
  
  na_qty_spp <- c("Non targeted", "False trigger")
  
  if (qty_check(dat_processed_spp)){
    qty_check_summ <- dat_processed_spp %>%
      filter(!ScientificName %in% na_qty_spp) %>%
      filter(is.na(Quantity)) %>%
      select(Station, SamplingDate, FileName, ScientificName, Quantity)
    
    print_capture <- function(x) paste(capture.output(print(x)), collapse = "\n")
    log_fn(paste0("WARNING: Some records have no quantity — replaced with 1. Please check:\n",
                  print_capture(qty_check_summ)))
    
    dat_processed_qty <- dat_processed_spp %>%
      mutate(Quantity = case_when(is.na(Quantity) ~ 1,
                                  ScientificName %in% na_qty_spp ~ NA,
                                  TRUE ~ Quantity))
    
  } else {
    dat_processed_qty <- dat_processed_spp %>%
      mutate(Quantity = replace(Quantity, ScientificName %in% na_qty_spp, NA))
  }
  
  if (qty_check(dat_processed_qty)){
    stop("Something still wrong with quantity.")
  }
  
  
  #### Extracting DateTime info from raw video metadata ####
  log_fn("Extracting metadata from raw videos/images. This might take a while...")
  dat_raw <- read_exif_parallel(path_raw) 
  
  dat_raw_datetime <- dat_raw %>%
    select(FileName, Directory, FileModifyDate) %>%
    mutate(RawPath = file.path(Directory, FileName),
           UniqueFileName = station_file_key(RawPath))

  check_station_found(dat_raw_datetime$RawPath, dat_raw_datetime$UniqueFileName, "raw")

  ## Each raw video must be identifiable by its path from the Station_SampleDate folder down,
  ## otherwise processed videos could pick up the date and time of the wrong one
  dup_raw <- unique(dat_raw_datetime$UniqueFileName[duplicated(dat_raw_datetime$UniqueFileName)])
  if (length(dup_raw) > 0) {
    stop("These raw videos/images appear more than once under the same Station_SampleDate ",
         "folder name, so their dates and times cannot be told apart:\n",
         paste(head(dup_raw, 10), collapse = "\n"),
         if (length(dup_raw) > 10) paste0("\n...and ", length(dup_raw) - 10, " more") else "",
         "\nPlease check that the raw folder contains each Station_SampleDate folder only once.")
  }

  dat_raw_datetime <- select(dat_raw_datetime, UniqueFileName, FileModifyDate)

  ## Processed videos may sit in a species folder that raw videos are not in. Use the full
  ## path if raw has it (e.g. camera folders only), otherwise the path without that folder.
  dat_processed_qty <- dat_processed_qty %>%
    mutate(UniqueFileName = ifelse(FileKey %in% dat_raw_datetime$UniqueFileName,
                                   FileKey, FileKeyNoParent))


  #### Correcting DateTime ####
  dat_correctDT <- merge(dat_processed_qty, dat_raw_datetime, by = "UniqueFileName")%>%
    #convert date time so excel can read them
    mutate(FileModifyDate = as.POSIXlt(FileModifyDate, format = "%Y:%m:%d %H:%M:%S", tz="Singapore"), 
           #minus 10 sec, so time record is at start of CT video
           FileModifyDate = FileModifyDate - 10, 
           Date = as.Date(FileModifyDate, tz="Singapore"),
           Time = format(as.POSIXlt(FileModifyDate), format = "%H:%M:%S", tz="Singapore")) %>%
    select(Station, SamplingDate, FileModifyDate, Date, Time, FileName, Genus, Species, 
           ScientificName, Quantity, Remarks) %>%
    arrange(Station, FileModifyDate) %>%
    #so that midnight timings will be printed
    mutate(FileModifyDate = format(FileModifyDate, format="%Y-%m-%d %H:%M:%S")) 
  
  
  #### Conducting checks to ensure everything is in order ####
  ## Checking that all the processed videos are included in the exif.csv output
  if(nrow(dat_correctDT) != nrow(dat_processed)) {
    
    missing_vids_location <- dat_processed_qty %>%
      filter(!UniqueFileName %in% dat_raw_datetime$UniqueFileName) %>%
      mutate(Location = station_file_key(FullPath))

    stop(paste("Some processed videos cannot be matched in the raw folder.",
               "Please ensure that the names of the videos, the Station_SamplingDate folders and",
               "any camera folders (e.g. 100RECNX) in the processed and raw folders are",
               "exactly the same:\n",
               paste(missing_vids_location$Location, collapse = "\n"), sep = " "))
  }
  
  
  #### Exporting output file ####
  output_file_name <- paste(basename(path_processed), "exif.csv", sep="_")
  output_file_path <- file.path(path_processed, output_file_name)
  
  write.csv(dat_correctDT, output_file_path, row.names = F) 
  
  log_fn(paste("Video metadata extracted successfully. Output saved in:", output_file_path))
}