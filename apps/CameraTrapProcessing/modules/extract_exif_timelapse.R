## This script contains functions required for extracting information when using Timelapse. 


read_ddb_datatable <- function(ddb_path){
  
  conn <- dbConnect(SQLite(), ddb_path)
  data_tl_raw <- dbGetQuery(conn, "SELECT * FROM DataTable")
  dbDisconnect(conn)
  
  ## Check if data was loaded correctly
  if (nrow(data_tl_raw) == 0) stop("No data loaded from ", ddb_path)
  
  return(data_tl_raw)
}


find_station_sampledate <- function(data_tl_raw, ddb_path){

  ## The ddb can sit in the Station_SampleDate folder, above it, or below it (e.g. in a
  ## camera folder such as 100RECNX), and its videos can sit in further subfolders (e.g. AI
  ## species classifications). Rebuild each video's full path and find the folder from there.
  ddb_data_station <- data_tl_raw %>%
    mutate(RelativePath = gsub("\\\\", "/", coalesce(RelativePath, "")),
           FullPath = ifelse(RelativePath == "",
                             file.path(dirname(ddb_path), File),
                             file.path(dirname(ddb_path), RelativePath, File)),
           Station_SampleDate = station_sampledate_of(FullPath))

  check_station_found(ddb_data_station$FullPath, ddb_data_station$Station_SampleDate,
                      paste("Timelapse", basename(ddb_path)))

  return(ddb_data_station)
}


process_ddb_datatable <- function(data_tl_raw, ddb_path, log = message){

  ddb_data_station <- find_station_sampledate(data_tl_raw, ddb_path)
  
  ddb_data <- ddb_data_station %>%
    mutate(FileName = File,
           Unsure = as.logical(Unsure),
           # Folders below the ddb may be camera folders (also in the raw folder) or species
           # folders (not in the raw folder), so both keys are kept and matched against raw
           FileKey = station_file_key(FullPath),
           FileKeyNoParent = station_file_key(FullPath, drop_parent = TRUE)) %>%
    separate(Station_SampleDate, 
             into = c("Station", "SamplingDate", "StationRemarks"), 
             sep = "_", extra = "merge", fill = "right", remove = FALSE) %>%
    # Use OtherSpecies if species was not found in the drop-down options
    mutate(FolderSpeciesName = case_when(
             Species == "Other Species" ~ OtherSpecies,
             Species == "Others" ~ OtherSpecies,
             TRUE ~ Species),
           FullStationPath = dirname(ddb_path))
  
  ## Check that there are no videos marked "Unsure"
  if (any(ddb_data$Unsure)) {
    
    unsure_stations <- ddb_data %>%
      filter(Unsure == T) %>%
      pull(Station_SampleDate) %>%
      unique()
    
    log(paste("WARNING: There are videos in", paste(unsure_stations, collapse = ", "),
              "that are still marked as UNSURE. Please check."))
  }
  
  ## Check that there are no videos that have not been sorted
  if (any(unique(ddb_data$FolderSpeciesName) == "")){
    
    unsorted_stations <- ddb_data %>%
      filter(FolderSpeciesName == "") %>%
      pull(Station_SampleDate) %>%
      unique()
    
    log(paste("WARNING: There are videos in", paste(unsorted_stations, collapse = ", "),
              "that have not been sorted (Species drop down box is left blank).",
              "Please check."))
  }
  
  ddb_data_final <- ddb_data %>%
    select(-c(Id, File, DeleteFlag, Unsure, Species, OtherSpecies, DateTime))
  
  return(ddb_data_final)
}


get_id_timelapse <- function(ddb_path, log = message){

  data_tl_raw <- read_ddb_datatable(ddb_path)
  data_tl <- process_ddb_datatable(data_tl_raw, ddb_path, log = log)

  return(data_tl)
}
