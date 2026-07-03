## This script contains functions required for extracting information when 
## using Timelapse to sort images. 


read_ddb_datatable <- function(ddb_path){
  
  conn <- dbConnect(SQLite(), ddb_path)
  data_tl_raw <- dbGetQuery(conn, "SELECT * FROM DataTable")
  dbDisconnect(conn)
  
  ## Check if data was loaded correctly
  if (nrow(data_tl_raw) == 0) stop("No data loaded from ", ddb_path)
  
  return(data_tl_raw)
}


find_station_sampledate <- function(data_tl_raw, ddb_path){
  
  ## Case 1: ddb file is in the station_sampledate folder
  station_sampledate <- basename(dirname(ddb_path))
  from_ddb_path <- grepl(x = station_sampledate, 
                         pattern = "^[[:graph:][:space:]]*_[[:digit:]]{8}$")
  
  if (from_ddb_path) {
    ddb_data_station <- data_tl_raw %>%
      mutate(Station_SampleDate = station_sampledate)
    
    return(ddb_data_station)
  }
  
  ## Case 2: videos are in the station_sampledate folder
  RelativePath <- unique(data_tl_raw$RelativePath)
  from_RelativePath <- grepl(x = RelativePath, 
                             pattern = "^[[:graph:][:space:]]*_[[:digit:]]{8}$")
  
  if (all(from_RelativePath)) {
    ddb_data_station <- data_tl_raw %>%
      mutate(Station_SampleDate = RelativePath)
    
    return(ddb_data_station)
  }
  
  ## Case 3: station_sampledate --> AI species classifications --> videos
  
  # Find Station_SampleDate based on the RelativePath
  ddb_data_station <- data_tl_raw %>%
    mutate(RelativePath = str_replace(RelativePath, "\\\\", "/"), 
           Station_SampleDate = dirname(RelativePath))
  
  if (!any(is.na(ddb_data_station$Station_SampleDate))){
    return(ddb_data_station) 
  } else {
    # This tries to match based on time... but this is giving issues
    parent_dir <- dirname(ddb_path)
    vids_data <- read_exif_parallel(parent_dir)
  
    vids_exif <- vids_data %>%
      select(FileName, Directory, FileModifyDate, FileCreateDate) %>%
      mutate(Station_SampleDate = basename(dirname(Directory))) %>%
      mutate(FileModifyDate = as.POSIXlt(FileModifyDate,
                                         format = "%Y:%m:%d %H:%M:%S",
                                         tz = "Singapore"),
             File_ModifyDateTime = paste(FileName, FileModifyDate),
             FileCreateDate = as.POSIXlt(FileCreateDate,
                                         format = "%Y:%m:%d %H:%M:%S",
                                         tz = "Singapore"),
             File_CreateDateTime = paste(FileName, FileCreateDate),
             DateNoSec = format(FileModifyDate, format = "%Y-%m-%d %H:%M"),
             File_NoSecDateTime = paste(FileName, DateNoSec)) %>%
      select(File_ModifyDateTime, File_CreateDateTime, File_NoSecDateTime, Station_SampleDate)
  
    afterAI <- unique(vids_exif$Station_SampleDate)
    from_afterAI <- grepl(x = afterAI,
                          pattern = "^[[:graph:][:space:]]*_[[:digit:]]{8}$")
  
    if (any(from_afterAI)) {
      ddb_data_station <- data_tl_raw %>%
        mutate(DateTime = as.POSIXlt(DateTime, tz = "Singapore"),
               File_DateTime = paste(File, DateTime)) %>%
        left_join(vids_exif, by = join_by(File_DateTime == File_ModifyDateTime)) %>%
        left_join(vids_exif, by = join_by(File_DateTime == File_CreateDateTime)) %>%
        mutate(Station_SampleDate = case_when(is.na(Station_SampleDate.x) ~ Station_SampleDate.y,
                                              .default = Station_SampleDate.x)) %>%
        select(names(data_tl_raw), Station_SampleDate)
  
      return(ddb_data_station)
    }
  }
  
  ## None of the cases above
  stop("Cannot find Station_SampleDate folder names. Check with JO.")
  
}


process_ddb_datatable <- function(data_tl_raw, ddb_path){
  
  ddb_data_station <- find_station_sampledate(data_tl_raw, ddb_path)
  
  ddb_data <- ddb_data_station %>%
    mutate(FileName = File,
           Unsure = as.logical(Unsure)) %>%
    unite(col = "UniqueFileName", Station_SampleDate, FileName, 
          sep = "_", remove = F) %>%
    separate(Station_SampleDate, 
             into = c("Station", "SamplingDate", "StationRemarks"), 
             sep = "_", extra = "merge", fill = "right", remove = FALSE) %>%
    # Use OtherSpecies if species was not found in the drop-down options
    mutate(FolderSpeciesName = case_when(
             Species == "Other Species" ~ OtherSpecies,
             Species == "Others" ~ OtherSpecies,
             TRUE ~ Species),
           FullStationPath = dirname(ddb_path), 
           FullPath = file.path(dirname(ddb_path), RelativePath, FileName))
  
  ## Check that there are no videos marked "Unsure"
  if (any(ddb_data$Unsure)) {
    
    unsure_stations <- ddb_data %>%
      filter(Unsure == T) %>%
      pull(Station_SampleDate) %>%
      unique()
    
    cat("Warning: There are videos in", paste(unsure_stations, collapse = ", "), 
        "that are still marked as UNSURE. Please check.\n")
  }
  
  ## Check that there are no videos that have not been sorted
  if (any(unique(ddb_data$FolderSpeciesName) == "")){
    
    unsorted_stations <- ddb_data %>%
      filter(FolderSpeciesName == "") %>%
      pull(Station_SampleDate) %>%
      unique()
    
    cat("Warning: There are videos in",paste(unsorted_stations,collapse = ", "),
        "that have not been sorted (Species drop down box is left blank).",
        "Please check.\n")
  }
  
  ddb_data_final <- ddb_data %>%
    select(-c(Id, File, DeleteFlag, Unsure, Species, OtherSpecies, DateTime))
  
  return(ddb_data_final)
}


get_id_timelapse <- function(ddb_path){
  
  data_tl_raw <- read_ddb_datatable(ddb_path)
  data_tl <- process_ddb_datatable(data_tl_raw, ddb_path)
  
  return(data_tl)
}
