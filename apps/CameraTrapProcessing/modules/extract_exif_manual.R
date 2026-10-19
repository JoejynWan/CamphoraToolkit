## This script contains functions required for extracting information when using the manual method 
## of sorting images (copy and pasting videos into the respective species folders). 


read_video_paths <- function(dir_path){
  
  vid_paths <- list.files(dir_path, recursive = T,
                          pattern = "(*.AVI|*.MP4|*.MOV)", full.names = T)
  
  return(vid_paths)
}


get_id_manual <- function(video_paths){
  
  data_manual <- data.frame(FullPath = video_paths) %>%
    mutate(FileName = basename(FullPath),
           Station_SampleDate = station_sampledate_of(FullPath),
           # FileKey will be used to get date and time from raw videos. The folder directly
           # above each video is always its species folder, which raw folders do not have.
           FileKey = station_file_key(FullPath, drop_parent = TRUE),
           FileKeyNoParent = FileKey) %>%
    # Extract station and sampling date
    separate(Station_SampleDate, 
             into = c("Station", "SamplingDate", "StationRemarks"), 
             sep = "_", extra = "merge", fill = "right", remove = FALSE) %>%
    # Extract species via folder name. Additional spaces are combined together 
    mutate(Species_Qty = basename(dirname(FullPath))) %>% 
    separate(Species_Qty, into = c("Genus", "Sp.","Quantity", "Remarks"), 
             sep = " ", extra = "merge", fill = "right", remove = FALSE) %>% 
    unite(col="FolderSpeciesName", Genus, Sp., sep=" ") %>%
    select(-Species_Qty)

  check_station_found(data_manual$FullPath, data_manual$Station_SampleDate, "processed")

  return(data_manual)
}
