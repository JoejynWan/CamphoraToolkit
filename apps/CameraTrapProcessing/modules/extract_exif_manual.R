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
           Station_SampleDate = basename(dirname(dirname(FullPath)))) %>%
    # UniqueFileName will be used to get date and time from raw videos
    unite(col = "UniqueFileName", Station_SampleDate, FileName, 
          sep = "_", remove = F) %>%
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
  
  return(data_manual)
}
