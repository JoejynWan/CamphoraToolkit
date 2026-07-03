## To be used only when remarks are in the video file names, and have to be 
## moved to the species folder names

rm(list=ls())
library(tidyverse)

dir_path <- "Z:\\01_Current_Projects\\Rifle Range Nature Park EMMP_Operational Phase_NParks\\02_Camera_Trapping\\Camera_Trap_Data\\02 Processed\\Operational phase monitoring\\Round 7 June 2023"

vids <- data.frame(From = list.files(dir_path, 
                                     pattern = "(*.AVI|*.MP4|*.MOV|*.avi)", 
                                     recursive = T, full.names = T)) %>%
  mutate(Parent = dirname(dirname(From)), 
         Station = basename(dirname(dirname(From))), 
         Dir = basename(dirname(From)), 
         Vid = basename(tools::file_path_sans_ext(From)), 
         Ext = tools::file_ext(From)) %>%
  separate(col = Vid, into = c("VidTo", "Remarks"), sep = " ", 
           extra = "merge", fill = "right", remove = F) %>%
  filter(!is.na(Remarks)) %>%
  mutate(DirTo = file.path(Parent, paste(Dir, Remarks, sep = " ")), 
         To = file.path(DirTo, paste(VidTo, Ext, sep = ".")),
         FileFrom = file.path(Station, Dir, paste(Vid, Ext, sep = ".")))

## Move, rename, and delete videos
create_dir <- lapply(unique(vids$DirTo), dir.create)
copy_files <- file.copy(from = vids$From, to = vids$To, 
                        copy.mode = T, copy.date = T)

if (any(!copy_files)) {
  stop("Some files were not copied:\n", paste(vids$FileFrom[!copy_files], 
                                             collapse = "\n"))
}

delete_files <- file.remove(vids$From)

if (any(!delete_files)) stop("Some files were not deleted.")

cat(nrow(vids), "files were renamed and moved:\n", 
    paste(vids$FileFrom, collapse = "\n"))
