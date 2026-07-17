## To be used only when videos from the camera trap were stored in multiple 
## folders. This will rename and merge all videos into one folder. 

rm(list=ls())
library(tidyverse)

dir_path <- "I:/DCIM/"

vids <- data.frame(From = list.files(dir_path, recursive = T, 
                                         full.names = T)) %>%
  mutate(Parent = dirname(dirname(From)), 
         Dir = basename(dirname(From)), 
         Vid = basename(tools::file_path_sans_ext(From)), 
         Ext = tools::file_ext(From)) %>%
  arrange(Dir, Vid) %>%
  mutate(NewVidNum = str_pad(1:nrow(.), width = 4, pad = 0), 
         To = file.path(Parent, paste0("RCNX", NewVidNum, ".", Ext)))

## Check before copying
vids_fixed <- select(vids, From, To)
status <- file.copy(from = vids_fixed$From, to = vids_fixed$To, copy.date = TRUE)
if (any(!status)) {
  stop("Some files failed to copy")
} else {
  cat("All files copied successfully.")
}
