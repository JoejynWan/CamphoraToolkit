## To be used only when videos in raw were accidentally deleted and have to be
## taken from the processed folder

rm(list=ls())
library(tidyverse)

processed_path <- "Z:\\01_Current_Projects\\CR202 EMMP_Obayashi\\02_Camera_Trapping\\Camera_Trap_Data\\02 Processed\\Windsor Forest Monitoring\\20250123\\CT39B_20250123"

raw_path <- "C:/TempDataForSpeed/CT39B_20250123"

vids <- data.frame(From = list.files(processed_path, recursive = T, 
                                     full.names = T)) %>%
  mutate(FileName = basename(From), 
         To = file.path(raw_path, FileName)) %>%
  arrange(To)

## Copy videos without species folders to the raw folder
copy_files <- c()
for (i in 1:nrow(vids)){
  
  if(!file.exists(vids$To[i])){
    copy_files[i] <- file.copy(from = vids$From[i], to = vids$To[i], 
                               copy.mode = T, copy.date = T)
  }
}

if (any(!copy_files)) {
  stop("Some files were not copied:\n", paste(vids$FileFrom[!copy_files], 
                                              collapse = "\n"))
}

cat(nrow(vids), "files were renamed and copied to", raw_path)
