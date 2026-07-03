#### Installing and loading required packages ####
rm(list=ls())
source("modules/utils.R")
install_load_packages(c("tidyverse"))


#### Variable Control Panel ####
exif_path = "Z:/01_Current_Projects_A-D/Clementi Stream EIA EMMP_Ramboll/02_Camera_Trapping/Camera_Trap_Data/02 Processed/2025 12/20251204_B/CT01B_20251204/CT01B_20251204_exif.csv"

## Offset may be the number of hours to be offset (i.e., offset = -12) OR
## Offset may be the correct DateTime of the first video (i.e., offset = "2024-01-10 11:02:20")
offset = "2025-11-13 08:00:00"


#### Offset hours to exif data ####
if (class(offset) == "character"){

  exif_raw <- read.csv(exif_path)
  first_datetime <- min(exif_raw$FileModifyDate)
  offset_sec <- difftime(offset, first_datetime, units = "secs")
  
} else if (class(offset) == "numeric"){
  
  offset_sec <- offset*60*60
  
} else stop("Please input a valid offset amount in hours or the actual DateTime of the first vid.")

exif <- read.csv(exif_path) %>%
  mutate(FileModifyDate = as.POSIXct(FileModifyDate),
         FileModifyDate = FileModifyDate+offset_sec,
         Date = as.Date(FileModifyDate, tz="Singapore"),
         Time = format(as.POSIXlt(FileModifyDate), 
                       format = "%H:%M:%S", tz="Singapore"))

exif_output <- paste(tools::file_path_sans_ext(exif_path), "_offset_exif.csv", sep = "")
write.csv(exif, exif_output, row.names = F)
