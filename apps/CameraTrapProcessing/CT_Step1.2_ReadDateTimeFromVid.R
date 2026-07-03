#### Installing and loading required packages ####
rm(list=ls())
source("modules/utils.R")
install_load_packages(c("av", "magick", "tesseract", "pbapply", "tidyverse"))


#### Variable Control Panel ####
raw_vid_dir <- "Z:/01_Current_Projects/Sembawang Air Base DSTA_DPA/02_Camera_Trapping/Camera_Trap_Data/01 Raw/20250424/"
exif_path <- "Z:/01_Current_Projects/Sembawang Air Base DSTA_DPA/02_Camera_Trapping/Camera_Trap_Data/02 Processed/exifs/All exifs/20250424_exif.csv"

brand <- "reconyx" #reconyx or browning


#### Functions ####
suppress_output <- function(expr) {
  out <- file(tempfile(), open = "wt")
  err <- file(tempfile(), open = "wt")
  sink(out)
  sink(err, type = "message")
  on.exit({
    sink(type = "message")
    sink()
    close(out)
    close(err)
  }, add = TRUE)
  force(expr)
}


get_datetime <- function(vid_file, geometry, time_format){
  
  ## Get first frame from video
  suppress_output({
    img <- image_read_video(vid_file, fps = 0.1)
  })

  ## Crop date time info and do image transformations
  img_scale <- image_scale(img, "1080")
  img_crop <- image_crop(img_scale, geometry)
  img_negate <- image_negate(img_crop)
  img_border <- image_border(img_negate, "white")
  img_final <- img_border
  
  ## Read text from image
  img_texts <- image_ocr_data(img_final)
  img_data <- data.frame(FilePath = vid_file,
                         FileName = basename(vid_file),
                         Date = img_texts[nrow(img_texts)-1, 1][[1]],
                         Time = img_texts[nrow(img_texts), 1][[1]]) %>%
    mutate(FileModifyDate = paste(Date, Time, sep = " "),
           FileModifyDate = as.POSIXct(FileModifyDate, format = time_format),
           Date = format(FileModifyDate, "%Y-%m-%d"),
           Time = format(FileModifyDate, "%H:%M:%S"), 
           Station_SamplingDate = basename(dirname(FilePath))) %>%
    separate(col = Station_SamplingDate, into = c("Station", "SamplingDate"), sep = "_") %>%
    select(FilePath, Station, SamplingDate, FileName, FileModifyDate, Date, Time)
  
  return(img_data)
}


#### Main code ####
## Get video paths found in the directory
vid_files <- list.files(raw_vid_dir, recursive = T,
                        pattern = "(*.AVI|*.MP4|*.MOV|*.avi)", full.names = T)

## Get geometry for crops based on the CT brand
brand <- tolower(brand)
if (brand == "browning"){
  geometry <- "380x28+700+572"
  time_format <- "%m/%d/%Y %I:%M:%S%p"
  
}else if (brand == "reconyx"){
  geometry <- "400x16+0+0"
  time_format <- "%Y-%m-%d %H:%M:%S"
  
} else {
  stop("Please insert the CT brand (reconyx/browning)")
}

## Get DateTime data from videos
vid_data <- pblapply(vid_files, FUN = get_datetime, 
                     geometry = geometry, time_format = time_format)
vid_data_df <- bind_rows(vid_data)

## Save out date time data
save_path <- file.path(raw_vid_dir, "video_dates.csv")
write.csv(vid_data_df, save_path, row.names = F)
cat("\nDate and times as shown on the videos have been saved successfully in", 
    save_path, "\n", sep = " ")

## Fix the DateTime in the exif
new_datetime <- vid_data_df %>%
  mutate(uniqueID = paste(Station, SamplingDate, FileName, sep = "_")) %>%
  select(uniqueID, FileModifyDate, Date, Time)

exif <- read.csv(exif_path)
  
exif_correct <- exif %>%
  mutate(FileModifyDate = as.POSIXct(FileModifyDate), 
         uniqueID = paste(Station, SamplingDate, FileName, sep = "_")) %>%
  merge(new_datetime, by = "uniqueID", all.x = T, all.y = T) %>%
  mutate(FileModifyDate = case_when(is.na(FileModifyDate.y) ~ FileModifyDate.x, 
                                    .default = FileModifyDate.y), 
         Date = case_when(is.na(Date.y) ~ Date.x, .default = Date.y), 
         Time = case_when(is.na(Time.y) ~ Time.x, .default = Time.y)) %>%
  select(-c(FileModifyDate.x, FileModifyDate.y, Date.x, Date.y, Time.x, Time.y, uniqueID)) %>%
  select(Station, SamplingDate, FileModifyDate, Date, Time, everything())

if (nrow(exif_correct) != nrow(exif)) stop("Something wrong: diff number of rows.")

save_exif_path <- str_replace(exif_path, "_exif.csv", "_CorrectedDateTime_exif.csv")
write.csv(exif_correct, save_exif_path, row.names = F)

cat("\nEditted exif with corrected date and times have been saved successfully in", 
    save_exif_path, "\n", sep = " ")
