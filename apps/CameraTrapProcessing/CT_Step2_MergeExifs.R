#--------------------------------------------------------#
#### Merge multiple exifs into a single combined exif ####
#--------------------------------------------------------#

#### Helper functions ####
EXIF_COLUMNS <- c("Station", "SamplingDate", "FileModifyDate", "Date", "Time", "FileName",
                  "Genus", "Species", "ScientificName", "Quantity", "Remarks")

## Only the exif columns are kept by default, so that an exif re-saved by Excel with a stray
## trailing column still reads; `all_columns` keeps any the user has added (edited combined CSV)
read.exif <- function(exif_file, log_fn = message, all_columns = FALSE){
  context <- basename(exif_file)

  raw  <- read_exif_csv(exif_file, EXIF_COLUMNS, context)
  exif <- if (all_columns) raw else raw[, EXIF_COLUMNS]

  exif <- parse_quantity(exif, context)
  exif <- read_exif_datetimes(exif, context = context, log_fn = log_fn)
  if (!all_columns) exif$path <- exif_file

  return(exif)
}

#### Main function ####
merging_exifs <- function(path_exif_folder, path_species_database, input_combined = NA, 
                          log_fn = message){
  
  species_database <- read.xlsx(path_species_database, sheet = "Species_Database")
  check_speciesdatabase(species_database)
  
  exif_files <- list.files(path_exif_folder, pattern = "*_exif.csv", 
                           full.names = T, recursive = T)
  log_fn(paste("Found", length(exif_files), "exif files."))
  if (length(exif_files) == 0){
    stop("No *_exif.csv files found in ", path_exif_folder, ".")
  }
  exif_data <- lapply(exif_files, read.exif, log_fn = log_fn)
  
  combined_exif <- bind_rows(exif_data) %>%
    dplyr::rename(FolderSpeciesName = ScientificName) %>%
    select(-Genus, -Species) %>% #remove to prevent duplicate from species database
    unique() #in case of duplicate _exif.csv files
  
  combined_exif_correct <- correct_sp_names(combined_exif, species_database) %>%
    select(Station, SamplingDate, FileModifyDate, Date, Time, FileName, 
           Genus, Species, ScientificName, Quantity, Remarks) %>%
    mutate(ScientificName = replace(ScientificName, 
                                    ScientificName == "Canis lupus_familiaris", 
                                    "Canis lupus familiaris")) %>%
    arrange(SamplingDate, Station, FileName)
  
  ## Check that DateTime is before SamplingDate 
  check_date <- combined_exif_correct %>%
    mutate(SamplingDate = as.Date(as.character(SamplingDate), format = "%Y%m%d"), 
           AfterSampleDate = case_when(Date > SamplingDate ~ T, .default = F), 
           TwoMonthsBefore = SamplingDate %m-% months(2), 
           TooBeforeSampleDate = case_when(Date < TwoMonthsBefore ~ T, .default = F))
  
  if (nrow(filter(check_date, (AfterSampleDate == T))) != 0){
    check_date_after <- check_date %>%
      filter(AfterSampleDate == T) %>%
      mutate(Station_SamplingDate = paste0(Station, " (", SamplingDate, ")"))
    
    log_fn(paste0("WARNING: Videos dated AFTER sampling date — please check and correct before merging: ",
                  paste(unique(check_date_after$Station_SamplingDate), collapse = ", ")))
  }
  
  if (nrow(filter(check_date, (TooBeforeSampleDate == T))) != 0){
    check_date_before <- check_date %>%
      filter(TooBeforeSampleDate == T) %>%
      mutate(Station_SamplingDate = paste0(Station, " (", SamplingDate, ")"))
    
    log_fn(paste0("WARNING: Videos dated >2 months BEFORE sampling date — please check and correct before merging: ",
                  paste(unique(check_date_before$Station_SamplingDate), collapse = ", ")))
  }
  
  ## Formatted rather than written as POSIXct, so that midnight timings are printed
  combined_exif_correct <- combined_exif_correct %>%
    mutate(FileModifyDate = format(FileModifyDate, format = "%Y-%m-%d %H:%M:%S"))

  combined_path <- file.path(path_exif_folder, "combined_exif_all.csv")
  write.csv(combined_exif_correct, combined_path, row.names = F)
  log_fn("Merged exif data (all species) generated!")
  
  
  #### Creating a mammals only .csv ####
  target_mammal_database <- species_database %>%
    select(ScientificName, CT_TargetMammal) %>%
    mutate(ScientificName = replace(ScientificName, 
                                    ScientificName == "Canis lupus_familiaris", 
                                    "Canis lupus familiaris")) %>%
    filter(CT_TargetMammal %in% T) %>%
    unique()
    
  # Read the editted combined_CT_data.csv if provided. It has usually been edited in Excel, so
  # its DateTimes are parsed and written back as ISO for Step 3, just like the exifs
  if (!is.na(input_combined)){
    combined_exif_correct <- read.exif(input_combined, log_fn = log_fn, all_columns = TRUE) %>%
      mutate(FileModifyDate = format(FileModifyDate, format = "%Y-%m-%d %H:%M:%S"))
  }
  
  mammals <- combined_exif_correct %>%
    filter(ScientificName %in% target_mammal_database$ScientificName) %>%
    mutate(ScientificName = replace(ScientificName, 
                                    ScientificName == "Canis lupus_familiaris", 
                                    "Canis lupus familiaris"))
  
  mammals_path <- file.path(path_exif_folder, "combined_exif_mammals_only.csv")
  write.csv(mammals, mammals_path, row.names = F)
  log_fn(paste0("Merged exif data (mammals only) generated!", 
               "\nTargeted mammals include: ",
               paste(sort(unique(mammals$ScientificName)), collapse = ", "), "."))
}