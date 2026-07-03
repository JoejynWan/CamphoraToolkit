install_load_packages <- function(packages){
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(new.packages)) install.packages(unlist(new.packages))
  
  lapply(packages, require, character.only = T)
}


check_missing_sp <- function(data_lower, species_database_lower){
  
  species_database_names <- unique(species_database_lower$FolderSpeciesName)
  species_present <- unique(data_lower$FolderSpeciesName)
  missing_species_log <- !species_present %in% species_database_names
  missing_species <- species_present[missing_species_log]
  
  data_missingspp <- data_lower %>%
    filter(FolderSpeciesName %in% missing_species) %>%
    select(FolderSpeciesName, Station, SamplingDate) %>%
    unique() %>%
    mutate(MissingSpp = paste(FolderSpeciesName, " (", Station, ", ", SamplingDate, ")", sep = ""))
  
  if (any(missing_species_log)){
    missing_species_names <- paste(sort(data_missingspp$MissingSpp), collapse = ", ")
    
    stop("There are species missing in the Species_Database.csv: ", missing_species_names, 
         ". Please add them in.")
  }
}

  
correct_sp_names <- function(data, species_database){
  ## Import corrected species names without taxonomic information
  ## Column name of species to be corrected in data should be FolderSpeciesName
  
  data_lower <- data %>%
    mutate(FolderSpeciesName = trimws(tolower(FolderSpeciesName)))
  
  species_database_lower <- species_database %>%
    mutate(FolderSpeciesName = trimws(tolower(FolderSpeciesName))) %>%
    unique()
  
  ## Check for missing species in Species Database
  check_missing_sp(data_lower, species_database_lower)
  
  ## Correct for species name if there are no missing species
  data_correct <- merge(data_lower, species_database_lower, by = "FolderSpeciesName", all.x = T) %>%
    select(-FolderSpeciesName)
  
  ## Check that all the rows are present and/or not duplicated
  if (nrow(data_correct) != nrow(data)) {
    stop("There are some missing or duplicated rows.")
  }
  
  return(data_correct)
}


read_exif_parallel <- function(dir_path){
  
  num_cores <- detectCores()-1
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, library(exifr))
  
  vid_files <- list.files(dir_path, recursive = T,
                          pattern = "(*.AVI|*.MP4|*.MOV|*.avi|*.JPG|*.jpg)", full.names = T)
  if (length(vid_files) < num_cores){
    file_batches <- msplit(vid_files, length(vid_files))
  } else {
    file_batches <- msplit(vid_files, num_cores)
  }
  
  exif_dat_list <- parLapply(cl, file_batches, read_exif) %>%
    lapply(., function(df) {
      if ("ShutterSpeedValue" %in% names(df)) {
        df$ShutterSpeedValue <- as.character(df$ShutterSpeedValue)
      }
      if ("Compression" %in% names(df)) {
        df$Compression <- as.character(df$Compression)
      }
      return(df)
    })
  exif_dat <- bind_rows(exif_dat_list)
  
  stopCluster(cl)
  
  return(exif_dat)
}


check_animal_captures_dir <- function(manual_data){
  animal_captures <- manual_data %>%
    filter(FolderSpeciesName %in% "Animal captures") %>%
    select(Station_SampleDate) %>%
    unique()
  
  if (nrow(animal_captures) != 0){
    animal_captures_location <- paste(animal_captures$Station_SampleDate, collapse = ", ")
    
    stop('There are "Animal captures" folders still present after manual sorting here: ',
         animal_captures_location, '. Please check and delete if the videos have been sorted.')
  }
}


check_speciesdatabase <- function(species_database){
  species_database_sim <- species_database %>%
    select(-FolderSpeciesName) %>%
    unique()
  
  num_spp <- length(unique(species_database_sim$ScientificName))
  if (nrow(species_database_sim) != num_spp){
    inconsistent_spp <- species_database_sim %>%
      dplyr::group_by(ScientificName) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1L) 
    
    species_database_wrong <- species_database_sim %>%
      filter(ScientificName %in% inconsistent_spp$ScientificName)
    
    stop("These species in the Species Database have inconsistent data: ", 
         paste(inconsistent_spp$ScientificName, collapse = ", "), 
         ". Please check the data entered in all columns for these species.")
  }
}

