######################################################################
# (\__/)
# (>'.'<)   Utility functions (non task-specific functions)
# (")_(")
######################################################################

install_load_packages <- function(packages){

  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

  if (length(new.packages)) install.packages(unlist(new.packages))

  lapply(packages, require, character.only = T, warn.conflicts=FALSE)
}


check_missing_sp <- function(data_lower, species_database_lower){

  species_database_names <- unique(species_database_lower$FolderSpeciesName)
  species_present <- unique(data_lower$FolderSpeciesName)
  missing_species_log <- !species_present %in% species_database_names
  missing_species <- species_present[missing_species_log]

  data_missingspp <- data_lower %>%
    filter(FolderSpeciesName %in% missing_species)

  if (any(missing_species_log)){
    missing_species_names <- paste(unique(data_missingspp$FolderSpeciesName),
                                   collapse = ", ")
    stop("There are species missing in the Species_Database_Bats.csv: ",
         missing_species_names, ". Please add them in.")
  }
}


check_no_id <- function(data_lower){
  no_sorts <- data_lower %>%
    filter(FolderSpeciesName == "")

  if (nrow(no_sorts) != 0) {
    stop("There are .wav files that have not been ID-ed in Kaleidoscope. ",
         "Please check these files: ", paste(no_sorts$IN.FILE, collapse = ", "))
  }
}


correct_sp_names <- function(data, species_database){
  ## Import corrected species names without taxonomic information
  ## Column name of species to be corrected in data should be FolderSpeciesName

  data_lower <- data %>%
    mutate(FolderSpeciesName = trimws(tolower(FolderSpeciesName)))

  species_database_lower <- species_database %>%
    mutate(FolderSpeciesName = tolower(FolderSpeciesName)) %>%
    unique()

  ## Check for .wav files that have not been ID-ed
  check_no_id(data_lower)

  ## Check for missing species in Species Database
  check_missing_sp(data_lower, species_database_lower)

  ## Correct for species name if there are no missing species
  # species_database_nameonly <- species_database_lower %>%
  #   select(FolderSpeciesName, ScientificName)

  data_correct <- merge(data_lower, species_database_lower,
                        by = "FolderSpeciesName") %>%
    select(-FolderSpeciesName)

  ## Check that all the rows are present and/or not duplicated
  if (nrow(data_correct) != nrow(data)) {
    stop("There are some missing or duplicated rows.")
  }

  return(data_correct)
}
