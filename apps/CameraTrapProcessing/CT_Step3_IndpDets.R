#----------------------------------------------------------------------#
#### Independent detection generation for camera trap data analyses ####
#----------------------------------------------------------------------#

#### Helper functions ####
## Calculate time intervals between subsequent records within these data frames 
## ("difftime" column) and group them based on the independent detections
calc_difftime <- function(x, indp_interval = 3600){
  
  x <- x[order(x$FileModifyDate),]
  difftime_tmp <- diff(x$FileModifyDate)
  units(difftime_tmp) <- "secs"
  x$difftime <- c(indp_interval+1, difftime_tmp)
  
  # Group up records that are part of the same independent detection
  indpdet_num = 0
  for (i in 1:nrow(x)){
    
    if (x$difftime[i] > indp_interval){
      indpdet_num <- indpdet_num + 1
      x$indpdet_gp[i] <- indpdet_num
      
    } else x$indpdet_gp[i] <- indpdet_num
  }
  
  return(x)
}


#### Main function ####
indp_dets <- function(input_ct_file, path_species_database, indp_interval = 3600, rm_stations = NA, 
                      log_fn = message){

  ## Load the raw data file
  ctdata <- read.csv(input_ct_file, head = T, fileEncoding = "UTF-8-BOM") #ctdataSample
  
  species_database <- read.xlsx(path_species_database, sheet = "Species_Database")
  check_speciesdatabase(species_database)
  
  ## Data cleaning
  ctdata_cleaned <- ctdata %>%
    mutate(Station = as.character(Station), 
           ScientificName = as.character(ScientificName),
           FileModifyDate = as.POSIXct(FileModifyDate),
           Date = as.Date(Date),
           Time = as.factor(Time)) %>% #caution when using factors
    filter(!Station %in% rm_stations)
  
  ## Split it into separate data frames for each station/species combination
  recs_split <- split(x = ctdata_cleaned, 
                      f = list(ctdata_cleaned$Station,
                               ctdata_cleaned$ScientificName),
                      drop = TRUE)
  
  
  
  rec.df.list <- lapply(recs_split, calc_difftime, indp_interval = indp_interval)
  
  ## Get the max quantity for each independent detection, followed by the number 
  ## of independent detections and quantity for each station and species  
  ctdata_full <- bind_rows(rec.df.list) %>% #Reassemble into one data frame
    group_by(ScientificName, Station, indpdet_gp) %>%
    mutate(Quantity = max(Quantity),
           Remarks = paste(unique(Remarks)[!unique(Remarks) == ""], 
                           collapse = "; ")) %>%
    ungroup() %>%
    filter(!difftime < indp_interval) %>%
    select(-difftime, -indpdet_gp)
  
  output_dir <- dirname(input_ct_file)
  output_indp_det_full_path <- file.path(output_dir, 'ct_indp_det_full.csv')
  write.csv(ctdata_full, output_indp_det_full_path, row.names = F)
  log_fn("ct_indp_det_full.csv generated!")
  
  ctdata_summ <- ctdata_full %>% 
    group_by(ScientificName, Station) %>%
    summarise(IndpDet = n(),
              MaxQty = max(Quantity),
              .groups = "drop") %>%
    select(Station, ScientificName, IndpDet, MaxQty) %>%
    arrange(Station, ScientificName) 
  # print(ctdata_summ) #if you want to view it
  
  output_indp_det_path <- file.path(output_dir, 'ct_indp_det.csv')
  write.csv(ctdata_summ, output_indp_det_path, row.names = F)
  log_fn("ct_indp_det.csv generated!")
  
  
  #----------------------------------------------------------------------------#
  #### Summary of stations and number of independent detections per species ####
  #----------------------------------------------------------------------------#
  
  extract_station <- function(x){
    stations_idx <- which(!is.na(x))
    stations <- paste(sort(names(x)[stations_idx]), collapse = ", ")
    
    return(stations)
  }
  
  
  count_stations <- function(x){
    num_stations <- sum(!is.na(x))
    
    return(num_stations)
  }
  
  
  species_summ <- ctdata_summ %>%
    select(-MaxQty) %>%
    pivot_wider(names_from = Station, values_from = IndpDet)
  
  ## Getting station information 
  species_summ_num <- select(species_summ, where(is.numeric))
  
  station_info <- data.frame(
    ScientificName = species_summ$ScientificName, 
    Stations = apply(species_summ_num, 1, extract_station),
    NumStations = apply(species_summ_num, 1, count_stations))
  
  ## Getting indp det info and combining with station info
  species_summ2 <- species_summ %>%
    replace(is.na(.), 0) %>%
    mutate(IndpDet = rowSums(across(where(is.numeric)))) %>%
    merge(station_info, by = "ScientificName") %>%
    select(ScientificName, Stations, NumStations, IndpDet) %>%
    rename("No. of independent detections" = IndpDet,
           "No. of stations detected" = NumStations)
  
  output_species_summ_path <- file.path(output_dir, 'ct_indp_det_species_summary.csv')
  write.csv(species_summ2, output_species_summ_path, row.names = F)
  log_fn("ct_indp_det_species_summary.csv generated!")
  
  
  #-------------------------------------------------------------------------#
  #### Summary of unique species and non-volant mammals for each station ####
  #-------------------------------------------------------------------------#
  
  extract_species <- function(data_row){
    species_idx <- which(!is.na(data_row))[-1]
    species <- paste(sort(names(data_row)[species_idx]), collapse = ", ")
    
  }
  
  species_info <- species_database %>%
    select(-FolderSpeciesName) %>%
    unique() %>%
    # Change dog in species database to without the underscore
    mutate(ScientificName = replace(ScientificName,
                                    ScientificName == "Canis lupus_familiaris",
                                    "Canis lupus familiaris"))
  
  ## Remove any Scientific names with spp. or unidentified if there is a similar 
  ## species in the same species group
  ctdata_summ_fix <- ctdata_summ %>%
    select(Station, ScientificName) %>%
    # Change dog in data to without the underscore
    mutate(ScientificName = replace(ScientificName,
                                    ScientificName == "Canis lupus_familiaris",
                                    "Canis lupus familiaris")) %>%
    merge(y = species_info, all.x = T, by = "ScientificName")
  
  if (any(is.na(ctdata_summ_fix$SpeciesCountGroups))){
    missing_spp <- ctdata_summ_fix %>%
      filter(is.na(SpeciesCountGroups)) %>%
      select(ScientificName) %>%
      unique() %>%
      as.vector()
    
    stop(paste0("There are some missing species: ", paste(missing_spp, sep=", "), 
               ". Please check code (Line 182)."))
  }
    
  ctdata_info <- ctdata_summ_fix %>%
    filter(!SpeciesCountGroups == "Exclude count") %>% 
    mutate(Station_SpGp = paste(Station, SpeciesCountGroups, sep = "_")) %>%
    arrange(Station)
  
  # To view which classes and species are excluded from the species count, based 
  # on the SpeciesCountGroups column in the species database
  excluded_species <- species_info %>%
    filter(SpeciesCountGroups == "Exclude count") %>%
    select(SpeciesCountGroups, ScientificName) %>%
    unique()
  
  # Stations with only one species in the same SpeciesCountGroup
  stations_UniqueSp <- ctdata_info %>%
    group_by(Station_SpGp) %>%
    summarise(NumSpecies = n(), .groups = "drop") %>%
    filter(NumSpecies == 1)
  
  ctdata_UniqueSp <- ctdata_info %>%
    filter(Station_SpGp %in% stations_UniqueSp$Station_SpGp)
  
  # Stations with more than one species in the same SpeciesCountGroup
  stations_DupSp <- ctdata_info %>%
    group_by(Station_SpGp) %>%
    summarise(NumSpecies = n(), .groups = "drop") %>%
    filter(NumSpecies > 1)
  
  ctdata_DupSp <- ctdata_info %>%
    filter(Station_SpGp %in% stations_DupSp$Station_SpGp) %>%
    filter(!grepl('spp.|Unidentified', ScientificName))
  
  ctdata_NoSpp <- rbind(ctdata_UniqueSp, ctdata_DupSp) 
  
  ## Unique species at each station
  ctdata_unique_mat <- ctdata_NoSpp %>%
    select(Station, ScientificName) %>%
    mutate(Present = 1) %>%
    pivot_wider(names_from = ScientificName, values_from = Present)
  ctdata_unique_mat$UniqueSpecies <- apply(ctdata_unique_mat, 1, extract_species)
  
  ctdata_unique_df <- ctdata_unique_mat %>%
    replace(is.na(.), 0) %>%
    mutate(num_species = rowSums(across(where(is.numeric)))) %>%
    select(Station, num_species, UniqueSpecies) %>%
    rename("No. of Unique Species Recorded" = num_species, 
           "Unique Species" = UniqueSpecies) 
  
  ## Non-volant mammal species
  ctdata_nonvolant <- ctdata_NoSpp %>%
    mutate(Volant = as.logical(Volant)) %>%
    filter(Class == "Mammalia" & Volant == F) %>%
    select(Station, ScientificName) %>%
    mutate(Present = 1) %>%
    pivot_wider(names_from = ScientificName, values_from = Present)
  ctdata_nonvolant$NonVolSpecies <- apply(ctdata_nonvolant, 1, extract_species)
  
  ctdata_nonvolant_df <- ctdata_nonvolant %>%
    replace(is.na(.), 0) %>%
    mutate(num_species = rowSums(across(where(is.numeric)))) %>%
    select(Station, num_species, NonVolSpecies) %>%
    rename("No. of Non-volant Species Recorded" = num_species,
           "Non-volant species" = NonVolSpecies)
  
  station_summ <- merge(ctdata_unique_df, ctdata_nonvolant_df, by = "Station") %>%
    mutate("Total No. of Trap Nights" = NA) %>%
    arrange(Station)
  
  output_station_summ_path <- file.path(output_dir, 'ct_indp_det_station_summary.csv')
  write.csv(station_summ, output_station_summ_path, row.names = F)
  log_fn("ct_indp_det_station_summary.csv generated!")
  
  
  #------------------------------------------------#
  #### Table of detected species per CT station ####
  #------------------------------------------------#
  prep_class_data <- function(det_ls, classes, i){
    det_df <- det_ls[[i]]
    class_name <- classes[i]
    
    det_class <- det_df %>%
      add_row(SpeciesCol = class_name, .before = 1) %>%
      select(-Class) %>%
      rename(Species = SpeciesCol)
    
    return(det_class)
  }
  
  species_info_basic <- species_info %>%
    select(ScientificName, CommonName, Class)
  
  det_ls <- ctdata_unique_mat %>%
    as.data.frame() %>%
    select(-UniqueSpecies) %>%
    column_to_rownames(var = "Station") %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(var = "Species") %>%
    merge(species_info_basic, by.x = "Species", by.y = "ScientificName", all.x = T) %>%
    mutate(SpeciesCol = paste(Species, CommonName, sep = "\n")) %>%
    select(-Species, -CommonName) %>%
    select(SpeciesCol, everything()) %>%
    split(f = as.factor(.$Class))
  
  det_df <- lapply(seq_along(det_ls), prep_class_data, 
                    det_ls = det_ls, classes = names(det_ls)) %>%
    bind_rows() 
  
  spp_sum <- det_df %>%
    summarise(across(.cols = where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(Species = "Total species count") %>%
    select(Species, everything())
  
  def_final <- rbind(det_df, spp_sum)
  
  ## Build and format the workbook
  class_name_idxs <- which(det_df$Species %in% names(det_ls)) + 1
  bold_rows_idxs <- c(1, class_name_idxs, nrow(def_final)+1)
  species_idxs <- seq(nrow(def_final)+1)[-bold_rows_idxs]
  
  wb <- buildWorkbook(def_final)
  
  wrap_style <- createStyle(wrapText = TRUE)
  addStyle(wb, sheet = 1, style = wrap_style, 
           rows = 1:nrow(def_final), cols = 1, gridExpand = T)
  
  bold_style <- createStyle(textDecoration = "bold")
  addStyle(wb, sheet = 1, style = bold_style, 
           rows = bold_rows_idxs, cols = 1:length(def_final), gridExpand = T)
  for (class_name_idx in class_name_idxs){
    mergeCells(wb, sheet = 1, rows = class_name_idx, cols = 1:length(def_final))
  }
  
  setRowHeights(wb, sheet = 1, rows = species_idxs, heights = 30)
  setColWidths(wb, sheet = 1, cols = 1:ncol(def_final), widths = "auto")
  
  output_species_det_path <- file.path(output_dir, "ct_species_detection.xlsx")
  saveWorkbook(wb, output_species_det_path, overwrite = T)
  log_fn("ct_species_detection.xlsx generated!")
  
  #-----------------------------------#
  #### Summary of pig distribution ####
  #-----------------------------------#
  
  if (!any(ctdata_cleaned$ScientificName %in% "Sus scrofa")){
    
    log_fn("No Sus scrofa detected — skipping wild boar summary.")
    
  } else {
    
    pig_det_days <- ctdata_cleaned %>%
      filter(ScientificName %in% "Sus scrofa") %>%
      select(Station, Date) %>%
      unique() %>%
      droplevels() %>%
      group_by(Station) %>%
      summarise(DetDays = n(), .groups = "drop")
    
    pig_indp_det <- ctdata_summ %>%
      filter(ScientificName %in% "Sus scrofa") %>%
      droplevels() %>%
      ungroup() %>%
      select(Station, IndpDet, MaxQty) 
    
    pig_summ <- merge(pig_indp_det, pig_det_days, by = "Station") %>%
      arrange(Station) 
    
    all_stations <- data.frame(Station = unique(ctdata_cleaned$Station)) %>%
      arrange(Station)
    
    pig_summ_all_stations <- merge(all_stations, pig_summ, all.x = T) %>%
      replace_na(list(IndpDet = 0, MaxQty = 0, DetDays = 0)) %>%
      rename("Max No. of Individuals Observed per Independent Detection" = MaxQty,
             "No. of Independent Detections of Wild Boars" = IndpDet,
             "No. of Detected Trap Nights" = DetDays) %>%
      mutate("Total No. of Trap Nights" = NA,
             "Detection Rate" = NA)
    
    output_wildboar_summ_path <- file.path(output_dir, 'ct_indp_det_wildboar_summary.csv')
    write.csv(pig_summ_all_stations, output_wildboar_summ_path, row.names = F)
    log_fn("ct_indp_det_wildboar_summary.csv generated!")
    
  }
  
  
  #--------------------------------------------------#
  #### Summary for arboreal crossing camera traps ####
  #--------------------------------------------------#
  
  if (!any(grepl("crossing", tolower(ctdata_cleaned$Remarks)))){
    
    log_fn("No crossing remarks detected — skipping arboreal summary.")
    
  } else {
  
    mono_crossing <- ctdata_cleaned %>%
      mutate(Remarks = tolower(Remarks)) %>%
      filter(grepl("toward", Remarks)) %>%
      mutate(Direction = trimws(word(Remarks, start = -1, sep = "towards")), 
             Direction = sub('[[:punct:]]+$', '', word(Direction)), 
             Direction = paste("Towards", toupper(Direction))) %>%
      count(ScientificName, Direction, name = "Total") 
    
    bi_crossing <- ctdata_cleaned %>%
      filter(grepl("bi-directional|bidirectional", tolower(Remarks))) %>%
      count(ScientificName, name = "Total") %>%
      mutate(Direction = "Bi-directional")
    
    crossings <- rbind(mono_crossing, bi_crossing)
    
    crossing_count <- crossings %>%
      group_by(ScientificName) %>%
      summarise(Total = sum(Total), .groups = "drop") 
    
    false_humans <- ctdata_cleaned %>%
      filter(ScientificName %in% c("False trigger", "Non targeted")) %>%
      count(ScientificName, name = "Total")
    
    others <- ctdata_cleaned %>%
      filter(!ScientificName %in% c("False trigger", "Non targeted")) %>%
      filter(!grepl("toward|bi-directional|bidirectional", tolower(Remarks))) 
    
    species_activity <- crossing_count %>%
      rbind(data.frame(ScientificName = "Others", Total = nrow(others)), false_humans)
    
    num_vids <- sum(species_activity$Total)
    
    if (num_vids != nrow(ctdata_cleaned)) stop("Something wrong, line 433.")
    
    arboreal_table1 <- data.frame(ScientificName = "Total number of crossings", 
                               Total = sum(crossing_count$Total)) %>%
      rbind(species_activity) %>%
      mutate(Perc = round(Total/num_vids*100, 1), 
             Total = paste0(Total, " (", Perc, "%)")) %>%
      rename(Activity = ScientificName) %>%
      select(Activity, Total) %>%
      rbind(data.frame(Activity = "Total number of videos", 
                       Total = num_vids))
    
    num_crossings <- sum(crossings$Total)
    
    arboreal_table2 <- crossings %>%
      mutate(Perc = round(Total/num_crossings*100, 1), 
             Total = paste0(Total, " (", Perc, "%)")) %>%
      select(-Perc) %>% 
      rbind(mutate(crossing_count, Direction = "Total")) %>%
      pivot_wider(names_from = ScientificName, values_from = Total, values_fill = "0")
    
    ## Save as .xlsx
    wb <- createWorkbook()
    
    addWorksheet(wb, "Summary activity")
    writeData(wb, "Summary activity", arboreal_table1, colNames = F)
    
    addWorksheet(wb, "Crossing direction")
    writeData(wb, "Crossing direction", arboreal_table2)
    
    output_arboreal_path <- file.path(output_dir, 'ct_arboreal.xlsx')
    saveWorkbook(wb, output_arboreal_path, overwrite = T)
    log_fn("ct_arboreal.xlsx generated!")
  }
  
}