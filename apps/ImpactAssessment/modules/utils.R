install_load_packages <- function(packages){
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(new.packages)) install.packages(unlist(new.packages))
  
  lapply(packages, require, character.only = T)
}


load_cs_impact_sheet <- function(fauna_database_path){
  
  fauna_database <- read.xlsx(fauna_database_path,
                              sheet = "CS species impact intensity",
                              startRow = 2) 
  
  cs_impact <- fauna_database %>%
    rename(ScientificName = Scientific.name,
           CP_HumanDisturbance = "Human.disturbances.(CP)",
           CP_AccInjuryMortality = "Accidental.injury.or.mortality.(CP)",
           CP_LightDisturbance = "Light.disturbances.(CP)",
           CP_LossConnectivity = "Loss.of/reduction.in.ecological.connectivity.for.faunal.movement.(CP)",
           CP_HumanWildlifeConflict = "Human-wildlife.conflict.(CP)",
           OP_HumanDisturbance = "Human.disturbances.(OP)",
           OP_AccInjuryMortality = "Accidental.injury.or.mortality.(OP)",
           OP_LightDisturbance = "Light.disturbances.(OP)",
           OP_LossConnectivity = "Loss.of/reduction.in.ecological.connectivity.for.faunal.movement.(OP)",
           OP_HumanWildlifeConflict = "Human-wildlife.conflict.(OP)",
           OP_Poaching = "Poaching.(OP)") %>%
    mutate(CP_LossHabitat = NA) %>%
    select(ScientificName, CP_LossHabitat, starts_with(c("CP_", "OP_")))
  
  return(cs_impact)
}


fill_missing_spp <- function(cs_impact, input_spp_df){

  ## Find species missing in the fauna database
  target_spp <- input_spp_df$ScientificName
  missing_spp <- target_spp[!target_spp %in% cs_impact$ScientificName]
  
  if (length(missing_spp) != 0){
    ## Add missing species with empty impact intensity values
    cs_impact_missing <- data.frame(matrix(ncol = ncol(cs_impact), 
                                           nrow = length(missing_spp)))
    colnames(cs_impact_missing) <- colnames(cs_impact)
    cs_impact_missing$ScientificName <- missing_spp
    
    cs_impact_full <- rbind(cs_impact, cs_impact_missing)
    
    cat("WARNING: There are species missing from the fauna database: ", 
        paste(missing_spp, collapse = ", "), ". ", 
        "These missing species will be present in the output with missing ",
        "impact intensity information.\n", sep = "")
  } else {
    
    cs_impact_full <- cs_impact
  }
  
  ## Convert to long format 
  cs_impact_long <- cs_impact_full %>%
    pivot_longer(cols = !ScientificName, 
                 names_to = "ImpactType", values_to = "ImpactIntensity") %>%
    mutate(ProjectPhase = case_when(
      str_detect(ImpactType, "CP_") ~ "Construction",
      str_detect(ImpactType, "OP_") ~ "Operational"))
  
  return(cs_impact_long)
}


addSuperSubScriptToCell <- function(wb,
                                    sheet,
                                    row,
                                    col,
                                    texto,
                                    size = '10',
                                    colour = '000000',
                                    font = 'Arial',
                                    family = '2',
                                    bold = FALSE,
                                    italic = FALSE,
                                    underlined = FALSE) {
  
  placeholderText <- 'Placeholder text that should not appear anywhere in your document.'
  
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = placeholderText,
                      startRow = row,
                      startCol = col)
  
  #finds the string that you want to update
  stringToUpdate <- which(sapply(wb$sharedStrings,
                                 function(x){
                                   grep(pattern = placeholderText, x)
                                 }) == 1)
  
  #splits the text into normal text, superscript and subcript
  
  normal_text <- str_split(texto, "\\[.*?\\]|~.*?~") %>% 
    pluck(1) %>% purrr::discard(~ . == "")
  
  sub_sup_text <- str_extract_all(texto, "\\[.*?\\]|~.*?~") %>% pluck(1)
  
  if (length(normal_text) > length(sub_sup_text)) {
    sub_sup_text <- c(sub_sup_text, "")
  } else if (length(sub_sup_text) > length(normal_text)) {
    normal_text <- c(normal_text, "")
  }
  # this is the separated text which will be used next
  texto_separado <- map2(normal_text, sub_sup_text, ~ c(.x, .y)) %>% 
    reduce(c) %>% 
    purrr::discard(~ . == "")
  
  #formatting instructions
  
  sz    <- paste('<sz val =\"',size,'\"/>',
                 sep = '')
  col   <- paste('<color rgb =\"',colour,'\"/>',
                 sep = '')
  rFont <- paste('<rFont val =\"',font,'\"/>',
                 sep = '')
  fam   <- paste('<family val =\"',family,'\"/>',
                 sep = '')
  
  #if its sub or sup adds the corresponding xml code
  sub_sup_no <- function(texto) {
    
    if(str_detect(texto, "\\[.*\\]")){
      return('<vertAlign val=\"superscript\"/>')
    } else if (str_detect(texto, "~.*~")) {
      return('<vertAlign val=\"subscript\"/>')
    } else {
      return('')
    }
  }
  
  #get text from normal text, sub and sup
  get_text_sub_sup <- function(texto) {
    str_remove_all(texto, "\\[|\\]|~")
  }
  
  #formating
  if(bold){
    bld <- '<b/>'
  } else{bld <- ''}
  
  if(italic){
    itl <- '<i/>'
  } else{itl <- ''}
  
  if(underlined){
    uld <- '<u/>'
  } else{uld <- ''}
  
  #get all properties from one element of texto_separado
  
  get_all_properties <- function(texto) {
    
    paste0('<r><rPr>',
           sub_sup_no(texto),
           sz,
           col,
           rFont,
           fam,
           bld,
           itl,
           uld,
           '</rPr><t xml:space="preserve">',
           get_text_sub_sup(texto),
           '</t></r>')
  }
  
  
  # use above function in texto_separado
  newString <- map(texto_separado, ~ get_all_properties(.)) %>% 
    reduce(paste, sep = "") %>% 
    {c("<si>", ., "</si>")} %>% 
    reduce(paste, sep = "")
  
  # replace initial text
  wb$sharedStrings[stringToUpdate] <- newString
}

