rm(list=ls())

library(openxlsx)
library(tidyverse)

species_database_path <- "data/Species_Database2.xlsx"

species_database <- read.xlsx(species_database_path, sheet = "Species_Database")
edit_log <- read.xlsx(species_database_path, sheet = "Edit_Log", detectDates = T)

species_database_edit <- species_database %>%
  mutate(FolderSpeciesName = trimws(FolderSpeciesName)) %>%
  unique()

species_database_xlsx <- list("Species_Database" = species_database_edit,
                              "Edit_Log" = edit_log)

write.xlsx(species_database_xlsx, "data/Species_Database.xlsx",
           overwrite = T, keepNA = T)
