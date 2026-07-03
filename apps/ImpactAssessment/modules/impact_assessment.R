## modules/impact_assessment.R
## Core logic for running the fauna impact assessment.
## Called by app.R — do not run this file directly.

library(tools)
library(openxlsx)
library(tidyverse)

#### Fixed Variables #### 

IMPACT_TYPE_LEVELS <- c(
  "CP_LossHabitat", "CP_AccInjuryMortality", "CP_HumanWildlifeConflict", "CP_LossConnectivity", 
  "CP_LightDisturbance", "CP_HumanDisturbance", "OP_AccInjuryMortality", "OP_HumanWildlifeConflict", 
  "OP_Poaching", "OP_LossConnectivity", "OP_LightDisturbance", "OP_HumanDisturbance"
)

IMPACT_TYPE_LABELS <- c(
  CP_LossHabitat              = "Loss of/reduction in habitats and food sources",
  CP_AccInjuryMortality       = "Accidental injury or mortality",
  CP_HumanDisturbance         = "Human disturbance",
  CP_HumanWildlifeConflict    = "Human-wildlife conflict",
  CP_LightDisturbance         = "Light disturbance",
  CP_LossConnectivity         = "Loss of/reduction in ecological connectivity for faunal movement",
  OP_AccInjuryMortality       = "Accidental injury or mortality",
  OP_HumanDisturbance         = "Human disturbance",
  OP_HumanWildlifeConflict    = "Human-wildlife conflict",
  OP_LightDisturbance         = "Light disturbance",
  OP_LossConnectivity         = "Loss of/reduction in ecological connectivity for faunal movement",
  OP_Poaching                 = "Poaching"
)

COLUMN_RENAME_MAP <- c(
  ScientificName   = "Receptor (scientific name)",
  Common.Name      = "Receptor (common name)",
  `Global.Status.(IUCN)` = "Global status (IUCN/CITES)",
  National.Status  = "National status",
  Recorded.Species = "Recorded species",
  ProjectPhase     = "Project Phase",
  ImpactType       = "Impact type",
  Sensitivity      = "Sensitivity (S)",
  ImpactIntensity  = "Impact intensity (I)",
  Consequence      = "Consequence (C = S \u00d7 I)",
  Likelihood       = "Likelihood (L)",
  ImpactSig        = "Impact significance (C \u00d7 L)"
)


#### Helper functions ####
# Null-coalescing operator (base R doesn't have %||% before 4.4)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


#### Main function #### 

#' Run the fauna impact assessment and write the formatted output workbook.
#'
#' @param species_list_path  Path to the input species list (.xlsx).
#' @param fauna_database_path Path to the combined fauna database (.xlsx).
#' @param matrix_path        Path to ConsequenceSignificanceMatrix.xlsx.
#' @param output_path        Destination path for the output workbook (.xlsx).
#' @param log                A function used for progress messages, e.g.
#'                           \code{message} (default) or a Shiny \code{incProgress}.
#'
#' @return Invisibly returns the output path on success.
run_impact_assessment <- function(species_list_path,
                                  fauna_database_path,
                                  matrix_path,
                                  output_path,
                                  log = message) {

  # ── 1. Load inputs ────────────────────────────────────────────────────────

  log("Reading species list...")
  input_spp_df <- read.xlsx(species_list_path) %>%
    rename(ScientificName = Scientific.Name) %>%
    unique()

  log("Reading fauna database...")
  ## Read and clean "CS species impact intensity" sheet from fauna database
  cs_impact      <- load_cs_impact_sheet(fauna_database_path)
  
  ## Find species present in the input that is missing from the fauna database 
  ## These missing species will be added to the fauna database with missing values
  ## Data frame will then be converted from wide to long format 
  cs_impact_long <- fill_missing_spp(cs_impact, input_spp_df, log = log)

  # ── 2. Build species × impact table ───────────────────────────────────────

  log("Building impact table...")
  spp_impact <- merge(input_spp_df, cs_impact_long, by = "ScientificName") %>%
    mutate(
      ImpactType = factor(ImpactType, ordered = TRUE, levels = IMPACT_TYPE_LEVELS)
    ) %>%
    arrange(ScientificName, ProjectPhase, ImpactType) %>%
    mutate(
      ImpactType = as.character(ImpactType),
      ImpactType = recode(ImpactType, !!!IMPACT_TYPE_LABELS),
      Sensitivity = "High"
    ) %>%
    select(colnames(input_spp_df), 
           ProjectPhase, ImpactType, Sensitivity, ImpactIntensity, everything())

  # ── 3. Derive Excel column letters for formula injection ──────────────────

  col_letter <- function(df, col_name, offset = 0) {
    idx <- grep(col_name, names(df)) + offset
    # Handle columns beyond Z (e.g. AA, AB …)
    if (idx <= 26) LETTERS[idx]
    else paste0(LETTERS[ceiling(idx / 26) - 1], LETTERS[idx %% 26 %||% 26])
  }

  S_col  <- col_letter(spp_impact, "Sensitivity")
  I_col  <- col_letter(spp_impact, "ImpactIntensity")
  C_col  <- col_letter(spp_impact, "ImpactIntensity", offset = 2)
  L_col  <- col_letter(spp_impact, "ImpactIntensity", offset = 3)
  IR_col <- col_letter(spp_impact, "ImpactIntensity", offset = 8)
  CR_col <- col_letter(spp_impact, "ImpactIntensity", offset = 9)
  LR_col <- col_letter(spp_impact, "ImpactIntensity", offset = 10)

  # ── 4. Append formula and blank columns ───────────────────────────────────

  make_consequence_formula <- function(row, S, I) {
    sprintf(
      "VLOOKUP(%s%d, ConsequenceMatrix!$A$2:$D$6, MATCH(%s%d, ConsequenceMatrix!$A$2:$D$2, 0), FALSE)",
      I, row, S, row)
  }

  make_significance_formula <- function(row, C, L) {
    sprintf(
      "VLOOKUP(%s%d, SignificanceMatrix!$A$2:$F$7, MATCH(%s%d, SignificanceMatrix!$A$2:$F$2, 0), FALSE)",
      L, row, C, row)
  }

  data_rows <- seq_len(nrow(spp_impact)) + 1  # +1 for header row

  spp_impact_formulas <- spp_impact %>%
    mutate(
      "Rationale for impact intensity" = NA_character_,
      Consequence = make_consequence_formula(data_rows, S_col, I_col),
      Likelihood  = NA_character_,
      "Rationale for likelihood"       = NA_character_,
      "Key minimum controls"           = NA_character_,
      ImpactSig = make_significance_formula(data_rows, C_col, L_col),
      "Key mitigation measures"        = NA_character_,
      ResidualImpact      = NA_character_,
      ResidualConsequence = make_consequence_formula(data_rows, S_col, IR_col),
      ResidualLikelihood  = NA_character_,
      ResidualSig = make_significance_formula(data_rows, CR_col, LR_col)
    )

  # Tag formula columns so openxlsx writes them as Excel formulas
  for (col in c("Consequence", "ImpactSig", "ResidualConsequence", "ResidualSig")) {
    class(spp_impact_formulas[[col]]) <- c("character", "formula")
  }

  # ── 5. Rename columns for final output ────────────────────────────────────

  spp_impact_output <- spp_impact_formulas %>%
    rename(any_of(setNames(names(COLUMN_RENAME_MAP), COLUMN_RENAME_MAP)))
  # Replace any remaining dots in column names with spaces
  names(spp_impact_output) <- gsub(".", " ", names(spp_impact_output), fixed = TRUE)

  # ── 6. Validation check ───────────────────────────────────────────────────

  n_in  <- length(unique(input_spp_df$ScientificName))
  n_out <- length(unique(spp_impact_output$`Receptor (scientific name)`))   # first col = scientific name

  if (n_in != n_out) {
    stop(sprintf(
      "Species count mismatch: %d in input vs %d in output. Please check the fauna database.",
      n_in, n_out
    ))
  }
  log(sprintf("Validation passed: %d species in both input and output.", n_in))

  # ── 7. Build workbook ─────────────────────────────────────────────────────

  log("Writing output workbook...")
  wb <- createWorkbook()

  style_all     <- createStyle(halign = "center", valign = "center", wrapText = TRUE)
  style_sppname <- createStyle(halign = "center", valign = "center", wrapText = TRUE,
                               textDecoration = "italic")
  style_header  <- createStyle(halign = "center", valign = "center", wrapText = TRUE,
                               textDecoration = "bold", border = "bottom")
  style_bold    <- createStyle(textDecoration = "bold")

  addWorksheet(wb, "Receptor")
  writeData(wb, sheet = "Receptor", spp_impact_output)

  # Subscript headers for residual columns
  residual_cols <- list(
    ResidualImpact      = "Residual impact intensity (I~R~)",
    ResidualConsequence = "Residual consequence (C~R~ = S \u00d7 I~R~)",
    ResidualLikelihood  = "Residual likelihood (L~R~)",
    ResidualSig         = "Residual impact significance (C~R~ \u00d7 L~R~)"
  )
  for (col_key in names(residual_cols)) {
    col_idx <- grep(col_key, names(spp_impact_output), ignore.case = TRUE)
    if (length(col_idx)) {
      addSuperSubScriptToCell(wb, sheet = "Receptor", row = 1,
                              col = col_idx, bold = TRUE,
                              texto = residual_cols[[col_key]])
    }
  }

  n_rows <- nrow(spp_impact_output)
  n_cols <- ncol(spp_impact_output)

  addStyle(wb, "Receptor", style_all,
           rows = seq_len(n_rows) + 1, cols = seq_len(n_cols), gridExpand = TRUE)
  addStyle(wb, "Receptor", style_sppname,
           rows = seq(2, n_rows + 1), cols = 2, gridExpand = TRUE)
  addStyle(wb, "Receptor", style_header,
           rows = 1, cols = seq_len(n_cols), gridExpand = TRUE)
  setRowHeights(wb, "Receptor", rows = seq_len(n_rows), heights = 30.75)
  setColWidths(wb, "Receptor", cols = seq_len(n_cols - 4), widths = "auto")
  setColWidths(wb, "Receptor",
               cols = seq(n_cols - 3, n_cols),
               widths = c(27, 33, 22, 36))

  # Consequence / Significance matrix sheets
  matrix_wb         <- loadWorkbook(matrix_path)
  consequence_sheet  <- readWorkbook(matrix_wb, "ConsequenceMatrix",  colNames = FALSE)
  significance_sheet <- readWorkbook(matrix_wb, "SignificanceMatrix", colNames = FALSE)

  addWorksheet(wb, "ConsequenceMatrix")
  writeData(wb, "ConsequenceMatrix", consequence_sheet, colNames = FALSE)
  addStyle(wb, "ConsequenceMatrix", style_bold, rows = 1:2, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, "ConsequenceMatrix", style_bold, rows = 1:6, cols = 1, gridExpand = TRUE)

  addWorksheet(wb, "SignificanceMatrix")
  writeData(wb, "SignificanceMatrix", significance_sheet, colNames = FALSE)
  addStyle(wb, "SignificanceMatrix", style_bold, rows = 1:2, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, "SignificanceMatrix", style_bold, rows = 1:7, cols = 1, gridExpand = TRUE)
  
  saveWorkbook(wb, output_path, overwrite = TRUE)
  log(sprintf("Output saved to: %s", output_path))
  invisible(output_path)
}

