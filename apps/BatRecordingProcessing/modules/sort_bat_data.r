######################################################################
# (\__/)
# (>'.'<)   Functions to copy .wav files based on manual ids
# (")_(")
######################################################################

# Copy a wav file to another folder (target_folder) in the output_folder
copy_wav_file <- function(output_folder, manual_id, wav_filepath) {
    # Default to NA if there is no manual_id
    if (is.na(manual_id) || manual_id == '') {
        manual_id = "NA"
    }

    # Generate path to target_folder
    target_folder = file.path(output_folder, manual_id)

    # Create new folder if target_folder does not exist
    if (!(dir.exists(target_folder))) {
        dir.create(target_folder)
    }

    # Copy .wav file to target folder
    file.copy(wav_filepath, target_folder)
}

# Process a row in the dataframe
process_row <- function(row_data, wav_folder, output_folder) {
    # Generate path to .wav file
    wav_filename = row_data["IN.FILE"]
    wav_filepath = file.path(wav_folder, wav_filename)

    # Check that there is a .wav file at the specified location
    if (!file.exists(wav_filepath)) {
        stop("The .wav file ", wav_filepath, " does not exist. ",
             "Please check that the WAV_FOLDER has been set correctly.")
    }

    # Get manual id for row
    manual_id = trimws(row_data[["MANUAL.ID"]])

    # Replace any ? with _unsure since ? is not allowed in Windows folder name
    manual_id = gsub("\\?", "_unsure", manual_id)

    copy_wav_file(output_folder, manual_id, wav_filepath)
}

# Create new output folder for sorted .wav files
create_output_folder <- function(output_folder) {
    if (!dir.exists(output_folder)) {
      dir.create(output_folder)

    } else if (length(list.files(output_folder)) != 0){
      stop(paste0("ERROR: Output folder for sorted wav files exists ",
                  "and is not empty. Are you sure this is the right folder?"))
    }
}

# Sort .wav files based on manual ids
sort_wav_files <- function(meta_data, wav_folder, output_folder) {
    # Create output folder
    create_output_folder(output_folder)

    # Process each row in meta file
    apply(meta_data, 1, process_row, wav_folder = wav_folder, output_folder = output_folder)
}
