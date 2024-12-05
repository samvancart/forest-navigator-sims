source('scripts/settings.R')
source('./r/utils.R')

# Define the function to unzip a file and save it to a path
unzip_file <- function(zip_file_path, destination_path) {
  # Ensure the destination directory exists
  if (!dir.exists(destination_path)) {
    dir.create(destination_path, recursive = TRUE)
  }
  
  # Unzip the file to the destination directory
  unzip(zip_file_path, exdir = destination_path)
  
  cat("Unzipped", zip_file_path, "to", destination_path, "\n")
}


# Define the function to process filenames
process_filename <- function(filename, separator, desired_length) {
  # Split the filename using the specified separator
  filename_list <- unlist(strsplit(filename, separator))
  
  # Check the length of the split filename
  actual_length <- length(filename_list)
  
  # Handle different cases based on the length
  if (actual_length > desired_length) {
    # Cut from the front and collapse the list into a string
    processed_filename <- paste(tail(filename_list, desired_length), collapse = separator)
  } else if (actual_length < desired_length) {
    # Produce a warning but return the filename unchanged
    warning("The length of the filename list is shorter than the desired length.")
    processed_filename <- filename
  } else {
    # Return the filename unchanged
    processed_filename <- filename
  }
  
  return(processed_filename)
}

zip_pattern <- "7z"
all_files <- list.files(config$PATH_acc_base, recursive = T)
unzip_files <- all_files[grepl(zip_pattern, all_files)]

filenames <- unlist(lapply(unzip_files, function(file) {
  split1 <- unlist(strsplit(file, "/"))
  name_part <- split1[length(split1)]
  split2 <- unlist(strsplit(name_part, "\\."))
  filename <- paste(split2[1], split2[2], sep = ".")
  process_filename(filename, "_", 5)
}))























