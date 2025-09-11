# This script is for unzipping .7z files downloaded from the accelerator db.
# The files are extracted into the "unzipped" folder and renamed for future
# processing convenience.


# SOURCE_FILES ------------------------------------------------------------


source('scripts/settings.R')
source('./r/utils.R')


# DEFINE PATHS ------------------------------------------------------------


# Define paths: Check indices
acc_sites <- config$VAR_acc_sites[1]
acc_data_name <- config$VAR_acc_data[1]
acc_data_type <- config$VAR_acc_data_types[1]

dir <- file.path(config$PATH_acc_base, acc_sites)
print(dir)
zip_pattern <- "7z"
all_files <- list.files(dir, recursive = T)
unzip_files <- all_files[grepl(zip_pattern, all_files)] # Check which files are returned

# Define from and to paths
zip_file_paths <- file.path(dir, unzip_files)
dest_path <- file.path(dir, acc_data_name, acc_data_type, "unzipped")


# UNZIP -------------------------------------------------------------------


# Unzip all files
invisible(lapply(seq_along(zip_file_paths), function(i) {
  file <- zip_file_paths[i]
  archive_extract(archive = file, dir = dest_path)
  percent_completed <- ceiling((i/length(zip_file_paths))*100)
  cat("\014")
  cat("Unzipping: ", percent_completed, "%")
  if(percent_completed==100) cat("\n Done!")
}))

unzipped_files <- list.files(dest_path)


# RENAME ------------------------------------------------------------------


# Rename if necessary
invisible(lapply(unzipped_files, function(file) {
  filename <- process_acc_filename(file, "_", 5)
  filename <- tolower(filename)
  new_path <- file.path(dest_path, filename)
  old_path <- file.path(dest_path, file)
  if(old_path != new_path) {
    cat("Renaming ", old_path, "\n to \n", new_path, "\n\n")
    file.rename(from = old_path, to = new_path) 
  }
}))




































