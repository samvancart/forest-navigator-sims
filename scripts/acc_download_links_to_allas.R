# This script is for downloading data directly into Allas storage via Accelerator
# download links. If the file is a zip archive then it will be unzipped and only 
# the inflated files will be transferred. 
# USE: Define a directory path (section INPUT PATH).
# All txt files containing the download links in the specified directory will be downloaded,
# 1 txt file/array job.
# NAMING OF FILES: Files should be named like this: eg. download-file-1_name.txt.
# So the separator should be something other than an underscore except for the last
# separator. The text between the underscore (last separator) and the dot before the file extension
# will be used as the "folder name" in Allas. This "folder name" may include separators as
# long as they are not underscores.


# LOAD LIBS ---------------------------------------------------------------


source('scripts/settings.R')


# INPUT DIR ---------------------------------------------------------------


input_dir  <- "data/acc/input/simulation_sites_1km/raw/tree_data/"


# CHOOSE FILE -------------------------------------------------------------


txt_files <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE)

if (args$array_id < 1 || args$array_id > length(txt_files)) {
  stop(paste("Invalid array_id:", args$array_id, "- must be between 1 and", length(txt_files)))
}

# Select the target file
target_file <- txt_files[args$array_id]
cat("Processing file:", target_file, "\n")

target_file_name <- unlist(tstrsplit(basename(target_file), split = "[_.]", keep = 2))
cat("File name:", target_file_name, "\n")


# CREATE DIRS IN TEMPDIR --------------------------------------------------


# Create temp directories
temp_dir <- tempdir()
temp_download_dir <- file.path(temp_dir, "acc_download_dir")
dir.create(temp_download_dir, showWarnings = FALSE)
unzipped_dir <- file.path(temp_download_dir, "unzipped")
dir.create(unzipped_dir, showWarnings = FALSE)


# DOWNLOAD AND UNZIP ------------------------------------------------------


# Read and process links
links <- readLines(target_file)


for (url in links) {
  cat("Downloading:", url, "\n")
  clean_url <- sub("\\?.*$", "", url)
  dest_file <- file.path(temp_download_dir, basename(clean_url))
  # download.file(url, destfile = dest_file, mode = "wb")
  cmd <- sprintf("curl -L -C - --retry 5 --max-time 3600 -o '%s' '%s'", dest_file, url)
  system(cmd)
  
  
  # Unzip if it's a zip file
  if (grepl("\\.zip$", dest_file)) {
    cat("Unzipping:", dest_file, "\n")
    unzip(dest_file, exdir = unzipped_dir)
    files_in_dir <- unzipped_dir
  } else {
    cat("Skipping unzip: Not a zip file\n")
    files_in_dir <- temp_download_dir
  }
}


# EXTRACT DOWNLOADED FILES ------------------------------------------------


download_dir_contents <- list.files(files_in_dir, recursive = TRUE, full.names = TRUE)
unzipped_files <- download_dir_contents[file.info(download_dir_contents)$isdir == FALSE]


# SAVE TO ALLAS -----------------------------------------------------------


# TODO simsites as var
allas_basepath <- file.path("input", "simulation_sites_1km", target_file_name)

invisible(lapply(unzipped_files, function(file) {
  allas_path <- file.path(allas_basepath, basename(file))
  print(paste0("Saving ", allas_path, " to Allas..."))
  put_object(file = file, object = allas_path, bucket = bucket, multipart = TRUE, region = region)
  print("Done.")
}))

cat("\n")
print("All done.")









