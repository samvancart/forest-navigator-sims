# This script is for downloading data directly into Allas storage via Accelerator
# download links. If the file is a zip archive then it will be unzipped and only 
# the inflated files will be transferred. 
# USE: Pass a directory as parameter else default_input_dir will be used (section PARSE ARGS).
# All txt files containing the download links in the specified directory will be downloaded,
# 1 txt file/array job.
# NAMING OF FILES: Files should be named like this: eg. download-file-1_name.txt.
# So the separator should be something other than an underscore except for the last
# separator. The text between the underscore (last separator) and the dot before the file extension
# will be used as the "folder name" in Allas.


# LOAD LIBS ---------------------------------------------------------------


source('scripts/settings.R')


# PARSE ARGS --------------------------------------------------------------


default_input_dir  <- "data/acc/input/simulation_sites_1km/raw/tree_data/"

# Define command-line options
option_list <- list(
  make_option(c("-d", "--dir"), type = "character", default = default_input_dir,
              help = paste("Path to folder containing .txt files [default:", default_input_dir, "]"), metavar = "character"),
  
  make_option(c("-a", "--array_id"), type="integer", default=as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1)), 
              help="SLURM array job ID [default: %default]"),
  
  make_option(c("-c", "--array_count"), type="integer", default=as.integer(Sys.getenv("SLURM_ARRAY_TASK_COUNT", unset = 1)),
              help="Total number of array jobs [default: %default]")
)

# Parse options
parser <- OptionParser(option_list = option_list)
args <- parse_args(parser)

# Expand ~ to full path
args$dir <- path.expand(args$dir)


if (!dir.exists(args$dir)) {
  stop(paste("Directory does not exist:", args$dir))
}


if (is.null(args$array_id)) {
  stop("Please provide --array_id to select which .txt file to process.")
}

print("Dir:")
print(args$dir)
print("Array id:")
print(args$array_id)
print("Max arrays:")
print(args$array_count)


# CHOOSE FILE -------------------------------------------------------------


txt_files <- list.files(args$dir, pattern = "\\.txt$", full.names = TRUE)

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









