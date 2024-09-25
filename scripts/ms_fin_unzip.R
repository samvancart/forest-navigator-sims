# This script is for unzipping data downloaded from the 
# Luke MS-NFI database: https://kartta.luke.fi/opendata/valinta.html.

# Load libs and config
source('scripts/settings.R')

# Define year of measurements
year <- 2021

# Define file type for folder name
file_type <- "tif"

# Path to zipped folders
zip_nfi_path <- paste(config$PATH_nfi_finland)
zip_nfi_files <- list.files(zip_nfi_path, pattern = "*.zip", full.names = T, recursive = T)

# Unzip all downloads into folder
unzip_dir <- paste0(zip_nfi_path, "unzipped")
map(zip_nfi_files, ~ unzip(.x, exdir = unzip_dir))

# Unzip into folders by tile
zip_vars_path <- paste0(unzip_dir)
zip_vars_files <- list.files(zip_vars_path, pattern = "*.zip", full.names = T, recursive = T)

# Create directories by tile and unzip files into them. Remove zip files after. 
invisible(lapply(zip_vars_files, function(file) {
  
  # Split filenames into parts
  parts <- strsplit(strsplit(file, split = "\\.")[[1]][1], split = "_")[[1]]
  
  # Get tile
  tile <- parts[length(parts)]
  
  # Path for unzipped files based on tile
  unzip_tile_dir <- file.path(unzip_dir, year, tile, file_type)
  
  # Unzip into tile dir
  print(paste0("Unzipping ", file))
  unzip(file, exdir = unzip_tile_dir)
  cat("\n")
  print(paste0("Unzipped file into ", unzip_tile_dir))
  
  # Remove zip file
  message(paste0("Removing ", file))
  file.remove(file)
  
}))

















