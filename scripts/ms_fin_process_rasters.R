# This script is for processing rasters after unzipping data downloaded from the 
# Luke MS-NFI database: https://kartta.luke.fi/opendata/valinta.html.
# After running ms_fin_unzip.R, the rasters will be in folders based on their
# respective tile ids. By default, this script preprocesses rasters for all tiles
# in a loop and saves them as rdata files. NaNs and no measurement values are removed
# and columns are given English names but no further processing is done.

# Load libs and config
source('scripts/settings.R')

# Define year of measurements
year <- 2021
file_type <- "tif"


tiles_path <- file.path(config$PATH_nfi_finland, "unzipped", year)
tile_dirs <- list.files(tiles_path, full.names = T, recursive = F)
tile_names <- list.files(tiles_path, full.names = F, recursive = F)
pattern <- paste0("/", file_type, "$")
tile_subfolder_dirs <- grep(pattern,  list.files(tile_dirs, full.names = T, recursive = F), value = TRUE)


for(tile_i in 1:length(tile_dirs)) {
  tile_files <- list.files(tile_subfolder_dirs[[tile_i]], full.names = T, recursive = T)
  tile_file_names <- list.files(tile_subfolder_dirs[[tile_i]], full.names = F, recursive = T)
  
  print(paste0("Files to process are:"))
  print(tile_files)
  cat("\n")
  
  tif_dts <- invisible(lapply(seq_along(tile_files), function(i) {
    gc()
    file <- tile_files[i]
    file_name <- tile_file_names[i]
    tif <- raster(file)
    layer_name <- strsplit(file_name, split = "_vmi1x")[[1]][1]
    
    xy <- ifelse(i == 1, T, F)
    # xy <- T
    
    print(paste0("Converting tif '", layer_name, "' into data.table..."))
    tif_dt <- as.data.table(as.data.frame(tif, xy = xy))
    setnames(tif_dt, old = "Layer_1", new = layer_name)
    print("Done.")
    tif_dt
  }))
  
  gc()
  
  cat("\n")
  
  print(paste0("Combining tables..."))
  tif_dt <-do.call(cbind, c(list(tif_dts[[1]]), tif_dts[2:length(tif_dts)]))
  print(paste0("Done."))
  cat("\n")
  
  # Rm
  gc()
  rm(tif_dts)
  gc()
  
  print(paste0("Setting English column names..."))
  # English colnames
  setnames(tif_dt, old = names(config$VAR_nfi_fin_vars_dict), new = unname(unlist(config$VAR_nfi_fin_vars_dict)), skip_absent = T)
  print(paste0("Done."))
  cat("\n")
  
  print(paste0("Removing NaN and no measurement rows..."))
  # Remove NaN and rows with no measurement
  tif_dt <- tif_dt[complete.cases(tif_dt) & !(fert==32766 | fert==32767)]
  print(paste0("Done."))
  cat("\n")
  
  print(paste0("Creating dir..."))
  # Create dir
  rdata_dir <- file.path(tile_dirs[tile_i], "rdata")
  dir.create(rdata_dir)
  print(paste0("Done."))
  cat("\n")
  
  # Save
  rdata_full_path <- file.path(rdata_dir, paste0("preprocessed_", tile_names[tile_i], ".rdata"))
  print(paste0("Saving into ", rdata_full_path, "..."))
  save(tif_dt, file = rdata_full_path)
  print(paste0("Done."))
  cat("\n")
  
  rm(tif_dt)
  gc()
}















