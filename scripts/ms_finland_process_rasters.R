# This script is for processing rasters after unzipping data downloaded from the 
# Luke MS-NFI database: https://kartta.luke.fi/opendata/valinta.html.

# Load libs and config
source('scripts/settings.R')

# Define year of measurements
year <- 2021

tiles_path <- file.path(config$PATH_nfi_finland, "unzipped", year)
tile_dirs <- list.files(tiles_path, full.names = T, recursive = F)
tile_names <- list.files(tiles_path, full.names = F, recursive = F)

tile_files <- list.files(tile_dirs[[1]], full.names = T)
tile_file_names <- list.files(tile_dirs[[1]], full.names = F)



tif_dts <- invisible(lapply(seq_along(tile_files), function(i) {
  gc()
  file <- tile_files[i]
  file_name <- tile_file_names[i]
  tif <- raster(file)
  layer_name <- strsplit(file_name, split = "_vmi1x")[[1]][1]
  
  xy <- ifelse(i == 1, T, F)
  
  print(paste0("Converting tif '", layer_name, "' into data.table..."))
  tif_dt <- as.data.table(as.data.frame(tif, xy = xy))
  setnames(tif_dt, old = "Layer_1", new = layer_name)
  print("Done.")
  tif_dt
}))

gc()

tif_dt <-do.call(cbind, c(list(tif_dts[[1]]), tif_dts[2:length(tif_dts)]))
# tif_dt <- na.omit(tif_dt)

gc()
rm(tif_dts)
gc()







