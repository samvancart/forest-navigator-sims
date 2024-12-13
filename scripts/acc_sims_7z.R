# This script is for unzipping .7z files downloaded from the accelerator db.
# The files are extracted into the "unzipped" folder.


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# Unzip all files
invisible(lapply(seq_along(files_7z_paths), function(i) {
  file <- files_7z_paths[i]
  archive_extract(archive = file, dir = dest_path)
  percent_completed <- ceiling((i/length(files_7z_paths))*100)
  cat("\014")
  cat("Unzipping: ", percent_completed, "%")
  if(percent_completed==100) cat("\n Done!")
}))

unzipped_files <- list.files(dest_path)

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




# Process clim file
clim_paths <- list.files(dest_path, full.names = T)
clim_path <- clim_paths[7]

filtered_clim_dt <- filter_clim_by_tree_data_cells(clim_path, aaa_file)

transformed_clim_dt <- transform_and_add_columns(filtered_clim_dt, operations)



# Get climate scenario name
parts <- strsplit(clim_path, split = "/")[[1]]
name <- unlist(strsplit(parts[length(parts)], split = "_"), recursive = T)[1]


# TRAN MATRICES

# Variables to create TRAN tables for
tranVars <- c("par", "vpd", "co2", "precip", "tair")

print(paste0("Creating tran matrices..."))
cat("\n")

# Create list of TRAN matrices
tranMatrices <- lapply(tranVars, function(x) {
  formula <- as.formula(paste("cell", "~", "day"))
  dcast_dt <- as.matrix(dcast(transformed_clim_dt, formula, value.var = x))
})

names(tranMatrices) <- paste0(tranVars,"Tran")

















