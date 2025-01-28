# This script is for unzipping .7z files downloaded from the accelerator db.
# The files are extracted into the "unzipped" folder.


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)

# files_7z_paths <- files_7z_paths[1:2]
files_7z_paths <- grep("gwl4", files_7z_paths, value = T)



# Unzip all files
invisible(mclapply(seq_along(files_7z_paths), function(i) {
  file <- files_7z_paths[i]
  archive_extract(archive = file, dir = dest_path)
  percent_completed <- ceiling((i/length(files_7z_paths))*100)
  cat("\014")
  cat("Unzipping: ", percent_completed, "%")
  if(percent_completed==100) cat("\n Done!")
}, mc.cores = cores))

# unzipped_files <- list.files(dest_path)

# # Rename if necessary
# invisible(lapply(unzipped_files, function(file) {
#   filename <- process_acc_filename(file, "_", 5)
#   filename <- tolower(filename)
#   new_path <- file.path(dest_path, filename)
#   old_path <- file.path(dest_path, file)
#   if(old_path != new_path) {
#     cat("Renaming ", old_path, "\n to \n", new_path, "\n\n")
#     file.rename(from = old_path, to = new_path)
#   }
# }))



# # str_replace_all(conversions_dt$Units, "/", "_")
# 
# pre_process_clim_keep_cols <- c("PlgID", "time", "pr", "rsds", "tas", "vpd", "PlgID_05")
# cl_dt <- fread(file.path(dest_path,unzipped_files[1]))
# 
# filtered_cl_dt <- cl_dt[, ..pre_process_clim_keep_cols]
# 
# 
# # aaa_all_filtered <- aaa_all[Country_Code == "FI"]
# # unique(aaa_all_filtered$forest_type)
# # length(unique(aaa_all_filtered[grep("FIN", forest_type)]$cell_300arcsec))
# 
# 
# sel <- fread("data/acc/input/simulation_sites_200/raw/grid/prebas_selected_reduced_1km.csv")[, c("PlgID", "PlgID_05", "XLON", "YLAT")]
# 
# 
# bokuIDs <- fread("data/acc/docs/boku_id_map_1km.csv")
# 
# sel_bokuID <- merge(sel, bokuIDs[, c("PlgID_05", "BOKU_ID")], by = "PlgID_05")
# 
# 
# length(unique(aaa_filtered$PlgID))
# 
# 
# aaa_filtered <- merge(aaa_all, sel_bokuID, by.x = "cell", by.y = "BOKU_ID")
# 
# aaa_filtered[!complete.cases(aaa_filtered)]
# 
# unique(aaa_filtered$Country_Code)
# 
# folder <- "detrended"
# c_paths <- list.files(file.path(climate_7z_dir, folder))
# c_files <- grep("csv", c_paths, value = T)
# c_ids <- tstrsplit(c_files, split = "[_.]", keep = 6)
# 
# id_dt <- data.table(PlgID = as.integer(unlist(c_ids)))
# 
# 
# id_dt_sel <- merge(id_dt, sel_bokuID, by = "PlgID")
# 
# 
# aaa_id_dt <- merge(aaa_all, id_dt_sel, by.x = "cell", by.y = "BOKU_ID")
# 
# 
# 
# length(unique(aaa_id_dt$PlgID))









# cmcc_files <- list.files("data/acc/output/cmcc_output_example/detrended", full.names = T)
# fread(cmcc_files[500])






