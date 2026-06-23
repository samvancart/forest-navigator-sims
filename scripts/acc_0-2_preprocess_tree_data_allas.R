# This script is for pre-processing the BOKU AAA and init forest type files.
# The selection file (IDs lookup) is also modified by adding a BOKU_ID column to it.
# All data should first be downloaded into Allas (eg. by running acc_download_links_to_allas.R).
# The necessary init files are filtered and written into a separate folder in Allas.
# CHECK PATHS (aaa_prefix etc.) BEFORE RUNNING

# SOURCE_FILES ------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# LOAD_AAA_FROM_ALLAS -----------------------------------------------------


aaa_prefix <- file.path("input/simulation_sites_1km/aaa-1km/")

# Get keys
data_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = aaa_prefix, region = region)), "Key")

aaa_1km <- s3read_using(FUN = fread, object = data_keys_dt$Key, bucket = bucket, opts = list(region = region))


# FILTER_AAA --------------------------------------------------------------


sel <- fread("data/acc/input/simulation_sites_200/raw/grid/prebas_selected_reduced_1km.csv")[, c("PlgID", "PlgID_05", "XLON", "YLAT")]

bokuIDs <- fread("data/acc/docs/boku_id_map_1km.csv")

sel_bokuID <- merge(sel, bokuIDs[, c("PlgID_05", "BOKU_ID")], by = "PlgID_05")

aaa_filtered <- merge(aaa_1km, sel_bokuID, by.x = "cell", by.y = "BOKU_ID")

# Save the file if necessary
# fwrite(aaa_filtered, "data/acc/input/simulation_sites_200/raw/tree_data/aaa/AAA_cell1kmForestTypeList_filtered_1.csv")

length(unique(aaa_filtered$PlgID))

aaa_filtered[!complete.cases(aaa_filtered)] # Hopefully empty


# FILTER_SELECTION_FILE ---------------------------------------------------


filtered_sel <- sel_bokuID[BOKU_ID %in% aaa_filtered$cell]
filtered_sel10 <- merge(filtered_sel, aaa_filtered[, c("cell", "cell_300arcsec")], by.x = "BOKU_ID", by.y = "cell")


# LOAD_INIT_FILES_FROM_ALLAS ----------------------------------------------


init_files_prefix <- file.path("input/simulation_sites_1km/initFiles-1km/")

# Get keys
init_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = init_files_prefix, region = region)), "Key")


init_files_filtered <- unique(aaa_filtered$InitFileID)
init_filenames <- paste0(init_files_prefix, init_files_filtered, "_01.csv")

# Filter files
all_init_files <- init_keys_dt$Key
del_init_files <- all_init_files[!basename(all_init_files) %in% basename(init_filenames)]

# If you want to delete (slow) then do so in a batch job.
# delete_object(object = del_init_files, bucket = bucket, region = region)


# SAVE TO ALLAS -----------------------------------------------------------


# Save the required files into a new path.

# TODO simsites as var
allas_basepath <- file.path("input", "simulation_sites_1km", "initFiles-1km-filtered")

invisible(lapply(init_filenames, function(file) {
  allas_path <- file.path(allas_basepath, basename(file))
  print(paste0("Reading ", file, "..."))
  x <- s3read_using(FUN = fread, object = file, bucket = bucket, opts = list(region = region))
  print(paste0("Saving ", allas_path, " to Allas..."))
  s3write_using(x = x, FUN = fwrite, object = allas_path, bucket = bucket, opts = list(region = region))
  print("Done.")
}))

cat("\n")
print("All done.")





















