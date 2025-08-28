# This script is for processing acc climate data. First each climate data
# file is read and processed by filtering out those 1km-by-1km ids for which there
# are no corresponding forests in the BOKU tree data (AAA file). Then the matrices
# used for initialising PREBAS are created (1km-by-1km sites as rows, days as columns).
# After this the matrices are saved according to the name, plgid and save_path that are items
# in each returned acc_obj.
# Be aware of memory limitations when processing for simulation sites!



# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# DEFINE CLIMATE SCENARIOS ------------------------------------------------


clim_names <- c("detrended", "gwl2", "gwl3", "gwl4")


# GET ALL KEYS ------------------------------------------------------------


data_prefixes <- paste0("input/", simulation_site, "/1km/", clim_names, "/")

# Get keys
data_keys_dt <- rbindlist(lapply(data_prefixes, function(data_prefix) {
  setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")
}))


# CREATE PLGID AND CLIM_SCEN  COLS ----------------------------------------


data_keys_dt[, PlgID := as.integer(unlist(tstrsplit(basename(Key), split = "[_.]", keep = 5)))]
data_keys_dt[, clim_scen := tstrsplit(basename(Key), split = "[_.]", keep = 1)]


# SPLIT_IDS ----------------------------------------------------------------


# This can represent max number of array jobs
num_split_parts <- 31

# Define split by id (Default is array_jobID)
split_by_id <- args$array_id


# SPLIT_TABLE --------------------------------------------------------------


run_dt_max_part_size <- floor(nrow(data_keys_dt)/num_split_parts)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(data_keys_dt, run_dt_max_part_size, c("PlgID","clim_scen"))

print(paste0("Split by id: ", split_by_id))
cat("\n")

# Filter by split_by_id
run_dt <- split(run_dt_splitID, by = "splitID")[[split_by_id]]

clim_paths <- run_dt$Key

print("clim_paths")
print(clim_paths)


# RUN ---------------------------------------------------------------------


# Get list of acc objects
clim_acc_init_obj_list <- do.call(get_in_parallel, list(data = clim_paths,
                                                    FUN = get_acc_init_clim_object,
                                                    FUN_args = list(aaa_file = aaa_file, 
                                                                    operations = clim_operations,
                                                                    clean_data_base_path = clean_data_base_path,
                                                                    config = config,
                                                                    allas_opts = list(bucket = bucket_name,
                                                                                      FUN = fread,
                                                                                      opts = list(region = region))),
                                                    cores = cores,
                                                    type = type))


# SAVE --------------------------------------------------------------------


# Save all acc objects
invisible(lapply(clim_acc_init_obj_list, function(obj) {
  create_dir_and_save_acc_obj(obj, base_path = clean_data_base_path, test = F, ext = ".rds")
}))




























