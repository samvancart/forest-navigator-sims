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



# ARRAY_JOB_PARAMS ----------------------------------------------------------

array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")

print(paste0("Array job: ", array_jobID))
print(paste0("Max array jobs: ", max_array_jobID))



# GET_CLIM_PATHS ------------------------------------------------------------

# TODO Check the use of this function
all_clim_paths <- get_filtered_clim_paths_from_bucket(grid_file_path, allas_opts)

fread(grid_file_path)

# SPLIT_IDS ----------------------------------------------------------------



# This can represent max number of array jobs
num_split_parts <- 31

# Define split by id (Default is array_jobID)
split_by_id <- array_jobID



# SPLIT_TABLE --------------------------------------------------------------

# Get all clim_paths as dt with PlgID and clim_scen cols
all_paths_run_dt <- get_acc_clim_paths_run_dt(all_clim_paths)
run_dt_max_part_size <- floor(nrow(all_paths_run_dt)/num_split_parts)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(all_paths_run_dt, run_dt_max_part_size, c("PlgID","clim_scen"))


print(paste0("Split by id: ", split_by_id))
cat("\n")


# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[split_by_id]]

plgid_vec <- as.integer(unique(run_dt$PlgID))
clim_paths <- run_dt$path

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

















































