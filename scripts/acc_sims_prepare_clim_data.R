# This script is for processing acc climate data. First each climate data
# file is read and processed by filtering out those 1km-by-1km ids for which there
# are no corresponding forests in the BOKU tree data (AAA file). Then the matrices
# used for initialising PREBAS are created (1km-by-1km sites as rows, days as columns).
# After this the matrices are saved according to the name, plgid and save_path that are items
# in each returned acc_obj.
# Be aware of memory limitations when processing for simulation sites!



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# Clim files to process
# clim_paths <- list.files(dest_path, full.names = T)


all_clim_paths <- get_filtered_clim_paths_from_bucket(grid_file_path, allas_opts)


max_array_jobID <- 8
array_jobID <- 1

# Get all clim_paths as dt with PlgID and clim_scen cols
all_paths_run_dt <- get_acc_clim_paths_run_dt(all_clim_paths)
run_dt_max_part_size <- floor(nrow(all_paths_run_dt)/max_array_jobID)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(all_paths_run_dt, run_dt_max_part_size, c("PlgID","clim_scen"))

# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[array_jobID]][1:2]

plgid_vec <- as.integer(unique(run_dt$PlgID))
clim_paths <- run_dt$path





t <- system.time(
clim_acc_init_obj_list <- run_acc_with_combine_args(FUN = create_acc_clim_data,
                                                    acc_input_obj = clim_data_acc_input_obj,
                                                    plgid_vec = plgid_vec,
                                                    aaa_file = aaa_file,
                                                    clean_data_base_path = clean_data_base_path,
                                                    get_in_parallel_args = general_get_in_parallel_args,
                                                    config = config,
                                                    allas_opts = allas_opts,
                                                    clim_paths = clim_paths)
)
print(t)


format(clim_acc_init_obj_list[[1]]$data$parTran[1,1], scientific = F)




# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = clim_acc_init_obj_list,
                              FUN = create_dir_and_save_acc_obj,
                              FUN_args = list(base_path = clean_data_base_path, 
                                              test = F, ext = ".rds"),
                              cores = cores,
                              type = type))















































