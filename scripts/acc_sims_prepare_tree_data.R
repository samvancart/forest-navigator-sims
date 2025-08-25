# This script is for preparing BOKU tree data first into 1ha-by-1ha forested areas
# by sampling and then clustering the trees in each 1ha-by-1ha area. Each cluster
# is saved as "clusters_<10km-by-10km_cell_id>.rdata.


# SOURCE_FILES -------------------------------------------------------------



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)




# GET_PATHS ----------------------------------------------------------------


# Get PlgIDs to run from clim_paths

# TODO Check the use of this function
all_clim_paths <- get_filtered_clim_paths_from_bucket(grid_file_path, allas_opts)

# TODO Check the use of this function
all_paths_run_dt <- get_acc_clim_paths_run_dt(all_clim_paths)
plgid_vec <- as.integer(unique(all_paths_run_dt$PlgID))


# RUN ---------------------------------------------------------------------





clustered_acc_init_obj <- run_acc_with_combine_args(FUN = create_acc_clustered_tree_data,
                                                    acc_input_obj = tree_data_acc_input_obj,
                                                    plgid_vec = plgid_vec,
                                                    aaa_file = aaa_file,
                                                    clean_data_base_path = clean_data_base_path,
                                                    get_in_parallel_args = general_get_in_parallel_args)




# MODIFY_ACC-OBJ_NAME --------------------------------------------------------



# Modify name
clustered_acc_init_obj_name <- lapply(seq(clustered_acc_init_obj), function(i) {
  name <- clustered_acc_init_obj[[i]]$name
  plgid <- clustered_acc_init_obj[[i]]$plgid
  clustered_acc_init_obj[[i]]$name <- paste0(name, "_plgid_", plgid)
  clustered_acc_init_obj[[i]]

})



# SAVE --------------------------------------------------------------------



# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = clustered_acc_init_obj_name,
                                               FUN = create_dir_and_save_acc_obj,
                                               FUN_args = list(base_path = clean_data_base_path, 
                                                               test = F, ext = ".rds"),
                                               cores = cores,
                                               type = type))
























