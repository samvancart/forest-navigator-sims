# This script is for processing all acc clustered data into multiInitVar objects
# for PREBAS. An acc object is created containing the multiInitVar object and saved.



# sourceFiles -------------------------------------------------------------

source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# getPaths ----------------------------------------------------------------



clustered_paths <- list.files(clean_data_base_path, pattern = "clustered",  recursive = T, full.names = T)




# run ---------------------------------------------------------------------



# Get acc objects
multiInitVar_obj_list <- do.call(get_in_parallel, list(data = clustered_paths,
                                      FUN = get_multiInitVar_object,
                                      FUN_args = create_multiInitVar_for_layers_args,
                                      cores = cores,
                                      type = type))



# save --------------------------------------------------------------------




# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = multiInitVar_obj_list,
                                               FUN = create_dir_and_save_acc_obj,
                                               FUN_args = list(base_path = clean_data_base_path, 
                                                               test = F, ext = ".rds"),
                                               cores = cores,
                                               type = type))























