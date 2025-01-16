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
clim_paths <- list.files(dest_path, full.names = T)
# paths_dt <- as.data.table(tstrsplit(clim_paths, split = "[/_]", keep = 9))[, id := .GRP, by = "V1"][, path := clim_paths][, c("id", "path")]
# split(paths_dt, by = "id")

# Get list of acc objects
init_clim_obj_list <- do.call(get_in_parallel, list(data = clim_paths,
                                                    FUN = get_acc_init_clim_object,
                                                    FUN_args = list(aaa_file = aaa_file, 
                                                                    operations = operations,
                                                                    clean_data_base_path = clean_data_base_path),
                                                    cores = cores,
                                                    type = type))

# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = init_clim_obj_list,
                              FUN = create_dir_and_save_acc_obj,
                              FUN_args = list(base_path = clean_data_base_path, 
                                              test = F),
                              cores = cores,
                              type = type))



# init_clim_obj_list[[1]]$data[[1]][1:70,]



# clim <- fread(clim_paths[4])
# 
# class(clim$time)
# 
# filtered <- filter_data_by_tree_data_cells(clim, aaa_file)
# filtered_op <- transform_and_add_columns(filtered, operations)
# 
# names(filtered)
# 
# fread(selection_path)
























