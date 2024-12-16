# This script is for processing acc climate data. First each climate data
# file is read and processed by filtering out those 1km-by-1km ids for which there
# are no corresponding forests in the BOKU tree data (AAA file). Then the matrices
# used for initialising PREBAS are created (1km-by-1km sites as rows, days as columns).
# After this the matrices are saved according to the name and plgid that are items
# in each returned clim_data_obj.
# Be aware of memory limitations when processing for simulation sites!



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)

# Clim files to process
clim_paths <- list.files(dest_path, full.names = T)


# Get list of climate objects
init_clim_obj_list <- do.call(get_in_parallel, list(data = clim_paths,
                                                    FUN = get_acc_init_clim_object,
                                                    FUN_args = list(aaa_file = aaa_file, 
                                                                    operations = operations),
                                                    cores = cores,
                                                    type = type))

# Save all climate objects
save_obj_list <- do.call(get_in_parallel, list(data = init_clim_obj_list,
                              FUN = save_acc_init_clim_obj,
                              FUN_args = list(base_path = climate_data_base_path, 
                                              test = F),
                              cores = cores,
                              type = type))
















