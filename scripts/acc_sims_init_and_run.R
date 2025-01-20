# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script.
# The output is processed according to the ForestNav output template and saved.
# Finally the saved output is zipped by group.

source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)




# GET RUN TABLE FROM acc_create_run_table
acc_run_tables_list <- split(acc_run_table, by = c("plgid"))


# Get output using run table
output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_tables_list,
                                                 FUN = produce_acc_output_obj_from_run_table,
                                                 FUN_args = list(),
                                                 cores = cores,
                                                 type = type)), recursive = F)




# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = output_obj_list,
                                               FUN = create_dir_and_save_acc_obj,
                                               FUN_args = list(base_path = output_base_path, 
                                                               test = F, ext = ".csv"),
                                               cores = cores,
                                               type = type))





# Zip output
zip_dts <- get_split_grouped_output_dt(output_base_path)
zip_output_list <-  do.call(get_in_parallel, list(data = zip_dts,
                                                  FUN = zip_output_files_from_dt,
                                                  FUN_args = list(original_wrkdir = getwd()),
                                                  cores = cores,
                                                  type = type))






































