# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script.
# The output is processed according to the ForestNav output template and saved.

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




# # ZIP OUTPUT
# save_paths <- unlist(lapply(output_obj_list, function(obj) obj[["save_path"]]))
# out_files <- list.files(save_paths)
# full_paths <- file.path(save_paths, out_files)
# 
# 
# zip_dt <- as.data.table(tstrsplit(out_files, split = "[_.]"))[, path := full_paths]
# zip_dt[, zip_id := .GRP, by = c("V3", "V4")]
# zip_dt[, name := paste0(V3, "_", V4, ".zip")]
# zip_path <- "data/acc/output/test_sites/zip"
# zip_dt[, full_zip_path := file.path(zip_path, name)]
# 
# zip_dts <- split(zip_dt, by = c("zip_id"))
# 
# 
# zip(zip_dt$full_zip_path[1], files = zip_dt$path)







