# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script as well as a named list of the required paths.
# The output is processed according to the ForestNav output template and saved.




source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


acc_run_table_path <- run_table_sim200_noman_path

# GET RUN TABLE FROM acc_create_run_table
acc_run_table <- loadRDataFile(acc_run_table_path)


array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")


max_array_jobID <- 31
# array_jobID <- 1

run_dt_max_part_size <- floor(nrow(acc_run_table)/max_array_jobID)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(acc_run_table, run_dt_max_part_size, c("plgid","clim_scen"))

# Define split by id (Default is array_jobID)
split_by_id <- array_jobID


# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[split_by_id]]

acc_run_tables_list <- split(run_dt, by = c("plgid"))





#### TEST ##########

acc_run_test <- acc_run_tables_list[[1]]


acc_output_obj <- acc_run_table_controller(acc_run_test, produce_output_paths, produce_acc_output_obj)


acc_output_obj[[1]]$data


#### END TEST ##########



#### TEST PARALLEL ##########

acc_run_test_dts <- acc_run_tables_list[c(1,10,20)]

output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_test_dts,
                                                 FUN = acc_run_table_controller,
                                                 FUN_args = list(paths = produce_output_paths,
                                                                 FUN = produce_acc_output_obj),
                                                 cores = cores,
                                                 type = type)), recursive = F)



acc_output_obj[[1]]$data


#### END TEST PARALLEL ##########















# # Get output using run table
# output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_tables_list,
#                                                  FUN = produce_acc_output_obj_from_run_table,
#                                                  FUN_args = list(),
#                                                  cores = cores,
#                                                  type = type)), recursive = F)
# 
# 
# 
# 
# 
# 
# # Save to allas
# allas_output_path <- "output/simulation_sites_200/output_files"
# invisible(lapply(output_obj_list, function(item) {
#   dt <- item$data[[1]]
#   print(paste0("Saving ", item$name, " to ", allas_output_path, " in allas..."))
#   s3write_using(x = dt,
#                 FUN = fwrite,
#                 object = file.path(allas_output_path, paste0(item$name, ".csv")),
#                 bucket = allas_opts$bucket,
#                 opts = c(list(multipart = T), allas_opts$opts))
# }))









# # Save all acc objects
# save_obj_list <- do.call(get_in_parallel, list(data = output_obj_list,
#                                                FUN = create_dir_and_save_acc_obj,
#                                                FUN_args = list(base_path = output_base_path, 
#                                                                test = F, ext = ".csv"),
#                                                cores = cores,
#                                                type = type))





# Zip output
# output_folder <- file.path(output_base_path, "output_files")
# out_files <- list.files(output_folder, full.names = F)
# output_paths <- list.files(output_folder, full.names = T)







# zip_output_list <-  do.call(get_in_parallel, list(data = zip_dts,
#                                                   FUN = zip_output_files_from_dt,
#                                                   FUN_args = list(original_wrkdir = getwd()),
#                                                   cores = cores,
#                                                   type = type))






































