# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script as well as a named list of the required paths.
# The output is processed according to the ForestNav output template and saved.
# Manually determine number (num_split_parts) of data.tables to split into for array job processing.

# sourceFiles -------------------------------------------------------------



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# getRunTable -------------------------------------------------------------



acc_run_table_path <- run_table_sim200_noman_path
acc_run_table <- loadRDataFile(acc_run_table_path)



# arrayJobParams ----------------------------------------------------------



array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")

print(paste0("Array job: ", array_jobID))
print(paste0("Max array jobs: ", max_array_jobID))




# splitTable --------------------------------------------------------------


# This can represent max number of array jobs
num_split_parts <- 31

# Define split by id (Default is array_jobID)
split_by_id <- array_jobID


run_dt_max_part_size <- floor(nrow(acc_run_table)/num_split_parts)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(acc_run_table, run_dt_max_part_size, c("plgid","clim_scen"))




# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[split_by_id]]

acc_run_tables_list <- split(run_dt, by = c("plgid"))




# run ---------------------------------------------------------------------




output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_tables_list,
                                                        FUN = acc_run_table_controller,
                                                        FUN_args = list(paths = produce_output_paths,
                                                                        FUN = produce_acc_output_obj),
                                                        cores = cores,
                                                        type = type)), recursive = F)





# saveToAllas -------------------------------------------------------------




# Save to allas
allas_output_path <- "output/simulation_sites_200/output_files"
invisible(mclapply(output_obj_list, function(item) {
  dt <- item$data[[1]]
  print(paste0("Saving ", item$name, " to ", allas_output_path, " in allas..."))
  s3write_using(x = dt,
                FUN = fwrite,
                object = file.path(allas_output_path, paste0(item$name, ".csv")),
                bucket = allas_opts$bucket,
                opts = c(list(multipart = T), allas_opts$opts))
}, mc.cores = cores))














#### TEST ##########

# acc_run_test <- acc_run_tables_list[[1]]
# 
# 
# acc_output_obj <- acc_run_table_controller(acc_run_test, produce_output_paths, produce_acc_output_obj)
# 
# 
# acc_output_obj[[1]]$data


#### END TEST ##########



#### TEST PARALLEL ##########

# acc_run_test_dts <- acc_run_tables_list[c(1,10,20)]
# 
# output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_test_dts,
#                                                  FUN = acc_run_table_controller,
#                                                  FUN_args = list(paths = produce_output_paths,
#                                                                  FUN = produce_acc_output_obj),
#                                                  cores = cores,
#                                                  type = type)), recursive = F)
# 
# 
# 
# acc_output_obj[[1]]$data


#### END TEST PARALLEL ##########
















































