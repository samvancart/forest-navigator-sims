# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script as well as a named list of the required paths.
# The output is processed according to the ForestNav output template and saved.
# Manually determine number (num_split_parts) of data.tables to split into for array job processing.



# SOURCE_FILES -------------------------------------------------------------



source('scripts/settings.R')

source(config$PATH_acc_sims_prepare_init_settings)


# GET_RUN-TABLE -------------------------------------------------------------

print(paste0("Getting run_table from ", run_table_full_path))
acc_run_table <- loadRDataFile(run_table_full_path)


# FIN_RUNS ----------------------------------------------------------------


acc_run_table <- acc_run_table[country=="Finland"] # ONLY RUN FOR FINLAND



# END_FIN_RUNS ------------------------------------------------------------


# ARRAY-JOB_PARAMS ----------------------------------------------------------



array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")

print(paste0("Array job: ", array_jobID))
print(paste0("Max array jobs: ", max_array_jobID))


# SPLIT_TABLE --------------------------------------------------------------


# This can represent max number of array jobs. Determined in runTable_vars in settings
num_split_parts <- runTable_split_parts


# Define split by id (Default is array_jobID)
split_by_id <- array_jobID


run_dt_max_part_size <- floor(nrow(acc_run_table)/num_split_parts)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(acc_run_table, run_dt_max_part_size, c("plgid","clim_scen"))


# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[split_by_id]]

acc_run_tables_list <- split(run_dt, by = c("plgid"))



# RUN ---------------------------------------------------------------------




output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_tables_list,
                                                        FUN = acc_run_table_controller,
                                                        FUN_args = list(paths = produce_output_paths,
                                                                        FUN = produce_acc_output_obj,
                                                                        start_year = start_year,
                                                                        test_run = T),
                                                        cores = cores,
                                                        type = type)), recursive = F)




##### FRI 30.5.2025
# Work on apply_output_operations that is called from get_acc_output_dt. 
# Add d_classes. 
# Problem: print("dt before-1") shows that conversions table does not have 
# d_class units and previously merged d_classes are deleted from the table
# that has been merged with conversions_dt.
# output_obj_list2 should contain the result for runs using apply_output_operations fun.
# Possible solution1: Add d_classes to conversions_dt
# Possible solution1: Add Units "cm" to all rows of table first and see what happens


d_class_dt[, Units := "cm"]

d_class_dt <- melt.data.table(d_class_dt, id.vars = c("site", "year", "species"))
# Merge d_class_dt
dt <- merge(dt, d_class_dt, by = c("site", "year", "species"))


run_table <- acc_run_tables_list[[1]]
output_obj_list <- acc_run_table_controller(run_table = run_table, paths = produce_output_paths, FUN = produce_acc_output_obj, test_run = F)
output_obj_list2 <- acc_run_table_controller(run_table = run_table, paths = produce_output_paths, FUN = produce_acc_output_obj, start_year = start_year, test_run = F)



multi <- output_obj_list[[1]]$data[[1]]$multiOut
d_class_dt <- n_by_d_class_dt(prebas_out = multi, d_class = 5, is_multiOut = TRUE)


t1 <- output_obj_list[[1]]$data[[1]]
t2 <- output_obj_list2[[1]]$data[[1]]

all.equal(t1[[7]], t2[[7]])
all.equal(t1[[12]], t2[[12]])

which(t1[[7]] != t2[[7]])  # Rows where column 7 differs
which(t1[[12]] != t2[[12]])  # Rows where column 12 differs

t1[which(t1[[7]] != t2[[7]]),]
t2[which(t1[[7]] != t2[[7]]),]

lapply(names(t1), function(name) {
  identical(t1[[name]], t2[[name]]) 
})

unique(t2$Species)





# SAVE_TO_ALLAS -------------------------------------------------------------




# Save to allas
allas_output_path <- file.path("output", simulation_site, "output_files")


# ALLAS_FIN_RUNS ----------------------------------------------------------

allas_output_path <- file.path("output", simulation_site, "output_files_FIN") # FIN Runs


# END_ALLAS_FIN_RUNS ------------------------------------------------------


invisible(lapply(output_obj_list, function(item) {
  dt <- item$data[[1]]
  print(paste0("Saving ", item$name, " to ", allas_output_path, " in allas..."))
  s3write_using(x = dt,
                FUN = fwrite,
                object = file.path(allas_output_path, paste0(item$name, ".csv")),
                bucket = allas_opts$bucket,
                opts = c(list(multipart = T), allas_opts$opts))
}))






# SAVE_TO_FILE_SYSTEM --------------------------------------------------------

# invisible(lapply(output_obj_list, function(obj) {
#   create_dir_and_save_acc_obj(obj, output_base_path, test = F, ext = ".rds")
# })
# )
# 
# test_file <- file.path(output_obj_list[[1]]$save_path, paste0(output_obj_list[[1]]$name, ".rds"))
# test_list <- readRDS(test_file)



#### TEST ##########

# acc_run_test <- acc_run_tables_list[[1]]
# 
# 
# acc_output_obj <- acc_run_table_controller(acc_run_test, produce_output_paths, produce_acc_output_obj)
# 
# 
# acc_output_obj[[1]]$data


#### TEST_PARALLEL ##########

# acc_run_test_dts <- acc_run_tables_list[c(1,20)]
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
# output_obj_list[[1]]$data



































































