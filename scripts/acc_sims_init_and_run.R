# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script as well as a named list of the required paths.
# The output is processed according to the ForestNav output template and saved.
# Manually determine number (num_split_parts) of data.tables to split into for array job processing.



# sourceFiles -------------------------------------------------------------



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# getRunTable -------------------------------------------------------------

print(paste0("Getting run_table from ", run_table_full_path))
acc_run_table <- loadRDataFile(run_table_full_path)




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



# plotTest ----------------------------------------------------------------


# Define test parameters
plgid_test <- 8003115
clim_scen_test <- "detrended"

# Generate a base data table dynamically based on the number of scenarios
scenarios <- c("bau_ingrowthT", "bau_ingrowthF", "noman_ingrowthT", "noman_ingrowthF")
num_scenarios <- length(scenarios)
acc_run_table_def <- acc_run_table[plgid == plgid_test & clim_scen == clim_scen_test][rep(1, num_scenarios), ]

# Assign management scenarios
acc_run_table_def[, man_scen := scenarios]

# Loop through each row to modify `man_init_args`
for (i in seq_len(nrow(acc_run_table_def))) {
  # Extract the row as a list
  current_row <- acc_run_table_def[i]

  # Check if `man_init_args` exists, otherwise throw an error
  if (is.null(current_row$man_init_args[[1]])) {
    stop("man_init_args is missing in row ", i)
  }

  # Extract the nested list for modification
  current_args <- current_row$man_init_args[[1]]

  # Apply modifications based on the scenario
  scenario <- current_row$man_scen
  if (scenario == "bau_ingrowthF") {
    current_args$ingrowth <- FALSE
  } else if (scenario == "noman_ingrowthT") {
    current_args$defaultThin <- 0
    current_args$ClCut <- 0
  } else if (scenario == "noman_ingrowthF") {
    current_args$ingrowth <- FALSE
    current_args$defaultThin <- 0
    current_args$ClCut <- 0
  }

  # Reassign the modified list back into the appropriate column and row
  acc_run_table_def[i, man_init_args := list(list(current_args))]
}

# Split the data table by 'man_scen' if processing subsets separately is required
acc_run_tables_list <- split(acc_run_table_def, by = "man_scen")


invisible(lapply(acc_run_tables_list, function(dt) {
  print(paste0("man_scen:"))
  print(dt[["man_scen"]])
  cat("\n")
  print(dt[["man_init_args"]][[1]])
}))


# run ---------------------------------------------------------------------




output_obj_list <- unlist(do.call(get_in_parallel, list(data = acc_run_tables_list,
                                                        FUN = acc_run_table_controller,
                                                        FUN_args = list(paths = produce_output_paths,
                                                                        FUN = produce_acc_output_obj,
                                                                        test_run = T),
                                                        cores = cores,
                                                        type = type)), recursive = F)





output_obj_list[[2]]$name

# saveToAllas -------------------------------------------------------------




# Save to allas
allas_output_path <- "output/simulation_sites_200/output_files"
invisible(lapply(output_obj_list, function(item) {
  dt <- item$data[[1]]
  print(paste0("Saving ", item$name, " to ", allas_output_path, " in allas..."))
  s3write_using(x = dt,
                FUN = fwrite,
                object = file.path(allas_output_path, paste0(item$name, ".csv")),
                bucket = allas_opts$bucket,
                opts = c(list(multipart = T), allas_opts$opts))
}))






# saveToFilesystem --------------------------------------------------------

invisible(lapply(output_obj_list, function(obj) {
  create_dir_and_save_acc_obj(obj, output_base_path, test = F, ext = ".rds")
})
)

test_file <- file.path(output_obj_list[[1]]$save_path, paste0(output_obj_list[[1]]$name, ".rds"))
test_list <- readRDS(test_file)



#### TEST ##########

# acc_run_test <- acc_run_tables_list[[1]]
# 
# 
# acc_output_obj <- acc_run_table_controller(acc_run_test, produce_output_paths, produce_acc_output_obj)
# 
# 
# acc_output_obj[[1]]$data


#### TEST PARALLEL ##########

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



















































