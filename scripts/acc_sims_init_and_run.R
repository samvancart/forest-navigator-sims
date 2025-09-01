# This script is for initialising and running PREBAS using the regionPrebas function.
# A run_table is required for running this script as well as a named list of the required paths.
# The run_table should be created in the acc_create_run_table.R script. The paths and other
# parameters are set in acc_sims_prepare_init_settings.R.
# The output is processed according to the ForestNav output template and saved.
# Manually determine number (num_split_parts) of data.tables to split into for array job processing.


# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')

source(config$PATH_acc_sims_prepare_init_settings)


# PARSE_ARGS --------------------------------------------------------------


init_option_list <- list(
  make_option(c("-c", "--countries"), type = "character", default = NA,
              help = "Country names or abbreviations (e.g., 'FI' or 'Finland' or multiple e.g., 'se,FI' or 'Sweden, finland')")
)

parser <- OptionParser(option_list = init_option_list)
init_args <- parse_args(parser)


countries_arg <- init_args$countries
countries <- if (is.na(countries_arg)) NA else strsplit(countries_arg, ",")[[1]]
countries <- trimws(countries)  # Remove spaces around items


# GET_RUN-TABLE -------------------------------------------------------------


print(paste0("Getting run_table from ", run_table_full_path))
acc_run_table_all <- loadRDataFile(run_table_full_path)


# FILTER_BY_COUNTRY -------------------------------------------------------


acc_run_table <- filter_and_validate_by_country(dt = acc_run_table_all, lookup = country_codes, countries = countries)

print("Countries to run:")
print(unique(acc_run_table$country))


# SPLIT_TABLE --------------------------------------------------------------


# This can represent max number of array jobs. Determined in runTable_vars in settings
num_split_parts <- runTable_split_parts

# Define split by id (Default is args$array_id)
split_by_id <- args$array_id

run_dt_max_part_size <- floor(nrow(acc_run_table)/num_split_parts)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(acc_run_table, run_dt_max_part_size, c("plgid","clim_scen"))

# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[split_by_id]]

acc_run_tables_list <- split(run_dt, by = c("plgid"))


# RUN ---------------------------------------------------------------------


output_obj_list <- unlist(unlist(do.call(get_in_parallel, list(data = acc_run_tables_list,
                                                        FUN = acc_run_table_controller,
                                                        FUN_args = list(paths = produce_output_paths,
                                                                        FUN = produce_acc_output_obj,
                                                                        start_year = start_year,
                                                                        test_run = F),
                                                        cores = cores,
                                                        type = type)), recursive = F),
                          recursive = FALSE) # Unlist twice with recursive=F to unlist 2 levels


# SAVE_TO_ALLAS -------------------------------------------------------------


invisible(lapply(output_obj_list, function(item) {
  allas_output_path <- item$save_path
  dt <- item$data[[1]]
  obj = file.path(allas_output_path, paste0(item$name, ".csv"))
  print(paste0("Saving ", item$name, " to ", allas_output_path, " in allas..."))
  s3write_using(x = dt,
                FUN = fwrite,
                object = obj,
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

# acc_run_test <- acc_run_tables_list[[100]][1,]
# 
# 
# acc_output_obj <- acc_run_table_controller(acc_run_test, produce_output_paths, produce_acc_output_obj, start_year = start_year)
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



































































