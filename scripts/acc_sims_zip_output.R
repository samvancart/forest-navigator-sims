# This script is for zipping files that are stored in Allas. 
# The files belonging to a unique combination of climate scenario and management scenario
# are grouped together. Each group can be processed as a separate array job. 
# Currently paralellisation is not supported for the compression phase but only
# for loading the files to zip from ALlas.



# SOURCE_FILES -------------------------------------------------------------



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# CREATE_TEST_ZIP_PATH -------------------------------------------------------------



zip_file_path <- paste0("data/acc/output/", simulation_site, "/zip_test")
if(!dir.exists(zip_file_path)) dir.create(zip_file_path)



# ARRAY_JOB-PARAMS ----------------------------------------------------------


array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")

print(paste0("Array job: ", array_jobID))
print(paste0("Max array jobs: ", max_array_jobID))

# GET_FILES_TO_ZIP -----------------------------------------------------------


bucket_list_prefix <- file.path("output", simulation_site, "output_files")


# FIN_RUNS_PREFIX ---------------------------------------------------------



bucket_list_prefix <- file.path("output", simulation_site, "output_files_FIN")



# END_FIN-RUNS_PREFIX -----------------------------------------------------




# List all output files that are in Allas using custom function
bucket_list <- list_all_objects_in_bucket(bucket = allas_opts$bucket, 
                                region = allas_opts$opts$region, 
                                prefix = bucket_list_prefix,
                                only_keys = T)



filtered_bucket_list <- grep(man_name, bucket_list, value = T)


# CREATE_SPLIT_RUN-TABLES ------------------------------------------------------------



# Split run tables into groups by combination of clim_scen and man_scen
zip_dts <- get_split_grouped_output_dt(filtered_bucket_list, zip_folder_name =  "zip_test")


# Select one table from zip_dts
zip_dt <- zip_dts[[array_jobID]]

## For testing
# zip_dt <- head(zip_dts[[array_jobID]], n = 50)



# SET_ZIP-PARAMS ------------------------------------------------------------



# When writing to allas zipfile should be the basename of full_zip_path
zipfile <- basename(zip_dt$full_zip_path[1])

# aws.S3 function args for save_object and put_object
save_or_put_opts <- list(bucket = allas_opts$bucket, region = allas_opts$opts$region, multipart = TRUE)

# move_to_path is the s3 object key without the filename (In other words the "directory path" inside the S3 bucket)
move_to_path <- file.path("output", simulation_site, "zip")




# ZIP-PATH_FIN-RUNS -------------------------------------------------------



move_to_path <- file.path("output", simulation_site, "zip_FIN")



# END_ZIP-PATH_FIN-RUNS ---------------------------------------------------






# RUN ---------------------------------------------------------------------

# Load files to temp_dir, zip and then write
load_zip_move(zipfile = zipfile, 
              files = zip_dt$path,
              zip_opts = list(FUN = utils::zip,
                              extra_FUN_args = list(flags = "-u")), # Faster with -u (update) flag
              move_to_path = move_to_path,
              save_or_put_opts = save_or_put_opts,
              cores = cores,
              type = type)

















