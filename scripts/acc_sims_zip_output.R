# This script is for zipping files from Allas. Each zip file is
# created from all files belonging to a combination of clim_scen and man_scen.
# Each unique combination can be a separate array job. Currently paralellisation
# is not supported for the compression phase but only for loading the files to 
# zip from ALlas. 


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


zip_file_path <- file.path(output_base_path, "zip_test")
if(!dir.exists(zip_file_path)) dir.create(zip_file_path)

# List all output files that are in Allas
bucket_list <- as.data.table(get_bucket_df(bucket = allas_opts$bucket, 
                                           region = allas_opts$opts$region, 
                                           max = Inf, 
                                           prefix = "output"))$Key


# Create run tables
zip_dts <- get_split_grouped_output_dt(bucket_list, zip_folder_name =  "zip_test")


array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")


# Get output file to zip
zip_dt <- zip_dts[[array_jobID]]

# zip_dt <- head(zip_dts[[array_jobID]], n = 50)


# Determine zipfile
zipfile <- basename(zip_dt$full_zip_path[1])

# aws.S3 function args
save_or_put_opts <- list(bucket = allas_opts$bucket, region = allas_opts$opts$region, multipart = TRUE)

# Allas object key
move_to_path <- file.path("output", simulation_site, "zip")

# Load files to temp_dir, zip and then write
load_zip_move(zipfile = zipfile, 
                        files = zip_dt$path,
                        zip_opts = list(FUN = utils::zip,
                                        extra_FUN_args = list(flags = "-u")),
                        move_to_path = move_to_path,
                        save_or_put_opts = save_or_put_opts,
                        cores = cores,
                        type = type)
















