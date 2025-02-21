# This script is for transferring zipped acc output files from Allas to a local
# directory prior to uploading to accelerator. The uploading itself should be 
# done in a terminal using accli, after which the local files should be deleted.



# sourceFiles -------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# defineFilesToUpload ----------------------------------------------------------------



allas_zipfiles_prefix <- file.path("output", simulation_site, "zip")

# List all zipped output files that are in Allas
bucket_list <- as.data.table(get_bucket_df(bucket = allas_opts$bucket, 
                                           region = allas_opts$opts$region, 
                                           max = Inf, 
                                           prefix = allas_zipfiles_prefix))$Key

# Select files to upload from bucket_list
upload_files <- bucket_list



# getFilesToUpload --------------------------------------------------------


uploads_path <- file.path(output_base_path, "zip")


invisible(lapply(upload_files, function(object) {
  print(paste0("Saving ", basename(object), " into ", uploads_path, "..."))
  save_or_put_allas(FUN = save_object,
                    base_dir = uploads_path,
                    object = object,
                    bucket = allas_opts$bucket,
                    region = allas_opts$opts$region)
  print("Done")
}))























