# This script is for zipping files from allas


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


zip_file_path <- file.path(output_base_path, "zip")
if(!dir.exists(zip_file_path)) dir.create(zip_file_path)

bucket_list <- as.data.table(get_bucket_df(bucket = allas_opts$bucket, 
                                           region = allas_opts$opts$region, 
                                           max = Inf, 
                                           prefix = "output"))$Key


zip_dts <- get_split_grouped_output_dt(bucket_list)


mclapply(zip_dts, function(zip_dt) {
  zip_s3_output_file_from_dt(zip_dt, allas_opts, mc.cores = 1)
}, mc.cores = 1)






zip_s3_output_file <- function(full_zip_path, file_path, allas_opts, temp_dir = tempdir(), ...) {
  
  file <- basename(file_path)
  
  temp_path <- file.path(temp_dir, file)
  on.exit(unlink(temp_path))
  
  save_object(object =  file_path, bucket = allas_opts$bucket, file = temp_path, region = allas_opts$opts$region)
  
  zip_output_files(full_zip_path, temp_path, ...)
  
}



zip_s3_output_file_from_dt <- function(zip_file_name, allas_path_vec, allas_dest_dir = "output/zip", allas_opts, ...) {
  
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = T))
  
  
  to_zip_paths <- mclapply(allas_path_vec, function(file_path) {
          file <- basename(file_path)
          
          temp_path <- file.path(temp_dir, file)
          on.exit(unlink(temp_path))
          
          save_object(object =  file_path, bucket = allas_opts$bucket, file = temp_path, region = allas_opts$opts$region)
          
          temp_path
                    
  }, ...)
  
  
  
  
}















