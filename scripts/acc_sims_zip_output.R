# This script is for zipping files from allas


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


zip_file_path <- file.path(output_base_path, "zip_test")
if(!dir.exists(zip_file_path)) dir.create(zip_file_path)

bucket_list <- as.data.table(get_bucket_df(bucket = allas_opts$bucket, 
                                           region = allas_opts$opts$region, 
                                           max = Inf, 
                                           prefix = "output"))$Key


zip_dts <- get_split_grouped_output_dt(bucket_list, zip_folder_name =  "zip_test")


# mclapply(zip_dts, function(zip_dt) {
#   zip_s3_output_file_from_dt(zip_dt, allas_opts, mc.cores = 1)
# }, mc.cores = 1)




zip_dt <- head(zip_dts[[1]], n = 4)

allas_zip_path <- zip_dt$path
zip_file_name <- zip_dt$name[1]



# Modify to accept save_object or put_object FUN
load_file_from_allas <- function(object, base_dir, allas_opts, ...) {
  file <- file.path(base_dir, basename(object))
  save_object(object =  object,
              bucket = allas_opts$bucket,
              file = file,
              region = allas_opts$opts$region)
  assert_file_exists(file)
  return(file)
}




### Load files from allas, zip them and finally move the files to a location (Allas or filesystem).
#
# When move_to_path is provided and is a path the zipped file will be written into the Allas bucket provided in allas_opts
# using move_to_path as the object key. If zipfile is an absolute path to a existing directory and move_to_path
# is a path then the file will be written to both zipfile and Allas.
#
# Note: When zipfile is not an absolute path, it is viewed as a relative path to the temporary directory path that is 
# created by the function. If you want the zipfile to be written directly into a directory then you must
# specify the absolute path to the directory.
#
load_zip_move <- function(zipfile, 
                          files, 
                          zip_opts = list(),
                          move_to_path = NULL,
                          allas_opts = list(),
                          ...) {

  temp_dir <- tempdir()
  print(temp_dir)
  on.exit(
    if(dir.exists(temp_dir)) unlink(temp_dir, recursive = T)
  )
  
  
  
  # Get files from allas. Specify cores and type to run in parallel.
  files <- unlist(do.call(get_in_parallel, c(list(data = files,
                                                      FUN = load_file_from_allas,
                                                      FUN_args = list(base_dir = temp_dir,
                                                                      allas_opts = allas_opts)),
                                                 ...)))

  
    
  print(files)
  cat("\n")
  print(list.files(temp_dir))
  
  
  assert_directory_exists(dirname(zipfile))
  
  print(paste0("Zipping to ", zipfile))

  # Zip the files
  zip_args <- c(list(zipfile = zipfile, files = files), zip_opts)
  t <- system.time(
    do.call(zip_output_files_using, zip_args)
  )

  print(t)
  
  cat("\n")
  cat("\n")
  print(list.files(temp_dir, full.names = T))

}




zipfile <- basename(zip_dt$full_zip_path[1])
zipfile <- file.path(getwd(), zip_dt$full_zip_path[1])



load_zip_move(zipfile = zipfile, 
                        files = zip_dt$path,
                        zip_opts = list(FUN = utils::zip,
                                        extra_FUN_args = list(flags = "-u")),
                        move_to_path = NULL,
                        allas_opts = allas_opts,
                        cores = 3,
                        type = type)



temp_dir <- tempdir()





# FULL ZIP PATH
full_zip_path <- file.path(getwd(), output_base_path, "zip_test", zip_file_name)


file_paths <- unlist(lapply(allas_zip_path, function(file_path) {
  file <- basename(file_path)
  temp_path <- file.path(temp_dir, file)
  save_object(object =  file_path, bucket = allas_opts$bucket, file = temp_path, region = allas_opts$opts$region)
  temp_path
}))


list.files(temp_dir)

file_paths_1 <- file_paths[1:3]
file_paths_2 <- file_paths[4:6]

all_file_paths <- list(file_paths_1, file_paths_2)
  
zip_output_files_using(utils::zip, zipfile = full_zip_path, files = c(file_paths_1, file_paths_2), extra_FUN_args = list(flags = "-u"))

new_zips <- unlist(mclapply(seq(all_file_paths), function(i) {
  zip_output_files_using(utils::zip, zipfile = full_zip_path, files = all_file_paths[[i]], extra_FUN_args = list(flags = "-u"))
}, mc.cores = 1))


system(paste("unzip -l", full_zip_path))

unlink(temp_dir, recursive = T)











