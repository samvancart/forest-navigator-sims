# This script is for transferring zipped acc output files from Allas to a local
# directory prior to uploading to accelerator. The uploading itself should be 
# done in a terminal using accli, after which the local files should be deleted.


# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# LOAD KEYS ---------------------------------------------------------------


allas_zipfiles_prefix <- file.path("output", simulation_site, "zip")

data_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = allas_zipfiles_prefix, region = region)), "Key")


# MODIFY NAMES ------------------------------------------------------------


parts <- tstrsplit(data_keys_dt$Key, split = "/", keep = c(4:6))
parts[[2]] <- tstrsplit(parts[[2]], "_")[[1]]

data_keys_dt[, filename := do.call(paste, c(parts, sep = "-"))]

uploads_base_path <- "/scratch/project_2000994/PREBASruns/finRuns/Rsrc/samuel/forest-navigator-accli/data"

data_keys_dt[, file := file.path(uploads_base_path, filename)]


#  GET FILES TO UPLOAD ----------------------------------------------------


invisible(apply(data_keys_dt, 1, function(row) {
  obj <- row[["Key"]]
  file <- row[["file"]]
  print(paste0("Saving ", basename(obj), " as ", file, "..."))
  save_object(object = obj, bucket = bucket, file = file, region = region)
  print("Done")
}))

print("All done.")


# uploads_path <- file.path(output_base_path, "zip")
# 
# 
# invisible(lapply(upload_files, function(object) {
#   print(paste0("Saving ", basename(object), " into ", uploads_path, "..."))
#   save_or_put_allas(FUN = save_object,
#                     base_dir = uploads_path,
#                     object = object,
#                     bucket = allas_opts$bucket,
#                     region = allas_opts$opts$region)
#   print("Done")
# }))























