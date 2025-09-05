

# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')

source(config$PATH_acc_sims_prepare_init_settings)


# GET KEYS ----------------------------------------------------------------


data_prefix <- file.path("input", "simulation_sites_200", "1km/")

# Get keys
data_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")

data_keys_dt[, clim_scen := tstrsplit(dirname(Key), split = "/", keep = 4)]


# PROCESS -----------------------------------------------------------------


res_dt <- rbindlist(apply(data_keys_dt, 1, function(row) {
  clim_scen <- row[["clim_scen"]]
  obj <- row[["Key"]]
  print(paste0("Processing Key ", obj))
  in_d <- s3read_using(FUN = fread, object = obj, bucket = bucket, opts = list(region = region))
  res <- in_d[, length(tas), by= PlgID_05]
  res[, clim_scen := clim_scen]
}))

print("All done.")


# SAVE --------------------------------------------------------------------


print("Saving...")
fwrite(res_dt, "n_rows-PlgID_05.csv")
print("Done")










