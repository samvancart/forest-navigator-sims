


# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# CHOOSE CLIMATE SCENARIO -------------------------------------------------


clim_names <- c("detrended", "gwl2", "gwl3", "gwl4")

clim_name <- clim_names[args$array_id]



# GET ALL KEYS ------------------------------------------------------------


data_prefixes <- paste0("input/simulation_sites_1km/", clim_names, "/")

# Get keys
data_keys_dt <- rbindlist(lapply(data_prefixes, function(data_prefix) {
  setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")
}))

obj <- data_keys_dt$Key[1]
x <- s3read_using(FUN = fread, object = obj, bucket = bucket, opts = list(region = region))



