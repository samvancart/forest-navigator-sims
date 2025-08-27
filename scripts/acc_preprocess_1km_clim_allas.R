# This script is for combining clim data based on the PlgID. All the data.tables
# of a clim_scen that share the same PlgID are combined. The process is run as a batch job.
# The batch job params can be modified in the "GROUP KEYS" section. Results are saved
# into Allas.


# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# CHOOSE CLIMATE SCENARIO -------------------------------------------------


clim_names <- c("detrended", "gwl2", "gwl3", "gwl4")


# GET ALL KEYS ------------------------------------------------------------


data_prefixes <- paste0("input/simulation_sites_1km/", clim_names, "/")

# Get keys
data_keys_dt <- rbindlist(lapply(data_prefixes, function(data_prefix) {
  setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")
}))


# GROUP KEYS --------------------------------------------------------------


data_keys_dt[, PlgID_05 := tail(tstrsplit(basename(Key), split = "[_.]"), 2)[1], by = basename(Key)]
data_keys_dt[, PlgID_05 := as.integer64(PlgID_05)]
data_keys_dt[, clim_scen := tstrsplit(dirname(Key), split = "/", keep = 3)]

# Only keep IDs that are in AAA
data_keys_dt_filtered <- merge(aaa_all[, .(PlgID_05, PlgID)], data_keys_dt, by = "PlgID_05")
data_keys_dt_filtered[, group_id := .GRP , by = .(PlgID, clim_scen)]


array_job_count <- 31
max_part_size <- max(data_keys_dt_filtered$group_id) / array_job_count

# Split into array job groups
data_keys_splitID_dt <- split_dt_equal_with_constraint(data_keys_dt_filtered, max_part_size = max_part_size, split_by_constraint = "group_id")

# Get array job dt
data_keys_split_dt <- split(data_keys_splitID_dt, by = "splitID")[[args$array_id]]


# COMBINE CLIM BY PlgID_05 AND SAVE ---------------------------------------


ids <- unique(data_keys_split_dt$group_id)

combined_dt <- invisible(lapply(ids, function(i) {

# DEFINE VARS -------------------------------------------------------------
  
  dt <- data_keys_split_dt[group_id == i]
  plgid <- unique(dt$PlgID)
  clim_scen <- unique(dt$clim_scen)
  print(paste0("Processing plgid ", plgid, " for clim_scen ", clim_scen, "..."))
  keys <- dt$Key

# COMBINE -----------------------------------------------------------------
  
  combined_dt <- rbindlist(lapply(keys, function(key) {
    s3read_using(FUN = fread, object = key, bucket = bucket, opts = list(region = region))
  }))
  combined_dt[, PlgID := plgid]
  
# SAVE TO ALLAS -----------------------------------------------------------

  r_out_name <- str_flatten(c(clim_scen, "id", "prebas", "plgid", plgid, "csv"), collapse = "_", last = ".")
  
  obj <- file.path("input", "simulation_sites_200", "1km", clim_scen, r_out_name)
  
  print(paste0("Saving ", obj , " into Allas..."))
  
  s3write_using(x = combined_dt,
                FUN = fwrite,
                object = obj,
                bucket = bucket,
                opts = c(list(multipart = T, region = region)))
  
  print("Done.")
}))


cat("\n")
print("All done.")















