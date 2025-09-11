# This script is for preparing BOKU tree data first into 1ha-by-1ha forested areas
# by sampling and then clustering the trees in each 1ha-by-1ha area. Each cluster
# is saved as "clusters_<10km-by-10km_cell_id>.rdata.


# SOURCE_FILES -------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# SET TEMPDIR FLAG --------------------------------------------------------

# When load_initFiles_to_temp=TRUE all required initFiles that are stored in Allas
# will be loaded into a temporary directory prior to processing.

load_initFiles_to_temp <- TRUE


# DEFINE CLIMATE SCENARIOS ------------------------------------------------


clim_names <- c("detrended", "gwl2", "gwl3", "gwl4")


# GET ALL KEYS ------------------------------------------------------------


data_prefixes <- paste0("input/", simulation_site, "/1km/", clim_names, "/")

# Get keys
data_keys_dt <- rbindlist(lapply(data_prefixes, function(data_prefix) {
  setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")
}))


# CREATE PLGID AND CLIM_SCEN COLS -----------------------------------------


data_keys_dt[, PlgID := as.integer(unlist(tstrsplit(basename(Key), split = "[_.]", keep = 5)))]
data_keys_dt[, clim_scen := tstrsplit(basename(Key), split = "[_.]", keep = 1)]

plgid_vec <- unique(data_keys_dt$PlgID)


# LOAD INIT FILES INTO TEMPDIR --------------------------------------------


init_files_prefix <- file.path("input/simulation_sites_1km/initFiles-1km-filtered/")

# Get keys
init_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = init_files_prefix, region = region)), "Key")

init_files_path_base <- process_treedata_files_args$init_files_path
init_files_path_temp <- file.path(tempdir(), init_files_path_base)
process_treedata_files_args$init_files_path <- init_files_path_temp # Make temp path the default

invisible(lapply(init_keys_dt$Key, function(key) {
  temp_path <- file.path(init_files_path_temp, basename(key))
  print(temp_path)
  save_object(object = key, bucket = bucket, file = temp_path, region = region)
}))


# CREATE ACC_OBJECT -------------------------------------------------------


acc_input_obj <- list(
  args = list(
    process_treedata_files_args = process_treedata_files_args,
    assign_and_merge_args = assign_and_merge_args,
    perform_clustering_by_group_args = perform_clustering_by_group_args,
    aaa_split_col = aaa_split_col,
    plgid_vec = plgid_vec,
    aaa_file = aaa_file,
    clean_data_base_path = clean_data_base_path,
    get_in_parallel_args = general_get_in_parallel_args
  )
)


# RUN ---------------------------------------------------------------------


clustered_acc_init_obj <- create_acc_clustered_tree_data(acc_input_obj = acc_input_obj)


# CLEAN UP TEMPDIR --------------------------------------------------------


rm_init_files_dir <- unlist(tstrsplit(init_files_path_base, split = "/", keep = 1))
rm_init_files_path <- file.path(tempdir(), rm_init_files_dir)

unlink(rm_init_files_path, recursive = TRUE)

list.files(tempdir())


# MODIFY_ACC-OBJ_NAME --------------------------------------------------------


# Modify name
clustered_acc_init_obj_name <- lapply(seq(clustered_acc_init_obj), function(i) {
  name <- clustered_acc_init_obj[[i]]$name
  plgid <- clustered_acc_init_obj[[i]]$plgid
  clustered_acc_init_obj[[i]]$name <- paste0(name, "_plgid_", plgid)
  clustered_acc_init_obj[[i]]

})


# SAVE --------------------------------------------------------------------


# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = clustered_acc_init_obj_name,
                                               FUN = create_dir_and_save_acc_obj,
                                               FUN_args = list(base_path = clean_data_base_path, 
                                                               test = F, ext = ".rds"),
                                               cores = cores,
                                               type = type))
























