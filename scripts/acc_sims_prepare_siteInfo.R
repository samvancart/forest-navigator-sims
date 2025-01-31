source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


clustered_paths <- list.files(boku_data_path, pattern = "clustered",  recursive = T, full.names = T)
soil_dt <- fread(soil_file_path)
split_soil_dts <- split(soil_dt, by = "PlgID")


# Get acc objects
site_info_obj_list <- lapply(names(split_soil_dts), function(id) {
  dt <- split_soil_dts[[id]]
  get_site_info_acc_object(dt, 
                           aaa_file, 
                           grid_file_path, 
                           clustered_paths, 
                           clean_data_base_path, 
                           group_id_name,
                           species_id_name,
                           rep_times = as.integer(num_sample_runs),
                           clim_scen = "gwl2")

})



# Save all acc objects
invisible(lapply(site_info_obj_list, function(acc_obj) {
  do.call(create_dir_and_save_acc_obj, list(acc_obj = acc_obj,
                                            base_path = clean_data_base_path, 
                                            test = F))
}))




save_acc_data








































