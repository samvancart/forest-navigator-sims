# This script is for creating the Prebas siteInfo



# SOURCE_FILES -------------------------------------------------------------



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# GET_PLG-IDS ----------------------------------------------------------------


clustered_paths <- list.files(clean_data_base_path, pattern = "clustered",  recursive = T, full.names = T)
plgid_vec <- as.integer(unlist(tstrsplit(clustered_paths, split = "[/_]", keep = 9)))



# GET_SOIL-DT ---------------------------------------------------------------



soil_dt <- fread(soil_file_path)[PlgID %in% plgid_vec]
setnames(soil_dt, old = "soil depth", new = "soil_depth")


# SPLIT_SOIL-DT -----------------------------------------------------------------



split_soil_dts <- split(soil_dt, by = "PlgID")



# RUN ---------------------------------------------------------------------



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
                           clim_scen = "detrended")

})




# SAVE --------------------------------------------------------------------




# Save all acc objects
invisible(lapply(site_info_obj_list, function(acc_obj) {
  do.call(create_dir_and_save_acc_obj, list(acc_obj = acc_obj,
                                            base_path = clean_data_base_path, 
                                            test = F, ext = ".rds"))
}))













































