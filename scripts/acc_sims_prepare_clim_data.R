# This script is for processing acc climate data. First each climate data
# file is read and processed by filtering out those 1km-by-1km ids for which there
# are no corresponding forests in the BOKU tree data (AAA file). Then the matrices
# used for initialising PREBAS are created (1km-by-1km sites as rows, days as columns).
# After this the matrices are saved according to the name, plgid and save_path that are items
# in each returned acc_obj.
# Be aware of memory limitations when processing for simulation sites!



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)

slurm_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")



# Clim files to process
# clim_paths <- list.files(dest_path, full.names = T)

# Get climate paths from allas and filter by grid file PlgIDs
bucket_name <- "2000994-forest_navigator"
region <- Sys.getenv("AWS_REGION")
bucket_list <- as.data.table(get_bucket_df(bucket_name, region = region))$Key
allas_clim_paths <- bucket_list[!grepl(pattern = ".keep", bucket_list)]
allas_clim_paths_dt <- as.data.table(tstrsplit(allas_clim_paths, split = "[/]", keep = 3))[, id := .GRP, by = "V1"][, path := allas_clim_paths][, c("id", "path")]
plgids <- as.integer(str_extract(allas_clim_paths, "(?<=plgid_)[0-9]+"))
allas_clim_paths_dt[, PlgID := plgids]
grid_dt <- fread(grid_file_path)
filtered_clim_paths <- allas_clim_paths_dt[PlgID %in% unique(grid_dt$PlgID)]
split_clim_dts <- split(filtered_clim_paths, by = "id")


print(paste0("SLURM_ID: ", slurm_id))


# Get clim paths for id
clim_paths <- split_clim_dts[[slurm_id]]$path




# Get list of acc objects
init_clim_obj_list <- do.call(get_in_parallel, list(data = clim_paths,
                                                    FUN = get_acc_init_clim_object,
                                                    FUN_args = list(aaa_file = aaa_file, 
                                                                    operations = operations,
                                                                    clean_data_base_path = clean_data_base_path,
                                                                    config = config,
                                                                    allas_opts = list(bucket = bucket_name,
                                                                                      FUN = fread,
                                                                                      opts = list(region = region))),
                                                    cores = cores,
                                                    type = type))




# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = init_clim_obj_list,
                              FUN = create_dir_and_save_acc_obj,
                              FUN_args = list(base_path = clean_data_base_path, 
                                              test = F),
                              cores = cores,
                              type = type))










# init_clim_obj_list[[1]]$data[[1]][1:70,]



# clim <- fread(clim_paths[4])
# filtered <- filter_data_by_tree_data_cells(clim, aaa_file)
# filtered_op <- transform_and_add_columns(filtered, operations)
# filtered_op_co2 <- add_acc_co2(filtered_op, "detrended", config)
# 
# 
# co2_file_path <- config$VAR_acc_co2_files[["gwl3"]]


# 
# class(clim$time)
# 
# filtered <- filter_data_by_tree_data_cells(clim, aaa_file)
# filtered_op <- transform_and_add_columns(filtered, operations)
# 
# names(filtered)
# 
# fread(selection_path)
























