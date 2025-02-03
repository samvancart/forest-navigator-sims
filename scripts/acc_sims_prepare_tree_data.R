# This script is for preparing BOKU tree data first into 1ha-by-1ha forested areas
# by sampling and then clustering the trees in each 1ha-by-1ha area. Each cluster
# is saved as "clusters_<10km-by-10km_cell_id>.rdata.


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)

#######################################

############ TREE DATA

#######################################


all_clim_paths <- get_filtered_clim_paths_from_bucket(grid_file_path, allas_opts)

# Helper
get_acc_clim_paths_run_dt <- function(all_clim_paths, 
                                      clim_scen_pat = "(?<=/)[^/]+(?=_id_)", 
                                      plgid_pat = "(?<=plgid_)[0-9]+") {
  
  plgid_vec <- str_extract(all_clim_paths, plgid_pat)
  clim_scen_vec <- str_extract(all_clim_paths, clim_scen_pat)
  
  dt <- data.table(path = all_clim_paths, PlgID = plgid_vec, clim_scen = clim_scen_vec)
  return(dt)
}

max_array_jobID <- 8
array_jobID <- 1

# Get all clim_paths as dt with PlgID and clim_scen cols
all_paths_run_dt <- get_acc_clim_paths_run_dt(all_clim_paths)
run_dt_max_part_size <- floor(nrow(all_paths_run_dt)/max_array_jobID)

# Split with constraint
run_dt_splitID <- split_dt_equal_with_constraint(all_paths_run_dt, run_dt_max_part_size, c("PlgID","clim_scen"))

# Filter by array jobID
run_dt <- split(run_dt_splitID, by = "splitID")[[array_jobID]][1:2]

plgid_vec <- as.integer(unique(run_dt$PlgID))
clim_paths <- run_dt$path





t <- system.time(
  clustered_acc_init_obj_list <- run_acc_for_plgid(plgid_vec = plgid_vec,
                                                   aaa_file = aaa_file,
                                                   clean_data_base_path = clean_data_base_path,
                                                   tree_data_acc_input_obj = tree_data_acc_input_obj,
                                                   clim_data_acc_input_obj = clim_data_acc_input_obj,
                                                   config = config,
                                                   allas_opts = allas_opts,
                                                   clim_paths = clim_paths,
                                                   get_in_parallel_args = general_get_in_parallel_args)
)
print(t)













# Get all sampled_dt lists
tree_data_dts <- do.call(get_in_parallel, get_in_parallel_tree_data_args)



# Assign ids for each 1 ha forest then unlist into a single list and finally merge speciesIDs
dts <- assign_and_merge(tree_data_dts, id_columns, separator = "_", 
                        id_column_name = "forested_ha_id", codes_with_speciesID_dt,
                        by.x = "species", by.y = "code")

rm(tree_data_dts)
gc()



#######################################

############ CLUSTERS AND AGGREGATION

#######################################



clustering_args <- c(list(data = dts), get_in_parallel_all_clusters_dts_args)

# Get clusters in parallel and combine
all_clusters_dt <- rbindlist(
  do.call(get_in_parallel, clustering_args)
)


# Aggregate clustered dt
aggr_clusters_dt <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight)/100, b = sum(ba), age = as.integer(mean(age))),
                                    by = .(cell_300arcsec, PlgID, cell, forested_ha_id, speciesID, cluster_id)]

# # Height from cm to m
# aggr_clusters_dt[, h := h/100]


# Split for saving
split_aggr_clusters_dt <- split(aggr_clusters_dt, by = aaa_split_col)




#######################################

############ SAVE

#######################################



# Save
invisible(mclapply(seq_along(split_aggr_clusters_dt), function(i) {
  name <- names(split_aggr_clusters_dt)[i]
  aggr_dt <- split_aggr_clusters_dt[[i]]
  filename <- paste0("clustered_", name, ".rdata")
  save_path <- file.path(boku_data_path, "clustered", filename)
  print(paste0("Saving into ", save_path))
  save(aggr_dt, file = save_path)
  print("Done.")
  cat("\n")
}, mc.cores = cores))



















