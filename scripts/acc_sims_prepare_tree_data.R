# This script is for preparing BOKU tree data first into 1ha-by-1ha forested areas
# by sampling and then clustering the trees in each 1ha-by-1ha area. Each cluster
# is saved as "clusters_<10km-by-10km_cell_id>.rdata.


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



#######################################

############ TREE DATA

#######################################



# Get all sampled_dt lists
tree_data_dts <- do.call(get_in_parallel, get_in_parallel_tree_data_args)



# Assign ids for each 1 ha forest then unlist into a single list and finally merge speciesIDs
dts <- assign_and_merge(tree_data_dts, id_columns, separator = "_", 
                        id_column_name = "forested_ha_id", fin_codes_with_speciesID_dt,
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
                                    by = .(cell_300arcsec, cell, forested_ha_id, speciesID, cluster_id)]

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



















