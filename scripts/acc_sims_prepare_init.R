
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
aggr_clusters_dt <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight), b = sum(ba), age = as.integer(mean(age))),
                                    by = .(cell_300arcsec, cell, forested_ha_id, speciesID, cluster_id)]





#######################################

############ SAVE

#######################################


# Save
filename <- paste0("clustered_", cells_10[cells_10_id], ".rdata")
save_path <- file.path(boku_data_path, "clustered", filename)
print(paste0("Saving into ", save_path))
save(aggr_clusters_dt, file = save_path)



#######################################

############ MultiInitVar

#######################################


# nLayers <- aggr_clusters_dt[, .N, by = forested_ha_id]$N
# nSpecies <- aggr_clusters_dt[, .N, by = clustering_group_cols][, .N, by = forested_ha_id]$N
# 
# maxNlayers <- max(nLayers)


# # Initialize the multiInitVar array
# multiInitVar <- array(0, dim=c(nSites, 7, maxNlayers))
# 
# multiInitVar[,6:7,NA]
# 
# system.time({
#   # Split the data.table by groupID
#   split_data <- split(dt_nSites, by = "groupID")
#   
#   # Apply the process_subset function to each subset
#   results <- lapply(seq_along(split_data), function(i) process_subset(split_data[[i]], i))
#   for (i in seq_along(results)) {
#     multiInitVar[i, 1, 1:nLayers[i]] <- results[[i]]$speciesID # vector of species ID taken from data
#     multiInitVar[i, 2, 1:nLayers[i]] <- results[[i]]$Age # age by tree from NFI
#     multiInitVar[i, 3, 1:nLayers[i]] <- results[[i]]$Height # height from NFI data
#     multiInitVar[i, 4, 1:nLayers[i]] <- results[[i]]$Dbh # dbh from NFI data
#     multiInitVar[i, 5, 1:nLayers[i]] <- results[[i]]$basal_area # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
#     multiInitVar[i, 6, 1:nLayers[i]] <- results[[i]]$NA_values
#   }
# })














