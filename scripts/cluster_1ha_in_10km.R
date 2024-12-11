# Script to sample from Boku treedata and create clusters. 

# Data is sampled from individual tree data files and the cumulative sum of the
# basal area for each selection is calculated. 
# The main file (AAA_ForestTypeList_Finland_2024-11-11.csv) basal area is used
# as a threshold. The sampling ends when the cum_sum exceeds the threshold. The 
# returned ba is the last value before exceeding the threshold.

# The sampling function is given a seed for reproducibility. A list of seeds is 
# used to create as many unique samples as is specified in `num_sample_runs`.
# The goal is to fill each 1km-by-1km cell in the data with 1ha-by-1ha forests 
# that have been created by the sampling function. The sampling is done in parallel. 

# After completing the sampling the data is organised into a single list of data.tables, 
# one for each 1ha-by-1ha area. Clusters are created by species using dbh and 
# height as the clustering variables. This is done in parallel. Finally after
# the data has been combined, it is aggregated by the clusters in each 1ha-by-1ha area.



source('scripts/settings.R')
source("r/acc_sims.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")
source("r/utils.R")
source("r/csc_utils.R")


# Load data
boku_data_path <- "data/tree_data/test_sites"
aaa_file <- list.files(config$PATH_data, pattern = "AAA", full.names = T, recursive = T)
init_files <- list.files(config$PATH_data, "FIN_", full.names = T, recursive = T)
aaa_all <- fread(aaa_file)

# Select one 10km cell
cells_10 <- unique(aaa_all$cell_300arcsec)
# cells_10_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
cells_10_id <- 2
aaa <- aaa_all[cell_300arcsec == cells_10[cells_10_id]]


# Seed for creating reproducable list of seeds
seed <- 123
set.seed(seed)
num_sample_runs <- 100
seeds <- sample(c(1:1000000), num_sample_runs) # List of seeds to use


# Get tree_data in parallel
cores <- max(1, availableCores() - 1)
data <- seeds
# Args without seed
FUN_args <- list(aaa = aaa,
                 boku_data_path = boku_data_path,
                 del_cols = c("cum_sum"),
                 add_cols = c("cell", "cell_300arcsec"))

# Sample from all treedata files until ba reaches threshold (AAA file ba) then add 1km and 10km cell ids.
FUN <- process_treedata_files

# Get all sampled_dt lists
tree_data_dts <- get_in_parallel(data = data, FUN = FUN, FUN_args = FUN_args, df_name = "seed",
                                 cores = cores, type = "FORK")


# Assign ids for each 1 ha forest and unlist into a single list
dts <- assign_and_unlist_ids(tree_data_dts, c("i", "j", "cell"), separator = "_")


rm(tree_data_dts)
gc()

# Get clusters in parallel
data <- dts
FUN <- perform_clustering_by_group
FUN_args <- list(group_cols = c("forested_ha_id","species"), # Check group_cols based on dts structure
                 value_cols = c("dbh", "treeheight"),
                 seed = seed,
                 nstart = 25,
                 iter.max = 50)



all_clusters_dts <- get_in_parallel(data = data, FUN = FUN, FUN_args = FUN_args,
                                    cores = cores, type = "FORK")


all_clusters_dt <- rbindlist(all_clusters_dts)


# Aggregate clustered dt
aggr_clusters_dt <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight), b = sum(ba), age = as.integer(mean(age))),
                        by = .(cell_300arcsec, cell, forested_ha_id, species, cluster_id)]



# Save
filename <- paste0("clustered_", cells_10[cells_10_id])
save_path <- file.path(boku_data_path, "clustered", filename)
print(paste0("Saving into ", save_path))
save(aggr_clusters_dt, file = save_path)













