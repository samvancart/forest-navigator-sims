# Script to sample from Boku treedata and create clusters. 
# Data is sampled from individual tree data files and the cumulative sum of the
# basal area for each selection is calculated.
# The main file (AAA_ForestTypeList_Finland_2024-11-11.csv) basal area is used
# as a threshold. The sampling ends when the cum_sum exceeds the threshold. The 
# returned ba is the last value before exceeding the threshold.
# Next parallel processing is used to cluster the data.



source('scripts/settings.R')
source("r/acc_sims.R")
# source("r/utils.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")


boku_data_path <- file.path(config$PATH_nfi_finland, "3_points_comparison", "boku_111124")
aaa_file <- list.files(boku_data_path, pattern = "AAA", full.names = T)
init_files <- list.files(boku_data_path, "FIN_", full.names = T)
aaa_all <- fread(aaa_file)

# Select one 10km cell
cells_10 <- unique(aaa_all$cell_300arcsec)
cells_10_id <- 2
aaa <- aaa_all[cell_300arcsec == cells_10[cells_10_id]]


# Seed for creating reproducable list of seeds
seed <- 123
set.seed(seed)
seeds <- sample(c(1:1000000), 100) # List of seeds to use


# Get tree_data in parallel
cores <- availableCores()
data <- seeds
# Args without seed
FUN_args <- list(aaa = aaa,
                 boku_data_path = boku_data_path,
                 del_cols = c("cum_sum"), 
                 add_cols = c("cell", "cell_300arcsec"))

# Sample from all treedata files until ba reaches threshold (AAA file ba) then add 1km cell id.
FUN <- process_treedata_files

# Get all sampled_dt lists
tree_data_dts <- get_in_parallel(data = data, FUN = FUN, FUN_args = FUN_args, df_name = "seed",
                                    cores = cores, .packages = c( "checkmate", "data.table"), 
                                    .export = c("process_treedata_files", 
                                                "read_and_process_file", "sample_until_global_threshold"))


# Assign ids for each 1 ha forest and unlist into a single list
dts <- invisible(unlist(lapply(seq(tree_data_dts), function(i){
  dts_i <- tree_data_dts[[i]]
  lapply(seq(dts_i), function(j) {
    dt <- dts_i[[j]]
    dt[, forested_ha_id := paste(i, j, dt[["cell"]], sep = "_")]
    dt
  })
}), recursive = F))


# Get clusters in parallel
data <- dts
FUN <- perform_clustering_by_group
FUN_args <- list(group_cols = c("cell", "species"), 
                 value_cols = c("dbh", "treeheight"), 
                 seed = seed, 
                 nstart = 25, 
                 iter.max = 50)

all_clusters_dts <- get_in_parallel(data = data, FUN = FUN, FUN_args = FUN_args, 
                                    cores = cores, .packages = c( "checkmate", "data.table", "factoextra"), 
                                    .export = c("get_kmax", "get_centers"))


all_clusters_dt <- rbindlist(all_clusters_dts)

# Aggregate clustered dt
aggr_clusters_dt <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight), b = sum(ba), age = as.integer(mean(age))), 
                        by = .(cell_300arcsec, cell, forested_ha_id, species, cluster_id)]


















