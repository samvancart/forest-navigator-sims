# Script to sample from Boku treedata and create clusters. 
# Data is sampled from individual tree data files and the cumulative sum of the
# basal area for each selection is calculated.
# The main file (AAA_ForestTypeList_Finland_2024-11-11.csv) basal area is used
# as a threshold. The sampling ends when the cum_sum exceeds the threshold. The 
# returned ba is the last value before exceeding the threshold.
# Next parallel processing is used to cluster the data.



source('scripts/settings.R')
source("r/utils.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")


boku_data_path <- file.path(config$PATH_nfi_finland, "3_points_comparison", "boku_111124")

aaa_file <- list.files(boku_data_path, pattern = "AAA", full.names = T)
init_files <- list.files(boku_data_path, "FIN_", full.names = T)

aaa <- fread(aaa_file)

length(aaa[cell_300arcsec==190997]$cell_300arcsec)
unique(aaa$cell_300arcsec)

seed <- 123


# Sample from all treedata files until ba reaches threshold (AAA file ba) then add 1km cell id.
dts <- invisible(apply(aaa, 1, function(x) {
  filename <- paste0(x["InitFileName"], "_01.csv")
  path <- file.path(boku_data_path, filename)
  threshold <- as.numeric(x["ba"])
  cell_val <- x["cell"]
  dt <- fread(path)
  sampled_dt <- sample_until_global_threshold(dt, "ba", threshold, seed = seed)
  sampled_dt[, cum_sum := NULL]
  sampled_dt[, cell := cell_val]
}))


# Get clusters in parallel
cores <- availableCores()
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

test <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight), b = sum(ba), age = as.integer(mean(age))), 
                        by = .(cell, species, cluster_id)]
test[cell == 15806541]

















