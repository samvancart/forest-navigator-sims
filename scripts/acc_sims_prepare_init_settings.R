source("r/acc_sims.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")
source("r/utils.R")
source("r/csc_utils.R")
source('./r/multiSite.R')

# Load data
boku_data_path <- "data/tree_data/test_sites"
aaa_file <- list.files(config$PATH_data, pattern = "AAA", full.names = T, recursive = T)
init_files <- list.files(config$PATH_data, "FIN_", full.names = T, recursive = T)
aaa_all <- fread(aaa_file)

# Get species codes lookup
species_codes_file <- "test_sites_species_codes_lookup.csv"
species_codes_lookup_path <- list.files(config$PATH_data, pattern = species_codes_file, full.names = T, recursive = T)
fin_codes_with_speciesID_dt <- fread(file = species_codes_lookup_path)[, c("code", "speciesID")]


# Select one 10km cell
cells_10 <- unique(aaa_all$cell_300arcsec)
cells_10_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
# cells_10_id <- 2
aaa <- aaa_all[cell_300arcsec == cells_10[cells_10_id]]


# Seed for creating reproducable list of seeds
seed <- 123
set.seed(seed)
num_sample_runs <- 100
seeds <- sample(c(1:1000000), num_sample_runs) # List of seeds to use


# Cores
cores <- max(1, availableCores() - 1)
type <- "FORK"


# TREE DATA

tree_data <- seeds
process_treedata_files_args <- list(aaa = aaa,
                 boku_data_path = boku_data_path,
                 del_cols = c("cum_sum"),
                 add_cols = c("cell", "cell_300arcsec"))

# Sample from all treedata files until ba reaches threshold (AAA file ba) then add 1km and 10km cell ids.
process_treedata_files_FUN <- process_treedata_files

get_in_parallel_tree_data_args <- list(data = tree_data, 
                                       FUN = process_treedata_files_FUN, 
                                       FUN_args = process_treedata_files_args, 
                                       df_name = "seed",
                                       cores = cores, 
                                       type = type)

# Names to use for the forest_id
id_columns <- c("i", "j", "cell", "cell_300arcsec")




# CLUSTERING

clustering_group_cols <- c("forested_ha_id","speciesID")
clustering_value_cols <-  c("dbh", "treeheight")
perform_clustering_by_group_FUN <- perform_clustering_by_group
perform_clustering_by_group_args <- list(group_cols = clustering_group_cols, # Check group_cols based on dts structure
                 value_cols = clustering_value_cols,
                 seed = seed,
                 nstart = 25,
                 iter.max = 50)


get_in_parallel_all_clusters_dts_args <- list(FUN = perform_clustering_by_group_FUN, 
                                              FUN_args = perform_clustering_by_group_args,
                                              cores = cores, 
                                              type = type)






############ PRINT ################

# Get variables as named list
all_vars <- get_named_list(cells_10_id,
                           cores,
                           num_sample_runs,
                           boku_data_path,
                           id_columns,
                           perform_clustering_by_group_args)


# Named list as text
txt_vec <- get_named_list_as_txt(all_vars, sep_text = " is ")

print(txt_vec)




