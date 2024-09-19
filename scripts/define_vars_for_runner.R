# This script is for defining variables and creating a run table used by yaml_runner.R.


# Load libs and config
source('scripts/settings.R')
source("r/utils.R")


# NAMED VECTORS 

species_vector <- as.integer(names(config$VAR_species_dict))
management_vector <- as.integer(c(0,1))
estimated_vector <- as.integer(c(1))
# climate_vector <- seq_along(config$VAR_climate_paths)
climate_vector <- as.integer(c(4,5,6,7,8))

# # Get split ids from climate data files
# split_ids_list <- invisible(lapply(climate_vector, function(x) {
#   climate_path <- paste0(config$VAR_climate_paths[x])
#   print(paste0("Getting splitIDs for ", climate_path, "..."))
#   split_ids <- load_data(climate_path)[["splitID"]]
#   unique(split_ids)
# }))
# print(paste0("All done."))



## CREATE named_vector_list

# named_vector_list <- list(VAR_species_id = species_vector, 
#                           VAR_management_id = management_vector, 
#                           VAR_climate_id = climate_vector)


# named_vector_list <- list(VAR_climate_id = climate_vector)
named_vector_list <- list(VAR_species_id = species_vector, 
                          VAR_management_id = management_vector,
                          VAR_climate_id = climate_vector)



# SOURCES

# Create source_list
# src_vector <- config$SRC_mock_run_scripts
# src_vector <- config$SRC_multi_and_outputs_species
# src_vector <- config$SRC_climate_csv_to_prebas
# src_vector <- config$SRC_create_tran
# src_vector <- config$SRC_multi_species
# src_vector <- config$SRC_init_multi_species
# src_vector <- config$SRC_multi_species_test
src_vector <- config$SRC_outputs_species



# ADDITIONAL NAMED VECTORS

# # Load tran id vector
# load_tran_id_vector <- as.integer(c(0,1,1,1,1,1,1,1)) # Whether to load tran files. Load if 0 don't if 1.
# 
# # Repeat by nRows
# load_tran_id_vector_rep <- rep(load_tran_id_vector, nrow(expand.grid(named_vector_list))/length(load_tran_id_vector))
# 
# # Create additional_named_vector_list
# additional_named_vector_list <- list(VAR_load_tran_id = load_tran_id_vector_rep)


# TABLE

# Create run table
run_table_dt <- get_run_table_dt(named_vector_list, src_vector, additional_named_vector_list = NULL)


# # Create run table dynamically
# run_table_dt <- rbindlist(invisible(lapply(seq_along(climate_vector), function(id) {
#   named_vector_list <- list(VAR_climate_id = climate_vector[id],
#                             VAR_species_id = species_vector,
#                             VAR_estimated_id = estimated_vector,
#                             VAR_management_id = management_vector,
#                             VAR_split_id = split_ids_list[[id]])
#   
#   run_table_dt_id <- get_run_table_dt(named_vector_list, src_vector, additional_named_vector_list = NULL)
# })))


# # LOAD TRAN IDS
# n_split_ids <- 1
# split_id_len <- nrow(run_table_dt)/length(climate_vector)/n_split_ids
# n_times <- nrow(run_table_dt)/split_id_len
# load_tran_vector <- as.integer(rep(c(0, rep(1, split_id_len-1)), n_times))
# run_table_dt[, VAR_load_tran_id := load_tran_vector]



# DEFAULTS

# Defaults to set after running
one <- as.integer(c(1))
zero <- as.integer(c(0))
defaults <- list(VAR_species_id = one, 
                 VAR_management_id = zero, 
                 VAR_climate_id = one,
                 VAR_layer_id = one,
                 VAR_estimated_id = one ,
                 VAR_tabX_id = one,
                 VAR_load_tran_id = zero,
                 VAR_split_id = one)






