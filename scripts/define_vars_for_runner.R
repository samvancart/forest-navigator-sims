# This script is for defining variables and creating a run table used by yaml_runner.R.


# Load libs and config
source('scripts/settings.R')
source("r/utils.R")


# NAMED VECTORS 

species_vector <- as.integer(names(config$VAR_species_dict))
management_vector <- as.integer(c(0,1))
climate_vector <- seq_along(config$VAR_climate_paths)


# Create named_vector_list
named_vector_list <- list(VAR_species_id = species_vector, 
                          VAR_management_id = management_vector, 
                          VAR_climate_id = climate_vector)


# SOURCES

# Create source_list
src_vector <- config$SRC_mock_run_scripts
# src_vector <- config$SRC_multi_and_outputs_species


# ADDITIONAL NAMED VECTORS

# Load tran id vector
load_tran_id_vector <- as.integer(c(0,1,1,1,1,1,1,1))

# Repeat by nRows
load_tran_id_vector_rep <- rep(load_tran_id_vector, nrow(expand.grid(named_vector_list))/length(load_tran_id_vector))

# Create additional_named_vector_list
additional_named_vector_list <- list(VAR_load_tran_id = load_tran_id_vector_rep)


# TABLE

# Create run table
run_table_dt <- get_run_table_dt(named_vector_list, src_vector, additional_named_vector_list)



# DEFAULTS

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










