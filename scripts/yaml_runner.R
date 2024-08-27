# Script for running other scripts. 
# This script runs a series of R scripts based on configurations specified in a data table.
# The configuration must include IDs as a named list of integer vectors whose names 
# correspond to the id variables found in the YAML
# configuration file, as well as the source files to be run as a list, corresponding to those in the YAML.



# Load libs and config
source('scripts/settings.R')

# Create temp_env
temp_env <- new.env()

# Run inside local block
local(envir = temp_env, {
  
  # Load functions into temp_env
  source("r/utils.R", temp_env)
  
  # Variables for runner
  species_vector <- as.integer(names(config$VAR_species_dict))
  management_vector <- as.integer(c(0,1))
  climate_vector <- seq_along(config$VAR_climate_paths)
  
  # Create named_vector_list
  named_vector_list <- list(VAR_species_id = species_vector, 
                               VAR_management_id = management_vector, 
                               VAR_climate_id = climate_vector)
  
  # Create source_list
  src_list <- config$SRC_mock_run_scripts
  # src_list <- config$SRC_multi_and_outputs_species
  
  # Create run table
  run_table_dt <- get_run_table_dt(named_vector_list, src_list)
  
  # Run
  run_yaml_from_table(run_table_dt, config_path)
  
})



# Remove vars from global env
global_vars <- ls()
keep_vars <- c("config", "config_path", "temp_env")
rm(list = setdiff(global_vars, keep_vars), "global_vars", "keep_vars")
gc()



# Remove temp_env
rm(temp_env)

















