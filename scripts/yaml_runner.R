# Script for running other scripts. 
# This script runs a series of R scripts based on configurations specified in a data table.
# The configuration must include IDs as a named list of integer vectors whose names 
# correspond to the id variables found in the YAML configuration file, as well as 
# the source files to be run as a vector, corresponding to those in the YAML.
# It may also include a list of additional named vectors that are the same length
# as the expanded grid (the run table) that is created from the original named_vector list.

# These parameters (the named vector lists and the run_table itself) 
# should be created in define_vars_for_runner.R




# Load libs and config
source('scripts/settings.R')

# Create temp_env
temp_env <- new.env()

# Run inside local block
local(envir = temp_env, {
  
  # LOAD

  # Load vars into temp_env
  source("scripts/define_vars_for_runner.R", temp_env)


  # RUN
  
  # Run from table
  run_yaml_from_table(run_table_dt, config_path)
  
  
  # CLEAN UP
  
  # Set default ids
  set_default_ids_in_yaml(config_path)
  
  # Vars to keep
  keep_vars <- c("config", "config_path")
  
  cat("\n")
  print(paste0("Removing variables except: ", list(keep_vars)))
  cat("\n")
  
  # Remove vars
  remove_selected_variables_from_env(keep_vars)
  
})



















