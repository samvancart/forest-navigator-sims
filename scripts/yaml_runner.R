source("r/utils.R")

library(yaml)


# Path to config file
config_path <- paste0("config.yaml")

# Load configuration file
config <- yaml.load_file(config_path)

# Modify with named vector
ids <- c(VAR_climate_id= as.integer(1), VAR_species_id = as.integer(2), VAR_management_id = as.integer(0))
modify_yaml_settings_vector(config_path, ids)

