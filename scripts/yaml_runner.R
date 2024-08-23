source("r/utils.R")

library(yaml)


# Path to config file
config_path <- paste0("config.yaml")

# Load configuration file
config <- yaml.load_file(config_path)

# Modify with named vector
ids <- c(climateID= as.integer(1), speciesID = as.integer(2), managementID = as.integer(0))
modify_yaml_settings_vector(config_path, ids)

