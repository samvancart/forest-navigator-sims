source('scripts/settings.R')

# Create temp_env
temp_env <- new.env()

# Load functions into temp_env
source("r/utils.R", temp_env)


# Modify with named vector
ids <- c(VAR_climate_id = as.integer(3), VAR_species_id = as.integer(2), VAR_management_id = as.integer(0))
temp_env$modify_yaml_settings_vector(config_path, ids)

# Remove temp_env
rm(temp_env)
