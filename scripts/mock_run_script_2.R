source('scripts/settings.R')
source('./r/utils.R')

print(paste0("Running mock script 2..."))

print(paste0("Species id is ", config$VAR_species_id))
print(paste0("Management id is ", config$VAR_management_id))
print(paste0("Climate id is ", config$VAR_climate_id))