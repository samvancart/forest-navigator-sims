# Initialise prebas and save to file


source('scripts/settings.R')
source('scripts/loadData.R')
source('./r/utils.R')
source('./r/multiSite.R')


### Climate data loaded from loadData.R ###
### Soil data loaded from loadData.R ###
### SiteInfo created in loadData.R ###

species_name <- get_speciesName(config$VAR_species_id, config$VAR_species_dict)
estimated_name <- config$VAR_estimated_names[config$VAR_estimated_id]
management_name <- config$VAR_management_names[config$VAR_management_id+1]
climate_name <- config$VAR_climate_names[config$VAR_climate_id]
split_id <- config$VAR_split_id


print(paste0("Running multiSiteSpecies.R for species ", species_name, " and site type estimated by ", estimated_name))
print(paste0("Management: ", management_name))
print(paste0("Climate: ", climate_name))
print(paste0("Split id: ", split_id))
cat("\n")


# Get pPRELES parameter (different for speciesID 12)
pPRELES <- get_pPRELES(config$VAR_species_id)

# Set pCROBAS kRein parameter
pCROB_copy <- get_pCROBAS(speciesIDs = c(config$VAR_species_id), pCROBAS_multipliers = config$VAR_pCROBAS_multipliers, pCROB = pCROB)

# Set pCROBAS config$VAR_theta_max parameter
pCROB_copy[31, config$VAR_species_id] <- config$VAR_theta_max

# Define parameters for initialisation
initMultiSite_params <- list(pPRELES = pPRELES,
                             pCROBAS = pCROB_copy,
                             defaultThin=config$VAR_management_id, 
                             ClCut=config$VAR_management_id)



# if(!exists("initPrebas")) { # For parallel processing
if(config$VAR_load_tran_id == 0) {
  
  # Get initPrebas files
  init_prebas_path <- file.path(config$PATH_rdata, "multisite_species")
  init_prebas_files <- list.files(init_prebas_path, full.names = T)
  
  # Find by climate and split_id
  pattern_init_prebas <- paste0("initPrebas.*_", config$VAR_climate_names[config$VAR_climate_id],
                                "_", config$VAR_split_id, "\\.rdata$")
  init_prebas_file <- grep(pattern_init_prebas, init_prebas_files, value = TRUE)
    
  print(paste0("Loading initPrebas from ", init_prebas_file, "..."))
  initPrebas <- load_data(init_prebas_file)
  print(paste0("Done."))
  cat("\n")
  
}


print(paste0("Modifying initPrebas..."))
modify_params <- initMultiSite_params[c("pPRELES", "pCROBAS")]
modify_params[["defaultThin"]] <- rep(initMultiSite_params[["defaultThin"]], length(initPrebas[["defaultThin"]]))
modify_params[["ClCut"]] <- rep(initMultiSite_params[["ClCut"]], length(initPrebas[["ClCut"]]))
multiInitVar <- initPrebas$multiInitVar
multiInitVar[,1,] <- config$VAR_species_id
multiInitVar[,7,] <- config$VAR_initMultiSiteSpecies_Ac[as.character(config$VAR_species_id)][[1]]
modify_params[["multiInitVar"]] <- multiInitVar

# Modify initPrebas
initPrebas[names(modify_params)] <- map2(initPrebas[names(modify_params)], modify_params, ~ .y)

print("Done.")
cat("\n")


# Run multisite model
print(paste0("Running multiPrebas..."))
modOut <- multiPrebas(initPrebas)
print("Done.")

# Get output
print(paste0("Getting multiOut..."))
multiOut <- modOut$multiOut
print("Done.")

cat("\n")

# File name and path
file_name <- paste("multiOut", species_name, estimated_name, management_name, climate_name, split_id, sep = "_")
extension <- "rdata"
dir_path <- file.path(config$PATH_rdata, "multisite_species")

full_path <- file.path(dir_path, paste(file_name, extension, sep = "."))

# print(paste0("Full path: ", full_path))

# Write file
save(multiOut, file = full_path)
print(paste0("multiOut saved to ", full_path))




