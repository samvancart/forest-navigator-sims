# Initialise prebas and save to file as rdata


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


print(paste0("Running multiSiteSpeciesInit.R for species with site type estimated by ", estimated_name))
print(paste0("Climate: ", climate_name))
print(paste0("Split id: ", split_id))
cat("\n")

# Number of layers and species
nLayers <- nSpecies <- 1

# Get pPRELES parameter (different for speciesID 12)
pPRELES <- get_pPRELES(config$VAR_species_id)

# Set pCROBAS kRein parameter
pCROB_copy <- get_pCROBAS(speciesIDs = c(config$VAR_species_id), pCROBAS_multipliers = config$VAR_pCROBAS_multipliers, pCROB = pCROB)

# Set pCROBAS config$VAR_theta_max parameter
pCROB_copy[31, config$VAR_species_id] <- config$VAR_theta_max

# Create multiInitVar
multiInitVar <- get_multiInitVar_species(nRows = nSites, nLayers = nLayers, speciesID = config$VAR_species_id, initAge = 12) # CHECK AGE

# Define parameters for initialisation
initMultiSite_params <- list(nYearsMS = rep(nYears,nSites),
                             siteInfo = siteInfo,
                             multiInitVar = multiInitVar,
                             pPRELES = pPRELES,
                             pCROBAS = pCROB_copy,
                             PAR = parTran,
                             VPD = vpdTran,
                             CO2= co2Tran,
                             Precip=precipTran,
                             TAir=tairTran,
                             defaultThin=config$VAR_management_id, 
                             ClCut=config$VAR_management_id)



print(paste0("Initialising model..."))
t <- system.time({
  # Init model
  initPrebas <- do.call(InitMultiSite, initMultiSite_params)
})
print(t)
print("Done.")
cat("\n")

# File name and path
file_name <- paste("initPrebas", species_name, estimated_name, management_name, climate_name, split_id, sep = "_")
extension <- "rdata"
dir_path <- file.path(config$PATH_rdata, "multisite_species")

full_path <- file.path(dir_path, paste(file_name, extension, sep = "."))

# print(paste0("Full path: ", full_path))

# # Write file
save(initPrebas, file = full_path)
print(paste0("initPrebas saved to ", full_path))




