source('scripts/settings.R')
source('scripts/loadData.R')
source('./r/utils.R')
source('./r/multiSite.R')

# Run multisite prebas for sitetypes 1, 5 and estimated site type (by N in soildata). Ids in config.YAML.
# Produces multiOut_spID<speciesID> rdata file.
# Run for all species and both estimated N values from yaml_runner.R.


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




# print(paste0("Initialising model with site type estimated by soil N..."))
# # Initialise model with different site types
# initPrebas <- do.call(InitMultiSite, initMultiSite_params)
# print("Done.")
# 
# print("Initialising model with site type 1...")
# # setting site type to 1
# initMultiSite_params$siteInfo[,3] = 1
# initPrebas_st1 <- do.call(InitMultiSite, initMultiSite_params)
# print("Done.")
# 
# print("Initialising model with site type 5...")
# # setting site type to 5
# initMultiSite_params$siteInfo[,3] = 5
# initPrebas_st5 <- do.call(InitMultiSite, initMultiSite_params)
# print("Done.")




print(paste0("Initialising model..."))
t <- system.time({
  initPrebas <- do.call(InitMultiSite, initMultiSite_params)
  initPrebas_st1 <- do.call(InitMultiSite, initMultiSite_params)
  initPrebas_st5 <- do.call(InitMultiSite, initMultiSite_params)
})
print("Done.")
print(t)
# 
# 
# 
# # Run multisite model
# modOut <- multiPrebas(initPrebas)
# modOut_st1 <- multiPrebas(initPrebas_st1)
# modOut_st5 <- multiPrebas(initPrebas_st5)
# 
# # Get output
# multiOut<-modOut$multiOut
# multiOut_st1<-modOut_st1$multiOut
# multiOut_st5<-modOut_st5$multiOut


# File name and path
file_name <- paste("multiOut", species_name, estimated_name, management_name, climate_name, split_id, sep = "_")
extension <- "rdata"
dir_path <- file.path(config$PATH_rdata, "multisite_species")

full_path <- file.path(dir_path, paste(file_name, extension, sep = "."))

print(paste0("Full path: ", full_path))

# # Write file
# save(multiOut,multiOut_st1,multiOut_st5, file = full_path)
# print(paste0("multiOut saved to ", file_name))


# # Clean up if not using yaml.runner
# keep_vars <- c("config", "config_path")
# remove_selected_variables_from_env(keep_vars)



