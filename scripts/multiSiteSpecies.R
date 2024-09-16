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

print(paste0("Running multiSiteSpecies.R for species ",
             get_speciesName(config$VAR_species_id, config$VAR_species_dict), " and site type estimated by ", config$VAR_estimated_names[config$VAR_estimated_id]))
print(paste0("Management: ", config$VAR_management_names[config$VAR_management_id+1]))
print(paste0("Climate: ", config$VAR_climate_names[config$VAR_climate_id]))

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



# Initialise model with different site types
print(paste0("Initialising model with site type estimated by soil N..."))
initPrebas <- do.call(InitMultiSite, initMultiSite_params)
print("Done.")

print("Initialising model with site type 1...")
# setting site type to 1
initMultiSite_params$siteInfo[,3] = 1
initPrebas_st1 <- do.call(InitMultiSite, initMultiSite_params)
print("Done.")

print("Initialising model with site type 5...")
# setting site type to 5
initMultiSite_params$siteInfo[,3] = 5
initPrebas_st5 <- do.call(InitMultiSite, initMultiSite_params)
print("Done.")


# Run multisite model
modOut <- multiPrebas(initPrebas)
modOut_st1 <- multiPrebas(initPrebas_st1)
modOut_st5 <- multiPrebas(initPrebas_st5)

# Get output
multiOut<-modOut$multiOut
multiOut_st1<-modOut_st1$multiOut
multiOut_st5<-modOut_st5$multiOut

# Save
fileName <- paste0(config$PATH_rdata, "multiOut_spID", config$VAR_species_id, ".rdata")

save(multiOut,multiOut_st1,multiOut_st5, file=fileName)
print(paste0("multiOut saved to ", fileName))





