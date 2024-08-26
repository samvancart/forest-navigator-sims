source('scripts/settings.R')
source('scripts/loadData.R')
source('./r/utils.R')
source('./r/multiSite.R')

# Run multisite prebas for sitetypes 1, 5 and estimated site type (by N in soildata). Ids in settings.R.
# Produces multiOut_spID<speciesID> rdata file.
# Run for all species and both estimated N values from run.R.


### Climate data loaded from loadData.R ###
### Soil data loaded from loadData.R ###
### SiteInfo created in loadData.R ###

print(paste0("Running multiSiteSpecies.R for species ",
             get_speciesName(VAR_species_id, VAR_species_dict), " and site type estimated by ", VAR_estimated_names[VAR_estimated_id]))
print(paste0("Management: ", VAR_management_names[VAR_management_id+1]))
print(paste0("Climate: ", VAR_climate_names[VAR_climate_id]))

# Number of layers and species
nLayers <- nSpecies <- 1

# Get pPRELES parameter (different for speciesID 12)
pPRELES <- get_pPRELES(VAR_species_id)

# Set pCROBAS kRein parameter
pCROB_copy <- get_pCROBAS(speciesIDs = c(VAR_species_id), pCROBAS_multipliers = VAR_pCROBAS_multipliers, pCROB = pCROB)

# Set pCROBAS VAR_theta_max parameter
pCROB_copy[31, speciesID] <- VAR_theta_max

# Create multiInitVar
multiInitVar <- get_multiInitVar_species(nRows = nSites, nLayers = nLayers, speciesID = VAR_species_id, initAge = 12) # CHECK AGE


print(paste0("Initialising model with site type estimated by soil N..."))
# Initialise model
###using siteType estimate based on N
initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                            siteInfo = siteInfo,
                            multiInitVar = multiInitVar,
                            pPRELES = pPRELES,
                            pCROBAS = pCROB_copy,
                            PAR = parTran,
                            VPD = vpdTran,
                            CO2= co2Tran,
                            Precip=precipTran,
                            TAir=tairTran,
                            defaultThin=VAR_management_id, 
                            ClCut=VAR_management_id)
print("Done.")
print("Initialising model with site type 1...")
# setting site type to 1
siteInfo[,3]=1
initPrebas_st1 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                                siteInfo = siteInfo,
                                multiInitVar = multiInitVar,
                                pPRELES = pPRELES,
                                pCROBAS = pCROB_copy,
                                PAR = parTran,
                                VPD = vpdTran,
                                CO2= co2Tran,
                                Precip=precipTran,
                                TAir=tairTran,
                                defaultThin=VAR_management_id, 
                                ClCut=VAR_management_id)

print("Done.")
print("Initialising model with site type 5...")
# setting site type to 5
siteInfo[,3]=5
initPrebas_st5 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                                siteInfo = siteInfo,
                                multiInitVar = multiInitVar,
                                pPRELES = pPRELES,
                                pCROBAS = pCROB_copy,
                                PAR = parTran,
                                VPD = vpdTran,
                                CO2= co2Tran,
                                Precip=precipTran,
                                TAir=tairTran,
                                defaultThin=VAR_management_id,
                                ClCut=VAR_management_id)
print("Done.")

# # Save initPrebas
# filename <- paste0(rdata_path, "initPrebas_", VAR_species_names[VAR_species_id], ".rdata")
# save(initPrebas,initPrebas_st1,initPrebas_st5, file=filename)

# run multisite model
modOut <- multiPrebas(initPrebas)
modOut_st1 <- multiPrebas(initPrebas_st1)
modOut_st5 <- multiPrebas(initPrebas_st5)

# get output
multiOut<-modOut$multiOut
multiOut_st1<-modOut_st1$multiOut
multiOut_st5<-modOut_st5$multiOut

fileName <- paste0(rdata_path, "multiOut_spID", VAR_species_id, ".rdata")

save(multiOut,multiOut_st1,multiOut_st5, file=fileName)
print(paste0("multiOut saved to ", fileName))






