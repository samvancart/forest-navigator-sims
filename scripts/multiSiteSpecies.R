source('scripts/settings.R')
source('scripts/loadData.R')

# Run multisite prebas for sitetypes 1, 5 and estimated site type (by N in soildata). Ids in settings.R.
# Produces multiOut_spID<speciesID> rdata file.
# Run for all species and both estimated N values from run.R.


### Climate data loaded from loadData.R ###
### Soil data loaded from loadData.R ###
### SiteInfo created in loadData.R ###

print(paste0("Running multiSiteSpecies.R for species ",
             speciesNames[speciesID], " and site type estimated by ", estimatedNames[estimatedID]))

# Number of layers and species
nLayers <- nSpecies <- 1

# Create multiInitVar
multiInitVar <- array(NA,dim=c(nSites,7,nLayers))
multiInitVar[,1,] <- speciesID
multiInitVar[,3,] <- initSeedling.def[1]; multiInitVar[,4,] <- initSeedling.def[2]
multiInitVar[,5,] <- initSeedling.def[3]; multiInitVar[,6,] <- initSeedling.def[4]
multiInitVar[,2,] <- 100


print(paste0("Initialising model with site type estimated by soil N..."))
# Initialise model
###using siteType estimate based on N
initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                            siteInfo = siteInfo,
                            multiInitVar = multiInitVar,
                            PAR = PARtran,
                            VPD = VPDtran,
                            CO2= CO2tran,
                            Precip=Preciptran,
                            TAir=TAirtran,
                            defaultThin=0, 
                            ClCut=0)
print("Done.")
print("Initialising model with site type 1...")
# setting site type to 1
siteInfo[,3]=1
initPrebas_st1 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                                siteInfo = siteInfo,
                                multiInitVar = multiInitVar,
                                PAR = PARtran,
                                VPD = VPDtran,
                                CO2= CO2tran,
                                Precip=Preciptran,
                                TAir=TAirtran,
                                defaultThin=0, 
                                ClCut=0)

print("Done.")
print("Initialising model with site type 5...")
# setting site type to 5
siteInfo[,3]=5
initPrebas_st5 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                                siteInfo = siteInfo,
                                multiInitVar = multiInitVar,
                                PAR = PARtran,
                                VPD = VPDtran,
                                CO2= CO2tran,
                                Precip=Preciptran,
                                TAir=TAirtran,
                                defaultThin=0,
                                ClCut=0)
print("Done.")
# run multisite model
modOut <- multiPrebas(initPrebas)
modOut_st1 <- multiPrebas(initPrebas_st1)
modOut_st5 <- multiPrebas(initPrebas_st5)

# get output
multiOut<-modOut$multiOut
multiOut_st1<-modOut_st1$multiOut
multiOut_st5<-modOut_st5$multiOut

fileName <- paste0(rdata_path, "multiOut_spID", speciesID, ".rdata")

save(multiOut,multiOut_st1,multiOut_st5, file=fileName)
print(paste0("multiOut saved to ", fileName))






