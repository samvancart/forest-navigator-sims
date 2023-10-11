source('scripts/settings.R')
source('./r/multiSite.R')

# Run multisite prebas for sitetypes 1, 5 and estimated site type (by N in soildata). Ids in settings.R.
# Produces multiOut_spID<speciesID> rdata file.
# Run for all species and both estimated N values from run.R.



# Get tran files

## Reading tran csv files is very slow so instead read df and create tran tables.

# # Get eobs df
# df <- as_tibble(read.csv(prebas_eobs_path, header=T))

# # Get gitlab df
# df <- as_tibble(read.csv(prebas_gitlab_path, header=T))


# PARtran <- data.matrix(get_prebas_tran(df, "par"))
# VPDtran <- data.matrix(get_prebas_tran(df, "vpd"))
# CO2tran <- data.matrix(get_prebas_tran(df, "co2"))
# Preciptran <- data.matrix(get_prebas_tran(df, "precip"))
# TAirtran <- data.matrix(get_prebas_tran(df, "tair"))



# soilData loaded from settings.R

## VPD from hPa to kPa
# VPDtran_kpa <- VPDtran*0.1

# Number of sites in this case matches the number of climIDs
nSites <- nrow(PARtran)

# Number of simulation years
nYears <- floor(ncol(PARtran)/365)

# Number of layers and species
nLayers <- nSpecies <- 1

# Soil parameters
WP <- soilData[,13]/1000
FC <- soilData[,12]/1000
soilDepth <- 1000

# Create siteInfo matrix
siteID <- soilData[,1]
climID <- soilData[,14]

# SiteType estimated
estimated <- estimatedList[[estimatedID]]
soilData$siteType_N <- cut(soilData$N,breaks = c(0,estimated,max(soilData$N+10)),labels = F)


swInit <- rep(c(160), times=nSites)
zeros <- rep(c(0), times=nSites)
sInit <- rep(c(20), times=nSites)
nLayersCol <- rep(c(1), times=nSites)
nSpeciesCol <- rep(c(1), times=nSites)
soilDepthCol <- rep(c(soilDepth), times=nSites)

# Create siteInfo
param_table <- cbind(siteID,climID,soilData$siteType_N,swInit,zeros,zeros,sInit,nLayersCol,nSpeciesCol,soilDepthCol,FC,WP)
siteInfo <- build_siteInfo(param_table)



# Create multiInitVar
multiInitVar <- array(NA,dim=c(nSites,7,nLayers))
multiInitVar[,1,] <- speciesID
multiInitVar[,3,] <- initSeedling.def[1]; multiInitVar[,4,] <- initSeedling.def[2]
multiInitVar[,5,] <- initSeedling.def[3]; multiInitVar[,6,] <- initSeedling.def[4]
multiInitVar[,2,] <- 100



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







