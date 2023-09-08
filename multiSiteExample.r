
source('settings.R')

# load csv as dataframe

# PARdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/PAR_tran.csv",header = T)
# VPDdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/VPD_tran.csv",header = T)
# CO2df <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/CO2_tran.csv",header = T)
# Precipdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/Precip_tran.csv",header = T)
# TAirdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/TAir_tran.csv",header = T)

# load soilData
# soilData <- fread("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/soil/soil_data_wp_fc_gitlab_picus_prebas.csv")

# dataframe to matrix

# PARtran <- data.matrix(PARdf)
# VPDtran <- data.matrix(VPDdf)
# CO2tran <- data.matrix(CO2df)
# Preciptran <- data.matrix(Precipdf)
# TAirtran <- data.matrix(TAirdf)


#number of sites in this case matches the number of climIDs
nSites <- nrow(PARtran)

#number of simulation years
nYears <- floor(ncol(PARtran)/365)

# ?InitMultiSite()

# soil parameters

WP <- soilData[,13]/1000
FC <- soilData[,12]/1000
soilDepth <- 1000

# create siteInfo matrix
siteID <- soilData[,1]
climID <- soilData[,14]

# SiteType estimated
estimated <- estimatedList[[estimatedID]]
soilData$siteType_N <- cut(soilData$N,breaks = c(0,estimated,max(soilData$N+10)),labels = F)


swInit <- rep(c(160), times=nSites)
zeros <- rep(c(0), times=nSites)
sInit <- rep(c(20), times=nSites)
nLayersCol <- rep(c(nLayers), times=nSites)
nSpeciesCol <- rep(c(nSpecies), times=nSites)
soilDepthCol <- rep(c(soilDepth), times=nSites)

# create siteInfo
siteInfo <- cbind(siteID,climID,soilData$siteType_N,swInit,zeros,zeros,sInit,nLayersCol,nSpeciesCol,soilDepthCol,FC,WP)

colnames(siteInfo) <- c("siteID", "climID", "siteType", "SWinit", "CWinit",
                        "SOGinit", "Sinit", "nLayers", "nSpecies", "soildepth",
                        "effective field capacity", "permanent wilting point")

siteInfo[,1] <- 1:nSites
siteInfo[,2] <- 1:nSites


multiInitVar <- array(NA,dim=c(nSites,7,nLayers))
multiInitVar[,1,] <- speciesID
multiInitVar[,3,] <- initSeedling.def[1]; multiInitVar[,4,] <- initSeedling.def[2]
multiInitVar[,5,] <- initSeedling.def[3]; multiInitVar[,6,] <- initSeedling.def[4]
multiInitVar[,2,] <- 100
# multiInitVar[,2,] <- matrix(Ainits,nSites,maxNlayers)
# multiInitVar

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

fileName <- paste0("multiOut_spID",speciesID,".rdata")

save(multiOut,multiOut_st1,multiOut_st5, file=fileName)

