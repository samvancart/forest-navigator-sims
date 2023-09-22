
source('settings.R')

# load csv as dataframe

# PARdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/PAR_tran.csv",header = T)
# VPDdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/VPD_tran.csv",header = T)
# CO2df <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/CO2_tran.csv",header = T)
# Precipdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/Precip_tran.csv",header = T)
# TAirdf <- read.csv("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/tran/TAir_tran.csv",header = T)

# load soilData (should load from settings.R)
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
# nLayersCol <- rep(c(nLayers), times=nSites)
nLayersCol <- rep(c(1), times=nSites)
# nSpeciesCol <- rep(c(nSpecies), times=nSites)
nSpeciesCol <- rep(c(1), times=nSites)
soilDepthCol <- rep(c(soilDepth), times=nSites)

# create siteInfo
siteInfo <- cbind(siteID,climID,soilData$siteType_N,swInit,zeros,zeros,sInit,nLayersCol,nSpeciesCol,soilDepthCol,FC,WP)

colnames(siteInfo) <- c("siteID", "climID", "siteType", "SWinit", "CWinit",
                        "SOGinit", "Sinit", "nLayers", "nSpecies", "soildepth",
                        "effective field capacity", "permanent wilting point")

siteInfo[,1] <- 1:nSites
siteInfo[,2] <- 1:nSites



# multiInitVar <- array(NA,dim=c(nSites,7,nLayers))
# multiInitVar <- array(NA,dim=c(nSites,7,49))
# multiInitVar[,1,] <- speciesID
# multiInitVar[,3,] <- initSeedling.def[1]; multiInitVar[,4,] <- initSeedling.def[2]
# multiInitVar[,5,] <- initSeedling.def[3]; multiInitVar[,6,] <- initSeedling.def[4]
# multiInitVar[,2,] <- 100

# multiInitVar[,2,] <- matrix(Ainits,nSites,maxNlayers)
# multiInitVar



# # NFI DATA
path_trees_as_layers <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/sorted_group_species_cIDs_basal_area.csv")
path_clusters_as_layers <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/cluster_weighted_means.csv")

df <- fread(path_trees_as_layers)
df_clusters <- fread(path_clusters_as_layers)

# Choose sites
df_nSites <- df_clusters %>%
  group_by(groupID) %>%
  filter(groupID<=nSites)


nLayers <- (df_nSites %>% count(groupID))$n
nSpecies <- (df_nSites %>% count(speciesID,groupID) %>% count(groupID))$n


siteInfo[,8] <- nLayers
siteInfo[,9] <- nSpecies

maxNlayers <- max(nLayers)



multiInitVar <- array(0,dim=c(nSites,7,maxNlayers))
multiInitVar[,6:7,NA]
for(i in 1:nSites){
  filtered <- df_nSites %>% filter(groupID==i)
  multiInitVar[i,1,1:nLayers[i]] <- filtered$speciesID # vector of species ID taken from data
  multiInitVar[i,2,1:nLayers[i]] <- filtered$Age # age by tree from NFI
  multiInitVar[i,3,1:nLayers[i]] <- filtered$Height # height from NFI data
  multiInitVar[i,4,1:nLayers[i]] <- filtered$Dbh # dbh from NFI data
  multiInitVar[i,5,1:nLayers[i]] <- filtered$basal_area # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
  multiInitVar[i,6,1:nLayers[i]] <- NA
}


# multiInitVar[,3,1] - multiInitVar[,6,1]
# 
# LcCheck <- multiInitVar[,3,] - multiInitVar[,6,]
# negLayers <- which(LcCheck<0 | is.na(LcCheck),arr.ind = T)
# siteXss <- unique(negLayers[,1])
# print(multiInitVar[siteXss,6,][negLayers])
# multiInitVar[siteXss,6,][negLayers]<- 0.1
# 
# heightCheck <- multiInitVar[,3,]
# naHeights <- do.call(cbind, lapply(heightCheck, is.na))
# multiInitVar[,3,][naHeights] <- 1
# 
# multiInitVar[,3,]
# 
# 
# pCROB
# pPREL
# pCROB[, multiInitVar[, 1,1]]
# pHcM

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

initPrebas$nLayers
siteInfo

# # setting site type to 1
# siteInfo[,3]=1
# initPrebas_st1 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
#   siteInfo = siteInfo,
#   multiInitVar = multiInitVar,
#   PAR = PARtran,
#   VPD = VPDtran,
#   CO2= CO2tran,
#   Precip=Preciptran,
#   TAir=TAirtran,
#   defaultThin=0, 
#   ClCut=0)
# 
# # setting site type to 5
# siteInfo[,3]=5
# initPrebas_st5 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
#   siteInfo = siteInfo,
#   multiInitVar = multiInitVar,
#   PAR = PARtran,
#   VPD = VPDtran,
#   CO2= CO2tran,
#   Precip=Preciptran,
#   TAir=TAirtran,
#   defaultThin=0,
#   ClCut=0)

# run multisite model
modOut <- multiPrebas(initPrebas)
# modOut_st1 <- multiPrebas(initPrebas_st1)
# modOut_st5 <- multiPrebas(initPrebas_st5)

modOut$multiOut[4,,11,,1]
dim(modOut$multiOut)

# get output
multiOut<-modOut$multiOut
# multiOut_st1<-modOut_st1$multiOut
# multiOut_st5<-modOut_st5$multiOut

fileName <- paste0("multiOut_spID",speciesID,".rdata")

save(multiOut, file=fileName)

