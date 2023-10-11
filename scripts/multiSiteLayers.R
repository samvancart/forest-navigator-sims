source('scripts/settings.R')
source('./r/multiSite.R')


# Run multisite prebas for different layers (trees or clusters) based on NFI data. Ids in settings.R.
# Produces modOut_<layer name> and and multiOut_<layer name> rdata files.
# Run for both layers from run.R.


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
## Run only once!!!
# VPDtran <- VPDtran*0.1

# fileName <- paste0(rdata_path, "weather_inputs",".rdata")
# save(PARtran,VPDtran_kpa,CO2tran,Preciptran,TAirtran, file=fileName)

#number of sites in this case matches the number of climIDs
nSites <- nrow(PARtran)

#number of simulation years
nYears <- floor(ncol(PARtran)/365)

# Soil parameters
WP <- soilData[,13]/1000
FC <- soilData[,12]/1000
soilDepth <- 1000

# Create siteInfo matrix
siteID <- soilData[,1]
climID <- soilData[,14]
#
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


# NFI DATA
nfi_path <- nfi_sweden_paths[layerID]
df <- fread(nfi_path)

# Choose sites
df_nSites <- df %>%
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


# Initialise model
### Using siteType estimate based on N
initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),
  siteInfo = siteInfo,
  multiInitVar = multiInitVar,
  PAR = PARtran,
  VPD = VPDtran, # Check VPD or VPDtran_kpa
  CO2= CO2tran,
  Precip=Preciptran,
  TAir=TAirtran,
  defaultThin=0,
  ClCut=0)

initPrebas$nLayers
siteInfo


# Run multisite model
modOut <- multiPrebas(initPrebas)

# Save as rdata
fileName <- paste0(rdata_path, "modOut_", layerNames[layerID],".rdata")
save(modOut, file=fileName)


# Get multiOut output
multiOut<-modOut$multiOut

# Save as rdata
fileName <- paste0(rdata_path, "multiOut_", layerNames[layerID],".rdata")
save(multiOut, file=fileName)






