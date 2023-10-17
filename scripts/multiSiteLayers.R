source('scripts/settings.R')
source('scripts/loadData.R')


# Run multisite prebas for different layers (trees or clusters) based on NFI data. LayerIds in settings.R.
# Produces modOut_<layer name> and and multiOut_<layer name> rdata files.
# Run for both layers from run.R.


### Climate data loaded from loadData.R ###
### Soil data loaded from loadData.R ###
### SiteInfo created in loadData.R ###

print(paste0("Running multiSiteLayers.R for layer ", layerNames[layerID]))

# NFI DATA
nfi_path <- nfi_sweden_paths[layerID]
df <- fread(nfi_path)

print(paste0("NFI path is ", nfi_path))

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

print("Initialising model...")
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

print("Done.")


# Save as rdata
fileName <- paste0(rdata_path, "initPrebas_", layerNames[layerID],".rdata")
save(initPrebas, file=fileName)
print(paste0("initPrebas saved to ",fileName))


# Run multisite model
modOut <- multiPrebas(initPrebas)

# Save as rdata
fileName <- paste0(rdata_path, "modOut_", layerNames[layerID],".rdata")
save(modOut, file=fileName)
print(paste0("modOut saved to ",fileName))

# Get multiOut output
multiOut<-modOut$multiOut

# Save as rdata
fileName <- paste0(rdata_path, "multiOut_", layerNames[layerID],".rdata")
save(multiOut, file=fileName)
print(paste0("multiOut saved to ", fileName))





