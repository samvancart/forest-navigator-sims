source('scripts/settings.R')
source('scripts/loadData.R')
source('./r/utils.R')
source('./r/multiSite.R')


# Run multisite prebas for different layers (trees or clusters) based on NFI data. LayerIds in settings.R.
# Produces modOut_<layer name> and and multiOut_<layer name> rdata files.
# Run for both layers from run.R.


### Climate data loaded from loadData.R ###
### Soil data loaded from loadData.R ###
### SiteInfo created in loadData.R ###
### nSites from loadData.R ###

print(paste0("Running multiSiteLayers.R for layer ", layerNames[layerID]))
print(paste0("Management: ", managementNames[managementID+1]))

# NFI DATA
nfi_path <- nfi_sweden_paths[layerID]
df <- fread(nfi_path)

print(paste0("NFI path is ", nfi_path))


# Choose sites
df_nSites <- get_df_nSites(df, nSites)
# df_nSites <- df
nSites <- length(unique(df_nSites$groupID))


nLayers <- (df_nSites %>% count(groupID))$n
nSpecies <- (df_nSites %>% count(speciesID,groupID) %>% count(groupID))$n
 
### CHECK IF MODIFICATIONS TO PRELES AND CROBAS ARE NECESSARY ###

# Get pPRELES parameter (different for speciesID 12)
# pPRELES <- get_pPRELES(speciesID)



speciesIDs = as.numeric(names(speciesDict))
# speciesIDs = c(1,2,15,3)
# speciesIDs <- unique(df_nSites$speciesID)

pCROB_copy <- get_pCROBAS(speciesIDs = speciesIDs, pCROBAS_multipliers = pCROBAS_multipliers, pCROB = pCROB)
pCROB[17,]
pCROB_copy[17,]


siteInfo[,8] <- nLayers
siteInfo[,9] <- nSpecies

maxNlayers <- max(nLayers)


# # DATA TABLE multiInitVar
# dt_nSites <- as.data.table(df_nSites)
# dt_nSites[, miv_layerID := seq(.N), by = c("groupID")]
# nLayers2 <- dt_nSites[, max(miv_layerID), by = c("groupID")]$V1


multiInitVar <- array(0, dim=c(nSites,7,maxNlayers))
multiInitVar[,6:7,NA] # Redundant?
system.time(
  for(i in 1:nSites){
    filtered <- df_nSites %>% filter(groupID==i)
    multiInitVar[i,1,1:nLayers[i]] <- filtered$speciesID # vector of species ID taken from data
    multiInitVar[i,2,1:nLayers[i]] <- filtered$Age # age by tree from NFI
    multiInitVar[i,3,1:nLayers[i]] <- filtered$Height # height from NFI data
    multiInitVar[i,4,1:nLayers[i]] <- filtered$Dbh # dbh from NFI data
    multiInitVar[i,5,1:nLayers[i]] <- filtered$basal_area # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
    multiInitVar[i,6,1:nLayers[i]] <- NA
  }
)





print("Initialising model...")
# Initialise model
### Using siteType estimate based on N
initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),
  siteInfo = as.matrix(siteInfo),
  multiInitVar = multiInitVar,
  pPRELES = pPRELES,
  PAR = parTran,
  VPD = vpdTran, # Check VPD or VPDtran_kpa
  CO2= co2Tran,
  Precip=precipTran,
  TAir=tairTran,
  defaultThin=managementID,
  ClCut=managementID)

print("Done.")


### TEST START ### 


# siteInfo1 <- siteInfo2 <- as.matrix(siteInfo)
# siteInfo1[,10:12] <- modOut_original$siteInfo[,8:10]
# siteInfo2[,10:12] <- modOut$siteInfo[,8:10]
# 
# siteInfo2[,10] <- 10
# 
# initPrebas1 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
#                             siteInfo = as.matrix(siteInfo1),
#                             multiInitVar = multiInitVar,
#                             PAR = PARtran,
#                             VPD = VPDtran, # Check VPD or VPDtran_kpa
#                             CO2= CO2tran,
#                             Precip=Preciptran,
#                             TAir=TAirtran,
#                             defaultThin=0,
#                             ClCut=0)
# 
# initPrebas2 <- InitMultiSite(nYearsMS = rep(nYears,nSites),
#                             siteInfo = as.matrix(siteInfo2),
#                             multiInitVar = multiInitVar,
#                             PAR = PARtran,
#                             VPD = VPDtran, # Check VPD or VPDtran_kpa
#                             CO2= CO2tran,
#                             Precip=Preciptran,
#                             TAir=TAirtran,
#                             defaultThin=0,
#                             ClCut=0)
# 
# 
# 
# modOut1 <- multiPrebas(initPrebas1)
# modOut2 <- multiPrebas(initPrebas2)
# 
# cbind(modOut1$multiOut[3,,40,1,1], modOut2$multiOut[3,,40,1,1])
# 
# cbind(modOut1$dailyPRELES[3,150:200,3], modOut2$dailyPRELES[3,150:200,3])


### TEST END ### 



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



### TEST START ### 


# modOut_original <- multiPrebas(initPrebas)
# mod_path <- paste0(rdata_path, "modOut_", layerNames[layerID],".rdata")
# load(file=mod_path)
# 
# modOut_original$siteInfo
# modOut$siteInfo
# 
# cbind(rowSums(modOut_original$multiOut[3,,30,,1]), rowSums(modOut$multiOut[3,,30,,1]))
# 
# modOut_original$P0y[2,,1]
# modOut$P0y[2,,1]
# 
# cbind(modOut_original$multiOut[3,,40,1,1], modOut$multiOut[3,,40,1,1])

### TEST END ### 





