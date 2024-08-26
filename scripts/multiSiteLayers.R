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

print(paste0("Running multiSiteLayers.R for layer ", config$VAR_layer_names[config$VAR_layer_id]))
print(paste0("Management: ", config$VAR_management_names[config$VAR_management_id+1]))

# NFI DATA
nfi_path <- config$VAR_nfi_sweden_paths[config$VAR_layer_id]
dt <- fread(nfi_path)

print(paste0("NFI path is ", nfi_path))


# Choose sites
dt_nSites <- dt[groupID %in% unique(groupID)[1:nSites]]
# df_nSites <- df
nSites <- length(unique(dt_nSites$groupID))


nLayers <- dt_nSites[, .N, by = groupID]$N
nSpecies <- dt_nSites[, .N, by = c("speciesID","groupID")][, .N, by = groupID]$N
 
### CHECK IF MODIFICATIONS TO PRELES AND CROBAS ARE NECESSARY ###

# Get pPRELES parameter (different for speciesID 12)
pPRELES <- get_pPRELES(config$VAR_species_id)



speciesIDs = as.numeric(names(config$VAR_species_dict))
# speciesIDs = c(1,2,15,3)
# speciesIDs <- unique(df_nSites$speciesID)

pCROB_copy <- get_pCROBAS(speciesIDs = speciesIDs, pCROBAS_multipliers = config$VAR_pCROBAS_multipliers, pCROB = pCROB)
pCROB[17,]
pCROB_copy[17,]


siteInfo[,8] <- nLayers
siteInfo[,9] <- nSpecies

maxNlayers <- max(nLayers)


# # DATA TABLE multiInitVar
# dt_nSites <- as.data.table(df_nSites)
# dt_nSites[, miv_layerID := seq(.N), by = c("groupID")]
# nLayers2 <- dt_nSites[, max(miv_layerID), by = c("groupID")]$V1



# Initialize the multiInitVar array
multiInitVar <- array(0, dim=c(nSites, 7, maxNlayers))

multiInitVar[,6:7,NA] # Redundant?

system.time({
# Split the data.table by groupID
  split_data <- split(dt_nSites, by = "groupID")

# Apply the process_subset function to each subset
  results <- lapply(seq_along(split_data), function(i) process_subset(split_data[[i]], i))
  for (i in seq_along(results)) {
    multiInitVar[i, 1, 1:nLayers[i]] <- results[[i]]$speciesID # vector of species ID taken from data
    multiInitVar[i, 2, 1:nLayers[i]] <- results[[i]]$Age # age by tree from NFI
    multiInitVar[i, 3, 1:nLayers[i]] <- results[[i]]$Height # height from NFI data
    multiInitVar[i, 4, 1:nLayers[i]] <- results[[i]]$Dbh # dbh from NFI data
    multiInitVar[i, 5, 1:nLayers[i]] <- results[[i]]$basal_area # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
    multiInitVar[i, 6, 1:nLayers[i]] <- results[[i]]$NA_values
  }
})







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
  defaultThin=config$VAR_management_id,
  ClCut=config$VAR_management_id)

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
fileName <- paste0(config$PATH_rdata, "initPrebas_", config$VAR_layer_names[config$VAR_layer_id],".rdata")
save(initPrebas, file=fileName)
print(paste0("initPrebas saved to ",fileName))


# Run multisite model
modOut <- multiPrebas(initPrebas)

# Save as rdata
fileName <- paste0(config$PATH_rdata, "modOut_", config$VAR_layer_names[config$VAR_layer_id],".rdata")
save(modOut, file=fileName)
print(paste0("modOut saved to ",fileName))

# Get multiOut output
multiOut<-modOut$multiOut

# Save as rdata
fileName <- paste0(config$PATH_rdata, "multiOut_", config$VAR_layer_names[config$VAR_layer_id],".rdata")
save(multiOut, file=fileName)
print(paste0("multiOut saved to ", fileName))



### TEST START ### 


# modOut_original <- multiPrebas(initPrebas)
# mod_path <- paste0(config$PATH_rdata, "modOut_", config$VAR_layer_names[config$VAR_layer_id],".rdata")
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





