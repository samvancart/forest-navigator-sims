source('scripts/settings.R')
source('./r/multiSite.R')
source('./r/utils.R')

### Load data ###

# soil
soilData <- fread(config$PATH_soil_data)
estimated_quantile <- quantile(soilData$N,c(0.15,0.40,0.9,0.98))
estimatedList <- list(config$VAR_estimated_user, estimated_quantile)

# climate

# Get tran files

## Reading tran csv files is very slow so instead read df and create tran tables.

# if(config$VAR_data_from == "gitlab") {
#   print(paste0("Climate data is from ", config$VAR_data_from))
#   # Get gitlab df
#   df <- fread(config$VAR_climate_paths[config$VAR_climate_id], header = T)
#   print(paste0("Loaded ", config$VAR_climate_names[config$VAR_climate_id], " climate scenario."))
# } else if(config$VAR_data_from=="eobs") {
#   print(paste0("Climate data is from ", config$VAR_data_from))
#   # Get eobs df
#   df <- fread(config$PATH_prebas_eobs, header = T)
# } else {
#   df <- NULL
#   stop(paste0("'",config$VAR_data_from,"'"," is not a valid climate data source! Modify variable 'config$VAR_data_from' in settings.R."))
# 
# }


# print("Creating tran files...")
# PARtran <- get_prebas_tran_2(df, "par")
# VPDtran <- get_prebas_tran_2(df, "vpd")
# CO2tran <- get_prebas_tran_2(df, "co2")
# Preciptran <- get_prebas_tran_2(df, "precip")
# TAirtran <- get_prebas_tran_2(df, "tair")
# print("Done.")



# Load tran binaries

path_tran <- paste0(config$PATH_tran, config$VAR_climate_names[config$VAR_climate_id])
tran_files <- list.files(path_tran, full.names = T)

print(paste0("Loading tran files from ", path_tran))

# # Always load
# invisible(lapply(tran_files, function(x){
#   varName <- sub(".*\\/([^\\/]+)\\..*", "\\1", x)
#   print(paste0("Loading ", varName))
#   load(file = x, envir = .GlobalEnv)
# }))

# Load if not loaded
invisible(lapply(tran_files, function(x){
  varName <- sub(".*\\/([^\\/]+)\\..*", "\\1", x)
  if(config$VAR_load_tran_id == 0) {
    print(paste0("Loading ", varName))
    load(file = x, envir = .GlobalEnv)
  } else {
    print(paste0(varName, " already loaded."))
  }
}))

print("Done.")


print("Creating siteInfo...")

# siteInfo

# Number of sites in this case matches the number of climIDs
nSites <- nrow(parTran)

# Number of simulation years
nYears <- floor(ncol(parTran)/365)

# Soil parameters

# Filter siteIDs
soilData <- soilData[siteID %in% parTran[,1]]


### SOIL DEPTH WAS 1000. CHECK THIS!!! 
soilDepth <- soilData[,"soil depth"] * 10
WP <- soilData[,"WP"]/ 1000
FC <- soilData[,"FC"]/ 1000


# Create siteInfo matrix
siteID <- soilData[, "siteID"]
climID <- soilData[, "climID"]

# SiteType estimated
estimated <- estimatedList[[config$VAR_estimated_id]]
soilData$siteType_N <- cut(soilData$N,breaks = c(0,estimated,max(soilData$N+10)),labels = F)

# SiteInfo params
swInit <- rep(c(160), times=nSites)
zeros <- rep(c(0), times=nSites)
sInit <- rep(c(20), times=nSites)
nLayersCol <- rep(c(1), times=nSites)
nSpeciesCol <- rep(c(1), times=nSites)
# soilDepthCol <- rep(c(soilDepth), times=nSites)

# Create siteInfo
param_table <- cbind(siteID,climID,soilData$siteType_N,swInit,zeros,zeros,sInit,nLayersCol,nSpeciesCol,soilDepth,FC,WP)
siteInfo <- build_siteInfo(param_table)

print("Done.")
