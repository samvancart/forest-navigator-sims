source('scripts/settings.R')
source('./r/multiSite.R')


### Load data ###

# soil
soilData <- fread(soilData_path)
estimated_quantile <- quantile(soilData$N,c(0.15,0.40,0.9,0.98))
estimatedList <- list(estimated_user, estimated_quantile)



# climate

# Get tran files

## Reading tran csv files is very slow so instead read df and create tran tables.

# if(data_from == "gitlab") {
#   print(paste0("Climate data is from ", data_from))
#   # Get gitlab df
#   df <- fread(prebas_gitlab_path, header = T)
# } else if(data_from=="eobs") {
#   print(paste0("Climate data is from ", data_from))
#   # Get eobs df
#   df <- fread(prebas_eobs_path, header = T)
# } else {
#   df <- NULL
#   stop(paste0("'",data_from,"'"," is not a valid climate data source! Modify variable 'data_from' in settings.R."))
# 
# }
# 
# 
# print("Creating tran files...")
# PARtran <- get_prebas_tran_2(df, "par")
# VPDtran <- get_prebas_tran_2(df, "vpd")
# CO2tran <- get_prebas_tran_2(df, "co2")
# Preciptran <- get_prebas_tran_2(df, "precip")
# TAirtran <- get_prebas_tran_2(df, "tair")
# print("Done.")


print("Creating siteInfo...")

# siteInfo

# Number of sites in this case matches the number of climIDs
nSites <- nrow(PARtran)

# Number of simulation years
nYears <- floor(ncol(PARtran)/365)

# Soil parameters

### SOIL DEPTH WAS 1000. CHECK THIS!!! 
soilDepth <- soilData[,"soil depth"] * 10
WP <- soilData[,"WP"]/ 1000
FC <- soilData[,"FC"]/ 1000


# Create siteInfo matrix
siteID <- soilData[, "siteID"]
climID <- soilData[, "climID"]

# SiteType estimated
estimated <- estimatedList[[estimatedID]]
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