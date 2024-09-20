# This script is for creating the prebas siteInfo from soil data.

source('scripts/settings.R')
source('./r/multiSite.R')


soil_name <- "prebas_hwsd_data_fc_wp_depth"
soil_path <- paste0("data/soil/", soil_name, ".csv")
soil_dt <- fread(soil_path)


### -------- START hwsd only -------- ###

# Assign climate IDs
soil_dt[, climID := .GRP, by = PlgID] # Make sure these match the climate data

# Remove duplicated siteIDs
soil_dt <- soil_dt[!duplicated(soil_dt$PlgID)]

# Rename PlgID to siteID
setnames(soil_dt, "PlgID", "siteID")

### -------- END hwsd only -------- ###



# SITE INFO

print("Creating siteInfo...")

# Number of sites
nSites <- length(unique(soil_dt$siteID)) # What to do when PlgIDs are the same for different coords??

# # Number of simulation years
# nYears <- 2100-2015

# Soil parameters


# ### DEFAULTS FOR TESTING
# soilDepth <- 413.0
# WP <-  0.118
# FC <- 0.450

soilDepth <- soil_dt$soil_depth * 10
WP <-  soil_dt$WP / soilDepth
FC <- soil_dt$FC / soilDepth


# Create siteInfo matrix
siteID <- soil_dt[, "siteID"]
climID <- soil_dt[, "climID"]

# SiteType estimated
estimated_quantile <- quantile(soil_dt$N,c(0.15,0.40,0.9,0.98))
estimatedList <- list(config$VAR_estimated_user, estimated_quantile)
estimated <- estimatedList[[config$VAR_estimated_id]]
soil_dt$siteType_N <- cut(soil_dt$N,breaks = c(0,estimated,max(soil_dt$N+10)),labels = F)

# SiteInfo params
swInit <- rep(c(160), times=nSites)
zeros <- rep(c(0), times=nSites)
sInit <- rep(c(20), times=nSites)
nLayersCol <- rep(c(1), times=nSites)
nSpeciesCol <- rep(c(1), times=nSites)
# soilDepthCol <- rep(c(soilDepth), times=nSites)

# Create siteInfo
param_table <- cbind(siteID,climID,soil_dt$siteType_N,swInit,zeros,zeros,sInit,nLayersCol,nSpeciesCol,soilDepth,FC,WP)
siteInfo <- param_table

colnames(siteInfo) <- c("siteID", "climID", "siteType", "SWinit", "CWinit",
                        "SOGinit", "Sinit", "nLayers", "nSpecies", "soildepth",
                        "effective field capacity", "permanent wilting point")

print("Done.")

# Path
site_info_path <- paste0(config$PATH_site_info, soil_name, "_", config$VAR_estimated_names[config$VAR_estimated_id], ".rdata")
print(paste0("Saving site info into ", site_info_path))
save(siteInfo, file = site_info_path)



