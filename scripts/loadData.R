# This script is for loading climate data (TRAN files) and site info data for running
# prebas. Should be called from multiSiteSpecies.R and multiSiteLayers.R. TRAN files are
# loaded according to VAR_climate_id and VAR_split_id in config.


source('scripts/settings.R')

load_data_env <- new.env()
source('./r/multiSite.R', local = load_data_env)
source('./r/utils.R', local = load_data_env)

### LOAD DATA ###

local(envir = load_data_env, {
  
  # CLIMATE
  
  # Tran paths
  path_tran <- paste0(config$PATH_tran, config$VAR_climate_names[config$VAR_climate_id])
  tran_files <- list.files(path_tran, full.names = T)
  pattern_tran <- paste0("_", config$VAR_split_id, "\\.RData$")
  tran_files_split_id <- grep(pattern_tran, tran_files, value = TRUE)
  
  
  print(paste0("Loading tran files from ", path_tran))
  
  # Load tran binaries
  load_data_env$load_files(tran_files_split_id, config$VAR_load_tran_id)
  
  print("Done.")


  
  
  # SITE INFO
  
  print("Loading siteInfo...")
  
  # SiteInfo name
  siteInfo_name <- config$VAR_site_info_names[config$VAR_site_info_id]
  
  # Estimated name
  estimated_name <- config$VAR_estimated_names[config$VAR_estimated_id]
  
  # siteInfo path
  siteInfo_path <- paste0(config$PATH_site_info, siteInfo_name, "_", estimated_name, ".rdata")
  
})

# Sites to run
sites <- parTran[,1]

# Number of sites in this case matches the number of climIDs
nSites <- nrow(parTran)

# Number of simulation years
nYears <- floor(ncol(parTran)/365)

# Load siteInfo
siteInfo <- load_data_env$loadRDataFile(load_data_env$siteInfo_path)[siteID %in% sites]

# Add new climate ids
siteInfo[, climID := .GRP, by = siteID]


print("Done.")


# CLEAN UP

rm(load_data_env)
gc()





