source('scripts/settings.R')
source('./r/multiSite.R')
source('./r/utils.R')

### LOAD DATA ###


# CLIMATE

# Tran paths
path_tran <- paste0(config$PATH_tran, config$VAR_climate_names[config$VAR_climate_id])
tran_files <- list.files(path_tran, full.names = T)
pattern_tran <- paste0("_", config$VAR_split_id, "\\.RData$")
tran_files_split_id <- grep(pattern_tran, tran_files, value = TRUE)


print(paste0("Loading tran files from ", path_tran))

# Load tran binaries
load_files(tran_files_split_id, config$VAR_load_tran_id)

print("Done.")




# SITE INFO

print("Loading siteInfo...")

# Sites to run
sites <- parTran[,1]

# Number of sites in this case matches the number of climIDs
nSites <- nrow(parTran)

# Number of simulation years
nYears <- floor(ncol(parTran)/365)

# SiteInfo name
siteInfo_name <- config$VAR_site_info_names[config$VAR_site_info_id]

# Estimated name
estimated_name <- config$VAR_estimated_names[config$VAR_estimated_id]

# siteInfo path
siteInfo_path <- paste0(config$PATH_site_info, siteInfo_name, "_", estimated_name, ".rdata")

# Load siteInfo
siteInfo <- loadRDataFile(siteInfo_path)[siteID %in% sites]


print("Done.")

