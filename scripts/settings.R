
### Libraries ###

library(data.table)
library(Rprebasso)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(factoextra)
library(hash)
library(lubridate)
library(ncdf4)
library(geosphere)
library(zoo)
library(parallel)
library(foreach)

### Variables ###

layerID <- 2
speciesID <- 1
defaultThin <- 0
ClCut <- 0
speciesNames <- c('Pine','Spruce', 'Birch', 'Beech')
layerNames <- c("Trees","Clusters")
# SiteType estimates either by user, or by quantile
estimatedID <- 1
estimated_user <- c(3.5,4.5,6,7)
estimatedNames <- c("User", "Quantile")

# Where climate data is from.
data_from <- "gitlab"

### Paths ###

# soil
soilData_path <- "data/soil/soil_data_wp_fc_gitlab_picus_prebas.csv"

# climate
historical_climate_data_gitlab_path = paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/historical_climate_data.csv")
prebas_gitlab_path <- paste0("data/climate/provided/historical_only_prebas_picus_sites_vpd_corrected.csv")
prebas_eobs_path <- paste0("data/climate/extracted/eobs/leap_years_sampled_eobs_prebas.csv")

# nfi
nfi_sweden_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/")
nfi_sweden_paths <- c(paste0(nfi_sweden_path,"sorted_group_species_cIDs_basal_area.csv"), paste0(nfi_sweden_path,"cluster_weighted_means.csv"))

# rdata
rdata_path <- "data/rdata/"

# netcdf
chelsa_file <- "chelsa_1979_2016_all_vars"
chelsa_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/netcdf/CHELSA_EU/combined/all/",chelsa_file,".nc")

eobs_file <- "eobs_1979_2022_all_vars"
eobs_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/sweden-may23/data/netcdf/combined/all/",eobs_file,".nc")
eobs_var <- "hu"
eobs_folder <- paste0("C:/Users/samu/Documents/yucatrote/projects/sweden-may23/data/netcdf/vars/",eobs_var,"/")

# sites
sites_path <- paste0("data/climate/provided/prebas_sites_coords.csv")



