
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
library(doParallel)
library(foreach)
library(ggpubr)
library(sf)
library(foreign)
library(stars)
library(geoTS)
library(parallelly)
library(yaml)




### Variables ###
speciesID <- 12
speciesDict <- c('1' = 'Pine', '2' = 'Spruce', '3' = 'Birch', '12' = 'Beech')
speciesNames <- c('Pine','Spruce', 'Birch', 'Beech')
speciesCodes <- c('pinsy', 'picab', 'betpe', 'fagsy')
layerNames <- c("Trees","Clusters")
layerID <- 2

# pCROBAS multipliers
pCROBAS_multipliers <- c('1'=1.3, '2'=1.3, '3'=0.6, '12'=1)
thetaMax <- 0.03 # CHECK THIS

# SiteType estimates either by user, or by quantile
estimatedID <- 1
estimated_user <- c(3.5,4.5,6,7)
estimatedNames <- c("User", "Quantile")
tabXNames <- c("layer","layerAggr")
tabXID <- 1

# # Management 
managementID <- 1
managementNames <- c("noManagement", "managed")

# Output variable names from comparison protocol
outputNames <- c('hei', 'dg', 'ba', 'dens', 'npp', 'et', 'vol', 'mort', 'inc')

# Where climate data is from.
data_from <- "gitlab"

### Paths ###

# soil
# soilData_path <- "data/soil/original_with_soil_depth_1000.csv"
soilData_path <- "data/soil/grd5_soil_data_prebas.csv"

# climate
tranPath <- paste0("data/climate/tran/")
historical_climate_data_gitlab_path = paste0("data/climate/provided/historical_climate_data.csv")
prebas_gitlab_path <- paste0("data/climate/provided/historical_only_prebas_picus_sites_vpd_corrected.csv")
prebas_future_GFDL_ESM4_SSP370 <- paste0("data/climate/provided/GFDL-ESM4_SSP370_prebas.csv")
prebas_future_UKESM1_0_LL_ssp370 <- paste0("data/climate/provided/UKESM1-0-LL_ssp370_prebas.csv")
prebas_historical_detrend <- paste0("data/climate/provided/historical_detrend_climate_data_prebas.csv")
prebas_eobs_path <- paste0("data/climate/extracted/eobs/leap_years_sampled_eobs_prebas.csv")

climateID <- 3
climate_paths <- c(prebas_gitlab_path, prebas_future_GFDL_ESM4_SSP370, prebas_future_UKESM1_0_LL_ssp370, prebas_historical_detrend)
climateNames <- c("historical", "GFDL-ESM4_SSP370", "UKESM1-0-LL_ssp370", "historical_detrend")

# nfi
nfi_sweden_path <- paste0("data/nfi/sweden/")
nfi_sweden_paths <- c(paste0(nfi_sweden_path,"sorted_group_species_cIDs_speciesID11to4_basal_area.csv"), 
                      paste0(nfi_sweden_path,"cluster_weighted_means_speciesID11to4.csv"))

# rdata
rdata_path <- "data/rdata/"

# netcdf
chelsa_file <- "chelsa_1979_2016_all_vars"
chelsa_path <- paste0("data/netcdf/combined/all/",chelsa_file,".nc")

eobs_file <- "eobs_1979_2022_all_vars"
eobs_path <- paste0("data/netcdf/combined/all/",eobs_file,".nc")
eobs_var <- "hu"
eobs_folder <- paste0("data/netcdf/eobs_vars/",eobs_var,"/")

# sites
sites_path <- paste0("data/climate/provided/prebas_sites_coords.csv")



