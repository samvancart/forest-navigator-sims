library(data.table)
library(Rprebasso)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(factoextra)
library(hash)

layerID <- 2
speciesID <- 4
defaultThin <- 0
ClCut <- 0
# number of layers and species
# nLayers <- nSpecies <- 1
speciesNames <- c('Pine','Spruce', 'Birch', 'Beech')
layerNames <- c("Trees","Clusters")

# Load soilData
# soilData <- fread("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/soil/soil_data_wp_fc_gitlab_picus_prebas.csv")

# SiteType estimates either by user, or by quantile
estimatedID <- 2
estimated_user <- c(3.5,4.5,6,7)
estimated_quantile <- quantile(soilData$N,c(0.15,0.40,0.9,0.98))
estimatedList <- list(estimated_user, estimated_quantile)
estimatedNames <- c("User", "Quantile")

nfi_sweden_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/")
nfi_sweden_paths <- c(paste0(nfi_sweden_path,"sorted_group_species_cIDs_basal_area.csv"), paste0(nfi_sweden_path,"cluster_weighted_means.csv"))

rdata_path <- "data/rdata/"

