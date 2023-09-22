library(data.table)
library(Rprebasso)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(factoextra)
library(hash)

speciesID <- 4
defaultThin <- 0
ClCut <- 0
# number of layers and species
# nLayers <- nSpecies <- 1
speciesNames <- c('Pine','Spruce', 'Birch', 'Beech')


# load soilData
# soilData <- fread("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/soil/soil_data_wp_fc_gitlab_picus_prebas.csv")

# SiteType estimates either by user, or by quantile
estimatedID <- 1
estimated_user <- c(3.5,4.5,6,7)
estimated_quantile <- quantile(soilData$N,c(0.15,0.40,0.9,0.98))
estimatedList <- list(estimated_user, estimated_quantile)
estimatedNames <- c("User", "Quantile")
