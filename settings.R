library(data.table)
library(Rprebasso)
library(ggplot2)
library(reshape2)

speciesID <- 4
defaultThin <- 0
ClCut <- 0
# number of layers and species
nLayers <- nSpecies <- 1
speciesNames <- c('Pine','Spruce', 'Birch', 'Beech')
