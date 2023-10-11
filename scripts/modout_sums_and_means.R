
# Sums and averages of modOut vars to tabX_sums_means 

# Run multiSiteLayers.R first to get modOut!!!

source('scripts/settings.R')


fileName <- (paste0(rdata_path, "modOut_",layerNames[layerID],".rdata"))
load(fileName)


# Set varNames
varNames <- as.vector(unlist(dimnames(modOut$multiOut)[3]))

# NFI DATA
nfi_path <- nfi_sweden_paths[layerID]
df <- fread(nfi_path)

# Choose sites
df_nSites <- df %>%
  group_by(groupID) %>%
  filter(groupID<=nSites)


nLayers <- (df_nSites %>% count(groupID))$n
maxNlayers <- max(nLayers)

# Define variables
varXs_means <- c(11,12)
varXs_sums <- c(13,14,17,18,30,43)

# Get Lc
lc <- modOut$multiOut[,,11,,1] - modOut$multiOut[,,14,,1]

# Get sums
li <- list()
names_li <- list()
for(i in 1:length(varXs_sums)){
  sum <- apply(modOut$multiOut[,,varXs_sums[i],,1],1:2,sum)
  li[[i]] <- sum
  names_li <- append(names_li, varNames[varXs_sums[i]])
}

# Add Lc
sum <-apply(lc,1:2,sum)
li[[length(li)+1]] <- sum
names_li[length(names_li)+1] <- "Lc"


# Non existent layers to NA
for (i in 1:length(nLayers)) {
  minLayers <- nLayers[i]+1
  site <- i
  if((minLayers)<maxNlayers) {
    modOut$multiOut[site,,,minLayers:maxNlayers,] = NA
  }
}


# Get baWmeans
for(i in 1:length(varXs_means)){
  baWmean <- baWmean(modOut,varXs_means[i])
  li[[length(li)+1]] <- baWmean
  names_li[length(names_li)+1] <- varNames[varXs_means[i]]
}

# Build tabX
tabX <- data.table()
for (i in 1:length(li)) {
  tabXx <- data.table(melt(li[[i]]))
  tabXx$variable <- names_li[i]
  tabXx$LayerType <- layerID
  tabXx$LayerName <- layerNames[layerID]
  tabX <- rbind(tabXx,tabX)
}


# Write rdata
fileName <- paste0(rdata_path, "tabX_sums_means_", layerNames[layerID],".rdata")
save(tabX, file=fileName)




