
# Sums and averages of modOut vars to tabX_sums_means 

# Run multiSiteLayers.R first to get modOut!!!

source('scripts/settings.R')

print(paste0("Running modout_sums_and_means.R for layer ", layerNames[VAR_layer_id]))

fileName <- (paste0(rdata_path, "modOut_",layerNames[VAR_layer_id],".rdata"))
load(fileName)


# Set varNames
varNames <- as.vector(unlist(dimnames(modOut$multiOut)[3]))

# NFI DATA
nfi_path <- nfi_sweden_paths[VAR_layer_id]
df <- fread(nfi_path)

# Choose sites
df_nSites <- df %>%
  group_by(groupID) %>%
  filter(groupID<=nSites)


nLayers <- (df_nSites %>% count(groupID))$n
maxNlayers <- max(nLayers)

# Define variables
varXs_means <- c(11,12,14)
varXs_sums <- c(13,17,18,30,43)

li <- list() # List of all variable matrices
names_li <- list() # List of variable names

# Get sums
for(i in 1:length(varXs_sums)){
  varX_sum <- apply(modOut$multiOut[,,varXs_sums[i],,1],1:2, sum)
  li[[i]] <- varX_sum
  names_li <- append(names_li, varNames[varXs_sums[i]])
}


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


# Add Lc (Length of crown = Height - Hc_base)
lc_means <- li[[which(names_li=="H")]] - li[[which(names_li=="Hc_base")]]
li[[length(li)+1]] <- lc_means
names_li[length(names_li)+1] <- "Lc"


# Build tabX
tabX <- data.table()
for (i in 1:length(li)) {
  tabXx <- data.table(melt(li[[i]]))
  tabXx$variable <- names_li[i]
  tabXx$LayerType <- VAR_layer_id
  tabXx$LayerName <- layerNames[VAR_layer_id]
  tabX <- rbind(tabXx,tabX)
}


# Write rdata
fileName <- paste0(rdata_path, "tabX_sums_means_", layerNames[VAR_layer_id],".rdata")
save(tabX, file=fileName)
print(paste0("tabX saved to ", fileName))



