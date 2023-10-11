source('scripts/settings.R')

# multiOut vars to tabX_layer

# Run multiSiteLayers.R first to get multiOut!!!


# Functions


# Params:
# tabX = Initial data table
# varMatrix = Variable matrix (eg. multiOut[,,11,,1] = H)
# varName = Variable name as string
# speciesTable = table with species to merge with
build_long_format_table <- function(tabX,varMatrix,varName,speciesTable) {
  tabXx <- data.table(melt(varMatrix))
  tabXx$variable <- varName
  tabXx <- merge(tabXx,tabXsp)
  tabX <- rbind(tabXx,tabX)
  return(tabX)
}


format_table <- function(tabX) {
  tabX$site <- as.factor(tabX$site)
  tabX$layer <- as.factor(tabX$layer)
  tabX$species <- as.factor(tabX$species)
  
  # Set layer col values to integers
  tabX$layer <- as.integer(tabX$layer)
  
  return (tabX)
}


# NFI DATA
nfi_path <- nfi_sweden_paths[layerID]
df <- fread(nfi_path)

# Choose sites
df_nSites <- df %>%
  group_by(groupID) %>%
  filter(groupID<=nSites)


nLayers <- (df_nSites %>% count(groupID))$n



fileName <- (paste0(rdata_path, "multiOut_",layerNames[layerID],".rdata"))
load(fileName)

print(fileName)
print(nLayers)

# species <- speciesNames[speciesID]
speciesNames <- colnames(pCROB)

estimatedName <- estimatedNames[estimatedID]

# Define variables
varXs <- c(11:14,17,18,30,43)

# Build species table
tabXsp<- data.table(melt(multiOut[,,4,,1]))
setnames(tabXsp,c("site","year","layer","species"))
tabXsp$species <- speciesNames[tabXsp$species]

# Length of crown: H-Hc
lc <- multiOut[,,11,,1] - multiOut[,,14,,1]

# Initialise tabX
tabX <- data.table()

# Variables to long format table
for (i in varXs) {
  tabX <- build_long_format_table(tabX,multiOut[,,i,,1],varNames[i],tabXsp)
}

# Add Lc to table
tabX <- build_long_format_table(tabX,lc,"Lc",tabXsp)


# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

# Add Lc to names
varNames[55] = "Lc"

# Add lc to varXs
varXs <- append(varXs,55)

# Format tabX
tabX <- format_table(tabX)

# Remove non existent layers
tabX <- filter(tabX,layer<=nLayers[site])

species21 <- filter(tabX,site==21)
print(unique(species21$species))


# Write rdata
fileName <- paste0(rdata_path, "tabX_", layerNames[layerID],".rdata")
save(tabX, file=fileName)

