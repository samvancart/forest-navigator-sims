source('scripts/settings.R')
source('./r/cluster_aggregates.R')
source('./r/utils.R')

# Before running: Run multiSiteLayers.R to get modOut

# Calculates aggregated values for prebas output variables

# 1. Gets modOut
# 2. Associates clusterIDs from NFI input data with the prebas output data
# to find out which individual trees make up a layer in the clustered data
# 3. For each layer, calculates yearly basal area weighted means for variables "H", "D" and "Hc_base"
# 4. For each layer, calculates yearly sums for variables "BA", "N", "npp", "V" and "grossGrowth"
# 5. Adds "Lc" to data

# Note: Running for clustered data should produce a table which has "Lc" column added but is otherwise unchanged

print(paste0("Running layerAggr.R for layer ", layerNames[VAR_layer_id]))

# Define variables
varXs <- c(11:14,17,18,30,43)

# Load tabX
fileName <- (paste0(rdata_path, "modOut_",layerNames[VAR_layer_id],".rdata"))
load(fileName)
tabX <- data.table(melt(modOut$multiOut[,,varXs,,1]))

# Load speciesNames
speciesNames <- colnames(pCROB)

# NFI DATA
nfi_path <- nfi_sweden_paths[VAR_layer_id]
df <- fread(nfi_path)

print(paste0("NFI path is ", nfi_path))

# Get nSites
nSites <- length(unique(tabX$site))

# Get nLayers
nLayers <- get_nLayers(get_df_nSites(df, nSites))

# Layer col to factor
tabX <- to_factor(tabX, c(4))

# Set layer col values to integers
tabX$layer <- as.integer(tabX$layer)

# Remove non existent layers from tabXs
tabX <- filter(tabX, layer <= nLayers[site])

# Number of sites
nSites <- length(nLayers)

# Site column name to groupID
colnames(tabX)[colnames(tabX) == "site"] <- "groupID"

# Join clusterIDs and speciesIDs with tabX
system.time(
  tabX_ids <- df %>%
    group_by(groupID) %>%
    filter(groupID<=nSites) %>%
    select(groupID,speciesID,clusterID) %>%
    mutate(layer = as.integer(1:n())) %>%
    left_join(tabX, by=c("groupID", "layer")) %>%
    ungroup()
)
tabX_ids <- tabX_ids[with(tabX_ids,order(groupID,speciesID,clusterID)),]

# Define variables
varXs_means <- c(11,12,14)
varXs_sums <- c(13,17,18,30,43)

# tabX to wide format
tabX_ids <- data.table(tabX_ids)
tabX_wide <- reshape(tabX_ids, idvar = c("year","groupID","speciesID","clusterID","layer"),
                     timevar = "variable", direction = "wide")
colnames(tabX_wide)[6:ncol(tabX_wide)] <- varNames[varXs]


# Get aggregate sums for basal_areas
df_baLayers <- get_variable_sums(tabX_wide, var="BA",formula=paste0("BA","~groupID + speciesID + clusterID + year"))

# Sort
df_baLayers_sorted <- df_baLayers[with(df_baLayers,order(groupID,speciesID,clusterID)),]
df_baLayers_sorted <- data.table(df_baLayers_sorted)

# Merge basal_area aggregate sums with original df
df_merged <- tabX_wide %>% 
  left_join(df_baLayers_sorted, by=c("groupID","speciesID","clusterID","year"))


# Convert to data frame
df_merged <- data.frame(df_merged)

# Get weighted means
means <- data.frame()
for(varX in varNames[varXs_means]){
  df <- get_weighted_variable(df_merged,var=varX,formula=paste0(varX,"~groupID + speciesID + clusterID + year"), weighted_by = "BA")
  means <- append(means,list(df))
}

# Get sums
sums <- c()
for(varX in varNames[varXs_sums]){
  df <- get_variable_sums(tabX_wide, var=varX,formula=paste0(varX,"~groupID + speciesID + clusterID + year"))
  sums <- append(sums,list(df))
}

# Merge respective dfs 
means <- Reduce(merge, means[2:length(means)], means[1])
sums <- Reduce(merge, sums[2:length(sums)], sums[1])

# Rename sums cols
colnames(sums)[5:ncol(sums)] <- varNames[varXs_sums]

# Merge
df_weighted <- merge(sums,means)

# Add Lc
df_weighted$Lc <- df_weighted$H - df_weighted$Hc_base

# Sort
df_weighted_sorted <- df_weighted[with(df_weighted,order(groupID,speciesID,clusterID,year)),]

# Add layers col
df_layers <- df_weighted_sorted %>%
  group_by(groupID,year) %>%
  mutate(layer = as.integer(1:n())) %>%
  ungroup()

# Add site col
df_layers$site <-df_layers$groupID

# Add species name col
df_layers$species <- speciesNames[df_layers$speciesID]

# Get long format table
tabX <- data.table(melt(df_layers,id.vars = c("site","groupID","speciesID","clusterID","year","layer","species")))

# Calculate N value
tabX[variable=="N"]$value <- tabX[variable=="BA"]$value/(pi*(tabX[variable=="D"]$value/200)^2)

# To factor
tabX <- to_factor(tabX, c(1,7))
tabX$year <- as.integer(tabX$year)
tabX$variable <- as.character(tabX$variable)

# Write rdata
fileName <- paste0(rdata_path, "tabX_layerAggr_", layerNames[VAR_layer_id],".rdata")
save(tabX, file=fileName)
print(paste0("tabX saved to ", fileName))






