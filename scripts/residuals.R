source('scripts/settings.R')

# Functions
get_nLayers <- function(df, nSites) {
  # Choose sites
  df_nSites <- df %>%
    group_by(groupID) %>%
    filter(groupID<=nSites)
  
  
  nLayers <- (df_nSites %>% count(groupID))$n
  
  return(nLayers)
}

to_factor <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}

# Define variables
varXs <- c(11:14,17,18,30,43)

# Load tabX trees
fileName <- (paste0(rdata_path, "modOut_",layerNames[1],".rdata"))
load(fileName)
tabX_trees <- data.table(melt(modOut$multiOut[,,varXs,,1]))


# Load tabX clusters
fileName <- (paste0(rdata_path, "modOut_",layerNames[2],".rdata"))
load(fileName)
tabX_clusters <- data.table(melt(modOut$multiOut[,,varXs,,1]))

# NFI DATA

# Trees
nfi_trees_path <- nfi_sweden_paths[1]
df_trees <- fread(nfi_trees_path)

# Clusters
nfi_clusters_path <- nfi_sweden_paths[2]
df_clusters <- fread(nfi_clusters_path)

print(paste0("NFI trees path is ", nfi_trees_path))
print(paste0("NFI clusters path is ", nfi_clusters_path))

# Get nSites
nSites <- length(unique(tabX_trees$site))

# Get nLayers
nLayers_trees <- get_nLayers(df_trees, nSites)
nLayers_clusters <- get_nLayers(df_clusters, nSites)

# Layer col to factor
tabX_trees <- to_factor(tabX_trees, c(4))
tabX_clusters <- to_factor(tabX_clusters, c(4))

# Set layer col values to integers
tabX_trees$layer <- as.integer(tabX_trees$layer)
tabX_clusters$layer <- as.integer(tabX_clusters$layer)

# Remove non existent layers from tabXs
tabX_trees <- filter(tabX_trees, layer <= nLayers_trees[site])
tabX_clusters <- filter(tabX_clusters, layer <= nLayers_clusters[site])

# Number of sites
nSites <- length(nLayers_trees)

# site column name to groupID
colnames(tabX_trees)[colnames(tabX_trees) == "site"] <- "groupID"
colnames(tabX_clusters)[colnames(tabX_clusters) == "site"] <- "groupID"

# Join clusterIDs and speciesIDs with tabX_trees
system.time(
  tabX_trees_ids <- df_trees %>%
  group_by(groupID) %>%
  filter(groupID<=nSites) %>%
  select(groupID,speciesID,clusterID) %>%
  mutate(layer = as.integer(1:n())) %>%
  left_join(tabX_trees, by=c("groupID", "layer")) %>%
  ungroup()
)
tabX_trees_ids <- tabX_trees_ids[with(tabX_trees_ids,order(groupID,speciesID,clusterID)),]
tabX_clusters <- tabX_clusters[with(tabX_clusters,order(groupID,layer)),]
tabX_clusters <- as_tibble(tabX_clusters)

Reduce(sum, sapply(nLayers_trees, function(x) x*121*length(varXs)), 0)
Reduce(sum, sapply(nLayers_clusters, function(x) x*121*length(varXs)), 0)

print(tabX_trees_ids)
print(tabX_clusters)

print(filter(tabX_trees_ids, groupID==2 & year==2, variable=="H"))
print(filter(tabX_clusters, groupID==2 & year==2, variable=="H"))

xx  <- data.table(tabX_trees_ids)

xx_aggregate <- xx[variable=="BA",sum(value),by=.(groupID,speciesID,clusterID,year)]
# xx_aggregate <- xx_aggregate[groupID==1]
xx_aggregate$speciesID <- as.factor(xx_aggregate$speciesID)
xx_aggregate$clusterID <- as.factor(xx_aggregate$clusterID)

dt_clusters <- data.table(tabX_clusters)
dt_clusters$layer <- as.factor(dt_clusters$layer)

ggplot() +
  geom_line(data=xx_aggregate[groupID==1],mapping=aes(x=year,y=V1,col=speciesID,shape=clusterID)) +
  geom_line(data=dt_clusters[groupID==1 & variable=="BA"], mapping=aes(x=year,y=value,col=layer))






