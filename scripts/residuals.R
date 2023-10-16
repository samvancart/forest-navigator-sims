source('scripts/settings.R')

# Functions
get_nLayers <- function(df) {
  # Choose sites
  df_nSites <- df %>%
    group_by(groupID) %>%
    filter(groupID<=nSites)
  
  
  nLayers <- (df_nSites %>% count(groupID))$n
  
  return(nLayers)
}

# SAME FUNCTION IN plot_tables!!!
# MOVE TO R/
format_table <- function(tabX) {
  tabX$site <- as.factor(tabX$site)
  tabX$layer <- as.factor(tabX$layer)
  tabX$species <- as.factor(tabX$species)
  
  # Set layer col values to integers
  tabX$layer <- as.integer(tabX$layer)
  
  return (tabX)
}


# Load tabX trees
fileName <- (paste0(rdata_path, "modOut_",layerNames[1],".rdata"))
load(fileName)
tabX_trees <- data.table(melt(modOut$multiOut[,,11:13,,1]))

# Load tabX clusters
fileName <- (paste0(rdata_path, "modOut_",layerNames[2],".rdata"))
load(fileName)
tabX_clusters <- data.table(melt(modOut$multiOut[,,11:13,,1]))

# NFI DATA
nfi_trees_path <- nfi_sweden_paths[1]
df_trees <- fread(nfi_trees_path)


print(paste0("NFI path is ", nfi_trees_path))

# Get nLayers_trees
nLayers_trees <- get_nLayers(df_trees)

# Format tabX
tabX_trees <- format_table(tabX_trees)

# Drop species column (produced by format function)
tabX_trees <- subset(tabX_trees,select = -c(species))

# Remove non existent layers from tabX_trees
tabX_trees <- filter(tabX_trees, layer <= nLayers_trees[site])

s = 2
y = 1
v = "BA"

# f_trees <- filter(tabX_trees, site==s & year==y & variable==v)
f_trees <- filter(tabX_trees, site==s)
f_clusters <- filter(tabX_clusters, site==s & year==y & variable==v)
(f_trees$value[2]+f_trees$value[3])/2
f_clusters$value


# Merge clusterIDs and speciesIDs with tabX_trees
nSites <- length(nLayers)
tabX_trees_ids <- c()

for(s in 1:nSites) {
  f_trees <- filter(tabX_trees, site==s)
  
  df_ids <- df_trees %>%
    filter(groupID==unique(f_trees$site)) %>%
    select(groupID,speciesID,clusterID) %>%
    mutate(layer = as.integer(1:n()))
  
    df_merged <- merge(df_ids[,c(2:4)],f_trees, by=c("layer"))
    tabX_trees_ids <- rbind(tabX_trees_ids, df_merged)
}

tabX_trees_ids <- tabX_trees_ids[with(tabX_trees_ids,order(site,speciesID,clusterID)),]
tabX_trees <- tabX_trees[with(tabX_trees,order(site)),]

tabX_trees[which(site==7 & year==27 & variable=="BA" & layer==5),]
tabX_trees_ids[which(site==7 & year==27 & variable=="BA" & layer==5),]
setequal(nrow(tabX_trees),nrow(tabX_trees_ids))

