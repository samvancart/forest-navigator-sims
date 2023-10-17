source('scripts/settings.R')
source('./r/cluster_aggregates.R')

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
fileName <- (paste0(rdata_path, "modOut_",layerNames[layerID],".rdata"))
load(fileName)
tabX <- data.table(melt(modOut$multiOut[,,varXs,,1]))


# NFI DATA

# Trees
nfi_path <- nfi_sweden_paths[layerID]
df <- fread(nfi_path)

print(paste0("NFI path is ", nfi_path))


# Get nSites
nSites <- length(unique(tabX$site))

# Get nLayers
nLayers <- get_nLayers(df, nSites)

# Layer col to factor
tabX <- to_factor(tabX, c(4))

# Set layer col values to integers
tabX$layer <- as.integer(tabX$layer)

# Remove non existent layers from tabXs
tabX <- filter(tabX, layer <= nLayers[site])

# Number of sites
nSites <- length(nLayers)

# site column name to groupID
colnames(tabX)[colnames(tabX) == "site"] <- "groupID"

# Join clusterIDs and speciesIDs with tabX_trees
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

Reduce(sum, sapply(nLayers, function(x) x*121*length(varXs)), 0)

print(tabX_ids)

print(filter(tabX_ids, groupID==2 & year==2, variable=="H"))

tabX_ids <- to_factor(tabX_ids, c(1:5))
tabX_ids <- data.table(tabX_ids)

tabX_wide <- reshape(tabX_ids, idvar = c("year","groupID","speciesID","clusterID","layer"),
                     timevar = "variable", direction = "wide")
colnames(tabX_wide)[6:ncol(tabX_wide)] <- varNames[varXs]
tabX_wide

tabX_wide[groupID==1 & year==1 ]

# 1. Get aggregate sums for basal_areas and Multiplier_tree_Number_Ha
df_baLayers <- get_variable_sums(tabX_wide, var="BA",formula=paste0("BA","~groupID + speciesID + clusterID + year"))

# Sort
df_baLayers_sorted <- df_baLayers[with(df_baLayers,order(groupID,speciesID,clusterID)),]
df_baLayers_sorted <- data.table(df_baLayers_sorted)

df_baLayers_sorted[groupID==1 & year==1]


# 2. Merge basal_area aggregate sums with original df
df_merged <- tabX_wide %>% 
  left_join(df_baLayers_sorted, by=c("groupID","speciesID","clusterID","year"))

df_merged[groupID==1 & year==1]







xx  <- data.table(tabX_ids)

xx_aggregate <- xx[variable=="BA",sum(value),by=.(groupID,speciesID,clusterID,year)]

xx_aggregate$speciesID <- as.factor(xx_aggregate$speciesID)
xx_aggregate$clusterID <- as.factor(xx_aggregate$clusterID)

set.seed(45)
dat1 <- data.frame(
  name = rep(c("firstName", "secondName"), each=4),
  numbers = rep(1:4, 2),
  value = rnorm(8)
)

dat1

reshape(dat1, idvar = "numbers", timevar = "name", direction = "wide")

