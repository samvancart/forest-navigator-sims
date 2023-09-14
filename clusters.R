

source("settings.R")

# Load Swedish tree data 
nfi_swe <- fread("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/trad_2018_2022_inspire_tradid_corr.csv")

# Assign groupIDs
grouped_nfi_swe <- nfi_swe %>%
  group_by(ID_plot) %>%
  mutate(groupID = cur_group_id())

# Sort
grouped_nfi_swe_sorted <- arrange(grouped_nfi_swe, ID_plot)

# Choose sites where n trees>20
grouped_over_20 <- grouped_nfi_swe_sorted %>%
  group_by(groupID) %>%
  filter(n() > 20)

# Choose sites where n trees<21
grouped_under_21 <- grouped_nfi_swe_sorted %>%
  group_by(groupID) %>%
  filter(n() < 21) %>%
  mutate(clusterID = 1)


# Get unique groups as vector
nGroups <- unique(grouped_over_20$groupID)

## Initialise vector_list for clusterIDs
vector_list <- c()


# Get all clusterIDs
for (i in nGroups) {
  # Choose 1 site
  filtered <- grouped_over_20 %>% filter(groupID == i)
  
  # Df for clustering
  layer <- filtered[,c("Dbh","Height")]
  
  # Normalise data for clustering
  # means <- apply(layer,2,mean)
  # sds <- apply(layer,2,sd)
  # nor <- scale(layer,center=means,scale=sds)
  
  # K-means set up
  set.seed(123)
  model <- kmeans(layer, centers = 3, nstart = 25)
  
  ## Append clusterIDs to vector_list
  clusterID <- model$cluster
  vector_list <- append(vector_list, clusterID)
  
}

# Merge clusterIDs to df
grouped_over_20$clusterID <- vector_list


# Combine grouped_over_20 and grouped_under_21
grouped_cIDs <- rbind(grouped_over_20, grouped_under_21)

# Sort
grouped_sorted_cIDs <- arrange(grouped_cIDs, groupID)

# Write csv
path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/grouped_sorted_cIDs.csv")
write.csv(grouped_sorted_cIDs, path, row.names = F)


# Plot clusters
fviz_cluster(model, data = layer)

# Plot optimal number of clusters
fviz_nbclust(layer, kmeans, method = "wss")




