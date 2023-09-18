

source("settings.R")


# Functions

# Get all clusterIDs from groups
get_clusterIDs_groups <- function(df, groups_vector, cluster_cols=c("Dbh", "Height")) {
  
  ## Initialise vector_list for clusterIDs
  vector_list <- c()
  
  for (i in groups_vector) {
    # Choose 1 site
    filtered <- df %>% filter(groupID == i)
    
    # Df for clustering
    layer <- filtered[, cluster_cols]
    
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
  return(vector_list)
}

# Get all clusterIDs from groups and species
get_clusterIDs_groups_species <- function(df, groups_vector, cluster_cols=c("Dbh", "Height")) {
  
  ## Initialise vector_list for clusterIDs
  vector_list <- c()
  
  for (i in groups_vector) {
    # Choose 1 site
    filtered_groups <- df %>% filter(groupID == i)
    
    # Species vector
    species_vector <- unique(filtered_groups$speciesID)
    
    for (j in species_vector) {
      # Choose species
      filtered_species <- filtered_groups %>% filter(speciesID == j)
      
      # Df for clustering
      layer <- filtered_species[, ..cluster_cols]
      
      # Get max number of clusters
      if(nrow(unique(layer))>2) {
        kmax <- nrow(unique(layer))-1
      } else {
        kmax <- 2
      }
      
      
      centers <- 1
      
      # Get optimal number of clusters
      if (nrow(unique(layer))>2) {
        optimal_num_clusters <- fviz_nbclust(layer, kmeans, method = "silhouette", k.max = kmax)
        centers <- which.max(optimal_num_clusters$data$y)
      }
      
      # K-means set up
      set.seed(123)
      model <- kmeans(layer, centers = centers, nstart = 25)
      
      ## Append clusterIDs to vector_list
      clusterID <- model$cluster
      vector_list <- append(vector_list, clusterID)
    }
    
  }
  return(vector_list)
}


# Load sorted data
grouped_nfi_swe_sorted <- fread("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/grouped_nfi_swe_sorted.csv")
df <- grouped_nfi_swe_sorted

# pos <- which(df$groupID == 1000)
# df <- head(grouped_nfi_swe_sorted,pos[1]-1)

groups_vector <- unique(df$groupID)

vector_list <- get_clusterIDs_groups_species(df, groups_vector)

# # Add clusterIDs to df
df$clusterID <- vector_list

path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/sorted_group_species_cIDs.csv")
write.csv(df, path, row.names = F)

# TEST clusterfunc

v_list <- c()
filtered_groups <- df %>% filter(groupID == 3)

# Species vector
species_vector <- unique(filtered_groups$speciesID)

# Choose species
filtered_species <- filtered_groups %>% filter(speciesID == 1)

layer <- filtered_species[,c(filtered_species$Dbh,filtered_species$Height)]
cluster_cols = c("Dbh","Height")
filtered_species[, c("Dbh", "Height")]

# Df for clustering
layer <- filtered_species[, ..cluster_cols]

# Get max number of clusters
kmax <- if(nrow(unique(layer))>2) {
  kmax <- nrow(unique(layer))-1
} else {
  kmax <- 2
}


centers <- 1
nrow(layer)
nrow(unique(layer))

# Get optimal number of clusters
if (nrow(unique(layer))>2) {
  optimal_num_clusters <- fviz_nbclust(layer, kmeans, method = "silhouette", k.max = kmax)
  centers <- which.max(optimal_num_clusters$data$y)
} 



# K-means set up
set.seed(123)
model <- kmeans(layer, centers = centers, nstart = 25)

## Append clusterIDs to vector_list
clusterID <- model$cluster
v_list <- append(v_list, clusterID)

# Plot clusters
fviz_cluster(model, data = layer)


# Test k-means
filtered <- df %>% filter(groupID == 1)
layer <- filtered[, c("Dbh","Height")]
optimal_num_clusters <- fviz_nbclust(layer, kmeans, method = "silhouette")
centers <- which.max(optimal_num_clusters$data$y)
set.seed(123)
model <- kmeans(layer, centers = centers, nstart = 25)
# Plot clusters
fviz_cluster(model, data = layer)

# Plot optimal number of clusters
fviz_nbclust(layer, kmeans, method = "wss")

ggplot(layer, aes(Dbh,Height)) +
  geom_point() 
# stat_summary(geom = "line", fun = mean)

# Merge clusterIDs to df
grouped_over_20$clusterID <- vector_list


# Combine grouped_over_20 and grouped_under_21
grouped_cIDs <- rbind(grouped_over_20, grouped_under_21)

# Sort
grouped_sorted_cIDs <- arrange(grouped_cIDs, groupID)

# # Write csv
# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/grouped_sorted_cIDs.csv")
# write.csv(grouped_sorted_cIDs, path, row.names = F)


# Plot clusters
fviz_cluster(model, data = layer)

# Plot optimal number of clusters
fviz_nbclust(layer, kmeans, method = "wss")

xx<-fviz_nbclust(layer, kmeans, method = "silhouette")
which.max(xx$data$y)
nClusters <- xx$data
library(cluster) 
gap_stat <- clusGap(layer, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
gap_stat$Tab



