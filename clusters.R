

source("settings.R")


# Functions


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

# Vector with all groups
groups_vector <- unique(df$groupID)

# Get clusterIDs vector
vector_list <- get_clusterIDs_groups_species(df, groups_vector)

# Add clusterIDs to df
df$clusterID <- vector_list

# Sort by group then species then cluster
df_sorted <- df[with(df,order(df$groupID,df$speciesID,df$clusterID)),]

# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/all_sorted_group_species_cIDs.csv")
# write.csv(df_sorted, path, row.names = F)

