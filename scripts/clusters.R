source('scripts/settings.R')

# Chooses optimal number of clusters (fviz_nbclust function)
# for all subgroups of a grouped data frame based on given columns in that data frame,
# generates the clusters for each subgroup, and assigns a clusterID to each subgroup member.
# Result is the data frame sorted by original groupID, original subgroupID and assigned clusterID.

# Functions

# Optimal number of cluster centers
get_centers <- function(df, kmax) {
  
  centers <- 1
  
  if (nrow(unique(df))>2) {
    optimal_num_clusters <- fviz_nbclust(df, kmeans, method = "silhouette", k.max = kmax)
    centers <- which.max(optimal_num_clusters$data$y)
  }
  return(centers)
}


# Kmax for optimising number of clusters
get_kmax <- function(df) {
  
  if(nrow(unique(df))>2) {
    kmax <- nrow(unique(df))-1
  } else {
    kmax <- 2
  }
  return(kmax)
}


# Get all clusterIDs from groups and species
get_clusterIDs_groups_species <- function(df, groups_vector, cluster_cols=c("Dbh", "Height")) {
  
  # Convert df to tibble
  df <- as_tibble(df)
  
  # Initialise list for clusterIDs
  clusterIDs_list <- c()
  
  for (i in groups_vector) {
    # Choose 1 site
    filtered_groups <- df %>% filter(groupID == i)
    
    # Species vector
    species_vector <- unique(filtered_groups$speciesID)
    
    for (j in species_vector) {
      # Choose species
      filtered_species <- filtered_groups %>% filter(speciesID == j)
      
      # Df for clustering
      df_cluster_cols <- filtered_species[, cluster_cols]
      
      # Get max number of clusters
      kmax <- get_kmax(df_cluster_cols)
      
      # Get optimal number of clusters
      centers <- get_centers(df_cluster_cols, kmax)
      
      # K-means set up
      set.seed(123)
      model <- kmeans(df_cluster_cols, centers = centers, nstart = 25)
      
      ## Append clusterIDs to list
      clusterIDs <- model$cluster
      clusterIDs_list <- append(clusterIDs_list, clusterIDs)
    }
    
  }
  return(clusterIDs_list)
}


# Load sorted data
grouped_nfi_swe_sorted <- fread("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/grouped_nfi_swe_sorted.csv")
df <- grouped_nfi_swe_sorted

# Vector with all groups
groups_vector <- unique(df$groupID)

# Get clusterIDs vector
clusterIDs_list <- get_clusterIDs_groups_species(df, groups_vector)

# Add clusterIDs to df
df$clusterID <- clusterIDs_list

# Sort by group then species then cluster
df_sorted <- df[with(df,order(df$groupID,df$speciesID,df$clusterID)),]

# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/all_sorted_group_species_cIDs.csv")
# write.csv(df_sorted, path, row.names = F)

