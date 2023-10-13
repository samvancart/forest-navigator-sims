source('scripts/settings.R')

# Chooses optimal number of clusters (fviz_nbclust function)
# for all subgroups of a grouped data frame based on given columns in that data frame,
# generates the clusters for each subgroup, and assigns a clusterID to each subgroup member.
# Result is the data frame sorted by original groupID, original subgroupID and assigned clusterID.

# Functions


get_dfs_split_by_cores_list <- function(split_df, cores, number_of_groups) {
  
  dfs <- c()
  offset <- 1
  for (i in 1:cores) {
    if(i<cores) {
      df <- Reduce(rbind, split_df[offset:(offset+(number_of_groups-1))], data.table())    
    } else {
      df <- Reduce(rbind, split_df[offset:(offset+(length(split_df)-offset))], data.table())
    }
    dfs <- c(list(df),dfs)
    offset <-(offset+groups_in_df)
  }
  return(rev(dfs))
}



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
  # Import libraries here to enable parallel processing 
  library(tibble)
  library(dplyr)
  library(factoextra)
  
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
        model <- kmeans(df_cluster_cols, centers = centers, nstart = 25, iter.max=50)
        
        ## Append clusterIDs to list
        clusterIDs <- model$cluster
        clusterIDs_list <- append(clusterIDs_list, clusterIDs)
      }
      
    }

  return(clusterIDs_list)
}


# Load sorted data
grouped_nfi_swe_sorted <- fread("data/nfi/sweden/grouped_nfi_swe_sorted_speciesID11to4.csv")
df <- grouped_nfi_swe_sorted

# Vector with all groups
groups_vector <- unique(df$groupID)

length(groups_vector)


# TEST RUN IN PARALLEL (WORKS)
filtered_groups_vector <- groups_vector[1:1601]
df_filtered <- filter(df,groupID %in% filtered_groups_vector)

# Split df by groupID
split_df <- split(df_filtered, df_filtered$groupID)
# Get number of available cores
cores <- detectCores(logical=T)
# Number of groups in one df (last group will have possible remainder)
groups_in_df <- floor(length(filtered_groups_vector) / cores)

dfs <- get_dfs_split_by_cores_list(split_df,cores,groups_in_df)

cl <- makeCluster(cores, type="SOCK")
registerDoParallel(cl)
system.time(
  clusterIDs_list_parallel <- foreach(df = dfs) %dopar% get_clusterIDs_groups_species(df,unique(df$groupID))
)

stopCluster(cl)
clusterIDs_list_parallel_combined <- Reduce(append,clusterIDs_list_parallel,c())



print("Creating clusters...")

# Get clusterIDs vector
# clusterIDs_list <- get_clusterIDs_groups_species(df, groups_vector)
system.time(
  clusterIDs_list <- get_clusterIDs_groups_species(df_filtered, filtered_groups_vector)
)


print("Done.")

setequal(clusterIDs_list_parallel_combined,clusterIDs_list)

# # Add clusterIDs to df
# df$clusterID <- clusterIDs_list
# 
# # Sort by group then species then cluster
# df_sorted <- df[with(df,order(df$groupID,df$speciesID,df$clusterID)),]
# 
# path <- paste0("data/nfi/sweden/all_sorted_group_species_cIDs_speciesID11to4.csv")
# write.csv(df_sorted, path, row.names = F)




