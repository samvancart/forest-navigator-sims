source('scripts/settings.R')
source('./r/cluster_aggregates.R')

# Before running: Get clusters 

# Calculates weighted means (by cluster) for dbh, age and height

# 1. Read nfi data sorted by groupID, speciesID, clusterID and get basal area of each tree
# 2. Get aggreagated basal area and tree density sums for each cluster
# 3. Calculate weighted means (dbh, age and height) based on basal area sums
# 4. Calculate quadratic mean (dbh) based on basal area and tree density sums
# 5. Write cluster_weighted_means file

print(paste0("Running cluster_weighted_means.R"))

# Basal area for NFI Sweden
path <- paste0("data/nfi/sweden/all_sorted_group_species_cIDs_speciesID11to4.csv")
df <- fread(path)
df$Dbh <- df$Dbh/10

# Apply to each df row to get basal_area vector
basal_area <- apply(df, 1, get_basal_area, dbh_col=6, multiplier_col=15)

# Add vector as column to df
df$basal_area <- basal_area

# Write file for running multiSiteLayers.R for trees 
path <- paste0("data/nfi/sweden/sorted_group_species_cIDs_speciesID11to4_basal_area.csv")
# write.csv(df, path, row.names = F)

# Df to tibble
df <- as_tibble(df)

# 1. Get aggregate sums for basal_areas and Multiplier_tree_Number_Ha
df_baLayers <- get_variable_sums(df, "basal_area")
df_tree_density <- get_variable_sums(df, "Multiplier_tree_Number_Ha")

# Sort
df_baLayers_sorted <- df_baLayers[with(df_baLayers,order(groupID,speciesID,clusterID)),]
df_tree_density_sorted <- df_tree_density[with(df_tree_density,order(groupID,speciesID,clusterID)),]

# 2. Merge basal_area aggregate sums with original df
df_merged <- df %>% 
  left_join(df_baLayers_sorted, by=c("groupID","speciesID","clusterID")) %>%
  left_join(df_tree_density_sorted, by=c("groupID","speciesID","clusterID"))


# 3. Get weighted means for variables
dbh <- get_weighted_variable(df_merged,"Dbh")
h <- get_weighted_variable(df_merged,"Height")
age <- get_weighted_variable(df_merged,"Age")

# 4. Calculate quadratic mean diameter for all layers and get df
df_merged$dbh_qm_layer <- sqrt((df_merged$basal_area_layer/df_merged$Multiplier_tree_Number_Ha_layer)/pi)*200
dbh_qm <- unique(df_merged[c("groupID","speciesID","clusterID","dbh_qm_layer")])

# 4. Merge dfs
df_weighted <- merge(merge(merge(merge(dbh,h),age),df_baLayers_sorted),dbh_qm)

# # Plot
# plot(df_merged$Dbh,df_merged$dbh_qm_layer)

# Rename col
df_weighted <- df_weighted %>% rename("basal_area" = "basal_area_layer")

# Sort
df_weighted_sorted <- df_weighted[with(df_weighted,order(groupID,speciesID,clusterID)),]
df <- df_weighted_sorted

# # Write file for running multiSiteLayers.R for clusters 
path <- paste0("data/nfi/sweden/cluster_weighted_means_speciesID11to4.csv")
write.csv(df, path, row.names = F)










