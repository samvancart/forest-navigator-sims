# Helper functions

source('scripts/settings.R')

# Calculate basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
get_basal_area_nfi_sweden <- function(x) {
  dbh <- as.double(x[6])
  multiplier <- as.double(x[15])
  basal_area <- pi*(dbh/200)^2*multiplier
  return(basal_area)
}



# # Basal area for NFI Sweden
# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/all_sorted_group_species_cIDs.csv")
# df <- fread(path)
# df$Dbh <- df$Dbh/10
# 
#  
# # Apply to each df row to get basal_area vector
# basal_area <- apply(df, 1, get_basal_area_nfi_sweden)
# 
# 
# # Add vector as column to df
# df$basal_area <- basal_area
# 
# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/sorted_group_species_cIDs_basal_area.csv")
# write.csv(df, path, row.names = F)

# Aggregate sums of variable based on groupID, speciesID, clusterID
get_variable_sums <- function(df, var) {
  formula <- as.formula(paste0(var,"~groupID + speciesID + clusterID"))
  df <- aggregate(formula, data=df, FUN=sum)
  df <- rename_col(df, var, "_layer")
  
  return(df)
}

# Weighted aggregate means of a variable by groupID, speciesID, clusterID
get_weighted_variable <- function(df,var,weighted_by="basal_area") {
  
  df[,var] <- df[, var]*df[,weighted_by]
  weighted_layer <- paste0(weighted_by,"_layer")
  df[,var] <- df[,var]/df[,weighted_layer]
  formula <- as.formula(paste0(var,"~groupID + speciesID + clusterID"))
  df <- aggregate(formula,FUN=sum,data=df)
  
  return(df)
}


rename_col <- function(df,var,suffix){
  new_name <- paste0(var,suffix)
  df <- df %>% rename(!!new_name := var)
  return(df)
}


# # # NFI DATA
path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/sorted_group_species_cIDs_basal_area.csv")
df <- fread(path)

# data.table becomes tbl_df
df <- df %>%
  group_by(groupID)


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

# Plot
plot(df_merged$Dbh,df_merged$dbh_qm_layer)

# Rename col
df_weighted <- df_weighted %>% rename("basal_area" = "basal_area_layer")

# Sort
df_weighted_sorted <- df_weighted[with(df_weighted,order(groupID,speciesID,clusterID)),]
df <- df_weighted_sorted

# Write csv
# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/cluster_weighted_means.csv")
# write.csv(df, path, row.names = F)










