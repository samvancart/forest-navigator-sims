# Functions

# Calculate basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
get_basal_area <- function(x, dbh_col, multiplier_col) {
  dbh <- as.double(x[dbh_col])
  multiplier <- as.double(x[multiplier_col])
  basal_area <- pi*(dbh/200)^2*multiplier
  return(basal_area)
}


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