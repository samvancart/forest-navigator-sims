
# Functions

get_df_nSites <- function(df, nSites) {
  
  df_nSites <- df %>%
    group_by(groupID) %>%
    filter(groupID<=nSites)
  
  return(df_nSites)
}

get_nLayers <- function(df) {
  nLayers <- (df %>% count(groupID))$n
  return(nLayers)
}

to_factor <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}


# Centre point coordinates from bbox coordinates
get_grid_centre <- function(df, x, y) {
  y_centre <- max(y) - ((max(y)-min(y))/2)
  x_centre <- max(x) - ((max(x)-min(x))/2)
  return(data.table(x=x_centre,y=y_centre))
}


# Transformed sf crs grid centre point coordinates
# Params:
# sf (sf): Sf object to transform
# crs (string): Destination coordinate reference system
# newXName: New x coordinate name
# newYName: New y coordinate name
# Returns:
# data frame of the centre point coordinates in the original grid

get_sf_centre_coords <- function(sf, crs=4258, newXName = "lon", newYName = "lat") {
  lat_lons <- sf %>%
    st_transform(crs) %>% # Transform to desired coordinate reference system
    st_coordinates() %>% # Get all coordinates (bboxes of grid cells)
    as.data.frame() %>%
    group_by(.[,(ncol(.))]) %>% # Group by bboxes
    unique() %>% # Get rid of duplicates
    reframe(get_grid_centre(cur_data(),X,Y)) %>% # Get grid centre coords
    ungroup() %>%
    select(x,y) %>%
    setNames(c(newXName,newYName))
 
  
  return(lat_lons)
}

# CORINE forest class
get_forest_class_name <- function(df,conif_share,broadLeaf_share) {
  if(conif_share > 0.75) {
    return("coniferous")
  } else if(broadLeaf_share > 0.75) {
    return("broad-leaved")
  } else {
    return("mixed")
  }
}

# Get df rows that are outside 1.5 x interquartile range
get_outliers <- function(df, col) {
  df <- data.table(df)
  
  qs <- quantile(df[[col]], probs = c(0.25,0.75))
  
  q1 <- qs[[1]]
  q3 <- qs[[2]]
  
  iqr <- IQR(qs, type = 1)
  
  belowQ1 <- q1-1.5*iqr
  aboveQ3 <- q3 + 1.5*iqr
  
  lower <- df[which(df[,..col] < belowQ1),]
  upper <- df[which(df[,..col] > aboveQ3),]
  
  lower$upper_lower <- "lower"
  upper$upper_lower <- "upper"
  
  cols <- colnames(lower)
  
  df_outliers <- left_join(lower, upper, by=cols)
  
  return(data.table(df_outliers))
}



get_lowerQ <- function(df,col) {
  df <- data.table(df)
  
  qs <- quantile(df[[col]], probs = c(0.25,0.75))
  
  q1 <- qs[[1]]
  q3 <- qs[[2]]
  
  iqr <- IQR(qs, type = 1)

  iqr <- IQR(qs, type = 1)
  
  return(q1 - 1.5 * iqr)
}

get_upperQ <- function(df,col) {
  df <- data.table(df)
  
  qs <- quantile(df[[col]], probs = c(0.25,0.75))
  
  q1 <- qs[[1]]
  q3 <- qs[[2]]
  
  iqr <- IQR(qs, type = 1)
  
  iqr <- IQR(qs, type = 1)
  
  return(q3 + 1.5 * iqr)
}



get_colnames_with_prefix_from_ids <- function(ids, prefix="value.") {
  return(paste0(prefix, ids))
}


rename_column <- function(x, useSpeciesCode, name, prefix="value", splitBy = "\\.") {
  if(grepl(prefix, x)) {
    id <- as.integer(strsplit(x, split = splitBy)[[1]][2])
    new_val <- filtered_codes[filtered_codes[[useSpeciesCode]]==id][[name]]
    return(new_val)
  } else {
    return(x)
  }
}


# Some mixtype identifier names (eg. AC) contain multiple subspecies that need to be combined
combine_grouped_mixtype_cols <- function(codes_df, useSpeciesCode, name_col, short_name) {
  
  ids <- codes_df[codes_df[[name_col]]==short_name][, get(useSpeciesCode)]
  cols <- get_colnames_with_prefix_from_ids(ids)
  
  return(cols)
}
