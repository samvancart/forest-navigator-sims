
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


# Centre point coordinates from bbox coordinates
get_grid_centre <- function(df, x, y) {
  y_centre <- max(y) - ((max(y)-min(y))/2)
  x_centre <- max(x) - ((max(x)-min(x))/2)
  return(data.table(x=x_centre,y=y_centre))
}


# Transformed sf crs grid centre point coordinates
# Params:
# sf (sf): File to transform
# crs (string): Destination coordinate reference system
# newXName: New x coordinate name
# newYName: New y coordinate name
# Returns:
# data frame of the centre point coordinates of the original grid

get_sf_centre_coords <- function(sf, crs=4258, newXName = "lon", newYName = "lat") {
  lat_lons <- sf %>%
    st_transform(crs) %>% # Transform to desired coordinate reference system
    st_coordinates() %>% # Get all coordinates (bboxes of grid cells)
    as.data.frame() %>%
    group_by(L2) %>% # Group by bboxes
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