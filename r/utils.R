
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


# Split df or sf into equal chunks
split_df_equal <- function(df, n){
  print(paste0("Splitting frame..."))
  list <- split(df, factor(sort(rank(row.names(df))%%n)))
  print("Done.")
  
  return(list)
}

# Indexes in df2 of nearest neighbour coordinates for each coordinate pair in df1
get_haversine_dist <- function(df1, df2) {
  ind <- sapply(1:nrow(df1), function(x) {
    which.min(geosphere::distHaversine(df1[x, ], df2))
  })
  return(ind)
}

# Extract all raster values that are within an area specified by a polygon 
extract_raster_values <- function(vector_obj, raster_obj) {
  return(raster::extract(raster_obj, vector_obj))
}

# Get forest class of a cell
calculate_forest_class_10km <- function(x) {
  dt <- data.table(V1=x)
  counts <- get_counts(dt)
  total_forest <- get_total_forest(counts)
  forest_class <- get_forest_class_10km(counts, total_forest)
  return(forest_class)
}

# Total forest area of cell
get_total_forest <- function(dt) {
  total_forest <- sum(dt[V1 %between% c(23,25)]$n)
  return(total_forest)
}

# Sums of pixels that contain con, bl, mix and no forest respectively 
get_counts <- function(dt){
  dt[!V1 %between% c(23,25)] <- 0
  counts <- dt %>% count(V1)
  return(counts)
}

# Assign forest class based on shares
get_forest_class_10km <- function(dt, total_forest) {
  
  if(total_forest==0) {return("no_forest")} 
  
  bl <- sum(dt[V1==23]$n)
  con <- sum(dt[V1==24]$n)
  
  if((con/total_forest) > 0.7 & (bl/total_forest) < 0.1) {
    return("coniferous_dominated")
  } else if((bl/total_forest) > 0.7 & (con/total_forest) < 0.1) {
    return("broad-leaved_dominated")
  } else {
    return("mixed_forest")
  }
}


# Extract CORINE forest classes from raster values based on polygons in vector file. Uses parallel processing. 
extract_forest_classes_10km <- function(sf, cores, fromRow=1, toRow=nrow(sf), libs=list(), sources=list(), fun_kwargs=list()) {
  # Sf as spatial
  spatial_poly <- as(sf[fromRow:toRow,], "Spatial")
  
  # Split sf
  data <- split_df_equal(df=spatial_poly, n = cores)
  
  # Parallel process
  raster_vals_lists <- unlist(
    get_in_parallel(data = data, fun = extract_raster_values, cores = cores, libs = libs, sources = sources, fun_kwargs = fun_kwargs), 
    recursive = F)
  
  print(paste0("Assigning forest classes..."))
  
  # Get forest class
  forest_classes <- data.table(forest_class_10km=lapply(raster_vals_lists, function(x) calculate_forest_class_10km(x)))
  
  print("Done.")
  
  return(forest_classes)
}

