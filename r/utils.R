
# Functions

get_speciesName <- function(speciesID, speciesDict){
  return (speciesDict[as.character(speciesID)][[1]])
}


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


#' See if a specific variable exists and contains a directory path and return the path if it does. 
#' If the variable doesn't exist then a default path is returned. If the variable exists but
#' the directory path doesn't yet then the path is created.
#'
#' @param pathVarName character A string representing the variable name
#' @param defaultDir character The default directory path
#' @param subDir character The subdirectory that will be added
#'
#' @return character The path
#' @export
#'
#' @examples
get_or_create_path <- function(pathVarName, defaultDir, subDir="") {
  if(!exists(pathVarName)) {
    path = defaultDir
  } else {
    mainDir <- eval(parse(text=pathVarName))
    path <- file.path(mainDir, subDir)
    print(paste0("Creating ", pathVarName, " in ", path))
    dir.create(path = path, recursive = T, showWarnings = F)
    path <- mainDir
  }
  return(path)
}



#' Load R Data File
#'
#' This function loads an R data file into a new environment and returns the object.
#'
#' @param RDataFile The path to the R data file to be loaded.
#'
#' @return Returns the object loaded from the RDataFile.
#'
#' @examples
#' # Assuming 'data.RData' contains an object named 'my_data'
#' my_data <- loadRDataFile('data.RData')
#'
#' @export
loadRDataFile <- function(RDataFile) {
  temp_env <- new.env()
  obj <- get(load(RDataFile), temp_env)
  return(obj)
}


#' Get Functions in Environment
#'
#' This function retrieves the names of all functions in a specified environment.
#'
#' @param env The environment from which to retrieve function names. Defaults to the global environment (`.GlobalEnv`).
#' @return A character vector containing the names of all functions in the specified environment.
#' @examples
#' # Get functions in the global environment
#' get_functions_in_env()
#'
#' # Get functions in a specific environment
#' my_env <- new.env()
#' assign("my_function", function() {}, envir = my_env)
#' get_functions_in_env(env = my_env)
get_functions_in_env <- function(env = .GlobalEnv) {
  all_objects <- ls(envir = env)
  functions <- all_objects[sapply(all_objects, function(x) is.function(get(x, envir = env)))]
  return(functions)
}


#' Modify YAML Settings Vector
#'
#' This function modifies specific settings in a YAML configuration file based on a provided named vector.
#'
#' @param config_path The file path to the YAML configuration file.
#' @param settings_vector A named vector containing key-value pairs where the key is the setting name
#' and the value is the new setting value.
#'
#' @return None
#'
#' @examples
#' settings <- c(setting1 = "value1", setting2 = "value2")
#' modify_yaml_settings_vector("path/to/config.yaml", settings)
#'
#' @export
#'
#' @importFrom yaml yaml.load_file write_yaml
modify_yaml_settings_vector <- function(config_path, settings_vector) {
  
  # Validate inputs
  checkmate::assert_file_exists(config_path, access = "r")
  checkmate::assert_named(settings_vector, type = "named")
  
  # Read existing YAML configuration
  config <- yaml::yaml.load_file(config_path)
  
  # Modify specific settings based on provided key-value pairs in the named vector
  for (key in names(settings_vector)) {
    value <- settings_vector[[key]]
    print(paste("Modifying key:", key, "with value:", value))
    result <- modify_recursive(config, key, value)
    config <- result$config
    key_found <- result$key_found
    if (key_found) {
      print(paste("Key", key, "found and updated."))
    } else {
      warning(paste("Key", key, "not found in the configuration."))
    }
  }
  
  # Write the updated YAML back to the file
  yaml::write_yaml(config, config_path)
}



#' Modify Configuration Recursively
#'
#' This helper function recursively modifies a configuration list by updating the value of a specified key.
#'
#' @param config A list representing the configuration settings.
#' @param key A character string specifying the key to be modified.
#' @param value The new value to be assigned to the specified key.
#'
#' @return A list containing the updated configuration and a logical value indicating whether the key was found.
#'
#' @examples
#' config <- list(setting1 = "value1", nested = list(setting2 = "value2"))
#' result <- modify_recursive(config, "setting2", "new_value")
#' print(result$config)
#' print(result$key_found)
#'
#' @export
modify_recursive <- function(config, key, value) {
  key_found <- FALSE
  for (name in names(config)) {
    if (name == key) {
      config[[name]] <- value
      key_found <- TRUE
      break
    } else if (is.list(config[[name]])) {
      result <- modify_recursive(config[[name]], key, value)
      config[[name]] <- result$config
      key_found <- key_found || result$key_found
      if(key_found) break
    }
  }
  return(list(config = config, key_found = key_found))
}


#' Count Occurrences of a Pattern in a Script
#'
#' This function reads a script from a file and counts the occurrences of a specified pattern.
#'
#' @param file_path A character string specifying the path to the script file.
#' @param pattern A character string specifying the pattern to search for in the script.
#' @return A list containing the file path, counts of the pattern in each line, total count, lines with occurrences, and counts per line.
#' @examples
#' \dontrun{
#' count_occurences_of_pattern_in_script("path/to/script.R", "pattern")
#' }
count_occurences_of_pattern_in_script <- function(file_path, pattern) {
  script <- readLines(file_path)
  counts <- str_count(script, pattern)
  total <- sum(counts)
  lines <- which(counts > 0)
  line_counts <- counts[which(counts > 0)]
  return(list(file_path = file_path, 
              counts = counts, total = total, 
              lines = lines, 
              line_counts = line_counts))
}

#' Replace Pattern in a Script
#'
#' This function reads a script from a file and replaces all occurrences of a specified pattern with a replacement string.
#'
#' @param file_path A character string specifying the path to the script file.
#' @param pattern A character string specifying the pattern to be replaced.
#' @param replacement A character string specifying the replacement for the pattern.
#' @param test A logical value indicating whether to perform a test run without modifying the file. Default is FALSE.
#' @return None. The function modifies the script file in place if `test` is FALSE.
#' @examples
#' \dontrun{
#' replace_in_script("path/to/script.R", "pattern", "replacement")
#' replace_in_script("path/to/script.R", "pattern", "replacement", test = TRUE)
#' }
replace_in_script <- function(file_path, pattern, replacement, test = FALSE) {
  script <- readLines(file_path)
  mod_script <- gsub(pattern, replacement, script)
  
  if (!setequal(script, mod_script)) {
    if (!test) {
      print(paste0("Modifying ", file_path, ": Pattern is ", pattern, " and replacement is ", replacement))
      writeLines(mod_script, file_path)
    } else {
      print(paste0("TEST: Modifying ", file_path, ": Pattern is ", pattern, " and replacement is ", replacement))
    }
  }
}


#' Generate a Data Table from Named Vectors and Source List
#'
#' This function creates a data table by expanding a grid of named vectors and 
#' appending a source list to it.
#'
#' @param named_vector_list A list of named vectors to be expanded into a grid.
#' @param source_list A list of sources to be appended to the data table.
#'
#' @return A data.table object containing the expanded grid and the source list.
#' @export
#'
#' @examples
#' named_vector_list <- list(a = 1:3, b = 4:5)
#' source_list <- list("source1", "source2")
#' get_run_table_dt(named_vector_list, source_list)
get_run_table_dt <- function(named_vector_list, source_vector) {
  
  # Validate inputs
  assert_list(named_vector_list, types = "vector", any.missing = FALSE, min.len = 1)
  assert_vector(source_vector, any.missing = FALSE, min.len = 1)
  
  run_table_dt <- data.table(expand.grid(named_vector_list))
  run_table_dt[, src := list(source_vector)]
  
  return(run_table_dt)
}


#' Run YAML from Table
#'
#' This function executes scripts specified in a data table for each combination of IDs.
#'
#' @param run_table_dt A data.table containing the scripts to run and their associated IDs.
#' @param config_path A character string specifying the path to the configuration file.
#' @param src_name A character string specifying the column name for the source scripts. Default is "src".
#'
#' @details
#' The function splits the input data table by rows and runs each script in the `src_name` column for each combination of IDs.
#' It modifies the YAML settings based on the IDs before running the scripts.
#' The column names representing the IDs must correspond to the ID names in the YAML file (eg. VAR_climate_id).
#'
#' @return This function returns `NULL` invisibly.
#'
#' @examples
#' \dontrun{
#' run_table_dt <- data.table::data.table(
#'   id1 = c(1, 2),
#'   id2 = c("A", "B"),
#'   src = c("script1.R", "script2.R")
#' )
#' config_path <- "path/to/config.yaml"
#' run_yaml_from_table(run_table_dt, config_path)
#' }
#'
#' @export
run_yaml_from_table <- function(run_table_dt, config_path, src_name = "src") {
  
  # Validate inputs
  assert_data_table(run_table_dt, min.rows = 1, min.cols = 1)
  assert_file_exists(config_path)
  assert_string(src_name)
  assert_subset(src_name, names(run_table_dt))
  
  # Get ids
  id_names <- names(run_table_dt)[which(!names(run_table_dt) %in% src_name)]
  
  # Split by rows
  rows_list <- split(run_table_dt, seq(nrow(run_table_dt)))
  
  # Run each script in src_list for each combination of ids
  invisible(lapply(rows_list, function(row) {
    ids <- unlist(row[, ..id_names])
    modify_yaml_settings_vector(config_path, ids)
    cat("\n")
    
    lapply(unlist(row[,c(..src_name)]), function(src){
      source(src)
    })
    cat("\n")
  }))
}

#' Remove Selected Variables from Environment
#'
#' This function removes all variables from a specified environment except those specified to be kept.
#'
#' @param keep_vars A character vector of variable names to keep in the environment. Default is an empty vector.
#' @param env The environment from which to remove variables. Default is the global environment.
#'
#' @return None. The function is called for its side effects.
#' @examples
#' # Create some variables in the global environment
#' a <- 1
#' b <- 2
#' c <- 3
#' 
#' # Remove all variables except 'a'
#' remove_selected_variables_from_env(keep_vars = c("a"))
#' 
#' # Check remaining variables
#' ls()
#' 
#' @export
remove_selected_variables_from_env <- function(keep_vars = c(), env = .GlobalEnv) {
  
  # Validate input
  assert_character(keep_vars, any.missing = FALSE)
  assert_environment(env)
  
  all_vars <- ls(envir = env)
  rm(list = setdiff(all_vars, keep_vars), envir = env)
  gc()
}



set_default_ids_in_yaml <- function(config_path, defaults = NULL) {
  
  if(is.null(defaults)) {
    one <- as.integer(c(1))
    zero <- as.integer(c(0))
    defaults <- list(VAR_species_id = one, 
                     VAR_management_id = zero, 
                     VAR_climate_id = one,
                     VAR_layer_id = one,
                     VAR_estimated_id = one ,
                     VAR_tabX_id = one,
                     VAR_load_tran_id = zero)
  } 
  
  
  print(paste0("Setting defaults..."))
  modify_yaml_settings_vector(config_path, defaults)
}


