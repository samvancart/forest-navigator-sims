
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
  
  if(total_forest==0) {return("no_forest")} # Document says "no forest pixel lies within the 8 km cell". Check if typo!
  
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
  if(!exists(pathVarName, where = parent.frame())) {
    path = defaultDir
  } else {
    mainDir <- eval(parse(text=pathVarName), envir = parent.frame())
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
  file_ext <- tools::file_ext(RDataFile)
  FUN <- switch (file_ext,
    "rdata" = get(load(RDataFile)),
    "rds"   = readRDS(RDataFile)
  )
  
  # temp_env <- new.env()
  obj <- FUN
  return(obj)
}

#' Load data from a file
#'
#' This function loads data from a file. It supports CSV and RData file formats.
#'
#' @param file A string representing the path to the file.
#' @return A data frame or an object loaded from an RData file.
#' @examples
#' \dontrun{
#' data <- load_data("path/to/your/file.csv")
#' data <- load_data("path/to/your/file.RData")
#' }
#' @export
load_data <- function(file) {
  # Input validation
  assertCharacter(file, len = 1)
  assertFileExists(file)
  
  file_extension <- tolower(tools::file_ext(file))
  
  if (file_extension == "csv") {
    return(fread(file))
  } else if (file_extension == "rdata") {
    return(loadRDataFile(file))
  } else {
    stop("Unsupported file type")
  }
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


#' Validate and Add Additional Named Vectors
#'
#' This helper function validates the additional named vectors and adds them to the data table.
#'
#' @param run_table_dt The data table to which additional named vectors will be added.
#' @param additional_named_vector_list A list of additional named vectors to be added to the data table.
#'
#' @return The updated data.table object with additional named vectors.
validate_and_add_vectors <- function(run_table_dt, additional_named_vector_list) {
  assert_list(additional_named_vector_list, types = "vector", any.missing = FALSE, min.len = 1)
  
  # Ensure all vectors have the same length as the expanded grid (nrows in run_table)
  grid_length <- nrow(run_table_dt)
  lapply(seq_along(additional_named_vector_list), function(i) {
    vec <- additional_named_vector_list[[i]]
    vec_name <- names(additional_named_vector_list)[i]
    assert(length(vec) == grid_length,
           paste("Length of vector", vec_name, "must be", grid_length))
  })
  
  # Add additional named vectors if provided
  for (name in names(additional_named_vector_list)) {
    run_table_dt[[name]] <- additional_named_vector_list[[name]]
  }
  
  return(run_table_dt)
}

#' Generate a Data Table from Named Vectors and Source List
#'
#' This function creates a data table by expanding a grid of named vectors and 
#' appending a source list to it. It also allows adding additional named vectors.
#'
#' @param named_vector_list A list of named vectors to be expanded into a grid.
#' @param source_list A list of sources to be appended to the data table.
#' @param additional_named_vector_list A list of additional named vectors to be added to the data table.
#' Default is NULL.
#'
#' @return A data.table object containing the expanded grid, the source list, and additional named vectors.
#' @export
#'
#' @examples
#' named_vector_list <- list(a = 1:3, b = 4:5)
#' source_list <- list("source1", "source2")
#' additional_named_vector_list <- list(c = 6:8)
#' get_run_table_dt(named_vector_list, source_list, additional_named_vector_list)
get_run_table_dt <- function(named_vector_list, source_vector, additional_named_vector_list = NULL) {
  
  # Validate inputs
  assert_list(named_vector_list, types = "vector", any.missing = FALSE, min.len = 1)
  assert_vector(source_vector, any.missing = FALSE, min.len = 1)
  
  run_table_dt <- data.table(expand.grid(named_vector_list))
  run_table_dt[, src := list(source_vector)]
  
  if (!is.null(additional_named_vector_list)) {
    run_table_dt <- validate_and_add_vectors(run_table_dt, additional_named_vector_list)
  }
  
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
    print(paste0("Defaults is NULL, no settings changed."))
  } else {
    
    print(paste0("Setting defaults..."))
    modify_yaml_settings_vector(config_path, defaults)
  }
}




#' Split Data Table into Equal Parts with Constraint
#'
#' This function splits a data table into approximately equal parts based on a specified constraint.
#'
#' @param dt A data.table object to be split.
#' @param max_part_size An integer specifying the maximum size of each part.
#' @param split_by_constraint A character vector specifying the column(s) to split by.
#' @param split_id_name A character string specifying the name of the split ID column. Default is "splitID".
#' @return A data.table object with an additional column indicating the part each row belongs to.
#' @import data.table
#' @import checkmate
#' @export
#' @examples
#' library(data.table)
#' dt <- data.table(id = 1:100, value = rnorm(100))
#' split_dt_equal_with_constraint(dt, max_part_size = 10, split_by_constraint = "id", split_id_name = "partID")
split_dt_equal_with_constraint <- function(dt, max_part_size, split_by_constraint, split_id_name = "splitID") {
  
  # Input validation
  assert_data_table(dt)
  assert_integerish(max_part_size, lower = 1, len = 1)
  assert_character(split_by_constraint, min.len = 1)
  assert_names(names(dt), must.include = split_by_constraint)
  assert_string(split_id_name)
  
  # Get dt of unique IDs
  unique_ids <- unique(dt[, ..split_by_constraint])
  
  # Get number of IDs
  n_ids <- nrow(unique_ids)
  
  # Number of parts to split dt into
  n_split_parts <- ifelse(n_ids/max_part_size < 2, 1, floor(n_ids/max_part_size))
  
  print(paste0("Splitting ", n_ids, " unique id(s) into ", n_split_parts, " part(s)..."))
  
  # Assign splitIDs
  if (n_split_parts == 1) {
    unique_ids[, (split_id_name) := 1]
  } else {
    unique_ids[, (split_id_name) := cut(seq_len(.N), breaks = n_split_parts, labels = FALSE)]
  }
  
  # Merge splitIDs into original dt
  dt <- merge(dt, unique_ids, by = split_by_constraint)
  
  print(paste0("Done."))
  
  return(dt)
}



#' Load Files Function
#'
#' This function loads files based on the provided list and a load identifier.
#'
#' @param files A character vector of file paths to be loaded.
#' @param load_id An integer indicating whether to load the files (0) or not (any other value).
#' @return Invisible NULL.
#' @import checkmate
#' @export
load_files <- function(files, load_id) {
  # Input validation
  assertCharacter(files, any.missing = FALSE, min.len = 1)
  assertIntegerish(load_id, len = 1)
  
  invisible(lapply(files, function(x) {
    varName <- sub(".*\\/([^\\/]+)\\..*", "\\1", x)
    
    if (load_id == 0) {
      print(paste0("Loading ", varName))
      load(file = x, envir = .GlobalEnv)
    } else {
      print(paste0(varName, " already loaded."))
    }
  }))
}






#' Wrap a Script into a Function with Parameters
#'
#' This function takes a script and an optional list of parameter names, and returns a new function that, when called, assigns the parameters to the specified environment and evaluates the script.
#'
#' @param script A character vector containing the R script to be wrapped.
#' @param param_names An optional character vector of parameter names to be assigned in the specified environment. If not provided, the names of the parameters passed to the wrapped function will be used.
#' @param envir The environment in which to assign the parameters. Defaults to the global environment.
#' @return A function that takes parameters specified in `param_names` (if provided) or the names of the parameters passed to the function, and evaluates the `script` in the specified environment.
#' @examples
#' script <- "print(x + y)"
#' wrapped <- wrap_script(script)
#' wrapped(x = 1, y = 2) # Should print 3
wrap_script <- function(script, param_names = NULL, envir = .GlobalEnv) {
  # Input validation
  assert_character(script)
  if (!is.null(param_names)) {
    assert_character(param_names, min.len = 1)
  }
  assert_environment(envir)
  
  # Create a function template
  wrapped_function <- function(...) {
    params <- list(...)
    if (is.null(param_names)) {
      param_names <- names(params)
    }
    
    for (name in param_names) {
      assign(name, params[[name]], envir = envir)
    }
    eval(parse(text = script), envir = envir)
  }
  return(wrapped_function)
}


#' Get File Path Vector
#'
#' This function returns a vector of file names from a specified directory.
#' Optionally, it can filter the files based on a pattern.
#'
#' @param dir_path A character string specifying the directory path.
#' @param pattern An optional character string specifying a regex pattern to filter files.
#' @return A character vector of file names.
#' @examples
#' get_file_path_vector("path/to/directory")
#' get_file_path_vector("path/to/directory", pattern = "\\.txt$")
#' @export
get_file_path_vector <- function(dir_path, pattern = NULL) {
  # Load checkmate library
  library(checkmate)
  
  # Input validation
  assertDirectoryExists(dir_path, access = "r")
  assertString(pattern, null.ok = TRUE)
  
  # List files in the directory
  files <- list.files(dir_path, full.names = T)
  
  # Filter files based on pattern if provided
  if (!is.null(pattern)) {
    # Validate the pattern
    tryCatch({
      grep(pattern, "", value = TRUE)
    }, error = function(e) {
      stop("Invalid pattern provided.")
    })
    files <- grep(pattern, files, value = TRUE)
  }
  
  # Check if no files are found
  if (length(files) == 0) {
    warning("No files found.")
  }
  
  return(files)
}


#' Build a Filename
#'
#' This function constructs a filename from given variables, a separator, and an extension, and optionally prepends a base path.
#'
#' @param name_vars A character vector of name components.
#' @param sep A character string to separate the name components. Default is "_".
#' @param ext A character string for the file extension. Default is "rdata".
#' @param base_path A character string for the base path. Default is NULL.
#' @return A character string representing the constructed filename.
#' @examples
#' build_filename(c("data", "2024", "report"))
#' build_filename(c("data", "2024", "report"), sep = "-", ext = "csv")
#' build_filename(c("data", "2024", "report"), sep = "-", ext = ".csv")
#' build_filename(c("data", "2024", "report"), base_path = "/path/to/directory")
#' @export
build_filename <- function(name_vars, sep = "_", ext = "rdata", base_path = NULL) {
  # Input validation
  checkmate::assert_character(name_vars, min.len = 1)
  checkmate::assert_string(sep)
  checkmate::assert_string(ext)
  if (!is.null(base_path)) {
    checkmate::assert_string(base_path)
  }
  
  # Ensure the extension does not start with a dot
  if (startsWith(ext, ".")) {
    ext <- substr(ext, 2, nchar(ext))
  }
  
  # Construct the filename
  filename <- paste(name_vars, collapse = sep)
  filename <- paste0(filename, ".", ext)
  
  # Prepend the base path if provided
  if (!is.null(base_path)) {
    filename <- file.path(base_path, filename)
  }
  
  return(filename)
}



