# This file has all the functions required for the acc sims (except the ones for
# data transformation operations that are in the acc_sims_prepare_init_settings.R file).
# The code is organised in sections:
# Controller functions are functions that call (possibly multiple) worker functions.

# TREEDATA_WORKER ---------------------------------------------------------



#' Sample rows until a cumulative sum threshold is reached
#'
#' @description This function shuffles a `data.table`, calculates the cumulative sum of a specified column, and selects rows until the cumulative sum reaches or exceeds a given threshold.
#'
#' @param dt A `data.table` to be sampled.
#' @param value_col A character string specifying the column name on which the cumulative sum is calculated.
#' @param threshold A numeric value representing the cumulative sum threshold.
#' @param seed An integer value for setting the seed to ensure reproducibility. Defaults to `NULL`, meaning no seed is set.
#'
#' @return A `data.table` containing the sampled rows.
#'
#' @import data.table
#' @import checkmate
#' @export
#' @examples
#' library(data.table)
#' dt <- data.table(id = 1:10, value = runif(10))
#' result <- sample_until_global_threshold(dt, "value", 3)
#' print(result)

sample_until_global_threshold <- function(dt, value_col, threshold, seed = NULL) {
  
  # Input validations
  checkmate::assert_data_table(dt)
  checkmate::assert_string(value_col)
  checkmate::assert_names(value_col, subset.of = colnames(dt))
  checkmate::assert_number(threshold)
  checkmate::assert_number(seed, null.ok = TRUE) # Allows seed to be NULL
  
  # Set seed for reproducibility if provided
  
  set.seed(seed)
  
  # Shuffle the entire data.table
  dt <- dt[sample(.N)]
  
  # Calculate cumulative sum across all rows
  dt[, cum_sum := cumsum(get(value_col))]
  
  # Select rows until the cumulative sum reaches or exceeds the threshold
  selected_dt <- dt[cum_sum <= threshold]
  
  return(selected_dt)
}

# Get proportions by group in dt
get_grouped_proportions <- function(dt, group_col) {
  group_col_name <- paste(group_col,"prop", sep = "_")
  dt[, (group_col_name) := .N, by = group_col][, (group_col_name) := get(group_col_name)/.N]
}

#' Read and Process Individual Tree Data File
#'
#' @param path The path to the data file.
#' @param threshold The threshold value for the threshold_col.
#' @param seed The seed value for reproducibility.
#' @param del_cols Character vector of column names to delete after sampling.
#' @param add_cols Named list of columns to add after sampling.
#' @param threshold_col Name of the threshold column.
#' @return A data.table with sampled data, deleted, and added columns.
read_and_process_file <- function(path, threshold, seed, del_cols, add_cols, threshold_col = "ba") {
  dt <- handle_data_input(path)
  
  sampled_dt <- sample_until_global_threshold(dt, threshold_col, threshold, seed = seed)
  
  # Delete specified columns
  if (!is.null(del_cols) && length(del_cols) > 0) {
    sampled_dt[, (del_cols) := NULL]
  }
  
  # Add specified columns
  if (!is.null(add_cols) && length(add_cols) > 0) {
    for (col_name in names(add_cols)) {
      sampled_dt[, (col_name) := add_cols[[col_name]]]
    }
  }
  
  return(sampled_dt)
}

# Helper to get threshold from initFile if not present in AAA
get_threshold_from_init_file <- function(init_file, threshold_col = "ba") {
  
  dt <- handle_data_input(init_file)
  
  assert_names(names(dt), must.include = threshold_col)
  assert_numeric(dt[[threshold_col]])
  
  threshold <- sum(dt[[threshold_col]])
  return(threshold)
}

#' Process Tree Data Files Until Basal Area Threshold Reached
#'
#' This function samples from all tree data files until the basal area (`ba`) reaches
#' a threshold, then adds specified columns from `aaa`.
#'
#' @param aaa A data frame containing initial filenames, basal area (`ba`), and other columns.
#' @param boku_data_path A character string representing the path to the data files.
#' @param seed An integer representing the seed value for reproducibility.
#' @param del_cols Character vector of column names to delete after sampling.
#' @param add_cols Character vector of column names to add from `aaa` to the sampled data.
#' @param init_file_col Character representing the column that has the initFile file names.
#' @param threshold_col Character representing the column name of basal area.
#' @return A list of data.tables with sampled data and added columns.
#' @examples
#' dts <- process_treedata_files(aaa, "path/to/data", 123, del_cols = c("cum_sum"), add_cols = c("cell"))
#' @export
process_treedata_files <- function(aaa, init_files_path, seed, del_cols = NULL, add_cols = NULL, init_file_col = "InitFileName", threshold_col = "ba") {
  # Input validations
  assertDataFrame(aaa)
  assertCharacter(init_files_path, len = 1)
  assertInt(seed)
  assertCharacter(del_cols, null.ok = TRUE)
  assertCharacter(add_cols, null.ok = TRUE)
  assert_names(names(aaa), must.include = init_file_col)
  
  threshold_col_exists <- threshold_col %in% names(aaa)
  print(threshold_col_exists)
  
  # Sample from all tree data files until basal area reaches threshold
  dts <- lapply(seq_len(nrow(aaa)), function(i) {
    row <- aaa[i, ]
    filename <- paste0(row[[init_file_col]], "_01.csv")
    path <- file.path(init_files_path, filename)
    
    if(threshold_col_exists) {
      threshold <- as.numeric(row[[threshold_col]])
    } else {
      path <- handle_data_input(path)
      threshold <- get_threshold_from_init_file(path, threshold_col)
    }
    
    # Prepare columns to add
    add_cols_list <- lapply(add_cols, function(col) row[[col]])
    names(add_cols_list) <- add_cols
    
    read_and_process_file(path, threshold, seed, del_cols, add_cols_list, threshold_col)
    
  })
  
  return(dts)
}



#' Process a Filename by Splitting and Recombining
#'
#' This function processes a given filename by splitting it using a specified separator
#' and then either truncating or keeping it as is based on a desired length.
#'
#' @param filename A character string representing the filename to be processed.
#' @param separator A character string used to split the filename.
#' @param desired_length An integer indicating the desired length of the split filename list.
#' @return A character string representing the processed filename.
#' @examples
#' process_acc_filename("file_name_part1_part2_part3", "_", 2)
#' process_acc_filename("file_name_part1_part2_part3", "_", 4)
#' @export
process_acc_filename <- function(filename, separator, desired_length) {
  # Split the filename using the specified separator
  filename_list <- unlist(strsplit(filename, separator))
  
  # Check the length of the split filename
  actual_length <- length(filename_list)
  
  # Handle different cases based on the length
  if (actual_length > desired_length) {
    # Cut from the front and collapse the list into a string
    processed_filename <- paste(tail(filename_list, desired_length), collapse = separator)
  } else if (actual_length < desired_length) {
    # Produce a warning but return the filename unchanged
    warning("The length of the filename list is shorter than the desired length.")
    processed_filename <- filename
  } else {
    # Return the filename unchanged
    processed_filename <- filename
  }
  
  return(processed_filename)
}

#' Generalized Function to Assign IDs and Unlist Data
#'
#' @param data_list A nested list of data.tables to process.
#' @param id_columns A character vector of column names used to create the ID.
#' @param separator A character string to separate the ID components.
#' @param id_column_name A character string specifying the name of the column to store the assigned ID.
#' @return A list of data.tables with assigned IDs.
#' @examples
#' tree_data_dts <- list(list(data.table(cell=1), data.table(cell=2)), list(data.table(cell=3)))
#' result <- assign_and_unlist_ids(tree_data_dts, id_columns = c("i", "j", "cell"), separator = "_", id_column_name = "forested_ha_id")
#' @export
assign_and_unlist_ids <- function(data_list, id_columns, separator = "", id_column_name = "forested_ha_id") {
  # Check input
  checkmate::assert_list(data_list, min.len = 1)
  checkmate::assert_character(id_columns, min.len = 1)
  checkmate::assert_character(separator, len = 1, null.ok = TRUE)
  checkmate::assert_character(id_column_name, len = 1)
  
  # Process each data table in the nested list
  result <- unlist(lapply(seq(data_list), function(i) {
    dts_i <- data_list[[i]]
    lapply(seq(dts_i), function(j) {
      dt <- dts_i[[j]]
      
      # Prepare the ID components
      id_values <- sapply(id_columns, function(col) {
        if (col == "i") return(i)
        if (col == "j") return(j)
        return(dt[[col]][1])
      })
      
      # Assign the ID
      dt[, (id_column_name) := paste(id_values, collapse = separator)]
      dt
    })
  }), recursive = FALSE)
  
  return(result)
}


#' Get Unique Species Codes from Initialization File
#'
#' This function reads an initialization file and returns the unique species codes from the specified column.
#'
#' @param file_path A character string specifying the path to the initialization file.
#' @param species_col A character string specifying the name of the column containing species codes. Default is "species".
#'
#' @return A vector of unique species codes.
#' @examples
#' \dontrun{
#' get_species_codes_from_init_file("path/to/init_file.csv", "species")
#' }
#' @export
get_species_codes_from_init_file <- function(file_path, species_col = "species"){
  # Validate file_path
  assertFileExists(file_path)
  
  # Read the file
  init_file <- fread(file_path)
  
  # Validate species_col
  assertChoice(species_col, choices = colnames(init_file))
  
  # Return unique species codes
  return(unique(init_file[[species_col]]))
}


#' Assign IDs, Unlist Data, and Merge with Species Codes
#'
#' This function assigns IDs to each 1 ha forest, unlists into a single list, and merges with a data.table containing species codes.
#'
#' @param tree_data_dts A nested list of data.tables to process.
#' @param id_columns A character vector of column names used to create the ID.
#' @param separator A character string to separate the ID components.
#' @param id_column_name A character string specifying the name of the column to store the assigned ID.
#' @param species_codes_dt A data.table containing species codes to merge with.
#' @param ... Additional arguments passed to the merge function.
#' @return A list of data.tables with assigned IDs and merged with species codes.
#' @examples
#' tree_data_dts <- list(list(data.table(cell=1), data.table(cell=2)), list(data.table(cell=3)))
#' fin_codes_with_speciesID_dt <- data.table(species = c(1, 2, 3), code = c(1, 2, 3))
#' result <- assign_and_merge(tree_data_dts, c("i", "j", "cell", "cell_300arcsec"), separator = "_", id_column_name = "forested_ha_id", species_codes_dt = fin_codes_with_speciesID_dt)
#' @export
assign_and_merge <- function(tree_data_dts, id_columns, separator = "", id_column_name = "forested_ha_id", species_codes_dt, ...) {
  # Validate inputs
  checkmate::assert_list(tree_data_dts, min.len = 1)
  checkmate::assert_character(id_columns, min.len = 1)
  checkmate::assert_character(separator, len = 1, null.ok = TRUE)
  checkmate::assert_character(id_column_name, len = 1)
  checkmate::assert_data_table(species_codes_dt)
  
  # Assign IDs and unlist data.tables
  dts <- assign_and_unlist_ids(tree_data_dts, id_columns, separator, id_column_name)
  
  # Merge with species codes
  dts <- lapply(dts, function(dt) {
    merge(dt, species_codes_dt, ...)
  })
  
  return(dts)
}

# TREEDATA_CONTROLLER ---------------------------------------------------------

create_acc_clustered_tree_data <- function(acc_input_obj) {
  
  with(acc_input_obj$args, {
    
    
    # Validations
    required_names <- c(
      "process_treedata_files_args", 
      "assign_and_merge_args", 
      "perform_clustering_by_group_args",
      "aaa_split_col",
      "plgid_vec",
      "aaa_file",
      "clean_data_base_path",
      "get_in_parallel_args"
    )
    
    assert_list(acc_input_obj$args)
    assert_names(names(acc_input_obj$args), must.include = required_names)
    
    
    # Run
    print(paste0("Getting tree_data..."))
    
    aaa_dt <- handle_data_input(aaa_file)
    filtered_aaa_dt <- aaa_dt[PlgID %in% plgid_vec]
    split_aaa <- split(filtered_aaa_dt, by = "PlgID")
    
    process_treedata_files_parallel_args <- 
      c(list(data = split_aaa,
             FUN = process_treedata_files,
             FUN_args = process_treedata_files_args),
        get_in_parallel_args)
    
    
    tree_data_dts <- do.call(get_in_parallel, process_treedata_files_parallel_args)
    
    cat("\n")
    cat("\n")
    print(paste0("Merging..."))
    
    assign_and_merge_args$tree_data_dts <- tree_data_dts
    dts <- do.call(assign_and_merge, assign_and_merge_args)
    
    
    cat("\n")
    print(paste0("Clustering..."))
    
    clustering_args <- c(list(data = dts, 
                              FUN = perform_clustering_by_group, 
                              FUN_args = perform_clustering_by_group_args), 
                         get_in_parallel_args)
    
    # Get clusters in parallel and combine
    all_clusters_dt <- rbindlist(
      do.call(get_in_parallel, clustering_args)
    )
    
    cat("\n")
    cat("\n")
    print(paste0("Aggregating..."))
    
    by_cols <- c(process_treedata_files_args$add_cols, perform_clustering_by_group_args$group_cols, "cluster_id")
    
    # Aggregate clustered dt
    aggr_clusters_dt <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight)/100, b = sum(ba), age = as.integer(mean(age))),
                                        by = by_cols]
    
    
    # Split for saving
    split_aggr_clusters_dt <- split(aggr_clusters_dt, by = aaa_split_col)
    
    
    cat("\n")
    print(paste0("Creating clustered_acc_init_obj_list..."))
    
    # Create list of acc_init_objects
    clustered_acc_init_obj_list <- lapply(seq_along(split_aggr_clusters_dt), function(i) {
      
      plgid <- as.integer(names(split_aggr_clusters_dt)[i])
      clustered_dt <- split_aggr_clusters_dt[[i]]
      
      # Determine save path
      save_path <- get_acc_input_save_path(plgid, "clustered_trees", clean_data_base_path)
      
      # Return the result
      list(name = "clustered", plgid = plgid, data = list(clustered_dt), save_path = save_path)
      
    })
    
    cat("\n")
    print(paste0("Done."))
    
    return(clustered_acc_init_obj_list)
    
  })
}


# DT-TRANSFORMATIONS_CONTROLLER --------------------------------------------


#' Apply Transformations and Add Columns to a Data Table
#'
#' This function applies a series of transformations to a data.table based on a list of operations.
#' Each operation specifies a function to be applied, the columns involved, and the arguments to be passed to the function.
#' The function makes a deep copy of the passed data.table to perform the operations on.
#'
#' @param dt A `data.table` object containing the data to be transformed.
#' @param operations A list of operations to be applied. Each operation should be a named list with the following components:
#' \itemize{
#'   \item \strong{col_name}: A string specifying the name of the new column to be added. Required if \code{cols} is not NULL.
#'   \item \strong{fun}: A function to be applied to the specified columns or to the entire data.table.
#'   \item \strong{cols}: A character vector specifying the columns to be passed to the function. Can be NULL.
#'   \item \strong{names_cols}: A character vector specifying the names of the arguments for the columns. Required if \code{cols} is not NULL.
#'   \item \strong{args}: A list of additional arguments to be passed to the function. Can be NULL.
#' }
#' 
#' @return The transformed `data.table`.
#' 
#' @examples
#' library(data.table)
#' library(checkmate)
#' 
#' dt <- data.table(
#'   id = 1:5,
#'   value1 = c(10, 20, 30, 40, 50),
#'   value2 = c(5, 15, 25, 35, 45),
#'   value3 = c(1, 1, 1, 1, 1),
#'   time = as.Date(c("2000-02-29", "2021-03-01", "2020-02-28", "2020-02-29", "2022-02-28"))
#' )
#' 
#' operations <- list(
#'   list(col_name = "value1", fun = scale_by_2, cols = c("value1"), names_cols = c("x"), args = list()),
#'   list(col_name = "value2", fun = log_transform, cols = "value2", names_cols = c("x"), args = list()),
#'   list(col_name = "value_sum", fun = sum_three_columns, cols = c("value1", "value2", "value3"), names_cols = c("x", "y", "z"), args = list()),
#'   list(col_name = "category", fun = categorize_value1, cols = "value1", names_cols = c("x"), args = list()),
#'   list(col_name = "time", fun = remove_feb_29, cols = NULL, args = list(time_col = "time"))
#' )
#' 
#' result_dt <- transform_and_add_columns(dt, operations)
#' print(result_dt)
#'
#' @export
transform_and_add_columns <- function(dt, operations = list()) {
  
  # Validate inputs
  assert_data_table(dt, any.missing = FALSE)
  assert_list(operations, types = "list")
  
  # Make deep copy of dt
  dt_copy <- data.table::copy(dt)
  
  for (operation in operations) {
    assert_list(operation, names = "named", min.len = 1, max.len = 5)
    assert_function(operation$fun)
    assert(checkmate::check_null(operation$cols), checkmate::check_character(operation$cols))
    assert_list(operation$args, any.missing = FALSE, null.ok = TRUE)
    assert_character(operation$cols, null.ok = TRUE)
    
    col_name <- operation$col_name
    fun <- operation$fun
    cols <- operation$cols
    names_cols <- operation$names_cols
    args <- operation$args
    
    if (is.null(cols)) {
      # Call a function that takes dt as its first argument
      dt_copy <- do.call(fun, c(list(dt_copy), args))
    } else {
      assert_list(operation, names = "named", len = 5)
      assert_string(operation$col_name)
      assert_names(colnames(dt_copy), must.include = cols)
      assert_character(operation$names_cols, any.missing = FALSE)
      
      # Apply the function to specific columns of dt
      selected_cols <- as.list(dt_copy[, ..cols])
      named_list <- setNames(selected_cols, names_cols)
      dt_copy[, (col_name) := do.call(fun, c(named_list, args))]
    }
  }
  
  return(dt_copy)
}




# INPUT-HANDLER_WORKER ------------------------------------------------------


#' Handle Data Input
#'
#' This function handles the input data, reading it with `fread` if it's a file path, or returning it directly if it's already a `data.table`.
#'
#' @param data Either a character string specifying the path to the data file or a `data.table` object.
#' @return A `data.table` containing the data.
#' @import checkmate
#' @import data.table
#' @examples
#' data_path <- "/path/to/your/data.csv"
#' data_dt <- data.table(a = 1:5, b = letters[1:5])
#' result1 <- handle_data_input(data_path)
#' result2 <- handle_data_input(data_dt)
#' @export
handle_data_input <- function(data) {
  
  # Validate input
  if (is.character(data)) {
    assert_file_exists(data, access = "r")
    dt <- fread(data)
  } else if (inherits(data, "data.table")) {
    assert_data_table(data, min.rows = 1, col.names = "strict")
    dt <- data
  } else {
    stop("The 'data' parameter should be either a path to a data file or a data.table object.")
  }
  
  return(dt)
}


# FILTER-DATA_WORKER -------------------------------------------------------


#' Filter Data by Tree Data Cells
#'
#' This function filters data based on the cells present in the provided tree data.
#'
#' @param data Either a character string specifying the path to the data file or a `data.table` object.
#' @param tree_data Either a character string specifying the path to the tree data file or a `data.table` object.
#' @return A filtered `data.table` containing data for the specified tree data cells.
#' @examples
#' data_path <- "/path/to/your/data.csv"
#' tree_data_path <- "/path/to/your/tree_data.csv"
#' data_dt <- data.table(BOKU_ID = 1:5, value = 6:10)
#' tree_data_dt <- data.table(cell = 1:5, cell_300arcsec = 1:5)
#' result1 <- filter_data_by_tree_data_cells(data_path, tree_data_path)
#' result2 <- filter_data_by_tree_data_cells(data_dt, tree_data_dt)
#' @export
filter_data_by_tree_data_cells <- function(data, tree_data) {
  # Use handle_data_input to read the data
  data_dt <- handle_data_input(data)
  tree_dt <- handle_data_input(tree_data)
  
  # Validate data structures
  assert_data_table(data_dt)
  assert_true(all(!is.na(data_dt)))
  assert_data_table(tree_dt)
  
  cells_dt <- tree_dt[which(PlgID_05 %in% unique(data_dt$PlgID_05))][, .(PlgID_05, cell_300arcsec)]
  cell_10 <- unique(cells_dt$cell_300arcsec)
  assert_integer(cell_10, len = 1)
  cells_1 <- cells_dt$PlgID_05
  assert_integerish(cells_1, min.len = 1)
  
  print(paste0("10km-by-10km cell id: ", cell_10))
  print(paste0("Found ", length(cells_1), " 1km-by-1km cell(s)."))
  
  data_dt <- data_dt[which(PlgID_05 %in% cells_1)]
  data_dt[, cell_300arcsec := cell_10]
  
  return(data_dt)
}




# CLIM-DATA_WORKER ---------------------------------------------------------



get_nYears_from_acc_tran <- function(tran_dir_path) {
  assert_directory(tran_dir_path)
  parTran_path <- list.files(tran_dir_path, pattern = "par", full.names = T)
  assert_file_exists(parTran_path)
  
  parTran <- loadRDataFile(parTran_path)
  
  return(floor(ncol(parTran)/365))
}

#' Get Dcast Matrices List from Data Table
#'
#' This function processes a data.table and returns a list of matrices created using dcast.
#'
#' @param dt A data.table containing the data.
#' @param var_cols A character vector of variable column names to be used in dcast.
#' @param dcast_formula A formula to be used for dcast.
#' @param suffix_str A string to append to the names of the matrices in the list.
#' @return A list of matrices created using dcast on the provided data.table.
#' @examples
#' dt <- data.table(cell = 1:3, day = 1:3, par = rnorm(3), vpd = rnorm(3), co2 = rnorm(3), precip = rnorm(3), tair = rnorm(3))
#' result <- get_dcast_matrices_list_from_dt(dt, var_cols = c("par", "vpd", "co2", "precip", "tair"))
#' @export
get_dcast_matrices_list_from_dt <- function(dt, 
                                            var_cols = c("par", "vpd", "co2", "precip", "tair"), 
                                            dcast_formula = as.formula(paste("cell", "~", "day")),
                                            suffix_str = "") {
  # Validate inputs
  assert_data_table(dt)
  assert_character(var_cols, min.len = 1)
  assert_subset(var_cols, names(dt))
  assert_formula(dcast_formula)
  assert_character(suffix_str, len = 1)
  
  
  print(paste0("Applying dcast on dt with formula:"))
  print(dcast_formula)
  cat("\n")
  
  # Create list of matrices
  dcast_matrices <- lapply(var_cols, function(x) {
    formula <- dcast_formula
    dcast_dt <- as.matrix(dcast(dt, formula, value.var = x))
  })
  
  print(paste0("Adding suffix_str ", "'", suffix_str, "'", " to dt names."))
  names(dcast_matrices) <- paste0(var_cols, suffix_str)
  
  return(dcast_matrices)
}


#' Add Accumulated CO2 Data
#'
#' This function adds accumulated CO2 data to a climate data table based on the given name and configuration.
#'
#' @param clim_dt data.table. The climate data table.
#' @param name Character. The name indicating the type of data.
#' @param config List. The configuration containing paths to CO2 data files.
#'
#' @return data.table. The climate data table with accumulated CO2 data added.
#' @import data.table checkmate
#' @export
#'
#' @examples
#' config <- list(VAR_acc_co2_files = list(gwl3 = "/path/to/co2_data/rcp85.csv"))
#' clim_dt <- data.table(time = as.Date(c("2020-01-01", "2021-01-01")), co2 = c(NA, NA))
#' add_acc_co2(clim_dt, "gwl3", config)
add_acc_co2 <- function(clim_dt, name, config) {
  # Validate inputs
  assert_data_table(clim_dt, any.missing = FALSE, min.rows = 1)
  assert_names(names(clim_dt), must.include = "time")
  assert_character(name, any.missing = FALSE, len = 1)
  assert_list(config, any.missing = FALSE)
  assert_names(names(config), must.include = "VAR_acc_co2_files")
  assert_list(config$VAR_acc_co2_files, types = "character", any.missing = FALSE)
  assert_true(name %in% c("historical", "detrended", "gwl2", "gwl3", "gwl4"))
  
  print(paste0("Creating CO2 for ", name, "..."))
  
  if (name %in% c("detrended", "historical")) {
    return(clim_dt)
  }
  
  co2_file_path <- config$VAR_acc_co2_files[[name]]
  
  # Validate the CO2 file path
  assert_file_exists(co2_file_path)
  
  # Read the CO2 data
  co2_dt <- fread(co2_file_path)
  
  # Validate the CO2 data table
  assert_data_table(co2_dt, any.missing = FALSE, min.rows = 1)
  assert_names(names(co2_dt), must.include = c("Year", "CO2"))
  
  # Copy and modify the climate data table
  clim_dt_copy <- data.table::copy(clim_dt)
  clim_dt_copy[, co2 := NULL]
  clim_dt_copy[, Year := year(time)]
  clim_dt_copy <- merge(clim_dt_copy, co2_dt, by = "Year")
  setnames(clim_dt_copy, old = "CO2", new = "co2")
  clim_dt_copy[, Year := NULL]
  
  return(clim_dt_copy)
}


# CLIM-DATA_CONTROLLER -----------------------------------------------------



#' Get Accumulated Initial Climate Object
#'
#' This function processes a climate data path and a tree data file to create an initial climate data object.
#'
#' @param clim_path A character string specifying the path to the climate data file.
#' @param aaa_file A character string specifying the path to the tree data file.
#' @param clean_data_base_path character. Base path to the clean data.
#' @param operations A list of operations to be applied to transform the climate data. Defaults to an empty list.
#' @param suffix_str A string to append to the names of the matrices in the list. Defaults to "Tran".
#' @param allas_opts A list of parameters for the s3_read_using function if clim_path is in an s3 bucket. Default is list().
#' @return A list containing the name, plgid, and transformed matrices.
#' @examples
#' clim_path <- "/path/to/climate_data.csv"
#' aaa_file <- "/path/to/tree_data.csv"
#' operations <- list(operation1 = "mean", operation2 = "sum") # Example operations
#' result <- get_acc_init_clim_object(clim_path, aaa_file, operations)
#' @export
get_acc_init_clim_object <- function(clim_path, aaa_file, clean_data_base_path, config, operations = list(), suffix_str = "Tran", allas_opts = list()) {
  
  # Validate inputs
  if(length(allas_opts) == 0) {
    assert_file_exists(clim_path)
  }
  assert_file_exists(aaa_file)
  assert_list(operations)
  assert_character(suffix_str, len = 1)
  assert_list(allas_opts)
  
  print(paste0("Creating init clim data object..."))
  cat("\n")
  
  name <- str_extract(clim_path, "(?<=/)[^/]+(?=_id_)")
  plgid_str <- str_extract(clim_path, "(?<=plgid_)[0-9]+")
  plgid <- as.integer(plgid_str)
  
  print(paste0("name is ", name))
  print(paste0("plgid is ", plgid))
  cat("\n")
  
  if(!length(allas_opts) == 0) {
    clim_path <- do.call(s3read_using, c(allas_opts, list(object = clim_path)))
  }
  
  filtered_clim_dt <- filter_data_by_tree_data_cells(clim_path, aaa_file)
  transformed_clim_dt <- transform_and_add_columns(filtered_clim_dt, operations)
  transformed_clim_dt_co2 <- add_acc_co2(transformed_clim_dt, name, config)
  tranMatrices <- get_dcast_matrices_list_from_dt(transformed_clim_dt_co2, suffix_str = suffix_str)
  
  # Get save path
  save_dir <- file.path("climate", name)
  save_path <- get_acc_input_save_path(plgid, save_dir, clean_data_base_path)
  
  cat("\n")
  print("Done.")
  
  return(list(name = name, plgid = plgid, data = tranMatrices, save_path = save_path))
}


# MULTI-INIT-VAR_CONTROLLER -------------------------------------------------



#' Get multiInitVar Object
#'
#' This function creates a multiInitVar object based on the provided clustered path and grid file path.
#'
#' @param clustered_path A string representing the path to the clustered R data file.
#' @param grid_file_path A string representing the path to the grid file.
#' @param clean_data_base_path character. Base path to the clean data.
#' @param ... Additional arguments passed to the create_multiInitVar_for_layers function.
#'
#' @return A list containing the plgid and multiInitVar.
#' @import checkmate
#' @import data.table
#' @import stringr
#' @examples
#' # Example usage
#' multiInitVar_obj <- get_multiInitVar_object("path/to/clustered.RData", "path/to/grid.csv")
get_multiInitVar_object <- function(clustered_path, grid_file_path, clean_data_base_path, ...) {
  
  
  # Validate inputs
  assert_character(clustered_path, any.missing = FALSE, len = 1)
  assert_file_exists(clustered_path)
  assert_character(grid_file_path, any.missing = FALSE, len = 1)
  assert_file_exists(grid_file_path)
  
  # Create multiInitVar
  multiInitVar <- create_multiInitVar_for_layers(dt_path = clustered_path, ...)
  
  # Load the grid file
  grid_dt <- fread(grid_file_path)
  assert_data_frame(grid_dt, min.rows = 1, col.names = "strict")
  
  # Get plgid from clustered path or NA if not exists
  plgid <- as.integer(str_extract(clustered_path, "(?<=plgid_)[0-9]+"))
  
  # Get plgid from grid if it is NA
  if(is.na(plgid)) {
    # Extract boku_cell_10 from clustered_path
    boku_cell_10 <- str_extract(clustered_path, "(?<=_)[0-9]+")
    
    # Validate boku_cell_10 is not missing
    assert_character(boku_cell_10, any.missing = FALSE)
    
    # Extract plgid
    plgid <- unique(grid_dt[cell_300arcsec == boku_cell_10]$PlgID)
  }
  
  
  # Get save path
  save_path <- get_acc_input_save_path(plgid, "tree_data", clean_data_base_path)
  
  return(list(name = "multiInitVar", plgid = plgid, data = list(multiInitVar), save_path = save_path))
}


# SAVE-DATA_WORKER ---------------------------------------------------------



#' Save Acc Data Object
#'
#' This function saves the data object to a specified save path.
#'
#' @param data A list containing the data to be saved.
#' @param save_path A string representing the base path where the files will be saved.
#' @param test A logical flag indicating whether to run in test mode (default is FALSE).
#' @param default_name A string representing the default name to use if an item's name is NULL.
#' @param ext = A string representing the desired file extension to use (.csv or .rdata).
#'
#' @return NULL
#' @import checkmate
#' @importFrom utils dir.create
#' @examples
#' data <- list(matrix1 = matrix(1:4, 2, 2), matrix2 = matrix(5:8, 2, 2))
#' save_data(data, "/path/to/save", default_name = "default", test = TRUE)
save_acc_data <- function(data, save_path, default_name = "default", test = FALSE, ext = ".rdata") {
  
  # Validate inputs
  assert_list(data, min.len = 1)
  assert_character(save_path, len = 1)
  assert_directory_exists(dirname(save_path), access = "w")
  assert_character(default_name, len = 1)
  assert_logical(test, len = 1)
  assert_choice(ext, c(".csv", ".rdata", ".rds"))
  
  # Save each item in the data list
  invisible(lapply(seq_along(data), function(index) {
    item <- data[[index]]
    name <- names(data)[index]
    if (is.null(name) || name == "") {
      name <- default_name
    }
    save_file_path <- file.path(save_path, paste0(name, ext))
    
    if (!test) {
      message(paste0("Saving ", name, " into ", save_file_path))
      save_file_by_extension(item, file = save_file_path)
    } else {
      message(paste0("Not saved! Set test=FALSE to save ", name, " into ", save_file_path))
    }
  }))
}




#' Save a data object to a file based on the file extension
#'
#' This function saves a data object to a file with the specified path. The file
#' extension determines the format: "csv" for CSV files and "rdata" for RData files.
#'
#' @param data A data object to save. Typically a data.frame or data.table.
#' @param file_path A character string specifying the file path. The extension must be "csv" or "rdata".
#'
#' @return NULL. The function is called for its side effect of saving the file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a sample data.table
#' dt <- data.table(id = 1:5, value = letters[1:5])
#'
#' # Save the data.table as a CSV file
#' save_file(dt, "sample.csv")
#'
#' # Save the data.table as an RData file
#' save_file(dt, "sample.rdata")
#' }
save_file_by_extension <- function(data, file_path) {
  # Validate input arguments
  assert_string(file_path, pattern = "^[a-zA-Z0-9_./-]+$")
  
  # Extract the file extension
  file_extension <- tolower(tools::file_ext(file_path))
  
  # Validate the file extension
  valid_extensions <- c("csv", "rdata", "rds")
  assert_choice(file_extension, valid_extensions)
  
  # Save the file based on the extension
  if (file_extension == "csv") {
    assert_class(data, "data.table")
    fwrite(data, file_path)
  } else if (file_extension == "rdata") {
    save(data, file = file_path)
  } else if (file_extension == "rds"){
    saveRDS(data, file = file_path)
  } else {
    stop("Unsupported file extension")
  }
  
  invisible(NULL) # Return NULL invisibly
}












#' Get ACC Input Save Path
#'
#' This function constructs and returns the save path based on the provided plgid and save directory.
#'
#' @param plgid A character string representing the plgid.
#' @param save_dir A character string representing the save directory.
#'
#' @return A character string representing the constructed save path.
#' @import checkmate
#' @examples
#' path <- get_acc_input_save_path("123", "output_dir")
get_acc_input_save_path <- function(plgid, save_dir, base_path) {
  
  # Validate inputs
  assert_integer(plgid, len = 1, any.missing = FALSE)
  assert_character(save_dir, len = 1, any.missing = FALSE)
  
  # Construct the save path
  site_folder_name <- paste0("plgid_", plgid)
  sub_dir <- file.path(site_folder_name, save_dir)
  
  return(file.path(base_path, sub_dir))
}



# SAVE-DATA_CONTROLLER -----------------------------------------------------


#' Create Directory and Save ACC Object
#'
#' This function creates a directory and saves the ACC object to a specified base path.
#'
#' @param acc_obj A list containing the ACC object. Must include elements "name", "plgid", "data", and "save_path".
#' @param base_path A string representing the base path where the files will be saved.
#' @param test A logical flag indicating whether to run in test mode (default is FALSE).
#' @param ... Additional arguments to save_acc_data.
#'
#' @return NULL
#' @import checkmate
#' @examples
#' acc_obj <- list(
#'   name = "example",
#'   plgid = "123",
#'   data = list(matrix1 = matrix(1:4, 2, 2), matrix2 = matrix(5:8, 2, 2)),
#'   save_path = "/path/to/save"
#' )
#' create_dir_and_save_acc_obj(acc_obj, "/path/to/base", test = TRUE)
create_dir_and_save_acc_obj <- function(acc_obj, base_path, test = FALSE, ...) {
  
  # Validate inputs
  assert_list(acc_obj, min.len = 4)
  assert_subset(c("name", "plgid", "data", "save_path"), names(acc_obj))
  assert_character(acc_obj$name, len = 1)
  assert_integer(acc_obj$plgid, len = 1)
  assert_list(acc_obj$data, min.len = 1)
  assert_character(acc_obj$save_path, len = 1)
  assert_character(base_path, len = 1)
  assert_directory_exists(base_path, access = "w")
  assert_logical(test, len = 1)
  
  name <- acc_obj$name
  plgid <- acc_obj$plgid
  data <- acc_obj$data
  save_path <- acc_obj$save_path
  
  # Create directory path
  if(!test) {
    get_or_create_path(pathVarName = "save_path", defaultDir = base_path, subDir = "")
    assert_directory_exists(save_path, access = "w")
  }
  
  # Save data using save_acc_data function
  save_acc_data(data = data, save_path = save_path, default_name = name, test = test, ...)
}


# SITEINFO_CONTROLLER -----------------------------------------------------


#' Get Site Information ACC Object
#'
#' This function generates an ACC object containing site information based on the provided soil data, tree data, grid file, and other parameters.
#' 
#' @param soil_dt data.table containing soil data.
#' @param tree_data data.table or character. A data.table containing tree data or a valid file path to the tree data.
#' @param grid_file_path character. Path to the grid file.
#' @param clustered_paths character vector. Paths to the clustered data files.
#' @param clean_data_base_path character. Base path to the clean data.
#' @param group_id_name character. Name of the grouping column in clustered data.
#' @param species_id_name character. Name of the species column in clustered data.
#' @param site_type_probs numeric vector. Probabilities for site types. Default is c(.1, .3, .5, .7, .9).
#' @param site_types numeric vector. Site types. Default is c(5, 4, 3, 2, 1).
#' @param rep_times numeric. Number of repetitions in soil calculations.
#' @param rep_times character. Climate scenario.
#' 
#' @return list containing the name, plgid, data, and save_path.
#' @export
#' 
#' @examples
#' # Example usage
#' # get_site_info_acc_object(soil_dt, tree_data, grid_file_path, clustered_paths, clean_data_base_path, group_id_name, species_id_name)
get_site_info_acc_object <- function(soil_dt, 
                                     tree_data,
                                     grid_file_path,
                                     clustered_paths,
                                     clean_data_base_path,
                                     group_id_name,
                                     species_id_name,
                                     site_type_probs = c(.1, .3, .5, .7, .9),
                                     site_types = c(5, 4, 3, 2, 1),
                                     rep_times = 1,
                                     clim_scen = "historical") {
  
  # Validate inputs using checkmate
  assert_data_table(soil_dt, min.cols = 1)
  
  # Check if tree_data is a data.table or a valid file path
  if (is.character(tree_data)) {
    assert_file_exists(tree_data)
  } else {
    assert_data_table(tree_data, min.cols = 1)
  }
  
  assert_file_exists(grid_file_path)
  assert_character(clustered_paths, min.len = 1)
  assert_directory_exists(clean_data_base_path)
  assert_character(group_id_name, len = 1)
  assert_character(species_id_name, len = 1)
  assert_numeric(site_type_probs, lower = 0, upper = 1, len = length(site_types))
  assert_numeric(site_types, len = length(site_type_probs))
  
  # Filter soil data by tree data cells
  filtered_soil_dt <- filter_data_by_tree_data_cells(data = soil_dt, tree_data = tree_data)
  plgid <- unique(filtered_soil_dt$PlgID)
  
  # Load grid data and find the relevant cell
  grid <- fread(grid_file_path)
  cell_10 <- unique(grid[PlgID == plgid]$cell_300arcsec)
  clustered_path <- grep(as.character(cell_10), clustered_paths, value = TRUE)
  if(length(clustered_path) == 0) {
    clustered_path <- grep(as.character(plgid), clustered_paths, value = TRUE)
  }
  
  # Determine climate directory path
  base_clim_path <- file.path(clean_data_base_path, paste0("plgid_", plgid), "climate")
  clim_dirs <- list.files(base_clim_path)
  clim_dir_paths <- file.path(base_clim_path, clim_dirs)
  clim_dir_path <- grep(clim_scen, clim_dir_paths, value = TRUE)
  clim_file_path <- list.files(clim_dir_path, full.names = TRUE)[1]
  
  
  # Get site information data table
  site_info_dt <- get_acc_site_info_dt(filtered_soil_dt,
                                       clustered_path,
                                       clim_file_path,
                                       plgid,
                                       group_id_name,
                                       species_id_name,
                                       site_type_probs,
                                       site_types,
                                       rep_times)
  
  # Determine save path
  save_path <- get_acc_input_save_path(plgid, "site", clean_data_base_path)
  
  # Return the result
  return(list(name = "siteInfo", plgid = plgid, data = list(site_info_dt), save_path = save_path))
}



# SITEINFO_WORKER ---------------------------------------------------------


#' Get Site Information Data Table
#'
#' This function generates a data.table containing site information based on the provided soil data, cluster data, and climate data.
#' 
#' @param filtered_soil_dt data.table containing soil data with columns `soil_depth`, `FC`, and `WP`.
#' @param clustered_path character. Path to the clustered data file.
#' @param clim_dir_path character. Path to the climate data directory.
#' @param plgid integer. Seed for random number generation.
#' @param group_id_name character. Name of the grouping column in clustered data.
#' @param species_id_name character. Name of the species column in clustered data.
#' @param site_type_probs numeric vector. Probabilities for site types. Default is c(.1, .3, .5, .7, .9).
#' @param site_types numeric vector. Site types. Default is c(5, 4, 3, 2, 1).
#' 
#' @return data.table with site information.
#' @export
#' 
#' @examples
#' # Example usage
#' # get_site_info_dt(filtered_soil_dt, clustered_path, clim_dir_path, plgid, group_id_name, species_id_name)
get_acc_site_info_dt <- function(filtered_soil_dt,
                                 clustered_path,
                                 clim_file_path,
                                 plgid,
                                 group_id_name,
                                 species_id_name,
                                 site_type_probs = c(.1, .3, .5, .7, .9),
                                 site_types = c(5, 4, 3, 2, 1),
                                 rep_times = 1) {
  
  # Validate inputs using checkmate
  assert_data_table(filtered_soil_dt, min.cols = 3)
  assert_file_exists(clustered_path)
  assert_file_exists(clim_file_path)
  assert_integer(plgid, lower = 1)
  assert_character(group_id_name, len = 1)
  assert_character(species_id_name, len = 1)
  assert_numeric(site_type_probs, lower = 0, upper = 1, len = length(site_types))
  assert_numeric(site_types, len = length(site_type_probs))
  assert_integer(rep_times, lower = 1)
  
  print(paste0("Creating siteInfo for PlgID ", plgid, "..."))
  
  # Load data
  clustered_dt <- loadRDataFile(clustered_path)
  clim_mat <- loadRDataFile(clim_file_path)
  
  
  lookup_col <- "PlgID_05"
  
  # Create climID data.table
  climID_dt <- data.table(COL = as.character(clim_mat[, 1]))
  setnames(climID_dt, old = "COL", new = lookup_col)
  climID_dt[, climID := .GRP, by = lookup_col]
  
  # Calculate nLayers and nSpecies
  nLayers <- clustered_dt[, .N, by = group_id_name]$N
  nSpecies <- clustered_dt[, .N, by = c(group_id_name, species_id_name)][, .N, by = group_id_name]$N
  
  # Create id_lookup table
  id_lookup <- clustered_dt[!duplicated(clustered_dt[, c(..group_id_name)])][, c(..lookup_col, ..group_id_name)]
  id_lookup[, (lookup_col) := as.character(get(lookup_col))]
  assert_true(nrow(unique(id_lookup)) == nrow(id_lookup))
  
  
  # Merge to create id_lookup_climID
  id_lookup_climID <- merge(climID_dt, id_lookup, by = lookup_col)
  
  # Extract siteID and climID
  siteID <- seq_along(id_lookup_climID[[group_id_name]])
  climID <- id_lookup_climID[["climID"]]
  nSites <- length(siteID)
  
  print(paste0("nSites is ", nSites))
  
  # Sample site types
  set.seed(plgid)
  site_type <- sample(site_types, prob = site_type_probs, size = nSites, replace = TRUE)
  
  # Calculate soil-related variables
  soil_depth_div <- filtered_soil_dt$soil_depth * 10
  fc <- rep(filtered_soil_dt$FC / soil_depth_div, rep_times)
  wp <- rep(filtered_soil_dt$WP / soil_depth_div, rep_times)
  soil_depth <- rep(soil_depth_div, rep_times)
  
  # Initialize other variables
  SWinit <- rep(160, nSites)
  CWinit <- rep(0, nSites)
  SOGinit <- rep(0, nSites)
  Sinit <- rep(20, nSites)
  
  # Create the data.table
  site_info_dt <- data.table(
    siteID = siteID,
    climID = climID,
    siteType = site_type,
    SWinit = SWinit,
    CWinit = CWinit,
    SOGinit = SOGinit,
    Sinit = Sinit,
    nLayers = nLayers,
    nSpecies = nSpecies,
    soildepth = soil_depth,
    effective_field_capacity = fc,
    permanent_wilting_point = wp
  )
  
  print("Done.")
  
  return(site_info_dt)
}











# PRODUCE-OUTPUT_HELPER ---------------------------------------------------


merge_multiOut_species_and_harv_with_out_dt <- function(out_dt, multiOut, vHarv = c(30, 2)) {
  
  # Input validations using checkmate
  assert_data_table(out_dt)  # Ensure out_dt is a data.table
  assert_array(multiOut, min.d = 4)  # Ensure multiOut is at least a 4-dimensional array
  assert_integerish(vHarv, len = 2, lower = 1, any.missing = FALSE)
  
  # Convert multi-dimensional arrays to data.table and melt them
  species <- as.data.table(melt(multiOut[,,4,,1]))
  v_harv <- as.data.table(melt(multiOut[,,vHarv[1],,vHarv[2]]))
  
  # Rename columns for clarity
  setnames(species, old = "value", new = "species")
  
  # Merge datasets
  out_dt_species <- merge(out_dt, species, by = intersect(names(out_dt), names(species)))
  v_harv_species <- merge(v_harv, species, by = intersect(names(v_harv), names(species)))
  v_harv_species[, variable := "harv"]
  
  # Combine data.tables
  out_dt_all <- rbind(out_dt_species, v_harv_species)
  
  # Return the final data.table
  return(out_dt_all)
}

sum_bioms <- function(dt, name, sum_cols, by = c("year", "site", "layer")) {
  dt[, (name) := sum(unlist(.SD)), by = by, .SDcols = sum_cols]
  return(dt)
}

calculate_lc <- function(dt, name = "Lc", height = "H", hc_base = "Hc_base") {
  dt[, Lc := pmax(get(height) - get(hc_base), 0)]
  return(dt)
}

set_output_names <- function(dt, ...) {
  setnames(dt, ...)
}

convert_output_vals_to_correct_units <- function(dt, conversions_dt) {
  invisible(  apply(conversions_dt, 1, function(row) {
    var <- row[["Variable"]]
    conversion_char <- row[["PREBASconv"]]
    conversion <- eval(parse(text = conversion_char))
    
    if(var %in% names(dt)) {
      dt[, (var) := .SD * conversion, .SDcols = var]
    }
  }))
  return(dt)
}

add_columns_to_dt <- function(dt, columns) {
  # Ensure columns is a named list
  assert_list(columns, names = "named")
  
  # Add columns to the data.table
  dt[, names(columns) := mget(names(columns), envir = as.environment(columns))]
  
  return(dt)
}


# Helper function to create a dynamic name for get_acc_out_obj.
# Detrended clim_scen becomes "historical".
# Param extra_words is a character vector that will be added to the name.
create_name <- function(model, plgid, clim_scen, man_scen, extra_words = NULL) {
  clim_scen <- ifelse(clim_scen == "detrended", "historical", clim_scen)
  output_template_vector <- c(model, plgid, clim_scen, man_scen, extra_words)
  
  return(str_c(output_template_vector, collapse = "_"))
}

# PRODUCE-OUTPUT_WORKER ----------------------------------------------------

#' Initialize Prebas Model for a Given PLG ID
#'
#' This function initializes the Prebas model for a specified PLG ID using the provided climate scenario and clean data base path.
#'
#' @param plgid Integer. The PLG ID.
#' @param clim_scen Character. The climate scenario.
#' @param clean_data_base_path Character. The base path to the clean data.
#' @param ... Additional parameters passed to InitMultiSite.
#'
#' @return A list containing the initialized Prebas model.
#' @examples
#' \dontrun{
#' init_prebas <- get_init_prebas_for_plgid(plgid = 1, clim_scen = "scenarioA", clean_data_base_path = "path/to/data")
#' }
#' @import checkmate
#' @export
get_init_prebas_for_plgid <- function(plgid, clim_scen, clean_data_base_path, ...) {
  
  # Input validations
  assertInteger(plgid, lower = 1)
  assertString(clim_scen)
  assertString(clean_data_base_path)
  assertDirectoryExists(clean_data_base_path)
  
  base_path <- file.path(clean_data_base_path, paste0("plgid_", plgid))
  base_clim_path <- file.path(base_path, "climate")
  
  clim_dirs <- list.files(base_clim_path)
  clim_dir_paths <- file.path(base_clim_path, clim_dirs)
  clim_dir_path <- grep(clim_scen, clim_dir_paths, value = TRUE)
  
  
  required_files <- c("parTran", "co2Tran", "tairTran", "vpdTran", "precipTran")
  
  # Loading required files using lapply
  climate_data <- lapply(required_files, function(file) {
    file_path <- list.files(clim_dir_path, pattern = file, full.names = T)[1]
    assertFileExists(file_path, access = "r")
    mat <- loadRDataFile(file_path)
    mat <- apply(mat, 2, as.numeric)
  })
  
  names(climate_data) <- required_files
  
  site_info_path <- list.files(file.path(base_path, "site"), pattern = "siteInfo", full.names = T)[1]
  multi_init_var_path <- list.files(file.path(base_path, "tree_data"), pattern = "multiInitVar", full.names = T)[1]
  
  # Additional input validations
  assertFileExists(site_info_path, access = "r")
  assertFileExists(multi_init_var_path, access = "r")
  
  siteInfo <- loadRDataFile(site_info_path)
  multiInitVar <- loadRDataFile(multi_init_var_path)
  
  nYears <- get_nYears_from_acc_tran(clim_dir_path)
  nSites <- nrow(siteInfo)
  
  print("Initialising model...")
  initPrebas <- InitMultiSite(nYearsMS = rep(nYears, nSites),
                              siteInfo = as.matrix(siteInfo),
                              multiInitVar = multiInitVar,
                              PAR = climate_data[["parTran"]][,-1], # Remove cell column (column was required for siteInfo)
                              VPD = climate_data[["vpdTran"]][,-1],
                              CO2 = climate_data[["co2Tran"]][,-1],
                              Precip = climate_data[["precipTran"]][,-1],
                              TAir = climate_data[["tairTran"]][,-1],
                              ...)
  
  print("Done.")
  
  return(initPrebas)
}


#' Execute Model Function with Initialized Prebas Model
#'
#' This function executes a given model function with the initialized Prebas model and additional arguments.
#'
#' @param FUN Function. The model function to be executed.
#' @param initPrebas List. The initialized Prebas model.
#' @param ... Additional arguments passed to the model function.
#'
#' @return The output of the model function.
#' @examples
#' \dontrun{
#' modOut <- get_modOut(FUN = myModelFunction, initPrebas = initPrebas, arg1 = val1, arg2 = val2)
#' }
#' @import checkmate
#' @export
get_modOut <- function(FUN, initPrebas, ...) {
  
  # Input validations
  assertFunction(FUN)
  assertList(initPrebas)
  
  FUN_args <- c(list(initPrebas), ...)
  return(do.call(FUN, FUN_args))
}



#' Get Site ID Lookup
#'
#' This function generates a lookup table for site IDs based on the provided PlgID. The table contains the PlgID_05 and
#' the forest_type of each site.
#'
#' @param plgid Numeric. The PlgID for which the lookup table is generated.
#' @param filtered_selection_path Character. Path to the filtered selection data file.
#' @param clustered_base_path Character. Path to the directory containing the clustered data files.
#' @param aaa_path Character. Path to the AAA data file.
#' @param id_col_name Character. Name of the ID column in the clustered data. Default is "forested_ha_id".
#' @param sep Character. Separator used in the ID column. Default is "_".
#' @param keep Integer. Position of the element to extract from the ID column. Default is 2.
#'
#' @return A data.table containing the site lookup information.
#' @import data.table
#' @import checkmate
#' @export
#'
#' @examples
#' \dontrun{
#' get_siteID_lookup(
#'   plgid = 12345,
#'   filtered_selection_path = "path/to/filtered_selection.csv",
#'   clustered_base_path = "path/to/clustered/data",
#'   aaa_path = "path/to/aaa.csv"
#' )
#' }

get_siteID_lookup <- function(plgid, 
                              filtered_selection_path, 
                              clean_data_base_path, 
                              aaa_path, 
                              id_col_name = "forested_ha_id",
                              use_id = "PlgID_05",
                              sep = "_",
                              keep = 2) {
  
  # Input validations using checkmate
  assert_number(plgid, finite = TRUE)
  assert_file_exists(filtered_selection_path, access = "r")
  assert_directory_exists(clean_data_base_path)
  assert_file_exists(aaa_path, access = "r")
  assert_string(id_col_name)
  assert_string(sep)
  assert_integerish(keep, lower = 1, upper = 4, len = 1)
  
  # Read the filtered selection data and filter by PlgID
  selection_plgid_dt <- fread(filtered_selection_path)[PlgID == plgid]
  if (nrow(selection_plgid_dt) == 0) stop("No data found for the given PlgID")
  
  # Load the clustered data
  # clustered_dt <- loadRDataFile(file.path(clean_data_base_path, paste0("clustered_", cell_10, ".rdata")))
  clustered_path <- list.files(clean_data_base_path, 
                               pattern = paste0("clustered_plgid_", plgid), full.names = T, recursive = T)[1]
  clustered_dt <- loadRDataFile(clustered_path)
  if (is.null(clustered_dt) || nrow(clustered_dt) == 0) stop("No clustered data found")
  
  
  # Create a lookup table and extract necessary columns
  assert_names(names(clustered_dt), must.include = c(use_id, id_col_name))
  clustered_dt_lookup <- unique(clustered_dt[, c(..use_id, ..id_col_name)])
  clustered_dt_lookup[, site := as.integer(unlist(tstrsplit(get(id_col_name), split = sep, keep = keep)))]
  
  # Read the AAA data
  aaa_all <- fread(aaa_path)
  if (is.null(aaa_all) || nrow(aaa_all) == 0) stop("No AAA data found")
  
  # Merge with AAA data to include forest type
  siteID_all_lookup <- merge(clustered_dt_lookup, 
                             aaa_all[PlgID == plgid][, c(..use_id, "forest_type")], 
                             by = c(use_id))
  
  # Select final columns for the output
  siteID_lookup <- siteID_all_lookup[, .(site, PlgID_05, forest_type)]
  
  # Return the final lookup table
  return(siteID_lookup)
}



# TO DO: Add start_year to run_table?

# Transform multiOut to melted output table
get_acc_output_dt <- function(plgid, model, country, 
                              clim_scen, man_scen, canopy_layer,
                              vHarv, varOutID, multiOut, selection_path,
                              clean_data_base_path, aaa_file,
                              conversions_path,
                              species_lookup_path, ...) {
  
  
  assert_array(multiOut, d = 5)
  
  # Load lookup files
  siteID_lookup <- get_siteID_lookup(plgid, selection_path, clean_data_base_path, aaa_file)
  conversions_dt <- fread(conversions_path)
  species_lookup <- fread(species_lookup_path)

  # Get multiOut as dt
  out_dt <- as.data.table(melt(multiOut[,, varOutID,, 1]))
  
  print("out_dt")
  print(out_dt)
  
  out_dt_wide <- dcast.data.table(out_dt, site + year + layer ~ variable, value.var = "value")

  dt <- apply_output_operations(dt = out_dt_wide,
                                multiOut = multiOut,
                                conversions_dt = conversions_dt,
                                siteID_lookup = siteID_lookup,
                                species_lookup = species_lookup,
                                model = model,
                                country = country,
                                clim_scen = clim_scen,
                                man_scen = man_scen,
                                canopy_layer = canopy_layer,
                                start_year = start_year,
                                vHarv = vHarv)
  
  return(dt)
  
}


apply_output_operations <- function(dt, multiOut, conversions_dt, siteID_lookup, species_lookup, 
                                    model, country, clim_scen, man_scen, canopy_layer, start_year,
                                    vHarv = c(30,2)) {
  
  
  stem_cols <- c("Wstem", "Wbranch")
  root_cols = c("WfineRoots", "W_croot")
  old_output_col_names <- c("Units", "forest_type", "value", "year", "variable", "layer")
  new_output_col_names <- c("Unit", "Mixture_type", "Value", "Year", "Variable", "Layer")
  del_output_cols <- c("site", "species")
  output_col_order <- c("Model", "Country", "Climate_scenario", "Management_scenario", "PlgID_05", "Mixture_type", "Species", 
                       "Canopy_layer", "Variable", "Unit", "Year", "Value")
  
  # Define additional columns to add
  add_cols <- list(Model = model, Country = country, Climate_scenario = clim_scen,
                   Management_scenario = man_scen, Canopy_layer = canopy_layer)
  
  # Sum biomass for stems
  dt <- sum_bioms(dt, name = "stem_biom", sum_cols = stem_cols)
  
  # Sum biomass for roots
  dt <- sum_bioms(dt, name = "root_biom", sum_cols = root_cols)
  
  # Calculate crown length from height and base height
  dt <- calculate_lc(dt)
  
  # Remove unnecessary columns (stem biomass and crown base height)
  dt <- del_dt_cols(dt, del_cols = c(stem_cols, hc_base_col))
  
  # Rename columns using conversion table
  dt <- set_output_names(dt, old = conversions_dt$PREBAS, new = conversions_dt$Variable, skip_absent = TRUE)
  
  # Convert output values to correct units
  dt <- convert_output_vals_to_correct_units(dt, conversions_dt)
  
  # Reshape the data from wide to long format
  dt <- melt.data.table(dt, id.vars = c("site", "year", "layer"))
  
  # Merge multiOut species and harvest information
  dt <- merge_multiOut_species_and_harv_with_out_dt(dt, multiOut, vHarv)
  
  # Merge units from conversion table
  dt <- merge.data.table(dt, conversions_dt[, c("Variable", "Units")], by.x = "variable", by.y = "Variable")
  
  # Merge siteID lookup to get site details
  dt <- merge.data.table(dt, siteID_lookup, by = c("site"))
  
  # Add species codes based on species lookup
  dt <- dt[species_lookup[, c("speciesID", "prebas_species_code")], on = .(species = speciesID), Species := i.prebas_species_code]

  # Convert layer column to integer format
  dt[, layer := as.integer(unlist(tstrsplit(dt$layer, split = " ", keep = 2)))]
  
  # Add model, country, climate scenario, management scenario, and canopy layer columns
  dt <- add_columns_to_dt(dt, columns = add_cols)
  
  # Adjust year values based on the starting year
  dt[, year := as.integer(dt$year + (as.integer(start_year) - 1))]
  
  # Rename columns for final output format
  dt <- set_output_names(dt, old = old_output_col_names, new = new_output_col_names, skip_absent = TRUE)
  
  # Remove unnecessary columns
  dt <- del_dt_cols(dt, del_cols = del_output_cols)
  
  # Set final column order for output
  setcolorder(dt, neworder = output_col_order)
  
  return(dt)
}


# Get d_class dt and tranform into output format.
get_dclass_acc_output_dt <- function(plgid, model, country, 
                              clim_scen, man_scen, canopy_layer, 
                              multiOut, selection_path,
                              clean_data_base_path, aaa_file,
                              species_lookup_path, d_class = 5, ...) {
  
  
  assert_array(multiOut, d = 5)
  
  # Load lookup files
  siteID_lookup <- get_siteID_lookup(plgid, selection_path, clean_data_base_path, aaa_file)
  species_lookup <- fread(species_lookup_path)
  
  d_class_dt <- n_by_d_class_dt(prebas_out = multiOut, d_class = d_class, is_multiOut = TRUE)
  
  dt <- apply_dclass_operations(dt = d_class_dt,
                                siteID_lookup = siteID_lookup,
                                species_lookup = species_lookup,
                                model = model,
                                country = country,
                                clim_scen = clim_scen,
                                man_scen = man_scen,
                                canopy_layer = canopy_layer,
                                start_year = start_year)
  
  return(dt)
  
}


apply_dclass_operations <- function(dt, siteID_lookup, species_lookup, model, country, 
                                    clim_scen, man_scen, canopy_layer, start_year) {
  
  
  old_output_col_names <- c("forest_type", "year")
  new_output_col_names <- c("Mixture_type", "Year")
  del_output_cols <- c("site", "species")
  output_col_order <- c("Model", "Country", "Climate_scenario", "Management_scenario", "PlgID_05", "Mixture_type", "Species",
                       "Canopy_layer", "Variable", "Unit", "Year")
  
  
  # Define the d_class cols for ordering later
  d_class_cols <- names(dt)[!names(dt) %in% c("site", "year", "species")]
  
  # Define additional columns to add
  add_cols <- list(Model = model, Country = country, Climate_scenario = clim_scen,
                   Management_scenario = man_scen, Canopy_layer = canopy_layer)
  
  # Merge siteID lookup to get site details
  dt <- merge.data.table(dt, siteID_lookup, by = c("site"))
  
  # Add species codes based on species lookup
  dt <- dt[species_lookup[, c("speciesID", "prebas_species_code")], on = .(species = speciesID), Species := i.prebas_species_code]
  
  # Add model, country, climate scenario, management scenario, and canopy layer columns
  dt <- add_columns_to_dt(dt, columns = add_cols)
  
  # Adjust year values based on the starting year
  dt[, year := as.integer(dt$year + (as.integer(start_year) - 1))]
  
  # Add variable col
  dt[, Variable := "dbh_dist"]
  
  # Add unit col
  dt[, Unit := "N/ha"]
  
  # Rename columns for final output format
  dt <- set_output_names(dt, old = old_output_col_names, new = new_output_col_names, skip_absent = TRUE)
  
  # Remove unnecessary columns
  dt <- del_dt_cols(dt, del_cols = del_output_cols)
  
  # Add d_class cols to all cols vector
  output_col_order_dclass <- c(output_col_order, d_class_cols)
  
  # Set final column order for output
  setcolorder(dt, neworder = output_col_order_dclass)
  
  return(dt)
}


# Construct table name and save path then combine with data and plgid into 
# acc object for saving 
get_acc_out_obj <- function(out_dt, model, plgid, clim_scen, man_scen, 
                            save_path, test_run = FALSE, extra_words = NULL) {
  # Validate data
  if (!test_run) {
    assert_data_table(out_dt)
  } else {
    assert_list(out_dt)
  }
  
  # Generate name
  name <- create_name(model, plgid, clim_scen, man_scen, extra_words)
  
  # Create and return output object
  acc_out_obj <- list(name = name, plgid = plgid, data = list(out_dt), save_path = save_path)
  
  return(acc_out_obj)
}




handle_acc_test_run <- function(plgid, output_base_path, initPrebas, modOut, multiOut,
                                model, clim_scen, man_scen) {
  
  data <- list(initPrebas = initPrebas, modOut = modOut, multiOut = multiOut)
  
  # save_path not used in test run so it can be output_base_path
  output_object <- get_acc_out_obj(data, model, plgid, 
                                   clim_scen, man_scen, save_path = output_base_path, test_run = T)
  
  
  output_object$name <- paste0("testRun_", output_object$name)
  
  return(output_object)
}

# PRODUCE-OUTPUT_CONTROLLER ------------------------------------------------




# Wrapper to call the function that runs the model and gets the acc_output_obj.
# The run_table contains the varying values. Paths contains the static values (the paths).
# FUN is the function to call. The function is called sequentially for run_table rows.
# Returns a list of the objects returned from each call to FUN.
acc_run_table_controller <- function(run_table, paths, FUN = produce_acc_output_obj, ...) {
  
  assert_data_table(run_table, min.rows = 1)
  assert_list(paths)
  assert_function(FUN)
  
  result_list <- apply(run_table, 1, function(row) {
    
    vals <- as.list(row)
    args <- c(vals, paths, ...)
    
    do.call(FUN, args)
  })
  
  return(result_list)
}



# Get initPrebas and run regionPrebas to get multiOut then process into 
# acc output data.table using output_operations.
# Returns acc_output_obj that is used for saving the data.
# When test_run=TRUE returns acc_output_obj with data is a list containing initPrebas, modOut and multiOut
produce_acc_output_obj <- function(plgid, model, country, clim_scen, man_scen,
                                   canopy_layer, man_init_args,
                                   varOutID, vHarv,
                                   clean_data_base_path,
                                   selection_path, aaa_file,
                                   conversions_path, output_base_path,
                                   species_lookup_path, output_save_dir, dclass_save_dir, test_run = F, ...) {
  
  
  # Check input validity
  # Values
  assert_integer(plgid, len = 1)
  assert_character(model, len = 1)
  assert_character(country, len = 1)
  assert_character(clim_scen, len = 1)
  assert_character(man_scen, len = 1)
  assert_numeric(canopy_layer, len = 1)
  assert_list(man_init_args, min.len = 0)
  assert_numeric(vHarv)
  assert_numeric(varOutID)
  
  # Paths
  assert_directory_exists(clean_data_base_path)
  assert_file_exists(selection_path)
  assert_file_exists(aaa_file)
  assert_file_exists(conversions_path)
  assert_file_exists(species_lookup_path)
  
  assert_character(output_base_path) # This is an Allas path
  
  
  
  print(paste0("Creating output for plgid ", plgid, "..."))
  
  # Get initPrebas
  init_args <- list(plgid = plgid, clim_scen = clim_scen, clean_data_base_path = clean_data_base_path)
  initPrebas <- do.call(get_init_prebas_for_plgid, c(init_args, man_init_args))
  
  # Get modOut
  modOut <- get_modOut(regionPrebas, initPrebas)
  
  # Get multiOut
  multiOut <- modOut$multiOut
  
  
  if(test_run) {
    print(paste0("test_run = TRUE, returning initPrebas, modOut and multiOut."))
    
    output_object <- handle_acc_test_run(plgid = plgid, output_base_path = output_base_path, 
                                         initPrebas = initPrebas, modOut = modOut, multiOut = multiOut,
                                         model = model, clim_scen = clim_scen, man_scen = man_scen)
    
    return(output_object)
  }
  
  print(paste0("Creating output from multiOut..."))
  
  # Get output dt
  out_dt_melted <- get_acc_output_dt(plgid, model, country, clim_scen, 
                                     man_scen, canopy_layer,
                                     vHarv, varOutID, multiOut, selection_path,
                                     clean_data_base_path, aaa_file,
                                     conversions_path,
                                     species_lookup_path, ...)
  
  print("Done.")
  
  print("Creating acc output object...")
  
  # Create save path for get_acc_out_obj FUN
  out_save_path <- file.path(output_base_path, output_save_dir)
  
  main_output_object <- get_acc_out_obj(out_dt_melted, model, plgid, 
                                   clim_scen, man_scen, out_save_path)
  
  print("Done.")
  
  
  print(paste0("Creating dbh class dt..."))
  
  dclass_dt <- get_dclass_acc_output_dt(plgid, model, country, 
                                        clim_scen, man_scen, canopy_layer, 
                                        multiOut, selection_path,
                                        clean_data_base_path, aaa_file,
                                        species_lookup_path, d_class = 5, ...)
  
  
  print("Done")
  
  print("Creating d_class acc output object...")
  
  # Create save path for get_acc_out_obj FUN
  dclass_save_path <- file.path(output_base_path, dclass_save_dir)
  
  dclass_output_object <- get_acc_out_obj(dclass_dt, model, plgid, 
                                   clim_scen, man_scen, dclass_save_path, extra_words = "dbh-dist")
  
  print("Done.")
  
  output_object <- list(main_output_object = main_output_object, dclass_output_object = dclass_output_object)
  
  return(output_object)
}



# ZIP_WORKER --------------------------------------------------------------



# Helper to create list of dts for zipping files
get_split_grouped_output_dt <- function(output_paths, 
                                        output_base_path = "data/acc/output/simulation_sites_200", 
                                        zip_folder_name = "zip") {
  
  zip_dt <- as.data.table(tstrsplit(basename(output_paths), split = "[_.]"))
  zip_dt[, path := output_paths]
  zip_dt[, zip_id := .GRP, by = c("V3", "V4")]
  zip_dt[, name := paste0(V3, "_", V4, ".zip")]
  zip_folder_path <- file.path(output_base_path, zip_folder_name)
  zip_dt[, full_zip_path := file.path(zip_folder_path, name)]
  
  return(split(zip_dt, by = c("zip_id")))
}


#' Zip Output Files
#'
#' This function creates a zip archive containing specified files, ensuring that 
#' only the filenames (without their full paths) are included in the archive.
#'
#' @param full_zip_path Character. The full path to the zip file to be created.
#' @param file_paths Character vector. A vector of file paths to include in the zip archive.
#' @param original_wrkdir Character. The original working directory. Defaults to the current working directory.
#'
#' @return Creates a zip archive at the specified location.
#' @import checkmate
#' @export
#'
#' @examples
#' full_zip_path <- "/path/to/your/output/archive.zip"
#' file_paths <- c("/path/to/your/output_files/file1.txt", "/path/to/your/output_files/file2.txt")
#' zip_output_files(full_zip_path, file_paths)
zip_output_files <- function(full_zip_path, file_paths, original_wrkdir = getwd(), ...) {
  # Validate inputs
  assert_character(file_paths, any.missing = FALSE, min.len = 1)
  assert_directory_exists(dirname(full_zip_path))
  
  wrkdir <- original_wrkdir
  on.exit(setwd(wrkdir))
  zip_to_path <- file.path(full_zip_path)
  
  # Extract directory paths and filenames
  zip_files_dir <- unique(dirname(file_paths))
  zip_files <- basename(file_paths)
  
  # Validate that all file paths have the same directory
  assert_true(length(zip_files_dir) == 1)
  
  setwd(zip_files_dir)
  
  # Zip
  utils::zip(zip_to_path, files = basename(zip_files))
  
  # Validate that the zip file was created
  assert_file_exists(zip_to_path)
}


zip_output_files_using <- function(FUN, zipfile, extra_FUN_args = list(), files, original_wrkdir = getwd(), ...) {
  # Validate inputs
  assert_character(files, any.missing = FALSE, min.len = 1)
  assert_directory_exists(dirname(zipfile))
  
  wrkdir <- original_wrkdir
  on.exit(setwd(wrkdir))
  zip_to_path <- file.path(zipfile)
  
  # Extract directory paths and filenames
  zip_files_dir <- unique(dirname(files))
  zip_files <- basename(files)
  
  # Validate that all file paths have the same directory
  assert_true(length(zip_files_dir) == 1)
  
  setwd(zip_files_dir)
  
  args <- c(list(zipfile = zipfile, files = zip_files), extra_FUN_args)
  
  do.call(FUN, args)
  
  # Validate that the zip file was created
  assert_file_exists(zip_to_path)
}



# ZIP_CONTROLLER ----------------------------------------------------------


### Load files from allas, zip them and finally move the files to a location (Allas/filesystem/both).
#
# When move_to_path is provided and is a path, the zipped file will be written into the Allas bucket provided in 
# save_or_put_opts using <move_to_path/basename(zipfile)> as the object key. If zipfile is an absolute path to a 
# existing directory and move_to_path is a path then the file will be written to both zipfile and Allas.
#
# Note: When zipfile is not an absolute path, it is viewed as a path relative to the temporary directory path that is 
# created by the function. This directory is deleted on exiting the function. 
# If you want the zipfile to be written directly into a directory then you must specify the absolute path to the directory.
#
load_zip_move <- function(zipfile, 
                          files, 
                          zip_opts = list(),
                          move_to_path = NULL,
                          save_or_put_opts = list(),
                          ...) {
  
  
  temp_dir <- tempdir()
  on.exit(
    if(dir.exists(temp_dir)) unlink(temp_dir, recursive = T)
  )
  
  
  # Get files from Allas. Specify cores and type inside ... to run in parallel.
  files <- unlist(do.call(get_in_parallel, c(list(data = files,
                                                  FUN = save_or_put_allas,
                                                  FUN_args = c(list(FUN = save_object,
                                                                    base_dir = temp_dir),
                                                               save_or_put_opts)),
                                             
                                             ...)))
  
  
  
  # Check that all the files were created
  invisible(sapply(files, assert_file_exists))
  
  # Check zipfile dir exists if not temp_dir
  if(dirname(zipfile) != ".") {
    assert_directory_exists(dirname(zipfile))
    print(paste0("Zipping to ", zipfile, "..."))
  } else {
    print(paste0("Zipping to tempdir..."))
  }
  
  
  # Zip the files
  zip_args <- c(list(zipfile = zipfile, files = files), zip_opts)
  t <- system.time(
    do.call(zip_output_files_using, zip_args)
  )
  
  print(t)
  
  cat("\n")
  
  
  # Put object to Allas
  if(!is.null(move_to_path)) {
    object <- file.path(move_to_path, basename(zipfile))
    
    message(paste0("Saving into Allas path: ", object, "..."))
    
    # Set default base_dir
    base_dir <- dirname(zipfile)
    
    # Where to look for the object to put. Either temp_dir or zipfile dir.
    zipfile_in_temp <- file.exists(file.path(temp_dir, basename(zipfile)))
    if(zipfile_in_temp) {
      base_dir <- temp_dir
    }
    
    # Save to Allas
    save_or_put_allas(FUN = put_object,
                      base_dir = base_dir,
                      object = object,
                      save_or_put_opts)
  }
  
  
}

# # Helper to execute zip_output_files in parallel
# zip_output_files_from_dt <- function(zip_dt, ...) {
#   zip_dt[, zip_output_files(unique(full_zip_path), path, ...), by = zip_id]
# }


# SPECIES-LOOKUP_WORKER ----------------------------------------------------


get_prebas_species_codes_from_pCROB <- function(pCROB) {
  prebas_species_lookup <- data.table(
    prebas_species_name = names(pCROB[1,]),
    prebas_latin_name = c("Pinus sylvestris", "Picea abies", "Betula alba", "Fagus sylvatica", "Pinus pinaster", "Eucalyptus globulus", "Robinia pseudoacacia", "Populus tremula", "Eucalyptus gunnii", "Picea abies (DE)", "Quercus ilex", "Fagus sylvatica (Boreal)"),
    prebas_species_code = c("Pinsyl", "Picabi", "Betalb", "Fagsyl", "Pinpin", "Eucglo", "Robpse", "Poptre", "Eucgun", "Picabi", "Queile", "Fagsyl")
  )
  
  prebas_species_lookup[, speciesID := .GRP, by = c("prebas_species_name")]
  return(prebas_species_lookup)
}








# COUNTRY-CODES_WORKER -----------------------------------------------------
get_acc_country_codes_lookup <- function(aaa, country_codes, aaa_cols = c("PlgID", "Country_Code"), ...) {
  aaa_country_codes <- aaa_all[, ..aaa_cols]
  aaa_country_codes <- aaa_country_codes[!duplicated(aaa_country_codes)]
  country_codes_lookup <- merge(country_codes, aaa_country_codes, ...)
  assert_true(nrow(aaa_country_codes) == nrow(country_codes_lookup))
  country_codes_lookup
}


# RUN-TABLE_WORKER ---------------------------------------------------------

expand_vectors_to_dt <- function(vectors_list) {
  # Create expanded grid
  expanded_grid <- expand.grid(vectors_list)
  
  # Convert to data.table
  result_dt <- as.data.table(expanded_grid)
  
  # Set column names
  setnames(result_dt, names(vectors_list))
  
  return(result_dt)
}


# For creating management table with management params as list and id_vars that 
# can be merged with another table that is in this case the table with the other run table vars.
create_table_from_vars <- function(id_vars, value_vars_list, result_name = "result") {
  # Check input validity
  assert_list(id_vars)
  assert_true(all(vapply(id_vars, is.vector, logical(1))))
  assert_list(value_vars_list)
  assert_true(all(vapply(value_vars_list, length, integer(1)) == 1))
  assert_character(result_name, len = 1)
  
  # Create a data.table with id_vars
  dt <- as.data.table(id_vars)
  
  # Combine value_vars into a single list
  combined_value_vars <- list(value_vars_list)
  
  # Add the combined value_vars list as a new column
  dt[, (result_name) := combined_value_vars]
  
  return(dt)
}







# AWS-S3-HELPER_WORKER --------------------------------------------------


# Load or put object
# FUN is either save_object or put_object from the aws.S3 lib. 
# Object is the full allas object key. base_dir is the directory to save to/load from.
save_or_put_allas <- function(FUN, base_dir, object, ...) {
  
  # Input validation
  assert_function(FUN)
  
  file <- file.path(base_dir, basename(object))
  args <- c(list(object =  object, file = file), ...)
  do.call(FUN, args)
  return(file)
}



# Function to list all objects in an S3 bucket 1000 at a time 
# (the maximum in the aws.s3 package get_bucket function).
# If there are more than 1000 objects then they a collected in a loop by moving
# the marker param to start from the end of the last batch of fetched objects.
#
# Returns a list of the aws.s3 objects that were found or a character vector of 
# the unique keys of the objects when only_keys=TRUE
list_all_objects_in_bucket <- function(only_keys = F, ...) {
  all_objects <- list()
  marker <- NULL
  total_objects <- 0
  
  while(TRUE) {
    
    
    # Retrieve a batch of objects
    batch <- get_bucket(marker = marker, max = 1000, ...)
    
    total_objects <- total_objects + length(batch)
    
    # Update the marker to the last key in the current batch
    marker <- tail(batch, 1)$Contents$Key
    
    # If no more objects are returned, break the loop
    if (length(batch) == 0 | is.null(marker)) break
    
    # Append the batch to the list of all objects
    all_objects <- c(all_objects, batch)
    
  }
  
  print(paste0("Found a total of ", total_objects, " objects."))
  
  if(only_keys) return(unique(rbindlist(all_objects)$Key))
  
  return(all_objects)
}


# DCLASS_WORKER ----------------------------------------------------------

# n_by_d_class_dt helper
# This function processes the `multiOut` data from the `prebOut` object, melts it, renames columns, merges data.tables
d_class_melt_and_merge <- function(multiOut) {
  
  # Melt the data
  n_melt <- melt(multiOut[,,17,,1])
  d_melt <- melt(multiOut[,,12,,1])
  species_melt <- melt(multiOut[,,4,,1])
  
  # Convert to data.table
  n_melt <- data.table(n_melt)
  d_melt <- data.table(d_melt)
  species_melt <- data.table(species_melt)
  
  # Rename columns
  setnames(n_melt, "value", "N")
  setnames(d_melt, "value", "D")
  setnames(species_melt, "value", "species")
  
  # Merge tables
  merged_dt <- merge(n_melt, d_melt, by = c("site", "year", "layer"))
  merged_dt <- merge(merged_dt, species_melt, by = c("site", "year", "layer"))
  
  return(merged_dt)
}

# n_by_d_class_dt helper
# Assigns classes based on `D`
d_class_assign_classes <- function(merged_dt, d_class, max_d_class) {
  max_d <- pmin(max(merged_dt$D, na.rm = TRUE), max_d_class - d_class)
  breaks <- seq(0, max_d + d_class, by = d_class)
  labels <- paste(head(breaks, -1), tail(breaks, -1), sep = "_")
  merged_dt[, class := cut(D, breaks = breaks, labels = labels, right = FALSE)]
  
  return(merged_dt)
}

# n_by_d_class_dt helper
# Calculates the number of trees by classes of DBH by `site`, `year`, `species`
d_class_calculate_n_class <- function(d_class_dt, max_d_class) {
  result <- unique(d_class_dt[, .(Nclass = as.integer(sum(N))), by = .(site, year, species, class)][, .(site, year, Nclass, class, species)])
  result[is.na(class), class := paste0(">", max_d_class)] # NAs are max_d class
  return(result)
}

# Dcast d_class dt to wide format with d_class labels as columns and n as values. 
d_class_get_dcast_dt <- function(d_class_dt, d_class, max_d_class) {
  
  dcast_dt <- dcast.data.table(d_class_dt, as.formula(paste("site + year + species ~ class")), value.var = "Nclass", fill = 0)
  
  breaks <- seq(0, max_d_class, by = d_class)
  max_d_class_label <- paste0(">", max_d_class)
  labels <- c(paste(head(breaks, -1), tail(breaks, -1), sep = "_"), max_d_class_label)
  
  add_labels <- labels[!labels %in% names(dcast_dt)] # Columns that are not already in dt
  
  dcast_dt[, (add_labels) := 0]
  dcast_dt[, (names(dcast_dt)) := lapply(.SD, as.integer)]
  
  var_cols <- names(dcast_dt)[!names(dcast_dt) %in% labels]
  
  setcolorder(dcast_dt, c(var_cols, labels)) # Sort
  
  return(dcast_dt)
  
}

# DCLASS_CONTROLLER -------------------------------------------------------

#' Calculate number of trees by DBH class
#'
#' This function processes the `multiOut` data from the `prebOut` object, melts it, renames columns, merges data.tables, assigns classes based on `D`, and calculates the number of trees by classes of DBH by `site`, `year`, `species`
#'
#' @param prebas_out A multiPrebas/regionPrebas object or a multiOut array
#' @param d_class DBH class in cm
#' @param max_d_class Maximum value for classes. Values that are larger will have class ">'max_d_class'" eg. ">150"
#' @param is_multiOut # When is_multiOut=FALSE, prebas_out should be of class multiPrebas or regionPrebas, otherwise it should be a multiOut orray
#' @return A data.table with columns `site`, `year`, and `species`, containing count data across multiple class intervals ( eg. `0_5`, `5_10`, ..., `145_150`, `>150`), 
#' grouped by `site`, `year`, and `species`.
#' @examples
#' \dontrun{
#' prebOut <- list(multiOut = array(data = rnorm(1000), dim = c(10, 10, 20, 10, 2)))
#' result <- n_by_d_class_dt(prebOut, 5)
#' print(result)
#' }
n_by_d_class_dt <- function(prebas_out, d_class, max_d_class = 150, is_multiOut = FALSE) {
  
  assert_numeric(d_class)
  assert_true(d_class > 0)
  assert_numeric(max_d_class, lower = d_class, upper = 500)
  assert_true(d_class <= max_d_class)
  
  if(!is_multiOut) {
    assert_true(class(prebas_out) %in% c("multiPrebas", "regionPrebas"))
    prebas_out <- prebas_out$multiOut 
  }
  
  assert_array(prebas_out, d = 5) # Check multiOut
  
  # Get d, n and species from multiOut and merge into one data.table
  merged_dt <- d_class_melt_and_merge(prebas_out)
  
  # Assign classes based on D
  d_class_dt <- d_class_assign_classes(merged_dt, d_class, max_d_class)
  
  # Get final dt
  final_d_class_dt <- d_class_calculate_n_class(d_class_dt, max_d_class)
  
  # Get dcast dt with all class labels
  dcast_dt <- d_class_get_dcast_dt(final_d_class_dt, d_class, max_d_class)
  
  return(dcast_dt)
}


