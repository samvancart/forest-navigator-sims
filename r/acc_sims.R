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
#' @param threshold The threshold value for the basal area.
#' @param seed The seed value for reproducibility.
#' @param del_cols Character vector of column names to delete after sampling.
#' @param add_cols Named list of columns to add after sampling.
#' @return A data.table with sampled data, deleted, and added columns.
read_and_process_file <- function(path, threshold, seed, del_cols, add_cols) {
  dt <- fread(path)
  sampled_dt <- sample_until_global_threshold(dt, "ba", threshold, seed = seed)
  
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
#' @return A list of data.tables with sampled data and added columns.
#' @examples
#' dts <- process_treedata_files(aaa, "path/to/data", 123, del_cols = c("cum_sum"), add_cols = c("cell"))
#' @export
process_treedata_files <- function(aaa, boku_data_path, seed, del_cols = NULL, add_cols = NULL) {
  # Input validations
  assertDataFrame(aaa)
  assertCharacter(boku_data_path, len = 1)
  assertInt(seed)
  assertCharacter(del_cols, null.ok = TRUE)
  assertCharacter(add_cols, null.ok = TRUE)
  
  # Sample from all tree data files until basal area reaches threshold
  dts <- lapply(seq_len(nrow(aaa)), function(i) {
    row <- aaa[i, ]
    filename <- paste0(row[["InitFileName"]], "_01.csv")
    path <- file.path(boku_data_path, filename)
    threshold <- as.numeric(row[["ba"]])
    
    # Prepare columns to add
    add_cols_list <- lapply(add_cols, function(col) row[[col]])
    names(add_cols_list) <- add_cols
    
    read_and_process_file(path, threshold, seed, del_cols, add_cols_list)

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
  # Load necessary libraries
  library(checkmate)
  library(data.table)
  
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
  
  cells_dt <- tree_dt[which(cell %in% unique(data_dt$BOKU_ID))][, .(cell, cell_300arcsec)]
  cell_10 <- unique(cells_dt$cell_300arcsec)
  assert_integer(cell_10, len = 1)
  cells_1 <- cells_dt$cell
  assert_integer(cells_1, min.len = 1)
  
  print(paste0("10km-by-10km cell id: ", cell_10))
  print(paste0("Found ", length(cells_1), " 1km-by-1km cell(s)."))
  
  data_dt <- data_dt[which(BOKU_ID %in% cells_1)]
  data_dt[, cell_300arcsec := cell_10]
  
  return(data_dt)
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




#' Get Accumulated Initial Climate Object
#'
#' This function processes a climate data path and a tree data file to create an initial climate data object.
#'
#' @param clim_path A character string specifying the path to the climate data file.
#' @param aaa_file A character string specifying the path to the tree data file.
#' @param operations A list of operations to be applied to transform the climate data. Defaults to an empty list.
#' @param suffix_str A string to append to the names of the matrices in the list. Defaults to "Tran".
#' @return A list containing the name, plgid, and transformed matrices.
#' @examples
#' clim_path <- "/path/to/climate_data.csv"
#' aaa_file <- "/path/to/tree_data.csv"
#' operations <- list(operation1 = "mean", operation2 = "sum") # Example operations
#' result <- get_acc_init_clim_object(clim_path, aaa_file, operations)
#' @export
get_acc_init_clim_object <- function(clim_path, aaa_file, operations = list(), suffix_str = "Tran") {
  
  # Validate inputs
  assert_file_exists(clim_path)
  assert_file_exists(aaa_file)
  assert_list(operations)
  assert_character(suffix_str, len = 1)
  
  print(paste0("Creating init clim data object..."))
  cat("\n")
  
  name <- str_extract(clim_path, "(?<=/)[^/]+(?=_id_)")
  plgid_str <- str_extract(clim_path, "(?<=plgid_)[0-9]+")
  plgid <- as.integer(plgid_str)
  
  print(paste0("name is ", name))
  print(paste0("plgid is ", plgid))
  cat("\n")
  
  filtered_clim_dt <- filter_data_by_tree_data_cells(clim_path, aaa_file)
  transformed_clim_dt <- transform_and_add_columns(filtered_clim_dt, operations)
  tranMatrices <- get_dcast_matrices_list_from_dt(transformed_clim_dt, suffix_str = suffix_str)
  
  # Get save path
  save_dir <- file.path("climate", name)
  save_path <- get_acc_input_save_path(plgid, save_dir)
  
  cat("\n")
  print("Done.")
  
  return(list(name = name, plgid = plgid, data = tranMatrices, save_path = save_path))
}



#' Get multiInitVar Object
#'
#' This function creates a multiInitVar object based on the provided clustered path and grid file path.
#'
#' @param clustered_path A string representing the path to the clustered R data file.
#' @param grid_file_path A string representing the path to the grid file.
#' @param ... Additional arguments passed to the create_multiInitVar_for_layers function.
#'
#' @return A list containing the plgid and multiInitVar.
#' @import checkmate
#' @import data.table
#' @import stringr
#' @examples
#' # Example usage
#' multiInitVar_obj <- get_multiInitVar_object("path/to/clustered.RData", "path/to/grid.csv")
get_multiInitVar_object <- function(clustered_path, grid_file_path, ...) {
  
  
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
  
  # Extract boku_cell_10 from clustered_path
  boku_cell_10 <- str_extract(clustered_path, "(?<=_)[0-9]+")
  
  # Validate boku_cell_10 is not missing
  assert_character(boku_cell_10, any.missing = FALSE)
  
  # Extract plgid
  plgid <- unique(grid_dt[cell_300arcsec == boku_cell_10]$PlgID)
  
  # Get save path
  save_path <- get_acc_input_save_path(plgid, "tree_data")
  
  return(list(name = "multiInitVar", plgid = plgid, data = list(multiInitVar), save_path = save_path))
}




#' Save Acc Data Object
#'
#' This function saves the data object to a specified save path.
#'
#' @param data A list containing the data to be saved.
#' @param save_path A string representing the base path where the files will be saved.
#' @param test A logical flag indicating whether to run in test mode (default is FALSE).
#' @param default_name A string representing the default name to use if an item's name is NULL.
#'
#' @return NULL
#' @import checkmate
#' @importFrom utils dir.create
#' @examples
#' data <- list(matrix1 = matrix(1:4, 2, 2), matrix2 = matrix(5:8, 2, 2))
#' save_data(data, "/path/to/save", default_name = "default", test = TRUE)
save_acc_data <- function(data, save_path, default_name = "default", test = FALSE) {

  # Validate inputs
  assert_list(data, min.len = 1)
  assert_character(save_path, len = 1)
  assert_directory_exists(dirname(save_path), access = "w")
  assert_character(default_name, len = 1)
  assert_logical(test, len = 1)
  
  
  # Save each item in the data list
  invisible(lapply(seq_along(data), function(index) {
    item <- data[[index]]
    name <- names(data)[index]
    if (is.null(name) || name == "") {
      name <- default_name
    }
    save_file_path <- file.path(save_path, paste0(name, ".rdata"))
    
    if (!test) {
      message(paste0("Saving ", name, " into ", save_file_path))
      save(item, file = save_file_path)
    } else {
      message(paste0("Not saved! Set test=FALSE to save ", name, " into ", save_file_path))
    }
  }))
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
get_acc_input_save_path <- function(plgid, save_dir) {
  # Load checkmate library
  library(checkmate)
  
  # Validate inputs
  assert_integer(plgid, len = 1, any.missing = FALSE)
  assert_character(save_dir, len = 1, any.missing = FALSE)
  
  # Construct the save path
  site_folder_name <- paste0("plgid_", plgid)
  sub_dir <- file.path(site_folder_name, save_dir)
  
  return(file.path(clean_data_base_path, sub_dir))
}



#' Create Directory and Save ACC Object
#'
#' This function creates a directory and saves the ACC object to a specified base path.
#'
#' @param acc_obj A list containing the ACC object. Must include elements "name", "plgid", "data", and "save_path".
#' @param base_path A string representing the base path where the files will be saved.
#' @param test A logical flag indicating whether to run in test mode (default is FALSE).
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
create_dir_and_save_acc_obj <- function(acc_obj, base_path, test = FALSE) {
  
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
    get_or_create_path(pathVarName = "save_path", defaultDir = clean_data_base_path, subDir = "")
    assert_directory_exists(save_path, access = "w")
  }

  # Save data using save_acc_data function
  save_acc_data(data = data, save_path = save_path, default_name = name, test = test)
}






get_nYears_from_acc_tran <- function(tran_dir_path) {
  assert_directory(tran_dir_path)
  parTran_path <- file.path(tran_dir_path, "parTran.rdata")
  assert_file_exists(parTran_path)
  
  parTran <- loadRDataFile(parTran_path)
  
  return(floor(ncol(parTran)/365))
}
