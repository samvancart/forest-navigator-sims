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
#' @param clean_data_base_path character. Base path to the clean data.
#' @param operations A list of operations to be applied to transform the climate data. Defaults to an empty list.
#' @param suffix_str A string to append to the names of the matrices in the list. Defaults to "Tran".
#' @return A list containing the name, plgid, and transformed matrices.
#' @examples
#' clim_path <- "/path/to/climate_data.csv"
#' aaa_file <- "/path/to/tree_data.csv"
#' operations <- list(operation1 = "mean", operation2 = "sum") # Example operations
#' result <- get_acc_init_clim_object(clim_path, aaa_file, operations)
#' @export
get_acc_init_clim_object <- function(clim_path, aaa_file, clean_data_base_path, operations = list(), suffix_str = "Tran") {
  
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
  save_path <- get_acc_input_save_path(plgid, save_dir, clean_data_base_path)
  
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
  
  # Extract boku_cell_10 from clustered_path
  boku_cell_10 <- str_extract(clustered_path, "(?<=_)[0-9]+")
  
  # Validate boku_cell_10 is not missing
  assert_character(boku_cell_10, any.missing = FALSE)
  
  # Extract plgid
  plgid <- unique(grid_dt[cell_300arcsec == boku_cell_10]$PlgID)
  
  # Get save path
  save_path <- get_acc_input_save_path(plgid, "tree_data", clean_data_base_path)
  
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
  assert_choice(ext, c(".csv", ".rdata"))
  
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
  valid_extensions <- c("csv", "rdata")
  assert_choice(file_extension, valid_extensions)
  
  # Save the file based on the extension
  if (file_extension == "csv") {
    assert_class(data, "data.table")
    fwrite(data, file_path)
  } else if (file_extension == "rdata") {
    save(data, file = file_path)
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






get_nYears_from_acc_tran <- function(tran_dir_path) {
  assert_directory(tran_dir_path)
  parTran_path <- file.path(tran_dir_path, "parTran.rdata")
  assert_file_exists(parTran_path)
  
  parTran <- loadRDataFile(parTran_path)
  
  return(floor(ncol(parTran)/365))
}




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
  
  # Create climID data.table
  climID_dt <- data.table(cell = clim_mat[, 1])
  climID_dt[, climID := .GRP, by = "cell"]
  
  # Calculate nLayers and nSpecies
  nLayers <- clustered_dt[, .N, by = group_id_name]$N
  nSpecies <- clustered_dt[, .N, by = c(group_id_name, species_id_name)][, .N, by = group_id_name]$N
  
  # Create id_lookup table
  id_lookup <- clustered_dt[!duplicated(clustered_dt[, c(..group_id_name)])][, c("cell", ..group_id_name)]
  assert_true(nrow(unique(id_lookup)) == nrow(id_lookup))
  
  # Merge to create id_lookup_climID
  id_lookup_climID <- merge(climID_dt, id_lookup, by = "cell")
  
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
                                     rep_times = 1) {
  
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
  
  # Determine climate directory path
  base_clim_path <- file.path(clean_data_base_path, paste0("plgid_", plgid), "climate")
  clim_dirs <- list.files(base_clim_path)
  clim_dir_paths <- file.path(base_clim_path, clim_dirs)
  clim_dir_path <- grep("historical", clim_dir_paths, value = TRUE)
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
  
  required_files <- c("parTran.rdata", "co2Tran.rdata", "tairTran.rdata", "vpdTran.rdata", "precipTran.rdata")
  
  # Loading required files using lapply
  climate_data <- lapply(required_files, function(file) {
    file_path <- file.path(clim_dir_path, file)
    assertFileExists(file_path, access = "r")
    loadRDataFile(file_path)
  })
  names(climate_data) <- required_files
  
  site_info_path <- file.path(base_path, "site", "siteInfo.rdata")
  multi_init_var_path <- file.path(base_path, "tree_data", "multiInitVar.rdata")
  
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
                              PAR = climate_data[["parTran.rdata"]],
                              VPD = climate_data[["vpdTran.rdata"]],
                              CO2 = climate_data[["co2Tran.rdata"]],
                              Precip = climate_data[["precipTran.rdata"]],
                              TAir = climate_data[["tairTran.rdata"]],
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
                              clustered_base_path, 
                              aaa_path, 
                              id_col_name = "forested_ha_id",
                              sep = "_",
                              keep = 2) {
  
  # Input validations using checkmate
  assert_number(plgid, finite = TRUE)
  assert_file_exists(filtered_selection_path, access = "r")
  assert_directory_exists(clustered_base_path)
  assert_file_exists(aaa_path, access = "r")
  assert_string(id_col_name)
  assert_string(sep)
  assert_integerish(keep, lower = 1, upper = 4, len = 1)
  
  # Read the filtered selection data and filter by PlgID
  selection_plgid_dt <- fread(filtered_selection_path)[PlgID == plgid]
  if (nrow(selection_plgid_dt) == 0) stop("No data found for the given PlgID")
  
  # Extract unique cell identifier
  cell_10 <- unique(selection_plgid_dt$cell_300arcsec)
  if (length(cell_10) != 1) stop("Expected a single unique cell identifier")
  
  # Load the clustered data
  clustered_dt <- loadRDataFile(file.path(clustered_base_path, paste0("clustered_", cell_10, ".rdata")))
  if (is.null(clustered_dt) || nrow(clustered_dt) == 0) stop("No clustered data found")
  
  # Create a lookup table and extract necessary columns
  clustered_dt_lookup <- unique(clustered_dt[, c(1:3)])
  clustered_dt_lookup[, site := as.integer(unlist(tstrsplit(get(id_col_name), split = sep, keep = keep)))]
  
  # Read the AAA data
  aaa_all <- fread(aaa_path)
  if (is.null(aaa_all) || nrow(aaa_all) == 0) stop("No AAA data found")
  
  # Merge the lookup table with the selection data
  siteID_selection_lookup <- merge(clustered_dt_lookup, selection_plgid_dt, 
                                   by.x = c("cell", "cell_300arcsec"), 
                                   by.y = c("BOKU_ID", "cell_300arcsec"))
  
  # Merge with AAA data to include forest type
  siteID_all_lookup <- merge(siteID_selection_lookup, 
                             aaa_all[cell_300arcsec == cell_10][, c("cell", "cell_300arcsec", "forest_type")], 
                             by = c("cell", "cell_300arcsec"))
  
  # Select final columns for the output
  siteID_lookup <- siteID_all_lookup[, .(site, PlgID_05, forest_type)]
  
  # Return the final lookup table
  return(siteID_lookup)
}





#' Merge MultiOut Species and Harvest with Out Data Table
#'
#' This function merges species and harvest data from a multi-dimensional array into an existing data.table.
#'
#' @param out_dt Data.table. The existing data.table to which the species and harvest data will be merged.
#' @param multiOut Array. A multi-dimensional array containing the data.
#' @param vHarv Integer vector of length 2. Specifies the indices for harvest data. Default is c(30, 2).
#'
#' @return A merged data.table containing the combined output, species, and harvest information.
#' @import data.table
#' @import checkmate
#' @export
#'
#' @examples
#' \dontrun{
#' merge_multiOut_species_and_harv_with_out_dt(
#'   out_dt = data.table(id = 1:5),
#'   multiOut = array(data = rnorm(1000), dim = c(10, 10, 10, 10)),
#'   vHarv = c(30, 2)
#' )
#' }

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




# OUTPUT



# Helper
get_multiOut_from_run_table_row <- function(row) {
  # Check input validity
  assert_list(row)
  assert_true(all(c("plgid", "clim_scen", "clean_data_base_path", "man_init_args") %in% names(row)))
  
  multiOut <- with(row, {
    # Init model
    init_args <- list(plgid = plgid, clim_scen = clim_scen, clean_data_base_path = clean_data_base_path)
    initPrebas <- do.call(get_init_prebas_for_plgid, c(init_args, man_init_args))
    modOut <- get_modOut(regionPrebas, initPrebas)
    multiOut <- modOut$multiOut
    multiOut
  })
  
  return(multiOut)
}




# Helper
get_acc_output_dt_from_run_table_row <- function(row, multiOut, selection_path, 
                                                 clustered_base_path, aaa_file, 
                                                 conversions_path,
                                                 species_lookup_path) {
  # Check input validity
  assert_list(row)
  assert_array(multiOut, d = 5)
  assert_character(selection_path, len = 1)
  assert_character(clustered_base_path, len = 1)
  assert_character(aaa_file, len = 1)
  assert_character(conversions_path, len = 1)
  assert_character(species_lookup_path, len = 1)
  assert_true(all(c("plgid", "model", "country", "clim_scen", "man_scen", "canopy_layer") %in% names(row)))
  
  
  out_dt_melted <- with(row, {
    
    # Load lookup files
    siteID_lookup <- get_siteID_lookup(plgid, selection_path, clustered_base_path, aaa_file)
    conversions_dt <- fread(conversions_path)
    species_lookup <- fread(species_lookup_path)
    
    # Get multiOut as dt
    out_dt <- as.data.table(melt(multiOut[,,varOutID,,1]))
    out_dt_wide <- dcast.data.table(out_dt, site + year + layer ~ variable, value.var = "value")
    
    add_cols <- list(Model = model, Country = country, Climate_scenario = clim_scen, 
                     Management_scenario = man_scen, Canopy_layer = canopy_layer)
    # Get operations
    output_operations <- get_output_operations(plgid, multiOut, conversions_dt, siteID_lookup, species_lookup, add_cols)
    
    # Get output dt
    transform_and_add_columns(out_dt_wide, output_operations)
  })
  
  return(out_dt_melted)
}


# Helper
get_acc_out_obj_from_run_table_row <- function(row, out_dt) {
  # Check input validity
  assert_list(row)
  assert_true(all(c("model", "plgid", "clim_scen", "man_scen", "output_base_path") %in% names(row)))
  assert_data_table(out_dt)
  
  acc_out_obj <- with(row, {
    
    # Create name according to output template
    output_template_vector <- c(model, plgid, clim_scen, man_scen)
    name <- str_c(output_template_vector, collapse = "_")
    
    # Get save path
    save_path <- get_acc_input_save_path(plgid, "output", output_base_path)
    
    list(name = name, plgid = plgid, data = list(out_dt), save_path = save_path)
  })  
  
  return(acc_out_obj)
}




#' Produce ACC Output Objects from Run Table
#'
#' This function processes each row of the `acc_run_table` to produce ACC output objects.
#' It performs various steps including initializing models, creating output data tables,
#' and creating ACC output objects.
#'
#' @param acc_run_table A data.table containing the run table data with required columns.
#' @return A list of ACC output objects.
#' @import data.table
#' @import checkmate
#' @export
#' @examples
#' \dontrun{
#' acc_run_table <- data.table(plgid = 1:2, model = c("model1", "model2"), country = c("FI", "SE"),
#'                             clim_scen = c("scenario1", "scenario2"), man_scen = c("man1", "man2"),
#'                             canopy_layer = c("layer1", "layer2"), man_init_args = list(),
#'                             clean_data_base_path = c("path1", "path2"), selection_path = c("path3", "path4"),
#'                             clustered_base_path = c("path5", "path6"), aaa_file = c("file1", "file2"),
#'                             conversions_path = c("path7", "path8"), output_base_path = c("path9", "path10"),
#'                             species_lookup_path = c("path11", "path12"), varOutID = list(1:2, 3:4),
#'                             vHarv = list(1:2, 3:4))
#' produce_acc_output_obj_from_run_table(acc_run_table)
#' }
produce_acc_output_obj_from_run_table <- function(acc_run_table) {
  # Check input validity
  assert_data_table(acc_run_table)
  required_columns <- c("plgid", "model", "country", "clim_scen", "man_scen", 
                        "canopy_layer", "man_init_args", "clean_data_base_path", 
                        "selection_path", "clustered_base_path", "aaa_file", 
                        "conversions_path", "output_base_path", 
                        "species_lookup_path", "varOutID", "vHarv")
  assert_true(all(required_columns %in% names(acc_run_table)))
  
  output_objects <- apply(acc_run_table, 1, function(row) {
    with(row, {
      print(paste0("Creating output for plgid ", plgid, "..."))
      
      # Get multiOut
      multiOut <- get_multiOut_from_run_table_row(row)
      
      print(paste0("Creating output from multiOut..."))
      
      # Get output dt
      out_dt_melted <- get_acc_output_dt_from_run_table_row(row, multiOut, selection_path, 
                                                            clustered_base_path, aaa_file, 
                                                            conversions_path,
                                                            species_lookup_path)
      
      print("Done.")
      print("Creating acc output object...")
      get_acc_out_obj_from_run_table_row(row, out_dt_melted)
    })
  })
  
  print("Done.")
  
  return(output_objects)
}








