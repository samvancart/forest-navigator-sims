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



#' Filter Climate Data by Tree Data Cells
#'
#' This function filters climate data based on the cells present in the provided tree data.
#'
#' @param clim_data_path A character string specifying the path to the climate data file.
#' @param tree_data_path A character string specifying the path to the tree data file.
#' @return A filtered data.table containing climate data for the specified tree data cells.
#' @examples
#' clim_data_path <- "/path/to/your/clim_data.csv"
#' tree_data_path <- "/path/to/your/aaa_file.csv"
#' result <- filter_clim_by_tree_data_cells(clim_data_path, tree_data_path)
#' @export
filter_clim_by_tree_data_cells <- function(clim_data_path, tree_data_path) {
  # Validate input paths
  assert_file_exists(clim_data_path)
  assert_file_exists(tree_data_path)
  
  # Read climate data and tree data
  print(paste0("Loading ", clim_data_path, "..."))
  clim_dt <- fread(clim_data_path)
  aaa_all <- fread(tree_data_path)
  
  # Validate data structures
  assert_data_table(clim_dt)
  assert_true(all(!is.na(clim_dt)))
  assert_data_table(aaa_all)
  
  cells_dt <- aaa_all[which(cell %in% unique(clim_dt$BOKU_ID))][, c("cell", "cell_300arcsec")]
  cell_10 <- unique(cells_dt$cell_300arcsec)
  assert_integer(cell_10, len = 1)
  cells_1 <- cells_dt$cell
  assert_integer(cells_1, min.len = 1)
  
  print(paste0("10km-by-10km cell id: ", cell_10))
  print(paste0("Found ", length(cells_1), " 1km-by-1km cell(s)."))
  
  clim_dt <- clim_dt[which(BOKU_ID %in% cells_1)]
  
  clim_dt[, cell_300arcsec := cell_10]
  
  return(clim_dt)
}
