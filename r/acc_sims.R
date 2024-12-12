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

