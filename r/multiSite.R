# Functions


#' Build siteInfo table with correct column names
#'
#' @param param_table 
#'
#' @return
#' @export
#'
#' @examples
build_siteInfo <- function(param_table) {
  siteInfo <- param_table
  
  colnames(siteInfo) <- c("siteID", "climID", "siteType", "SWinit", "CWinit",
                          "SOGinit", "Sinit", "nLayers", "nSpecies", "soildepth",
                          "effective field capacity", "permanent wilting point")
  
  siteInfo[,1] <- 1:nSites
  siteInfo[,2] <- 1:nSites
  
  return(siteInfo)
}

read_tran <- function(folder_path, variable, header=T) {
  file <- paste0(folder_path, variable,".csv")
  return(fread(file = file, header = header))
}

# Data table where rows are sites and columns are days.
get_prebas_tran <- function(df, var) {
  
  siteIDs <- unique(df$siteID)
  all_wide <- data.table()
  
  for(id in siteIDs) {
    filtered <- filter(df,siteID==id)
    var_col <- filtered[, ..var]
    df_wide <- transpose(var_col)
    all_wide <- rbind(all_wide,df_wide)
  }
  
  return(all_wide)
}

#' Transpose a column of a table consisting of climate variables in a time series
#' into a format where rows are sites and columns are days. 
#'
#' @param dt The data.table with the climate data column and sites column.
#' @param var The climate data variable name as string.
#' @param id default = "siteID" The name of the sites column as string.
#'
#' @return Transposed table where rows are sites and columns are days.
#' @export
#'
#' @examples
get_prebas_tran_2 <- function(dt, var, id = "siteID") {
  siteIDs <- c(unique(dt[, ..id]))[[1]]
  result <- data.matrix(rbindlist(lapply(siteIDs, function(x) transpose(dt[eval(parse(text = id))==x][, ..var]))))
  return(result)
}



#' Dynamic dcast Function
#'
#' This function reshapes a data.table so that the specified `id_col` becomes the row identifier,
#' the `time_col` becomes the column identifier, and the `value_col` contains the values.
#'
#' @param dt A data.table containing the data to be reshaped.
#' @param id_col A character string specifying the column to be used as row identifiers.
#' @param time_col A character string specifying the column to be used as column identifiers.
#' @param value_col A character string specifying the column containing the values to be reshaped.
#' @return A reshaped data.table.
#' @examples
#' library(data.table)
#' climate_data <- data.table(
#'   siteID = c(1, 1, 2, 2),
#'   day = c('2024-08-01', '2024-08-02', '2024-08-01', '2024-08-02'),
#'   variable = c(25.3, 26.1, 24.8, 25.5)
#' )
#' reshaped_data <- dynamic_dcast(climate_data, "siteID", "day", "variable")
#' print(reshaped_data)
#' @export
dynamic_dcast <- function(dt, id_col, time_col, value_col) {
  formula <- as.formula(paste(id_col, "~", time_col))
  dcast_dt <- dcast(dt, formula, value.var = value_col)
  return(dcast_dt)
}




#' Create multiInitVar for initialising multisite prebas with one species
#'
#' @param data Type of data to initialise array with (default NA)
#' @param nRows Number of rows (corresponds to number of sites)
#' @param nLayers Number of layers
#' @param speciesID speciesID
#' @param initAge Initial age of forest
#'
#' @return 3 dimensional array
#' @export
#'
#' @examples
get_multiInitVar_species <- function(data = NA, nRows, nLayers, speciesID, initAge=100) {
  multiInitVar <- array(NA, dim=c(nRows, 7, nLayers))
  multiInitVar[,1,] <- speciesID
  multiInitVar[,3,] <- initSeedling.def[1]; multiInitVar[,4,] <- initSeedling.def[2]
  multiInitVar[,5,] <- initSeedling.def[3]; multiInitVar[,6,] <- initSeedling.def[4]
  multiInitVar[,2,] <- initAge
  
  return(multiInitVar)
}


#' Get pPRELES parameter for model initialisation (different for speciesID 12)
#'
#' @param speciesID 
#'
#' @return pPRELES vector
#' @export
#'
#' @examples
get_pPRELES <- function(speciesID) {
  if(speciesID == 12) {return(pPRELESfasy)}
  return(pPREL)
}



#' Modify parameter 17 in pCROBAS according to species 
#'
#' @param speciesIDs Numeric vector of speciesIDs to modify
#' @param pCROBAS_multipliers Dictionary of pCROBAS multipliers for each species by speciesID
#' @param pCROB Original pCROB array
#'
#' @return Copy of pCROB with modified values
#' @export
#'
#' @examples
get_pCROBAS <- function(speciesIDs, pCROBAS_multipliers, pCROB) {
  pCROB_copy <- pCROB
  for(speciesID in speciesIDs) {
    if(speciesID > length(pCROB_copy[17,])) {
      warning(paste0("No species with id ",  speciesID, "!"))
      next
    }
    pCROB_multiplier <- pCROBAS_multipliers[as.character(speciesID)][[1]]
    if(is.na(pCROB_multiplier)){
      pCROB_multiplier <- 1
    }
    pCROB_copy[17, speciesID] <- pCROB_copy[17, speciesID] * pCROB_multiplier
  }
  return(pCROB_copy)
}



#' #' Process a subset of data for a given site
#' #'
#' #' This function processes a subset of data for a given site, extracting relevant
#' #' information and preparing it for inclusion in the multiInitVar array.
#' #'
#' #' @param subset A data.table containing the subset of data for a specific site.
#' #' @param i An integer representing the site index.
#' #' @return A list containing vectors of speciesID, Age, Height, Dbh, basal_area, and NA values.
#' #' @examples
#' #' # Assuming dt_nSites is a data.table and nLayers is a vector
#' #' subset <- dt_nSites[groupID == 1]
#' #' result <- process_subset(subset, 1)
#' #' @export
#' process_subset <- function(subset, i) {
#'   nLayers_i <- nLayers[i]
#'   list(
#'     speciesID = subset$speciesID[1:nLayers_i],
#'     Age = subset$Age[1:nLayers_i],
#'     Height = subset$Height[1:nLayers_i],
#'     Dbh = subset$Dbh[1:nLayers_i],
#'     basal_area = subset$basal_area[1:nLayers_i],
#'     NA_values = rep(NA, nLayers_i)
#'   )
#' }




#' Process a Subset of Data
#'
#' This function processes a subset of data based on the given index and number of layers.
#' It extracts specific columns and ensures that the number of layers does not exceed the subset's length.
#'
#' @param subset A data frame containing the subset of data to process.
#' @param i An integer representing the index of the subset to process.
#' @param nLayers A numeric vector specifying the number of layers for each subset.
#' @param col_names A list of character strings specifying the column names in the subset. 
#' Default is a list with elements "speciesID", "Age", "Height", "Dbh", and "basal_area".
#'
#' @return A list containing the processed data for the specified subset.
#' @import checkmate
#' @examples
#' subset <- data.frame(
#'   speciesID = 1:10,
#'   Age = 11:20,
#'   Height = 21:30,
#'   Dbh = 31:40,
#'   basal_area = 41:50
#' )
#' nLayers <- c(5, 6, 7, 8, 9, 10)
#' col_names <- list(
#'   speciesID = "speciesID",
#'   Age = "Age",
#'   Height = "Height",
#'   Dbh = "Dbh",
#'   basal_area = "basal_area"
#' )
#' result <- process_subset(subset, 1, nLayers, col_names)
#' print(result)
process_subset <- function(subset, i, nLayers, col_names = list(
  speciesID = "speciesID",
  Age = "Age",
  Height = "Height",
  Dbh = "Dbh",
  basal_area = "basal_area"
)) {
  
  # Validate inputs
  assert_data_frame(subset, min.rows = 1, col.names = "strict")
  assert_integerish(i, lower = 1, upper = length(nLayers), any.missing = FALSE, len = 1)
  assert_numeric(nLayers, lower = 1, len = length(nLayers), any.missing = FALSE)
  assert_list(col_names, types = "character", len = 5, any.missing = FALSE)
  assert_names(names(col_names), must.include = c("speciesID", "Age", "Height", "Dbh", "basal_area"))
  assert_names(names(subset), must.include = unlist(col_names))
  
  # Ensure nLayers is within the valid range
  nLayers_i <- min(nLayers[i], nrow(subset))
  
  # Extract the specified columns using the provided or default column names
  result <- list(
    speciesID = head(subset[[col_names$speciesID]], nLayers_i),
    Age = head(subset[[col_names$Age]], nLayers_i),
    Height = head(subset[[col_names$Height]], nLayers_i),
    Dbh = head(subset[[col_names$Dbh]], nLayers_i),
    basal_area = head(subset[[col_names$basal_area]], nLayers_i),
    NA_values = rep(NA, nLayers_i)
  )
  
  return(result)
}


#' Create multiInitVar for Layers
#'
#' This function creates a multiInitVar array for layers based on the provided data file path and group/species identifiers.
#'
#' @param dt_path A string representing the path to the R data file.
#' @param group_id_name A string representing the name of the group ID column in the data.
#' @param species_id_name A string representing the name of the species ID column in the data.
#' @param ... Additional arguments passed to the process_subset function.
#'
#' @return A 3-dimensional array containing the multiInitVar data.
#' @import checkmate
#' @examples
#' # Example usage
#' multiInitVar <- create_multiInitVar_for_layers("path/to/data.RData", "groupID", "speciesID")
create_multiInitVar_for_layers <- function(dt_path, group_id_name, species_id_name, ...) {
  # Load checkmate library
  library(checkmate)
  
  # Validate inputs
  assert_character(dt_path, any.missing = FALSE, len = 1)
  assert_file_exists(dt_path)
  assert_character(group_id_name, any.missing = FALSE, len = 1)
  assert_character(species_id_name, any.missing = FALSE, len = 1)
  
  # Load the data
  dt <- loadRDataFile(dt_path)
  
  # Validate the loaded data
  assert_data_frame(dt, min.rows = 1, col.names = "strict")
  assert_names(names(dt), must.include = c(group_id_name, species_id_name))
  
  nSites <- length(unique(dt[[group_id_name]]))
  nLayers <- dt[, .N, by = c(group_id_name)]$N
  nSpecies <- dt[, .N, by = c(group_id_name, species_id_name)][, .N, by = c(group_id_name)]$N
  maxNlayers <- max(nLayers)
  
  # Initialize the multiInitVar array
  multiInitVar <- array(0, dim=c(nSites, 7, maxNlayers))
  multiInitVar[,6:7,] <- multiInitVar[,6:7,NA]
  
  print(paste0("Creating multiInitVar for ", nSites, " sites with max ", maxNlayers, " layers..."))
  
  system.time({
    # Split the data.table by groupID
    split_data <- split(dt, by = group_id_name)
    
    # Apply the process_subset function to each subset
    results <- lapply(seq_along(split_data), function(i) process_subset(split_data[[i]], i, nLayers, ...))
    
    # Fill matrix with the values
    for (i in seq_along(results)) {
      multiInitVar[i, 1, 1:nLayers[i]] <- results[[i]]$speciesID # vector of species ID taken from data
      multiInitVar[i, 2, 1:nLayers[i]] <- results[[i]]$Age # age by tree from NFI
      multiInitVar[i, 3, 1:nLayers[i]] <- results[[i]]$Height # height from NFI data
      multiInitVar[i, 4, 1:nLayers[i]] <- results[[i]]$Dbh # dbh from NFI data
      multiInitVar[i, 5, 1:nLayers[i]] <- results[[i]]$basal_area # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
      multiInitVar[i, 6, 1:nLayers[i]] <- results[[i]]$NA_values
    }
  })
  
  print("Done.")
  
  return(multiInitVar)
}



