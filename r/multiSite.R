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











