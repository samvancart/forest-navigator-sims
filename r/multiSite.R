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



















