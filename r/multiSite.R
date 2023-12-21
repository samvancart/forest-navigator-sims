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





