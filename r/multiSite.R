# Functions

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
    var_col <- filtered[var]
    df_wide <- as.data.frame(t(var_col))
    all_wide <- rbind(all_wide,df_wide)
  }
  
  return(all_wide)
}





