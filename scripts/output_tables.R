# Output variables to csv: Years as rows and sites as columns.

source('scripts/settings.R') 

# Functions

#' Select one variable from an array, then convert to matrix and transpose
#'
#' @param tabX The array of output variables
#' @param var The output variable
#' @param col_vector Column identifiers
#' @param row_vector Row identifiers
#' @param col_names Column names
#' @param row_names Row names
#'
#' @return Transposed table
#' @export
#'
#' @examples
transpose_to_output_format <- function(tabX, var, col_vector, row_vector, col_names="sites", row_names="years") {
  # Array to matrix and transpose
  table <- t(as.matrix(tabX[,,var]))
  
  # Column names to original sites
  colnames(table) <- col_vector
  rownames(table) <- row_vector
  
  attr(colnames(table), "name") <- col_names
  attr(rownames(table), "name") <- row_names
  
  return(table)

}


write_outputs_protocolFormat <- function(table, varName, namesVars, species, model="PREBAS", climate="historical", 
                                         region="norcen", managementName="managed", folder="comparison_protocol"){
  
  if(varName %in% namesVars) {
    varName <- outputNames[which(namesVars==varName)]
  }
  
  speciesCode <-  speciesCodes[which(speciesNames==species)]
  
  table_name <- paste0(model, "_", climate, "_", varName, "_", region, "_", speciesCode)
  path <- paste0("data/outputs/", managementName, "/", folder,"/", table_name,".csv")


  write.csv(table, path, row.names = T)
  print(paste0("Csv for ", table_name, " done."))
}


write_output_table <- function(tabX,clim_sites,var,varName,species, managementName="managed"){
  
  # Array to matrix and transpose
  table <- t(as.matrix(tabX[,,var]))
  
  # Column names to original sites
  colnames(table) <- clim_sites
  rownames(table) <- 1:nrow(table)
  print(rownames(table))
  
  attr(rownames(table), "name") <- "years"
  attr(colnames(table), "name") <- "sites"
  
  table_name <- tolower(paste0(varName, "_", species))
  folder <- tolower(species)
  path <- paste0("data/outputs/", managementName, "/", folder,"/", table_name,".csv")


  write.csv(table, path, row.names = T)
  print(paste0("Csv for ", table_name, " done."))
}



fileName <- paste0(rdata_path, "multiOut_spID",speciesID,".rdata")
load(fileName)

species <- speciesNames[speciesID]
managementName <- managementNames[managementID+1]

varXs <- c(11:14,17,18,22,30,42,43)

# Get crown length: H-hc_base
lc <- multiOut[,,11,1,1] - multiOut[,,14,1,1]

# Get other vars
tabXst <- multiOut[,,varXs,1,1]

# Set varNames
namesVars <- as.vector(unlist(dimnames(tabXst)[3]))

# Get original siteIDs from climate data
climateData <- fread("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/historical_only_prebas_picus_sites.csv")
clim_sites <- unique(climateData$siteID)

nSites <- length(clim_sites)
nYears <- length(unique(year(climateData$time)))
years_vector <- seq.int(from = 1, to = nYears)

# Set Lc dimensions
dim(lc) <- c(nSites, nYears, 1)

# for (i in 1:length(namesVars)) {
#   write_output_table(tabXst,clim_sites,i,namesVars[i],species, managementName)
# }
# 
# write_output_table(lc,clim_sites,1,"Lc",species, managementName)


for (i in 1:length(namesVars)) {
  table <- transpose_to_output_format(tabXst, i, clim_sites, years_vector)
  write_outputs_protocolFormat(table, namesVars[i],namesVars,species,managementName=managementName)
}





# # TEST
# table <- transpose_to_output_format(tabXst, 1, clim_sites, years_vector)
# write_outputs_protocolFormat(table,"H",namesVars,species,managementName=managementName)
# table <- transpose_to_output_format(lc, 1, clim_sites, years_vector)

