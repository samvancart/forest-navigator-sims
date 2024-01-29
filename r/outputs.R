


# Functions

#' Select one variable from an array, then convert to matrix and transpose
#'
#' @param tabX The array of output variables
#' @param var The output variable to transpose
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


#' Write output variable to file in protocol format
#'
#' @param table Matrix to write
#' @param varName Output variable name
#' @param namesVars All variable names
#' @param species Species name
#' @param model Used model 
#' @param climate used climate
#' @param region Region
#' @param managementName Management parameter (managed or noManagement) 
#' @param folder Folder to write results to
#'
#' @return Writes csv file
#' @export
#'
#' @examples
write_outputs_protocolFormat <- function(table, varName, namesVars, species, model="PREBAS", climate="historical", 
                                         region="norcen", managementName="managed", folder="comparison_protocol"){
  
  if(varName %in% namesVars) {
    varName <- outputNames[which(namesVars==varName)]
  }
  
  speciesCode <-  speciesCodes[which(speciesNames==species)]
  
  table_name <- paste0(model, "_", climate, "_", varName, "_", region, "_", speciesCode)
  path <- paste0("data/outputs/", managementName, "/", folder,"/", table_name,".csv")
  
  
  write.csv(table, path, row.names = T)
  print(paste0("Csv written to ", path))
}












