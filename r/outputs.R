


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
                                         region="norcen", managementName="managed", folder="comparison_protocol",
                                         outputNames, speciesCodes, speciesNames){
  
  if(varName %in% namesVars) {
    varName <- outputNames[which(namesVars==varName)]
  }
  
  speciesCode <-  speciesCodes[which(speciesNames==species)]
  
  table_name <- paste0(model, "_", climate, "_", varName, "_", region, "_", speciesCode)
  path <- paste0("data/outputs/", managementName, "/", folder,"/", table_name,".csv")
  
  
  write.csv(table, path, row.names = T)
  print(paste0("Csv written to ", path))
}



#' Get Comparison Protocol Variable Output Path
#'
#' Constructs the file path for a comparison protocol variable output.
#'
#' @param base_path A character string specifying the base directory path.
#' @param varName A character string specifying the variable name.
#' @param namesVars A character vector of variable names.
#' @param species A character string specifying the species name.
#' @param outputNames A character vector of output variable names.
#' @param speciesCodes A character vector of species codes.
#' @param speciesNames A character vector of species names.
#' @param extension A character string specifying the file extension (default is "csv").
#' @param model A character string specifying the model name (default is "PREBAS").
#' @param climate A character string specifying the climate scenario (default is "historical").
#' @param region A character string specifying the region (default is "norcen").
#' @param managementName A character string specifying the management name (default is "managed").
#' @param folder A character string specifying the folder name (default is "comparison_protocol").
#' @param separator A character string specifying the separator for the file name (default is "_").
#'
#' @return A character string representing the full file path.
#' @examples
#' base_path <- "C:/Users/YourName/Documents"
#' varName <- "GPP"
#' namesVars <- c("GPP", "NPP")
#' species <- "Pine"
#' outputNames <- c("GrossPrimaryProduction", "NetPrimaryProduction")
#' speciesCodes <- c("PINE", "SPRUCE")
#' speciesNames <- c("Pine", "Spruce")
#' get_comparison_protocol_variable_output_path(base_path, varName, namesVars, species, outputNames, speciesCodes, speciesNames)
#' 
#' @export
get_comparison_protocol_variable_output_path <- function(base_path, varName, namesVars, species,
                                                         outputNames, speciesCodes, speciesNames, extension = "csv", model="PREBAS", climate="historical", 
                                                         region="norcen", managementName="managed", folder="comparison_protocol", separator = "_") {
  
  # Load checkmate library
  library(checkmate)
  
  # Validate inputs
  assertDirectoryExists(base_path, access = "r")
  assertChoice(varName, namesVars)
  assertChoice(species, speciesNames)
  assertCharacter(extension, len = 1)
  assertCharacter(model, len = 1)
  assertCharacter(climate, len = 1)
  assertCharacter(region, len = 1)
  assertCharacter(managementName, len = 1)
  assertCharacter(folder, len = 1)
  assertCharacter(separator, len = 1)
  
  # Get comparison protocol variable name that matches prebas output variable name
  varName <- outputNames[which(namesVars == varName)]
  
  # Get corresponding comparison species name from prebas species name
  speciesCode <- speciesCodes[which(speciesNames == species)]
  
  # Construct the file name and directory path
  file_name <- paste(model, climate, varName, region, speciesCode, sep = separator)
  dir_path <- file.path(base_path, managementName, folder)
  
  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Construct the full file path
  full_path <- file.path(dir_path, paste(file_name, extension, sep = "."))
  
  return(full_path)
}









