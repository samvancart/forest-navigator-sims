# Output variables to csv: Years as rows and sites as columns.

source("settings.R")

fileName <- paste0("multiOut_spID",speciesID,".rdata")
load(fileName)

species <- speciesNames[speciesID]
varXs <- c(11:13,17,18,30,43)

tabXst <- multiOut[,,varXs,1,1]

# Set varNames
varNames <- as.vector(unlist(dimnames(tabXst)[3]))


for (i in 1:length(varNames)) {
  
  # Array to matrix and transpose
  table <- t(as.matrix(tabXst[,,varNames[i]]))
  
  table_name <- tolower(paste0(varNames[i], "_", species))
  folder <- tolower(species)
  path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/outputs/", folder,"/", table_name,".csv")
  
  write.csv(table, path, row.names = F)
  print(paste0("Csv for ", table_name, " done."))

}





