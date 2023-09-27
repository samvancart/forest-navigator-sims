# Output variables to csv: Years as rows and sites as columns.

source("settings.R")

# Functions

write_output_table <- function(tabX,clim_sites,var,varName,species){
  
  # Array to matrix and transpose
  table <- t(as.matrix(tabX[,,var]))
  
  # Column names to original sites
  colnames(table) <- clim_sites
  rownames(table) <- 1:nrow(table)
  
  attr(rownames(table), "name") <- "years"
  attr(colnames(table), "name") <- "sites"
  
  table_name <- tolower(paste0(varName, "_", species))
  folder <- tolower(species)
  path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/outputs/", folder,"/", table_name,".csv")
  
  
  write.csv(table, path, row.names = T)
  print(paste0("Csv for ", table_name, " done."))
}



fileName <- paste0(rdata_path, "multiOut_spID",speciesID,".rdata")
load(fileName)

species <- speciesNames[speciesID]
varXs <- c(11:14,17,18,30,43)

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

# Set Lc dimensions
dim(lc) <- c(nSites, nYears, 1)

for (i in 1:length(namesVars)) {
  write_output_table(tabXst,clim_sites,i,namesVars[i],species)
}

write_output_table(lc,clim_sites,1,"Lc",species)



