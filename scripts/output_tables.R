# Output variables to csv: Years as rows and sites as columns.

source('scripts/settings.R') 
source('./r/outputs.R')
source('./r/utils.R')


# Load multiout for species
fileName <- paste0(rdata_path, "multiOut_spID",VAR_species_id,".rdata")
load(fileName)

climateScenario <- tolower(climateNames[climateID])
species <- get_speciesName(VAR_species_id, speciesDict)
managementName <- managementNames[managementID+1]

# Specify output variables
varXs <- c(11:13,17,18,22,30,42,43)

# # Get crown length: H-hc_base
# lc <- multiOut[,,11,1,1] - multiOut[,,14,1,1]

# Get other vars
tabXst <- multiOut[,,varXs,1,1]

# Set varNames
namesVars <- as.vector(unlist(dimnames(tabXst)[3]))

# Get original siteIDs from climate data
climateData <- fread(climate_paths[climateID])
clim_sites <- unique(climateData$siteID)

nSites <- length(clim_sites)
nYears <- length(unique(year(climateData$time)))
years_vector <- seq.int(from = 1, to = nYears)

# # Set Lc dimensions
# dim(lc) <- c(nSites, nYears, 1)


# Transpose and write csv for all variables
for (i in 1:length(namesVars)) {
  table <- transpose_to_output_format(tabXst, i, clim_sites, years_vector)
  write_outputs_protocolFormat(table, namesVars[i], namesVars, species, 
                               climate = climateScenario, managementName = managementName)
}




