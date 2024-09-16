# Output variables to csv: Years as rows and sites as columns.

source('scripts/settings.R') 
source('./r/outputs.R')
source('./r/utils.R')


# Load multiout for species
fileName <- paste0(config$PATH_rdata, "multiOut_spID",config$VAR_species_id,".rdata")
load(fileName)

# Define vars
climateScenario <- tolower(config$VAR_climate_names[config$VAR_climate_id])
species <- get_speciesName(config$VAR_species_id, config$VAR_species_dict)
managementName <- config$VAR_management_names[config$VAR_management_id+1]
outputNames <- config$VAR_output_names
speciesCodes <- config$VAR_species_codes
speciesNames <- config$VAR_species_names


# Specify output variables
varXs <- c(11:13,17,18,22,30,42,43)

# # Get crown length: H-hc_base
# lc <- multiOut[,,11,1,1] - multiOut[,,14,1,1]

# Get other vars
tabXst <- multiOut[,,varXs,1,1]

# Set varNames
namesVars <- as.vector(unlist(dimnames(tabXst)[3]))

# Get original siteIDs from climate data
climateData <- fread(config$VAR_climate_paths[config$VAR_climate_id])
clim_sites <- unique(climateData$siteID)

nSites <- length(clim_sites)
nYears <- length(unique(year(climateData$time)))
years_vector <- seq.int(from = 1, to = nYears)

# # Set Lc dimensions
# dim(lc) <- c(nSites, nYears, 1)


# Transpose and write csv for all variables
for (i in 1:length(namesVars)) {
  
  # Transpose output
  output_mat <- transpose_to_output_format(tabXst, i, clim_sites, years_vector)
  
  # Get output path
  path <- get_comparison_protocol_variable_output_path("data/outputs", namesVars[i], namesVars, species,
                                                       climate = climateScenario, managementName = managementName,
                                                       outputNames = outputNames, speciesCodes = speciesCodes,
                                                       speciesNames = speciesNames)
  
  
  
  # Write file
  print(paste0("Writing file ", path, "..."))
  fwrite(output_mat, file = path)
  
}





















