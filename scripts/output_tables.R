# Script to produce comparison test output variables in protocol format.
# Produces csv with years as rows and sites as columns.

source('scripts/settings.R') 
source('./r/outputs.R')
source('./r/utils.R')


# Define vars
climate_name <- config$VAR_climate_names[config$VAR_climate_id]
species_name <- get_speciesName(config$VAR_species_id, config$VAR_species_dict)
estimated_name <- config$VAR_estimated_names[config$VAR_estimated_id]
management_name <- config$VAR_management_names[config$VAR_management_id+1]
output_names <- config$VAR_output_names
species_codes <- config$VAR_species_codes
species_names <- config$VAR_species_names
split_id <- config$VAR_split_id


# Load multiout
multiOut_path <- file.path(config$PATH_rdata, "multisite_species")
multiOut_files <- list.files(multiOut_path, full.names = T)

# Find by full file name
pattern_multiOut <- paste0(paste("multiOut",
                                 species_name,
                                 estimated_name,
                                 management_name,
                                 climate_name,
                                 split_id, sep = "_")
                           , "\\.rdata$")

multiOut_file <- grep(pattern_multiOut, multiOut_files, value = TRUE)

print(paste0("Loading MultiOut from ", multiOut_file, "..."))
multiOut <- load_data(multiOut_file)
print(paste0("Done."))
cat("\n")



# Specify output variables
output_vars <- c(11:13,17,18,22,30,42,43)

# # Get crown length: H-hc_base
# lc <- multiOut[,,11,1,1] - multiOut[,,14,1,1]

# Get other vars
filtered_multiOut <- multiOut[,,output_vars,1,1]

# Set varNames
namesVars <- as.vector(unlist(dimnames(filtered_multiOut)[3]))



# # Get original siteIDs (Would be preferable that multiOut contains original siteIDs. If not load original climate data)

# climateData <- load_data(config$VAR_climate_paths[config$VAR_climate_id])

site_ids <- unique(multiOut[,1,1,1,1])
nSites <- length(site_ids)
nYears <- dim(multiOut)[[2]]
years_vector <- seq.int(from = 1, to = nYears)


# Transpose and write csv for all variables
for (i in 1:length(namesVars)) {
  
  # Transpose output
  output_mat <- transpose_to_output_format(filtered_multiOut, i, site_ids, years_vector)
  output_dt <- as.data.table(output_mat)
  
  # Get output path
  path <- get_comparison_protocol_variable_output_path("data/output", namesVars[i], namesVars, species_name,
                                                       climate = climate_name, managementName = management_name,
                                                       outputNames = output_names, speciesCodes = species_codes,
                                                       speciesNames = species_names)
  
  
  
  # Write file
  print(paste0("Writing file ", path, "..."))
  fwrite(output_dt, file = path)
  
}














