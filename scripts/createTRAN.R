source('scripts/settings.R')
source('./r/multiSite.R')
source('./r/utils.R')


# Create TRAN files from climate data and save to folder


# Climate scenario name
climateScenario <- tolower(VAR_climate_names[VAR_climate_id])

print(paste0("Climate scenario is: ", climateScenario))

# Load climate data
climateData <- fread(VAR_climate_paths[VAR_climate_id])

# Add day column
climateData[, day := .GRP, by = c("time")]

# Variables to create TRAN tables for
tranVars <- c("par", "vpd", "co2", "precip", "tair")

# Create list of TRAN matrices
tranMatrices <- lapply(tranVars, function(x) as.matrix(dynamic_dcast(climateData, "siteID", "day", x)))

# Add names to list
names(tranMatrices) <- paste0(tranVars,"Tran")

# Create tranPath if it doesn't exist
path_tran <- get_or_create_path(pathVarName = "tranPath", defaultDir = tranPath, subDir = climateScenario)

# Create save path
savePath <- paste0(path_tran, climateScenario)

# Invisibly save
invisible(lapply(names(tranMatrices), function(x) {
  
  # Create temp environment
  env <- new.env()

  # Store list item as object
  item <- tranMatrices[[x]]
  
  # Assign name to item
  assign(x, item, envir = env)

  # Save
  save(list = x, file = paste0(savePath, "/", x, ".RData"), envir = env)

  # Remove temp environment
  rm(env)
  
}))



 




















