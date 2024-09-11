source('scripts/settings.R')
source('./r/multiSite.R')
source('./r/utils.R')


# Create TRAN files from climate data and save to folder


# Climate scenario name
climateScenario <- tolower(config$VAR_climate_names[config$VAR_climate_id])

print(paste0("Climate scenario is: ", climateScenario))
print(paste0("VAR_split_id is: ", config$VAR_split_id))

# Load climate data using custom function
climateData <- load_data(config$VAR_climate_paths[config$VAR_climate_id])

# Assign splitIDs

max_part_size <- 200     # Define rough size of a split part
split_by <- c("siteID")     # Define constraint
split_id_name <- "splitID"
split_dt <- split_dt_equal_with_constraint(climateData, max_part_size, split_by, split_id_name = split_id_name) # Assign

# Add day column
split_dt[, day := .GRP, by = c("time")]

# Variables to create TRAN tables for
tranVars <- c("par", "vpd", "co2", "precip", "tair")

# Create list of TRAN matrices
tranMatrices <- lapply(tranVars, function(x) 
  as.matrix(dynamic_dcast(split_dt[get(split_id_name)==config$VAR_split_id], "siteID", "day", x)))

# Add names to list
names(tranMatrices) <- paste0(tranVars,"Tran")

# Create PATH_tran if it doesn't exist
path_tran <- get_or_create_path(pathVarName = "config$PATH_tran", defaultDir = config$PATH_tran, subDir = climateScenario)

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
  
  print(paste0("Saving ", x, " into ", savePath, " with split_id ", config$VAR_split_id))

  # Save
  save(list = x, file = paste0(savePath, "/", x, "_", config$VAR_split_id, ".RData"), envir = env)

  # Remove temp environment
  rm(env)
  
}))



 




















