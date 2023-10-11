source('scripts/settings.R')
# Run scripts with different settings


# Functions

# Read file lines
get_file_lines <- function(file_name) {
  f <- file(file_name, open="r")
  lines <- readLines(f)
  close(f)
  return(lines)
}


# get specified line from file lines
get_line <- function(lines, line) {
  found <- NULL
  for (i in seq_along(lines)){
    if (substr(lines[i],0,nchar(line)) == line) {
      found <- lines[i]
      return(found)
    }
  }
  if (is.null(found)) {
    print("Line not found!")
    return(NULL)
  }
}

# replace patterns in file: Old and new patterns given as vectors
replace_patterns <- function(old_patterns, new_patterns, lines) {
  for (i in 1:length(old_patterns)) {
    new_lines <- gsub(pattern = old_patterns[i], replacement = new_patterns[i], lines)
    lines <- new_lines
  }
  return(new_lines)
}


# Run species and estimate
run_species_and_estimate <- function(estimated_vector,species_vector,sources,settings_file="scripts/settings.R") {
  for (i in estimated_vector) {
    eID <- i
    new_eID <- paste0("estimatedID <- ", as.character(eID))
    for (j in species_vector) {
      sID <- j
      new_sID <- paste0("speciesID <- ", as.character(sID))
      lines <- get_file_lines(settings_file)
      new_lines <- replace_patterns(c(old_sID,old_eID),c(new_sID,new_eID),lines)
      writeLines(new_lines, settings_file)
      old_sID <- new_sID
      old_eID <- new_eID
      sapply(sources, source)
      print(paste0("Plots for speciesID ", sID, " and estimatedID ", eID, " done."))
    }
  }
}

# Run for species
run_ID <- function(id_vector,id_name, old_id, sources, settings_file="scripts/settings.R") {
  for (i in id_vector) {
    id <- i
    new_id <- paste0(id_name, " <- ", as.character(id))
    lines <- get_file_lines(settings_file)
    new_lines <- replace_patterns(c(old_id),c(new_id),lines)
    writeLines(new_lines, settings_file)
    old_id <- new_id
    sapply(sources, source)
  }
}



# Initialise variables

# R files to run (vector in order)
multiSpecies <- c("scripts/multiSiteSpecies.R")
multiLayers <- c("scripts/multiSiteLayers.R")
multi_and_plots_species <- c("scripts/multiSiteSpecies.R", "scripts/plotsSpecies.R")
multi_and_plots_layers <- c("scripts/multiSiteLayers.R", "scripts/plotsLayers.R")
multi_and_outputs_species <- c("scripts/multiSiteSpecies.R", "scripts/output_tables.R")
multi_and_outputs_layers <- c("scripts/multiSiteLayers.R", "scripts/output_tables.R")
plot_tables <- c("scripts/plot_tables.R")
util_f <- c("scripts/utility_functions.R")
sums_means <- c("scripts/modout_sums_and_means.R")

# Vector values for loop. Values correspond to ids
species_vector <- c(1:4)
estimated_vector <- c(1:2)
layer_vector <- c(1:2)
settings_file <- "scripts/settings.R"

# 1. Get file lines
lines <- get_file_lines(settings_file)

# 2. Get lines to modify
old_sID <- get_line(lines, "speciesID")
old_eID <- get_line(lines, "estimatedID")
old_lID <- get_line(lines, "layerID")

# run_ID(layer_vector, "layerID", old_lID, multiLayers)
# run_ID(species_vector, "speciesID", old_sID, multi_and_outputs_species)
# run_species_and_estimate(estimated_vector, species_vector, multi_and_plots_species)
# run_ID(layer_vector, "layerID", old_lID, plot_tables)
# run_ID(layer_vector, "layerID", old_lID, util_f)
# run_ID(layer_vector, "layerID", old_lID, sums_means)





# GET ALL PLOTS

# # MultiSiteSpecies and plotsSpecies
# run_species_and_estimate(estimated_vector, species_vector, multi_and_plots_species)

# # Get layers and then sums and means
# run_ID(layer_vector, "layerID", old_lID, multiLayers)
# run_ID(layer_vector, "layerID", old_lID, sums_means)
# source("scripts/plotsSumsMeans.R")

# # Get layers and then plot tables to get side by side layer plots
# run_ID(layer_vector, "layerID", old_lID, multiLayers)
# run_ID(layer_vector, "layerID", old_lID, plot_tables)
# source("scripts/plotsLayers.R")

# # Get weather plots
# source("scripts/plotsWeather.R")









