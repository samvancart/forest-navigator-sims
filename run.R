
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
run_species_and_estimate <- function(estimated_vector,species_vector,sources) {
  for (i in estimated_vector) {
    eID <- i
    new_eID <- paste0("estimatedID <- ", as.character(eID))
    for (j in species_vector) {
      sID <- j
      new_sID <- paste0("speciesID <- ", as.character(sID))
      lines <- get_file_lines("settings.R")
      new_lines <- replace_patterns(c(old_sID,old_eID),c(new_sID,new_eID),lines)
      writeLines(new_lines,"settings.R")
      old_sID <- new_sID
      old_eID <- new_eID
      sapply(sources, source)
      print(paste0("Plots for speciesID ", sID, " and estimatedID ", eID, " done."))
    }
  }
}

# Run for species
run_ID <- function(id_vector,id_name, old_id, sources) {
  for (i in id_vector) {
    id <- i
    new_id <- paste0(id_name, " <- ", as.character(id))
    lines <- get_file_lines("settings.R")
    new_lines <- replace_patterns(c(old_id),c(new_id),lines)
    writeLines(new_lines,"settings.R")
    old_id <- new_id
    sapply(sources, source)
  }
}



# Initialise variables

# R files to run (vector in order)
multi <- c("multiSiteExample.R")
multi_and_plots <- c("multiSiteExample.R", "plots.R")
multi_and_outputs <- c("multiSiteExample.R", "output_tables.R")
plot_tables <- c("plot_tables.R")
util_f <- c("utility_functions.R")
sums_means <- c("modout_sums_and_means.R")

# Vector values for loop. Values correspond to ids
species_vector <- c(1:4)
estimated_vector <- c(1:2)
layer_vector <- c(1:2)

# 1. Get file lines
lines <- get_file_lines("settings.R")

# 2. Get lines to modify
old_sID <- get_line(lines, "speciesID")
old_eID <- get_line(lines, "estimatedID")
old_lID <- get_line(lines, "layerID")

# run_ID(layer_vector, "layerID", old_lID, multi)
# run_ID(species_vector, "speciesID", old_sID, multi_and_outputs)
run_species_and_estimate(estimated_vector, species_vector, multi_and_plots)
# run_ID(layer_vector, "layerID", old_lID, plot_tables)
# run_ID(layer_vector, "layerID", old_lID, util_f)
# run_ID(layer_vector, "layerID", old_lID, sums_means)





