
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
run_species <- function(species_vector,sources) {
  for (i in species_vector) {
    sID <- i
    new_sID <- paste0("speciesID <- ", as.character(sID))
    lines <- get_file_lines("settings.R")
    new_lines <- replace_patterns(c(old_sID),c(new_sID),lines)
    writeLines(new_lines,"settings.R")
    old_sID <- new_sID
    sapply(sources, source)
  }
}



# Initialise variables

# R files to run (vector in order)
multi_and_plots <- c("multiSiteExample.R", "plots.R")
multi_and_outputs <- c("multiSiteExample.R", "output_tables.R")

# Vector values for loop. Values correspond to ids
species_vector <- c(1:4)
estimated_vector <- c(1:2)

# 1. Get file lines
lines <- get_file_lines("settings.R")

# 2. Get lines to modify
old_sID <- get_line(lines, "speciesID")
old_eID <- get_line(lines, "estimatedID")


run_species(species_vector, multi_and_outputs)
# run_species_and_estimate(estimated_vector, species_vector, multi_and_plots)






