

# Run and plot selected species with selected ways to estimate site type

# Read settings.R file lines
f <- file("settings.R", open="r")
t <- readLines(f)
close(f)

old_sID <- 0
old_eID <- 0

# Vector values for loop. Values correspond to ids
species_vector <- c(1:4)
estimated_vector <- c(1:2)


# Get current speciesID and estimatedID from settings.R file
for (i in seq_along(t)){
  if (substr(t[i],0,9) == "speciesID") {
    old_sID <- t[i]
  } 
  if (substr(t[i],0,11) == "estimatedID") {
    old_eID <- t[i]
  }
}

# Check that ids were found
if (old_sID != 0 && old_eID != 0) {
  
  # Run
  for (i in estimated_vector) {
    eID <- i
    new_eID <- paste0("estimatedID <- ", as.character(eID))
    for (j in species_vector) {
      sID <- j
      new_sID <- paste0("speciesID <- ", as.character(sID))
      t2  <- gsub(pattern = old_sID, replace = new_sID, x = t)
      t2  <- gsub(pattern = old_eID, replace = new_eID, x = t2)
      writeLines(t2,"settings.R")
      source("multiSiteExample.R")
      source("plots.R")
      print(paste0("Plots for speciesID ", sID, " and estimatedID ", eID, " done."))
    }
  }
} else {
  print("Error! Check settings.R file content!")
}



