# Helper functions

source("settings.R")

# Calculate basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
get_basal_area_nfi_sweden <- function(x) {
  dbh <- as.double(x[6])
  multiplier <- as.double(x[15])
  basal_area <- pi*(dbh/200)^2*multiplier
  return(basal_area)
}

# Basal area for NFI Sweden
path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/all_sorted_group_species_cIDs.csv")
df <- fread(path)

# Apply to each df row to get basal_area vector
basal_area <- apply(df, 1, get_basal_area_nfi_sweden)

# Add vector as column to df
df$basal_area <- basal_area

# path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/nfi/sweden/sorted_group_species_cIDs_basal_area.csv")
# write.csv(df, path, row.names = F)