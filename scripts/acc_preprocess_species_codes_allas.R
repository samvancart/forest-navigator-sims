# This script is for extracting all BOKU species codes from initFiles that are stored
# in Allas and creating the species lookup using them. The species
# codes that are found are compared with an existing database that contains all
# the latin_name to prebas speciesID pairs that are currently available. If there
# are codes in the initFiles that do not yet exist in the database, those codes are
# saved to a file and the script raises an error and exits. The database should then
# be manually updated with the new codes and this script be run again. Once there
# are no missing IDs, the lookup is saved.

                 
# SOURCE_FILES ------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# LOAD INIT FILES ---------------------------------------------------------


data_prefix <- file.path("input", "simulation_sites_1km", "initFiles-1km-filtered/")

# Get keys
data_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")


# GET SPECIES CODES -------------------------------------------------------


# First get all required species codes from the initFiles that are in Allas.

species_codes_list <- list()

species_codes_list <- lapply(seq(data_keys_dt$Key), function(i) {
  key <- data_keys_dt$Key[[i]]
  total_keys <- length(data_keys_dt$Key)
  progress <- floor((i/total_keys)*100)
  print(paste0("Progress: ", progress, " %..."))
  init_dt <- s3read_using(FUN = fread, object = key, bucket = bucket, opts = list(region = region))
  species_codes <- unlist(unique(init_dt$species))
  append(species_codes_list, species_codes)
})


unique_species_codes <- unique(unlist(species_codes_list))

# unique_species_codes <- fread("data/acc/docs/species_codes_1km.csv")$code


# FILTER ------------------------------------------------------------------


# Filter the table with all BOKU species codes and latin names etc. by the codes.

species_codes_lookup_path <- "data/acc/docs/new_species_codes_consistent_CHO_2024-05-21.csv"
species_codes_lookup_dt <- fread(species_codes_lookup_path)

filtered_species_codes_lookup_dt <- species_codes_lookup_dt[which(code %in% unique_species_codes)]

unique_species_codes_dt <- data.table(code = sort(unique_species_codes))


# COMPARE WITH SPECIES LOOKUP DATABASE ------------------------------------


# Compare with table containing all available latin name to prebas speciesID info.
# If there are missing IDs, manually update the lookup table.

species_db_base_path <- "data/acc/docs/species"
species_db_path <- file.path(species_db_base_path, "latin-name-to-prebas-speciesID_lookup.csv")
species_db <- fread(species_db_path)

missing_speciesID <- filtered_species_codes_lookup_dt[!which(latin_name %in% species_db$latin_name)]


# HANDLE MISSING IDS ------------------------------------------------------


# If there are missing IDs, save them to a file and exit
if(nrow(missing_speciesID) > 0) {
  timestamp <- format(Sys.time(), "%d%m%Y_%H%M%S") # Create timestamp
  filename <- paste0("missing_speciesIDs_", timestamp, ".rds")
  missing_speciesID_path <- file.path(species_db_base_path, filename)
  print(paste0("Saving missing IDs to ", missing_speciesID_path, "..."))
  saveRDS(missing_speciesID, file = missing_speciesID_path)
  print("Done.")
  stop("Missing speciesIDs found, exiting.")
}


# CREATE FINAL LOOKUP -----------------------------------------------------


final_species_lookup <- merge(filtered_species_codes_lookup_dt, species_db, by = "latin_name")


# SAVE --------------------------------------------------------------------

# TODO Save to correct location with correct name
final_species_lookup_path <- "data/acc/docs/species_codes_lookup_1km.csv"
print(paste0("Saving to ", final_species_lookup_path, "..."))
fwrite(final_species_lookup, file = final_species_lookup_path)
print("Done")




























