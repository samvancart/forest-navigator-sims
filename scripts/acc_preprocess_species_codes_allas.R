# This code is for extracting all BOKU species codes from initFiles that are stored
# Allas. The codes are saved to a csv file from which they can be compared with an
# existing species lookup file's codes.

# TODO Compare new species codes ("data/acc/docs/species_codes_1km.csv") with old ones.                    


# SOURCE_FILES ------------------------------------------------------------


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# LOAD INIT FILES ---------------------------------------------------------


data_prefix <- file.path("input", "simulation_sites_1km", "initFiles-1km-filtered/")

# Get keys
data_keys_dt <- setnames(as.data.table(list_all_objects_in_bucket(only_keys = T, bucket = bucket, prefix = data_prefix, region = region)), "Key")


# GET SPECIES CODES -------------------------------------------------------


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

species_codes_lookup_path <- list.files("data", pattern = "new_species_codes", recursive = T, full.names = T)
species_codes_lookup_dt <- fread(species_codes_lookup_path)

filtered_species_codes_lookup_dt <- species_codes_lookup_dt[which(code %in% unique_species_codes)]

unique_species_codes_dt <- data.table(code = sort(unique_species_codes))


# SAVE --------------------------------------------------------------------


path <- "data/acc/docs/species_codes_1km.csv"
print(paste0("Saving to ", path, "..."))
fwrite(unique_species_codes_dt, file = path)
print("Done")



