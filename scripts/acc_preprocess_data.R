# Code for filtering AAA file, creating species code lookup


source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



# ### PRE-PROCESS AAA FILE
# 
# sel <- fread("data/acc/input/simulation_sites_200/raw/grid/prebas_selected_reduced_1km.csv")[, c("PlgID", "PlgID_05", "XLON", "YLAT")]
# 
# 
# bokuIDs <- fread("data/acc/docs/boku_id_map_1km.csv")
# 
# sel_bokuID <- merge(sel, bokuIDs[, c("PlgID_05", "BOKU_ID")], by = "PlgID_05")
# 
# 
# aaa_filtered <- merge(aaa_all, sel_bokuID, by.x = "cell", by.y = "BOKU_ID")
# 
# length(unique(aaa_filtered$PlgID))
# 
# aaa_filtered[!complete.cases(aaa_filtered)]
# 

filtered_sel <- sel_bokuID[BOKU_ID %in% aaa_filtered$cell]
filtered_sel10 <- merge(filtered_sel, aaa_filtered[, c("cell", "cell_300arcsec")], by.x = "BOKU_ID", by.y = "cell")
fwrite(filtered_sel10, file = "data/acc/input/simulation_sites_200/raw/grid/filtered_selection_cell10.csv")



init_files_path <- file.path(boku_tree_data_path, "init_files")
init_files_filtered <- unique(aaa_filtered$InitFileID)
init_filenames <- file.path(init_files_path, paste0(init_files_filtered, "_01.csv"))

# # fwrite(aaa_filtered, file = file.path(boku_tree_data_path, "aaa", "AAA_cell1kmForestTypeList_filtered"))


# GET UNIQUE SPECIES CODES
use_cores <- availableCores()
species_codes_list <- list()

species_codes_list <- mclapply(init_filenames, function(filename) {
  species_codes <- unlist(unique(fread(filename)$species))
  append(species_codes_list, species_codes)
}, mc.cores = use_cores)


unique_species_codes <- unique(unlist(species_codes_list))


species_codes_lookup_path <- list.files("data", pattern = "new_species_codes", recursive = T, full.names = T)
species_codes_lookup_dt <- fread(species_codes_lookup_path)

filtered_species_codes_lookup_dt <- species_codes_lookup_dt[which(code %in% unique_species_codes)]

unique_species_codes_dt <- data.table(code = sort(unique_species_codes))


swe_codes <- fread(list.files("data", pattern = "tree_species_codes_and", full.names = T, recursive = T))

swe_codes[Species=="Pinus silvestris"]$Species <- "Pinus sylvestris"
swe_codes[Species=="Fagus silvatica"]$Species <- "Fagus sylvatica"
swe_codes[Species=="Other broadleaved"]$Species <- "other broadleaves"
swe_codes[, code := NULL]
setnames(swe_codes, old = "Species", new = "latin_name")

species_codes_lookup_save_path <- "data/tree_data/docs/"

# MATCH WITH FILTERED CODES
swe_codes[which(latin_name %in% filtered_species_codes_lookup_dt$latin_name)]
swe_codes[!which(latin_name %in% filtered_species_codes_lookup_dt$latin_name)]
speciesID_lookup_swe <- swe_codes[which(latin_name %in% filtered_species_codes_lookup_dt$latin_name)]


filtered_species_codes_lookup_dt[which(latin_name %in% swe_codes$latin_name)]
missing_speciesID <- filtered_species_codes_lookup_dt[!which(latin_name %in% swe_codes$latin_name)]
# save(missing_speciesID, file = "data/acc/docs/missing_speciesIDs.rdata")

# Get missing ids
missing_speciesID_fm <- loadRDataFile("data/acc/docs/missing_speciesIDs_fm.rdata")
setnames(missing_speciesID_fm, old = "prebasID", new = "speciesID")
missing_speciesID_fm_lookup <- missing_speciesID_fm[, c("latin_name", "speciesID")]

speciesID_lookup_combined <- rbind(speciesID_lookup_swe, missing_speciesID_fm_lookup)

sim_sites_200_species_lookup <- merge(filtered_species_codes_lookup_dt, speciesID_lookup_combined, by = "latin_name")


# fwrite(sim_sites_200_species_lookup, file = "data/acc/docs/sim_sites_200_species_codes_lookup.csv")


# # MATCH WITH ALL CODES
# fin_codes <- swe_codes[which(latin_name %in% species_codes_lookup_dt$latin_name)]
# swe_codes[which(latin_name %in% species_codes_lookup_dt$latin_name)]
# swe_codes[!which(latin_name %in% species_codes_lookup_dt$latin_name)]

















