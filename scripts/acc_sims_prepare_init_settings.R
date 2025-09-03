
# This script contains the general settings for the acc sims.
# All required sources, paths and variables should be stored here.

# SOURCE_FILES -------------------------------------------------------------

source("r/acc_sims.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")
source("r/utils.R")
source("r/csc_utils.R")
source('./r/multiSite.R')


# SIM-SITES_VARS ----------------------------------------------------------

simulation_sites <- c("simulation_sites_200", "test_sites")
simulation_site_id <- 1
simulation_site <- simulation_sites[simulation_site_id]


# ALLAS_OPTS ---------------------------------------------------------------

# Allas
bucket_name <- "2000994-forest_navigator"
region <- Sys.getenv("AWS_REGION")
allas_options <- list(region = region)
allas_read_FUN <- fread
allas_opts <- list(FUN = allas_read_FUN, bucket = bucket_name, opts = allas_options)
read_allas <- c(TRUE, FALSE)[simulation_site_id]
write_allas <- c(TRUE, FALSE)[simulation_site_id]


# TREEDATA_PATHS -----------------------------------------------------------


# Load tree data
boku_data_path <- paste0("data/acc/input/", simulation_site, "/raw")
boku_tree_data_path <- file.path(boku_data_path, "tree_data")
assert_directory_exists(boku_data_path)
assert_directory_exists(boku_tree_data_path)

# AAA
aaa_file <- list.files(file.path(boku_tree_data_path, "aaa"), 
                       pattern = paste0("AAA_cell1kmForestTypeList_filtered_", simulation_site_id,".csv"), 
                       full.names = T, 
                       recursive = T)

assert_file_exists(aaa_file)
aaa_all <- fread(aaa_file) # This is actually the filtered AAA. The aaa_1km file in acc_preprocess_1_aaa.R is the original. 




# Init files
init_files_path <- file.path(boku_tree_data_path, "init_files")
init_files <- list.files(init_files_path, full.names = T)
init_file_col <- c("InitFileID", "InitFileName")[simulation_site_id]
# assert_character(init_files, len = 64)


# CLEAN_DATA_PATH -----------------------------------------------------------



# Cleaned data path
clean_data_base_path <- file.path("data/acc/input", simulation_site, "clean")



# SPECIES_LOOKUP_VARS -----------------------------------------------------------



# Get species codes lookup
species_codes_name <- paste0("species_codes_lookup_", simulation_site_id,".csv")
species_codes_path <- file.path(boku_tree_data_path, "species_codes")
species_codes_cols <- c("code", "speciesID")
species_codes_lookup_path <- file.path(species_codes_path, species_codes_name)
codes_with_speciesID_dt <- fread(file = species_codes_lookup_path)[, ..species_codes_cols]
assert_names(names(codes_with_speciesID_dt), must.include = species_codes_cols)

# lookup for species IDs
species_lookup <- fread(species_codes_lookup_path)

# # Pre-processing step to merge PREBAS codes 
# prebas_species_lookup <- get_prebas_species_codes_from_pCROB(pCROB)
# species_lookup_prebas <- merge(species_lookup, prebas_species_lookup, by = "speciesID")



# COUNTRY_CODES_VARS -------------------------------------------------------

# Create the data.table with European countries and their 2-letter codes
country_codes <- data.table(
  # Names are specifically designed to support further processing
  country = c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City"),
  Country_Code = c("AL", "AD", "AM", "AT", "AZ", "BY", "BE", "BA", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE", "GR", "HU", "IS", "IE", "IT", "KZ", "XK", "LV", "LI", "LT", "LU", "MT", "MD", "MC", "ME", "NL", "MK", "NO", "PL", "PT", "RO", "RU", "SM", "RS", "SK", "SI", "ES", "SE", "CH", "TR", "UA", "GB", "VA")
)
# CELLS10_VARS -----------------------------------------------------------------


# Select one 10km cell
cells_10 <- unique(aaa_all$cell_300arcsec)
# cells_10_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
cells_10_id <- 2
aaa <- aaa_all[cell_300arcsec == cells_10[cells_10_id]]



# PARALLEL_OPTS ------------------------------------------------------------


# Cores and parallelisation type
cores <- max(1, availableCores() - 1)
type <- "FORK"
general_get_in_parallel_args <- list(cores = cores, type = type)



# SEED_OPTS -----------------------------------------------------------------


# Seed for creating reproducable list of seeds
seed <- 123
set.seed(seed)
num_sample_runs <- 1
seeds <- sample(c(1:1000000), num_sample_runs) # List of seeds to use


# TREEDATA_VARS ----------------------------------------------------------------



# TREE DATA

# Split by 10km id for parallel processing
aaa_split_col <- "PlgID"
split_aaa <- split(aaa_all, by = aaa_split_col)


tree_data_seed <- seeds
tree_data_path <- file.path(boku_data_path, "tree_data")
process_treedata_files_args <- list(seed = tree_data_seed,
                                    init_files_path = init_files_path,
                                    del_cols = c("cum_sum"),
                                    add_cols = c("cell", "cell_300arcsec", "PlgID", "PlgID_05"),
                                    init_file_col = init_file_col)

# Sample from all treedata files until ba reaches threshold (AAA file ba) then add 1km and 10km cell ids.
process_treedata_files_FUN <- process_treedata_files

get_in_parallel_tree_data_args <- list(data = split_aaa, 
                                       FUN = process_treedata_files_FUN, 
                                       FUN_args = process_treedata_files_args,
                                       cores = cores, 
                                       type = type)

# Names to use for the forest_id
id_columns <- c("i", "j", "cell", "cell_300arcsec")


assign_and_merge_args <- list(id_columns = id_columns, separator = "_",id_column_name = "forested_ha_id", 
                              species_codes_dt = codes_with_speciesID_dt, by.x = "species", by.y = "code")


# CLUSTERING


group_id_name <- "forested_ha_id"
species_id_name <- "speciesID"
clustering_group_cols <- c(group_id_name, species_id_name)
clustering_value_cols <-  c("dbh", "treeheight")
perform_clustering_by_group_FUN <- perform_clustering_by_group
perform_clustering_by_group_args <- list(group_cols = clustering_group_cols, # Check group_cols based on dts structure
                                         value_cols = clustering_value_cols,
                                         seed = seed,
                                         nstart = 25,
                                         iter.max = 50)


get_in_parallel_all_clusters_dts_args <- list(FUN = perform_clustering_by_group_FUN, 
                                              FUN_args = perform_clustering_by_group_args,
                                              cores = cores, 
                                              type = type)



# TREE DATA INPUT OBJ
tree_data_acc_input_obj <- list(args = list(process_treedata_files_args = process_treedata_files_args,
                                            assign_and_merge_args = assign_and_merge_args,
                                            perform_clustering_by_group_args = perform_clustering_by_group_args,
                                            aaa_split_col = aaa_split_col))





# GRID_FILE_PATH ------------------------------------------------------------

grid_file_path <- list.files(file.path(boku_data_path, "grid"), 
                             pattern = paste0("filtered_selection_cell10_", simulation_site_id,"\\.csv"), 
                             recursive = T, 
                             full.names = T)


# MULTI-INIT-VAR_VARS ------------------------------------------------------------


cluster_data_col_names = list(
  speciesID = "speciesID",
  Age = "age",
  Height = "h",
  Dbh = "d",
  basal_area = "b"
)

create_multiInitVar_for_layers_args <- list(grid_file_path = grid_file_path,
                                            group_id_name = group_id_name,
                                            species_id_name = species_id_name,
                                            col_names = cluster_data_col_names,
                                            clean_data_base_path = clean_data_base_path)





# SOIL_PATH ----------------------------------------------------------------



soil_file_path <- list.files(file.path(boku_data_path, "soil"), 
                             pattern = paste0("selection_soil_data_", simulation_site_id,"\\.csv"), 
                             recursive = T, 
                             full.names = T)


# SITEINFO_VARS ----------------------------------------------------------------


nYears_lookup <- c(detrended = 121, gwl2 = 110, gwl3 = 110, gwl4 = 110, historical = 72)

site_type_probs <- c(.1, .3, .5, .7, .9)
site_types <- c(5, 4, 3, 2, 1)




# CLIM_PATHS ---------------------------------------------------------------



# Climate paths
climate_7z_dir <- paste0( "data/acc/input/", simulation_site, "/raw/climate")
assert_directory_exists(climate_7z_dir)
print(climate_7z_dir)
zip_pattern <- "\\.7z"
all_files <- list.files(climate_7z_dir, recursive = T)
files_7z <- all_files[grepl(zip_pattern, all_files)] # Check which files are returned

# Define from and to paths
files_7z_paths <- file.path(climate_7z_dir, files_7z)
dest_path <- file.path(climate_7z_dir, "unzipped")





# CLIM_VARS ----------------------------------------------------------------



# Climate data processing operations


# Define functions
# PAR <- swRad*0.48*4.6*0.08640000224 #mol PPFD m-2 d-1
# par_fun <- function(x) x * 0.48 * 4.6/1000 # OLD
# PAR from https://nature.berkeley.edu/biometlab/pdf/Ruimy%20et%20al%201995%20Adv%20Ecol%20Research.pdf
par_fun <- function(x) x * 0.48 * 4.6 * 0.08640000224
kelvin_to_c <- function(x) x - 273.15
precip_to_mm_per_day <- function(x) {
  x <- x * 86400
  ifelse(x < 0, 0, x)
}

remove_feb_29 <- function(dt, time_col) {
  dt[!(format(get(time_col), "%d.%m") == "29.02")]
}

setnames_fun <- function(dt, old = NULL, new) {
  if(is.null(old)) {
    setnames(dt, old = colnames(dt), new = new)
  } else {
    setnames(dt, old = old, new = new, skip_absent = T) 
  }
}



del_dt_cols <- function(dt, del_cols) {
  d_cols <- del_cols[which(del_cols %in% names(dt))]
  dt[, (d_cols) := NULL]
  dt
}


filter_dt_cols <- function(dt, keep_cols) {
  filtered_cols <- keep_cols[keep_cols %in% names(dt)]
  return(dt[, ..filtered_cols])
}

filter_years <- function(dt, start_year, end_year) {
  dt <- dt[year(time) >= start_year & year(time) <= end_year]
  return(dt)
}

del_dt_cols_args <- list(del_cols = c("rsds", "tasmax", "tasmin", "XLON", "YLAT", "PlgID_05", "PET", "Longitude", "Latitude"))

clim_keep_cols <- c("PlgID","time","pr","tas","vpd", "BOKU_ID", "PlgID_05", "cell_300arcsec", "par", "co2")
start_year <- "2010"
end_year <- "2099"


# Define operations
clim_operations <- list(
  list(fun = setnames_fun,
       args = list(old = "Date", new = "time")),
  
  list(fun = filter_years,
       args = list(start_year = start_year, end_year = end_year)),
  
  list(col_name = "par", 
       fun = par_fun, 
       cols = "rsds",
       names_cols = "x", 
       args = list()),
  
  list(col_name = "tas", 
       fun = kelvin_to_c, 
       cols = "tas",
       names_cols = "x", 
       args = list()),
  
  list(col_name = "pr", 
       fun = precip_to_mm_per_day, 
       cols = "pr",
       names_cols = "x", 
       args = list()),
  
  list(col_name = "vpd", 
       fun = function(dt) dt[, vpd := vpd/1000],
       args = list()),
  # Default CO2 for detrended scenario = 2010 value from co2_historic_annual-1980_2010.txt.
  list(col_name = "co2",
       fun = function(dt) dt[, co2 := 388.76],
       args = list()),
  
  list(fun = remove_feb_29,
       args = list(time_col = "time")),
  
  list(fun = filter_dt_cols,
       args = list(keep_cols = clim_keep_cols)),
  
  list(fun = setnames_fun, 
       args = list(old = "BOKU_ID", new = "cell")),
  
  list(fun = setnames_fun,
       args = list(new = c("PlgID", "time", "precip", "tair", "vpd", "cell", "cell_300arcsec", "par", "co2"))),
  
  list(fun = function(dt) dt[, cell := as.character(cell)], # Cell is actually PlgID_05. Check and fix!
       args = list()),
  
  list(fun = function(dt) dt[, day := .GRP, by = c("time")],
       args = list())
)


# CLIM DATA INPUT OBJ
clim_data_acc_input_obj <- list(args = list(clim_operations = clim_operations,
                                            simulation_site = simulation_site))





# SELECTION_PATH -----------------------------------------------------------

# Paths for siteID lookup creation
selection_path <- grid_file_path

# CLUSTERED-BASE_PATH -------------------------------------------------------


clustered_base_path <- paste0("data/acc/input/", simulation_site, "/raw/clustered")



# CONVERSIONS_PATH ---------------------------------------------------------

# Lookup for converting names and units
conversions_path <- paste0("data/acc/docs/forest_nav_units_and_names_conversions_lookup.csv")
conversions_dt <- fread(conversions_path)


# OUTPUT_PATHS ------------------------------------------------------------------

# output_base_path <- file.path("data", "acc", "output", simulation_site) # FILESYSTEM PATH

output_base_path <- file.path("output", simulation_site) # ALLAS PATH



# SAVE_DIRS --------------------------------------------------------------

output_save_dir <- "output_files"

dclass_save_dir <- "dbh_classes"


# MAN PATHS ---------------------------------------------------------------

sweden_man_path <- "data/acc/docs/management/SE_SpeciesSharePerForestTypeClusterElevSiteMean_2025-07-18_BAUMgt_20082025.csv"

man_paths_list <- list(Sweden = sweden_man_path)

# MAN_VARS ----------------------------------------------------------------

man_names <- c("noman", "bau")
man_id <- 2
man_name <- man_names[man_id]

man_params <- list(noman = list(defaultThin = 0, 
                                ClCut = 0, 
                                mortMod = 3, 
                                ingrowth = T),
                   
                   bau = list(defaultThin = 1,
                              ClCut = 1,
                              mortMod = 3,
                              ingrowth = T))

# OUTPUT_VARS --------------------------------------------------------------

produce_output_paths <- list(clean_data_base_path = clean_data_base_path,
                             selection_path = selection_path,
                             aaa_file = aaa_file,
                             conversions_path = conversions_path,
                             output_base_path = output_base_path,
                             species_lookup_path = species_codes_lookup_path,
                             output_save_dir = output_save_dir,
                             dclass_save_dir = dclass_save_dir,
                             man_paths_list = man_paths_list) # Management paths as named list

# Output IDs
varOutID <- c(7, 11:14, 17, 18, 19, 22, 24, 25, 30, 31:33, 42, 43, 44, 47, 50)
vHarv <- c(30,2)


# RUN-TABLE_PATHS ----------------------------------------------------------

run_table_base_path <- "data/acc/docs/run_table"
run_table_name <- str_c("run_table", simulation_site, man_name, sep = "-")
run_table_full_path <- file.path(run_table_base_path, paste0(run_table_name, ".rds"))

if(!file.exists(run_table_full_path)) warning(paste0(run_table_full_path, " does not exist."))



# RUN-TABLE_VARS -----------------------------------------------------------

runTable_split_parts <- ifelse(simulation_site=="simulation_sites_200", 3, 1)

# PRINT_VARS --------------------------------------------------------------






# Get variables as named list
all_vars <- get_named_list(simulation_site,
                           cells_10_id,
                           cores,
                           num_sample_runs,
                           boku_data_path,
                           clean_data_base_path,
                           id_columns,
                           perform_clustering_by_group_args,
                           cluster_data_col_names,
                           species_codes_lookup_path,
                           grid_file_path,
                           soil_file_path,
                           man_name,
                           runTable_split_parts)


# Named list as text
txt_vec <- get_named_list_as_txt(all_vars, sep_text = ": ")

print(txt_vec)







