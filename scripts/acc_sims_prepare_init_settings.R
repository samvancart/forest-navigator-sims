source("r/acc_sims.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")
source("r/utils.R")
source("r/csc_utils.R")
source('./r/multiSite.R')

initSeedling.def

simulation_sites <- c("simulation_sites_200", "test_sites")
simulation_site_id <- 2
simulation_site <- simulation_sites[simulation_site_id]


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
aaa_all <- fread(aaa_file)



# Init files
init_files_path <- file.path(boku_tree_data_path, "init_files")
init_files <- list.files(init_files_path, full.names = T)
# assert_character(init_files, len = 64)



# Cleaned data path
clean_data_base_path <- file.path("data/acc/input", simulation_site, "clean")


# Get species codes lookup
species_codes_name <- paste0("species_codes_lookup_", simulation_site_id,".csv")
species_codes_path <- file.path(boku_tree_data_path, "species_codes")
species_codes_cols <- c("code", "speciesID")
species_codes_lookup_path <- file.path(species_codes_path, species_codes_name)
codes_with_speciesID_dt <- fread(file = species_codes_lookup_path)[, ..species_codes_cols]
assert_names(names(codes_with_speciesID_dt), must.include = species_codes_cols)


# Select one 10km cell
cells_10 <- unique(aaa_all$cell_300arcsec)
# cells_10_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
cells_10_id <- 2
aaa <- aaa_all[cell_300arcsec == cells_10[cells_10_id]]



# Cores and parallelisation type
cores <- max(1, availableCores() - 1)
type <- "FORK"
general_get_in_parallel_args <- list(cores = cores, type = type)


# Seed for creating reproducable list of seeds
seed <- 123
set.seed(seed)
num_sample_runs <- 1
seeds <- sample(c(1:1000000), num_sample_runs) # List of seeds to use



# TREE DATA

# Split by 10km id for parallel processing
aaa_split_col <- "PlgID"
split_aaa <- split(aaa_all, by = aaa_split_col)



tree_data_seed <- seeds
tree_data_path <- file.path(boku_data_path, "tree_data")
process_treedata_files_args <- list(seed = tree_data_seed,
                 init_files_path = init_files_path,
                 del_cols = c("cum_sum"),
                 add_cols = c("cell", "cell_300arcsec", "PlgID"))

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
tree_data_acc_input_obj <- list(args = list(process_treedata_files = process_treedata_files_args,
                                            assign_and_merge = assign_and_merge_args,
                                            perform_clustering_by_group = perform_clustering_by_group_args,
                                            aaa_split_col = aaa_split_col))






### multiInitVar

grid_file_path <- list.files(file.path(boku_data_path, "grid"), 
                             pattern = paste0("filtered_selection_cell10_", simulation_site_id,"\\.csv"), 
                             recursive = T, 
                             full.names = T)


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



# Site info


soil_file_path <- list.files(file.path(boku_data_path, "soil"), 
                             pattern = paste0("selection_soil_data_", simulation_site_id,"\\.csv"), 
                             recursive = T, 
                             full.names = T)


nYears_lookup <- c(detrended = 121, gwl2 = 110, gwl3 = 110, gwl4 = 110, historical = 72)

site_type_probs <- c(.1, .3, .5, .7, .9)
site_types <- c(5, 4, 3, 2, 1)





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
operations <- list(
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

  list(fun = function(dt) dt[, day := .GRP, by = c("time")],
       args = list())
)





# OUTPUT

output_base_path <- paste0("data/acc/output/", simulation_site)


# Paths for siteID lookup creation
selection_path <- paste0("data/acc/input/", simulation_site, "/raw/grid/filtered_selection_fi_cell10.csv")
clustered_base_path <- paste0("data/acc/input/", simulation_site, "/raw/clustered")


# Output IDs
varOutID <- c(44,18,19,11:14,17,30,43,42,7,22,31:33,24:25,47,50)
vHarv <- c(30,2)

# Lookup for converting names and units
conversions_path <- paste0("data/acc/docs/forest_nav_units_and_names_conversions_lookup.csv")
conversions_dt <- fread(conversions_path)


# lookup for species IDs
species_lookup <- fread(species_codes_lookup_path)





# Operations

sum_bioms <- function(dt, name, sum_cols, by = c("year", "site", "layer")) {
  dt[, (name) := sum(unlist(.SD)), by = by, .SDcols = sum_cols]
  return(dt)
}

calculate_lc <- function(dt, name = "Lc", height = "H", hc_base = "Hc_base") {
  dt[, Lc := pmax(get(height) - get(hc_base), 0)]
  return(dt)
}

set_output_names <- function(dt, ...) {
  setnames(dt, ...)
}

convert_output_vals_to_correct_units <- function(dt, conversions_dt) {
  invisible(  apply(conversions_dt, 1, function(row) {
    var <- row[["Variable"]]
    conversion_char <- row[["PREBASconv"]]
    conversion <- eval(parse(text = conversion_char))
    
    if(var %in% names(dt)) {
      dt[, (var) := .SD * conversion, .SDcols = var]
    }
  }))
  return(dt)
}

add_columns_to_dt <- function(dt, columns) {
  # Ensure columns is a named list
  assert_list(columns, names = "named")
  
  # Add columns to the data.table
  dt[, names(columns) := mget(names(columns), envir = as.environment(columns))]
  
  return(dt)
}


stem_cols <-  c("Wstem", "Wbranch")
root_cols <- c("WfineRoots", "W_croot")
hc_base_col <- c("Hc_base")
old_output_col_names <- c("Units", "forest_type", "value", "year", "layer")
new_output_col_names <- c("Unit", "Mixture_type", "Value", "Year", "Layer")
del_output_cols <- c("site", "species")
output_col_order <- c("Model", "Country", "Climate_scenario", "Management_scenario", 
                      "PlgID_05", "Mixture_type", "Species", "Canopy_layer", "Variable", "Unit", "Year", "Value")


get_output_operations <- function(plgid, 
                                  multiOut, 
                                  conversions_dt,
                                  siteID_lookup,
                                  species_lookup,
                                  add_cols,
                                  vHarv = c(30,2), 
                                  stem_cols = c("Wstem", "Wbranch"), 
                                  root_cols = c("WfineRoots", "W_croot"),
                                  old_output_col_names = c("Units", "forest_type", "value", "year", "variable"),
                                  new_output_col_names = c("Unit", "Mixture_type", "Value", "Year", "Variable"),
                                  del_output_cols = c("site", "species"),
                                  output_col_order = c("Model", "Country", "Climate_scenario", "Management_scenario", 
                                                       "PlgID_05", "Mixture_type", "Species", "Canopy_layer", "Variable", "Unit", "Year", "Value")) {
  
  output_operations <- list(
    # Get stem and root biomasses
    list(fun = sum_bioms,
         args = list(name = "stem_biom", sum_cols = stem_cols)),
    
    list(fun = sum_bioms,
         args = list(name = "root_biom", sum_cols = root_cols)),
    # Calculate crown length from H and Hc_base
    list(fun = calculate_lc, 
         args = list()),
    # Delete unnecessary cols
    list(fun = del_dt_cols,
         args = list(del_cols = c(stem_cols, hc_base_col))),
    # Get new names from conversion table
    list(fun = set_output_names,
         args = list(old = conversions_dt$PREBAS, new = conversions_dt$Variable, skip_absent = T)),
    
    list(fun = convert_output_vals_to_correct_units,
         args = list(conversions_dt = conversions_dt)),
    
    list(fun = melt.data.table,
         args = list(id.vars = c("site", "year", "layer"))),
    
    list(fun = merge_multiOut_species_and_harv_with_out_dt,
         args = list(multiOut = multiOut, vHarv = vHarv)),
    
    list(fun = merge.data.table,
         args = list(y = conversions_dt[,c("Variable", "Units")], by.x = "variable", by.y = "Variable")),
    # Get PlgID_05 and mixture type
    list(fun = merge.data.table,
         args = list(y = siteID_lookup, by = c("site"))),
    # Get species codes
    list(fun = function(dt) dt[species_lookup[, c("speciesID", "species_code")], on = .(species = speciesID), Species := i.species_code],
         args = list()),
    # Layer to int
    list(fun = function(dt) dt[, layer := as.integer(unlist(tstrsplit(dt$layer, split = " ", keep = 2)))],
         args = list()),
    # Add Model, Country clim_scen, harv_scen and Canopy_layer
    list(fun = add_columns_to_dt,
         args = list(columns = add_cols)),
    
    list(fun = function(dt, start_year) dt[, year := as.integer(dt$year + (as.integer(start_year) - 1))],
         args = list(start_year = start_year)),
    
    list(fun = set_output_names, 
         args = list(old = old_output_col_names, new = new_output_col_names, skip_absent = T)),
    
    list(fun = del_dt_cols, 
         args = list(del_cols = del_output_cols)),
    
    list(fun = setcolorder, 
         args = list(neworder = output_col_order))
    
  )
  
}








############ PRINT ################

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
                           soil_file_path)


# Named list as text
txt_vec <- get_named_list_as_txt(all_vars, sep_text = ": ")

print(txt_vec)




