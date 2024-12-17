source("r/acc_sims.R")
source("r/clusters_dt.R")
source("r/parallel_process.R")
source("r/utils.R")
source("r/csc_utils.R")
source('./r/multiSite.R')


simulation_sites <- c("simulation_sites", "test_sites")
simulation_site_id <- 2
simulation_site <- simulation_sites[simulation_site_id]


# Load tree data
boku_data_path <- paste0("data/acc/input/", simulation_site, "/raw")
assert_directory_exists(boku_data_path)
aaa_file <- list.files(config$PATH_data, pattern = "AAA", full.names = T, recursive = T)
assert_file_exists(aaa_file)
init_files <- list.files(config$PATH_data, "FIN_", full.names = T, recursive = T)
assert_character(init_files, len = 64)
aaa_all <- fread(aaa_file)


# Get species codes lookup
species_codes_file <- "test_sites_species_codes_lookup.csv"
species_codes_cols <- c("code", "speciesID")
species_codes_lookup_path <- list.files(config$PATH_data, pattern = species_codes_file, full.names = T, recursive = T)
fin_codes_with_speciesID_dt <- fread(file = species_codes_lookup_path)[, ..species_codes_cols]
assert_names(names(fin_codes_with_speciesID_dt), must.include = species_codes_cols)


# Select one 10km cell
cells_10 <- unique(aaa_all$cell_300arcsec)
cells_10_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
# cells_10_id <- 2
aaa <- aaa_all[cell_300arcsec == cells_10[cells_10_id]]



# Seed for creating reproducable list of seeds
seed <- 123
set.seed(seed)
num_sample_runs <- 100
seeds <- sample(c(1:1000000), num_sample_runs) # List of seeds to use


# Cores and parallelisation type
cores <- max(1, availableCores() - 1)
type <- "FORK"


# TREE DATA

tree_data <- seeds
tree_data_path <- file.path(boku_data_path, "tree_data")
process_treedata_files_args <- list(aaa = aaa,
                 boku_data_path = tree_data_path,
                 del_cols = c("cum_sum"),
                 add_cols = c("cell", "cell_300arcsec"))

# Sample from all treedata files until ba reaches threshold (AAA file ba) then add 1km and 10km cell ids.
process_treedata_files_FUN <- process_treedata_files

get_in_parallel_tree_data_args <- list(data = tree_data, 
                                       FUN = process_treedata_files_FUN, 
                                       FUN_args = process_treedata_files_args, 
                                       df_name = "seed",
                                       cores = cores, 
                                       type = type)

# Names to use for the forest_id
id_columns <- c("i", "j", "cell", "cell_300arcsec")


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


### multiInitVar

grid_file_path <- list.files(file.path(boku_data_path, "grid"), 
                             pattern = "filtered_selection_fi_cell10\\.csv", 
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
                                            col_names = cluster_data_col_names)



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


# Cleaned data path
clean_data_base_path <- file.path("data/acc/input", simulation_site, "clean")



# Climate data processing operations


# Define functions
par_fun <- function(x) x * 0.48 * 4.6/1000
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
    if(old %in% names(dt)) {
      setnames(dt, old = old, new = new) 
    } else {
      dt 
    }
  }
}



del_dt_cols <- function(dt, del_cols) {
  d_cols <- del_cols[which(del_cols %in% names(dt))]
  dt[, (d_cols) := NULL]
  dt
}

del_dt_cols_args <- list(del_cols = c("rsds", "tasmax", "tasmin", "XLON", "YLAT", "PlgID_05", "PET", "Longitude", "Latitude"))


# Define operations
operations <- list(
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
  
  list(col_name = "co2",
       fun = function(dt) dt[, co2 := 380],
       args = list()),
  
  list(fun = setnames_fun,
       args = list(old = "Date", new = "time")),
  
  list(fun = remove_feb_29,
       args = list(time_col = "time")),
  
  list(fun = del_dt_cols,
       args = del_dt_cols_args),
  
  list(fun = setnames_fun, 
       args = list(old = "BOKU_ID", new = "cell")),
  
  list(fun = setnames_fun,
       args = list(new = c("PlgID", "time", "precip", "tair", "vpd", "cell", "cell_300arcsec", "par", "co2"))),

  list(fun = function(dt) dt[, day := .GRP, by = c("time")],
       args = list())
)










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
                           grid_file_path)


# Named list as text
txt_vec <- get_named_list_as_txt(all_vars, sep_text = ": ")

print(txt_vec)




