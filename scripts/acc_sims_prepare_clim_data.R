# This script is for processing acc climate data. First each climate data
# file is read and processed by filtering out those 1km-by-1km ids for which there
# are no corresponding forests in the BOKU tree data (AAA file). Then the matrices
# used for initialising PREBAS are created (1km-by-1km sites as rows, days as columns).
# After this the matrices are saved according to the name, plgid and save_path that are items
# in each returned acc_obj.
# Be aware of memory limitations when processing for simulation sites!



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)

slurm_id <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")



# Clim files to process
# clim_paths <- list.files(dest_path, full.names = T)







# Get climate paths from allas and filter by grid file PlgIDs
bucket_name <- "2000994-forest_navigator"
region <- Sys.getenv("AWS_REGION")
bucket_list <- as.data.table(get_bucket_df(bucket_name, region = region))$Key
allas_clim_paths <- bucket_list[!grepl(pattern = ".keep", bucket_list)]
allas_clim_paths_dt <- as.data.table(tstrsplit(allas_clim_paths, split = "[/]", keep = 3))[, id := .GRP, by = "V1"][, path := allas_clim_paths][, c("id", "path")]
plgids <- as.integer(str_extract(allas_clim_paths, "(?<=plgid_)[0-9]+"))
allas_clim_paths_dt[, PlgID := plgids]
grid_dt <- fread(grid_file_path)
filtered_clim_paths <- allas_clim_paths_dt[PlgID %in% unique(grid_dt$PlgID)]
split_clim_dts <- split(filtered_clim_paths, by = "id")


print(paste0("SLURM_ID: ", slurm_id))


# Get clim paths for id
clim_paths <- split_clim_dts[[slurm_id]]$path




# Get list of acc objects
init_clim_obj_list <- do.call(get_in_parallel, list(data = clim_paths,
                                                    FUN = get_acc_init_clim_object,
                                                    FUN_args = list(aaa_file = aaa_file, 
                                                                    operations = clim_operations,
                                                                    clean_data_base_path = clean_data_base_path,
                                                                    config = config,
                                                                    allas_opts = list(bucket = bucket_name,
                                                                                      FUN = fread,
                                                                                      opts = list(region = region))),
                                                    cores = cores,
                                                    type = type))



# Save all acc objects
save_obj_list <- do.call(get_in_parallel, list(data = init_clim_obj_list,
                              FUN = create_dir_and_save_acc_obj,
                              FUN_args = list(base_path = clean_data_base_path, 
                                              test = F),
                              cores = cores,
                              type = type))



acc_obj <- clustered_acc_init_obj_list[[1]]$data$parTran

create_dir_and_save_acc_obj(acc_obj = acc_obj, test = F, write_allas = T, allas_opts = allas_opts)

acc_obj$save_path



Sys.getenv("TMP")

s3write_using(acc_obj, FUN = fwrite, object = "data/parTran.csv", bucket = allas_opts$bucket, opts = allas_opts$opts)



s3read_using(FUN = loadRDataFile, object = "data/parTran.rdata", bucket = allas_opts$bucket, opts = allas_opts$opts)

s3saveRDS(acc_obj$data$parTran, object = "parTran.rds", bucket = allas_opts$bucket, region = allas_opts$opts$region)

s3readRDS(object = "parTran.rds", bucket = allas_opts$bucket, region = allas_opts$opts$region)


data <- clustered_acc_init_obj_list[[1]]$data
save_path <- clustered_acc_init_obj_list[[1]]$save_path
ext <- ".rdata"

item_list <- get_acc_save_data_list(data, save_path)
lapply(item_list, function(item) save_using(write_fun = FUN, x = item$item, file = item$save_path))

match.fun(FUN)

FUN = "save"
item <- item_list[[1]]$item
file = item_list[[1]]$save_path


dir.create("data/acc/input/simulation_sites_200/clean/plgid_7302942/climate/detrended", recursive = T)
save_using(item, FUN, file = file)

# Helper
save_using <- function(write_fun, ...) {
  write_fun(...)
}




#' Create Directory and Save ACC Object
#'
#' This function creates a directory and saves the ACC object to a specified base path.
#'
#' @param acc_obj A list containing the ACC object. Must include elements "name", "plgid", "data", and "save_path".
#' @param base_path A string representing the base path where the files will be saved.
#' @param test A logical flag indicating whether to run in test mode (default is FALSE).
#' @param ... Additional arguments to save_acc_data.
#'
#' @return NULL
#' @import checkmate
#' @examples
#' acc_obj <- list(
#'   name = "example",
#'   plgid = "123",
#'   data = list(matrix1 = matrix(1:4, 2, 2), matrix2 = matrix(5:8, 2, 2)),
#'   save_path = "/path/to/save"
#' )
#' create_dir_and_save_acc_obj(acc_obj, "/path/to/base", test = TRUE)
create_dir_and_save_acc_obj <- function(acc_obj, test = FALSE, write_allas = F, allas_opts = list(), FUN_name = "save", ...) {
  
  # Validate inputs
  assert_list(acc_obj, min.len = 4)
  assert_subset(c("name", "plgid", "data", "save_path"), names(acc_obj))
  assert_character(acc_obj$name, len = 1)
  assert_integer(acc_obj$plgid, len = 1)
  assert_list(acc_obj$data, min.len = 1)
  assert_character(acc_obj$save_path, len = 1)
  assert_logical(test, len = 1)
  
  
  name <- acc_obj$name
  plgid <- acc_obj$plgid
  data <- acc_obj$data
  save_path <- acc_obj$save_path

  
  # Get items and paths obj for saving
  item_list <- get_acc_save_data_list(data = data, save_path = save_path, default_name = name, FUN_name = FUN_name)


  if(test) {
    message("Test not saved! save_paths:")
    lapply(item_list, function(item) print(paste0(item$save_path)))
  } else {
    FUN <- match.fun(FUN_name) 
    if(write_allas) {
      bucket <- allas_opts$bucket
      opts <- allas_opts$opts
      
      print(paste0("Saving into bucket: ", bucket, "..."))
      invisible(lapply(item_list, function(item) {
        save_using(write_fun = s3write_using,
                   x = item$item, 
                   FUN = FUN, 
                   file = item$save_path,
                   object = item$save_path, 
                   bucket = bucket, 
                   opts = opts)
        
        print(paste0("Saved ", item$save_path))
      }))
      
  } else {
    if(!dir.exists(save_path)) dir.create(save_path, recursive = T)
    assert_directory_exists(save_path, access = "w")
    invisible(lapply(item_list, function(item) {
      save_using(write_fun = FUN, x = item$item, file = item$save_path)
      print(paste0("Saved ", item$save_path))
    }))
  }
  
}

}




get_acc_save_data_list <- function(data, save_path, default_name = "default", FUN_name = "save") {
  
  # Validate inputs
  assert_list(data, min.len = 1)
  assert_character(save_path, len = 1)
  assert_character(default_name, len = 1)
  assert_function(match.fun(FUN_name))
  
  ext = ".rdata"
  
  if(FUN_name != "save") {
    ext = ".csv"
  }
  
  item_list <- invisible(lapply(seq_along(data), function(index) {
    item <- data[[index]]
    name <- names(data)[index]
    if (is.null(name) || name == "") {
      name <- default_name
    }
    save_file_path <- file.path(save_path, paste0(name, ext))
    list(item = item, save_path = save_file_path)
  }))
  
  return(item_list)
}




# init_clim_obj_list[[1]]$data[[1]][1:70,]



# clim <- fread(clim_paths[4])
# filtered <- filter_data_by_tree_data_cells(clim, aaa_file)
# filtered_op <- transform_and_add_columns(filtered, operations)
# filtered_op_co2 <- add_acc_co2(filtered_op, "detrended", config)
# 
# 
# co2_file_path <- config$VAR_acc_co2_files[["gwl3"]]


# 
# class(clim$time)
# 
# filtered <- filter_data_by_tree_data_cells(clim, aaa_file)
# filtered_op <- transform_and_add_columns(filtered, operations)
# 
# names(filtered)
# 
# fread(selection_path)
























