source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


aws.s3::bucket_list_df(region = "")
bucket_name <- "2000994-forest_navigator"
region <- Sys.getenv("AWS_REGION")

bucket_list <- as.data.table(get_bucket_df(bucket_name, region = region))$Key
filtered_bucket_list <- bucket_list[!grepl(pattern = ".keep", bucket_list)]


c_path <- filtered_bucket_list[601]

t_c <- s3read_using(FUN = fread, object = c_path, bucket = bucket_name, opts = list(region = region))





# FUNCTION TO RUN EVERY STEP FOR A VECTOR OF PlgIDs

# Steps:
# 1. Process tree data to get clusters
# 2. Process climate data
# 3. Use clustered tree data to get multiInitVar
# 4. Create siteInfo from tree data, clim data and soil data
# 5. Initialise model with the processed data and run
# 6. Write output to folder or bucket


run_acc_for_plgid <- function(plgid_vec, 
                              tree_data_acc_input_obj,
                              clim_data_acc_input_obj,
                              aaa_file, 
                              clean_data_base_path, 
                              get_in_parallel_args,
                              config,
                              allas_opts,
                              clim_paths) {
  
  # clustered_acc_init_obj <- run_acc_with_combine_args(FUN = create_acc_clustered_tree_data,
  #                                                     acc_input_obj = tree_data_acc_input_obj,
  #                                                     plgid_vec = plgid_vec,
  #                                                     aaa_file = aaa_file,
  #                                                     clean_data_base_path = clean_data_base_path,
  #                                                     get_in_parallel_args = get_in_parallel_args)
  
  
  init_clim_obj_list <- run_acc_with_combine_args(FUN = create_acc_clim_data,
                                                  acc_input_obj = clim_data_acc_input_obj,
                                                  plgid_vec = plgid_vec, 
                                                  aaa_file = aaa_file, 
                                                  clean_data_base_path = clean_data_base_path, 
                                                  get_in_parallel_args = get_in_parallel_args,
                                                  config = config,
                                                  allas_opts = allas_opts,
                                                  clim_paths = clim_paths)
  
  return(init_clim_obj_list)
}


# Helper
get_filtered_clim_paths_from_bucket <- function(grid_file_path, allas_opts, plgid_pat = "(?<=plgid_)[0-9]+") {
  bucket_list <- as.data.table(get_bucket_df(bucket = allas_opts$bucket, region = allas_opts$opts$region))$Key
  plgid_vec <- unique(fread(grid_file_path)$PlgID)
  clim_paths <- bucket_list[is_regex_match(bucket_list, plgid_pat, plgid_vec)]
  
  return(clim_paths)
}

# Helper
is_regex_match <- function(str_vec, regex_pat, compare_to_str) {
  return(str_extract(str_vec, regex_pat) %in% compare_to_str)
}






create_acc_clim_data <- function(acc_input_obj) {
  
  with(acc_input_obj$args, {
    
    print(paste0("Getting climate data..."))
    
    process_clim_files_parallel_args <- 
      c(list(data = clim_paths,
             FUN = get_acc_init_clim_object,
             FUN_args = list(aaa_file = aaa_file, 
                             operations = clim_operations,
                             clean_data_base_path = clean_data_base_path,
                             config = config,
                             allas_opts = allas_opts)),
        get_in_parallel_args)
    
    
    
    # Get list of acc objects
    init_clim_obj_list <- do.call(get_in_parallel, process_clim_files_parallel_args)
    
    return(init_clim_obj_list)
  })
}




run_acc_with_combine_args <- function(FUN, acc_input_obj, ...) {
  # Extract the list of arguments from the object
  args_list <- acc_input_obj$args
  
  # Combine the list with additional arguments
  combined_args <- c(args_list, list(...))
  
  # Update the object's args with the combined arguments
  acc_input_obj$args <- combined_args
  
  # print(acc_input_obj)

  # Call FUN with object as argument
  result <- do.call(FUN, list(acc_input_obj = acc_input_obj))


  return(result)
}




create_acc_clustered_tree_data <- function(acc_input_obj) {
  
  with(acc_input_obj$args, {
    
    
    # Validations
    required_names <- c(
      "process_treedata_files_args", 
      "assign_and_merge_args", 
      "perform_clustering_by_group_args",
      "aaa_split_col",
      "plgid_vec",
      "aaa_file",
      "clean_data_base_path",
      "get_in_parallel_args"
    )
    
    assert_list(acc_input_obj$args)
    assert_names(names(acc_input_obj$args), must.include = required_names)
    
    
    # Run
    print(paste0("Getting tree_data..."))
    
    aaa_dt <- handle_data_input(aaa_file)
    filtered_aaa_dt <- aaa_dt[PlgID %in% plgid_vec]
    split_aaa <- split(filtered_aaa_dt, by = "PlgID")
    
    process_treedata_files_parallel_args <- 
      c(list(data = split_aaa,
             FUN = process_treedata_files,
             FUN_args = process_treedata_files_args),
        get_in_parallel_args)
    
    
    tree_data_dts <- do.call(get_in_parallel, process_treedata_files_parallel_args)
    
    cat("\n")
    cat("\n")
    print(paste0("Merging..."))
    
    assign_and_merge_args$tree_data_dts <- tree_data_dts
    dts <- do.call(assign_and_merge, assign_and_merge_args)
    
    
    cat("\n")
    print(paste0("Clustering..."))
    
    clustering_args <- c(list(data = dts, 
                              FUN = perform_clustering_by_group, 
                              FUN_args = perform_clustering_by_group_args), 
                         get_in_parallel_args)
    
    # Get clusters in parallel and combine
    all_clusters_dt <- rbindlist(
      do.call(get_in_parallel, clustering_args)
    )
    
    cat("\n")
    cat("\n")
    print(paste0("Aggregating..."))
    
    # Aggregate clustered dt
    aggr_clusters_dt <- all_clusters_dt[, .(d = mean(dbh), h = mean(treeheight)/100, b = sum(ba), age = as.integer(mean(age))),
                                        by = .(cell_300arcsec, PlgID, cell, forested_ha_id, speciesID, cluster_id)]
    
    
    # Split for saving
    split_aggr_clusters_dt <- split(aggr_clusters_dt, by = aaa_split_col)
    
    
    cat("\n")
    print(paste0("Creating clustered_acc_init_obj_list..."))
    
    # Create list of acc_init_objects
    clustered_acc_init_obj_list <- lapply(seq_along(split_aggr_clusters_dt), function(i) {
      
      plgid <- as.integer(names(split_aggr_clusters_dt)[i])
      clustered_dt <- split_aggr_clusters_dt[[i]]
      
      # Determine save path
      save_path <- get_acc_input_save_path(plgid, "clustered_trees", clean_data_base_path)
      
      # Return the result
      list(name = "clustered", plgid = plgid, data = list(clustered_dt), save_path = save_path)
      
    })
    
    cat("\n")
    print(paste0("Done."))
    
    return(clustered_acc_init_obj_list)
    
  })
}







create_acc_multiInitVar <- function() {
  
}


create_acc_siteInfo <- function() {
  
}





















