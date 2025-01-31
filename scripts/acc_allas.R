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


run_acc_for_plgid <- function(plgid_vec) {
  print(plgid_vec)
}




create_acc_clustered_tree_data <- function(plgid_vec, aaa_file, clean_data_base_path, acc_input_obj, get_in_parallel_args){
  
  args <- acc_input_obj$args
  
  
  print(paste0("Getting tree_data..."))
  
  aaa_dt <- handle_data_input(aaa_file)
  filtered_aaa_dt <- aaa_dt[PlgID %in% plgid_vec]
  split_aaa <- split(filtered_aaa_dt, by = "PlgID")
  
  process_treedata_files_parallel_args <- 
      c(list(data = split_aaa,
             FUN = process_treedata_files,
             FUN_args = args$process_treedata_files),
      get_in_parallel_args)
  
  
  tree_data_dts <- do.call(get_in_parallel, process_treedata_files_parallel_args)
  
  cat("\n")
  cat("\n")
  print(paste0("Merging..."))
  
  args$assign_and_merge$tree_data_dts <- tree_data_dts
  dts <- do.call(assign_and_merge, args$assign_and_merge)
  
  
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
  split_aggr_clusters_dt <- split(aggr_clusters_dt, by = args$aaa_split_col)
  
  
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
}




create_acc_clim_data <- function() {
  
}


create_acc_multiInitVar <- function() {
  
}


create_acc_siteInfo <- function() {
  
}





















