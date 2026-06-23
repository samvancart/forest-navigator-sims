# LEGACY CODE

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
  
  clustered_acc_init_obj <- run_acc_with_combine_args(FUN = create_acc_clustered_tree_data,
                                                      acc_input_obj = tree_data_acc_input_obj,
                                                      plgid_vec = plgid_vec,
                                                      aaa_file = aaa_file,
                                                      clean_data_base_path = clean_data_base_path,
                                                      get_in_parallel_args = get_in_parallel_args)
  
  
  clim_acc_init_obj_list <- run_acc_with_combine_args(FUN = create_acc_clim_data,
                                                  acc_input_obj = clim_data_acc_input_obj,
                                                  plgid_vec = plgid_vec,
                                                  aaa_file = aaa_file,
                                                  clean_data_base_path = clean_data_base_path,
                                                  get_in_parallel_args = get_in_parallel_args,
                                                  config = config,
                                                  allas_opts = allas_opts,
                                                  clim_paths = clim_paths)
  
  return(clim_acc_init_obj_list)
}










create_acc_multiInitVar <- function() {
  
}


create_acc_siteInfo <- function() {
  
}





















