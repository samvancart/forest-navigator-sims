source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


aws.s3::bucket_list_df(region = "")
bucket_name <- "2000994-forest_navigator"
region <- Sys.getenv("AWS_REGION")

bucket_list <- as.data.table(get_bucket_df(bucket_name, region = region))$Key
filtered_bucket_list <- bucket_list[!grepl(pattern = ".keep", bucket_list)]


c_path <- filtered_bucket_list[601]

t_c <- s3read_using(FUN = fread, object = c_path, bucket = bucket_name, opts = list(region = region))

