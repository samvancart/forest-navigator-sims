# This script is for zipping files that are stored in Allas. 
# The files belonging to a unique combination of climate scenario and management scenario
# are grouped together. Each group can be processed as a separate array job. 
# Currently paralellisation is not supported for the compression phase but only
# for loading the files to zip from ALlas.



# SOURCE_FILES -------------------------------------------------------------



source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# PARSE_ARGS --------------------------------------------------------------

option_list <- list(
  make_option(c("-c", "--countries"), type = "character", default = NA,
              help = "Country names or abbreviations (e.g., 'FI' or 'Finland' or multiple e.g., 'se,FI' or 'Sweden, finland')"),
  make_option(c("-o", "--output_type"), type = "character", default = "output_files", help = "output file type either output_files or dbh_classes.")
)

parser <- OptionParser(option_list = option_list)
args <- parse_args(parser)

assert_true(c("output_type") %in% names(args))

args$countries <- "SE"

countries_arg <- args$countries
countries <- if (is.na(countries_arg)) NA else strsplit(countries_arg, ",")[[1]]
countries <- trimws(countries)  # Remove spaces around items

resolved_country_codes <- resolve_countries_from_lookup(lookup = country_codes, countries = countries)
res_country_codes_str <- resolved_country_codes$country_codes_str
country_codes_str <- ifelse(is.null(res_country_codes_str), "", res_country_codes_str)

output_file <- file.path(args$output_type, country_codes_str)

print("country_codes_str")
print(country_codes_str)
print("output_file")
print(output_file)

# CREATE_TEST_ZIP_PATH -------------------------------------------------------------



zip_file_path <- paste0("data/acc/output/", simulation_site, "/zip_test")
if(!dir.exists(zip_file_path)) dir.create(zip_file_path)



# ARRAY_JOB-PARAMS ----------------------------------------------------------


array_jobID <- get_parameter("SLURM_ARRAY_TASK_ID", 1, "integer")
max_array_jobID <- get_parameter("SLURM_ARRAY_TASK_COUNT", 1, "integer")

print(paste0("Array job: ", array_jobID))
print(paste0("Max array jobs: ", max_array_jobID))

# GET_FILES_TO_ZIP -----------------------------------------------------------


bucket_list_prefix <- file.path("output", simulation_site, paste0(output_file, "/"))



# List all output files that are in Allas using custom function
bucket_list <- list_all_objects_in_bucket(bucket = allas_opts$bucket, 
                                region = allas_opts$opts$region, 
                                prefix = bucket_list_prefix,
                                only_keys = T)



filtered_bucket_list <- grep(man_name, bucket_list, value = T)


# CREATE_SPLIT_RUN-TABLES ------------------------------------------------------------



# Split run tables into groups by combination of clim_scen and man_scen
zip_dts <- get_split_grouped_output_dt(filtered_bucket_list, zip_folder_name =  "zip_test")


# Select one table from zip_dts
zip_dt <- zip_dts[[array_jobID]]

## For testing
# zip_dt <- head(zip_dts[[array_jobID]], n = 50)



# SET_ZIP-PARAMS ------------------------------------------------------------



# When writing to allas zipfile is the name of the file. The files are grouped by clim_scen
# and man_scen so zip_dt should only have one combination e.g. "gwl2_bau.zip.
zipfile <- basename(zip_dt$full_zip_path[1])

# aws.S3 function args for save_object and put_object
save_or_put_opts <- list(bucket = allas_opts$bucket, region = allas_opts$opts$region, multipart = TRUE)

# move_to_path is the s3 object key without the filename (In other words the "directory path" inside the S3 bucket)
move_to_path_output_file <- file.path(country_codes_str, args$output_type)
move_to_path <- file.path("output", simulation_site, "zip", move_to_path_output_file)

print("move_to_path")
print(move_to_path)

# RUN ---------------------------------------------------------------------

# Load files to temp_dir, zip and then write
load_zip_move(zipfile = zipfile, 
              files = zip_dt$path,
              zip_opts = list(FUN = utils::zip,
                              extra_FUN_args = list(flags = "-u")), # Faster with -u (update) flag
              move_to_path = move_to_path,
              save_or_put_opts = save_or_put_opts,
              cores = cores,
              type = type)

















