# This script is for loading necessary libraries and config






# LOAD_PREBAS_PUHTI ---------------------------------------------------------



library(devtools)

vPREBAS <- "master"
# vPREBAS <- "newVersion"

print("NOT UPDATING PREBAS")

if(vPREBAS=="master") {
  print("Checking master...")
  RprebassoFolder = "/projappl/project_2000994/Rpackages/Rprebasso_master"
} else {
  print("Checking newVersion...")
  RprebassoFolder = "/projappl/project_2000994/Rpackages/Rprebasso_newV"
}

.libPaths(c(RprebassoFolder,
            "/projappl/project_2000994/Rpackages/project_rpackages",
            .libPaths()))




# UPDATE TO TEMPORARY DIR -------------------------------------------------

print("UPDATING PREBAS INTO data/temp_prebas")
prebas_temp_path <- "/scratch/project_2000994/PREBASruns/finRuns/Rsrc/samuel/forest-navigator-sims/data/temp_prebas/"

.libPaths(c(prebas_temp_path,
            .libPaths()))


tryCatch({
  install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)
}, error = function(e) {
  message("",e)
})

rm(vPREBAS, RprebassoFolder)


# LOAD_LIBS ----------------------------------------------------------------


require(aws.s3)
require(bit64)
require(archive)
require(checkmate)
require(data.table)
require(dplyr)
require(doParallel)
require(factoextra)
require(foreach)
require(foreign)
require(geoTS)
require(geosphere)
require(ggplot2)
require(ggpubr)
require(gridExtra)
require(hash)
require(lubridate)
require(ncdf4)
require(optparse)
require(parallel)
require(parallelly)
require(purrr)
require(Rprebasso)
require(reshape2)
require(sf)
require(stars)
require(stringr)
require(testthat)
require(withr)
require(yaml)
require(zoo)


# LOAD_CONFIG --------------------------------------------------------------


config_path <- paste0("config.yaml")

# Load configuration file
config <- yaml.load_file(config_path)


# ALLAS ENV VARS ----------------------------------------------------------


bucket <- Sys.getenv("AWS_BUCKET")
region <- Sys.getenv("AWS_REGION")


# PARSE_ARGS --------------------------------------------------------------


# Define command-line options
option_list <- list(
  make_option(c("-a", "--array_id"), type="integer", default=as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1)), 
              help="SLURM array job ID [default: %default]"),
  
  make_option(c("-c", "--array_count"), type="integer", default=as.integer(Sys.getenv("SLURM_ARRAY_TASK_COUNT", unset = 1)),
              help="Total number of array jobs [default: %default]"),
  
  make_option(c("-c", "--countries"), type = "character", default = NA,
              help = "Country names or abbreviations (e.g., 'FI' or 'Finland' or multiple e.g., 'se,FI' or 'Sweden, finland')"),
  
  make_option(c("-o", "--output_type"), type = "character", default = "output_files", help = "output file type either output_files or dbh_classes.")
)

# Parse options
parser <- OptionParser(option_list = option_list)
args <- parse_args(parser)

print("Array id:")
print(args$array_id)
print("Max arrays:")
print(args$array_count)
print("countries:")
print(args$countries)
print("output type:")
print(args$output_type)















