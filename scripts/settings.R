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

# .libPaths(c(RprebassoFolder,
#             "/projappl/project_2000994/Rpackages/project_rpackages",
#             .libPaths()))




# UPDATE TO TEMPORARY DIR -------------------------------------------------


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


