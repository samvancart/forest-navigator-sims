
### Libraries ###

library(devtools)

vPREBAS <- "master"
RprebassoFolder = "/projappl/project_2000994/Rpackages/Rprebasso_master"
.libPaths(c(RprebassoFolder,
            "/projappl/project_2000994/Rpackages/project_rpackages",
            .libPaths()))

install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

rm(vPREBAS, RprebassoFolder)

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



if(!exists("config_path")) {
  # Path to config file
  config_path <- paste0("config.yaml")
}

# Load configuration file
config <- yaml.load_file(config_path)


