
### Libraries ###

library(checkmate)
library(data.table)
library(dplyr)
library(doParallel)
library(factoextra)
library(foreach)
library(foreign)
library(geoTS)
library(geosphere)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(hash)
library(lubridate)
library(ncdf4)
library(parallel)
library(parallelly)
library(purrr)
library(Rprebasso)
library(reshape2)
library(sf)
library(stars)
library(stringr)
library(withr)
library(yaml)
library(zoo)


if(!exists("config_path")) {
  # Path to config file
  config_path <- paste0("config.yaml")
}

# Load configuration file
config <- yaml.load_file(config_path)


