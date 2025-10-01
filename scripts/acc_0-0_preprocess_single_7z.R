# This script is for extracting from a single 7z archive.


# SOURCE_FILES ------------------------------------------------------------


source('scripts/settings.R')
source('./r/utils.R')


# DEFINE PATHS ------------------------------------------------------------


input_7z_path <- "data/acc/input/simulation_sites_1km/raw/clim_data/7506291.7z"
output_path <- "data/acc/input/simulation_sites_1km/raw/clim_data/7506291"


# UNZIP -------------------------------------------------------------------


archive_extract(archive = input_7z_path, dir = output_path)



