# This script is for transforming RAW_climate_data into CLEAN_climate_data.
# It is designed for processing the comparison data on gitlab in the Comparison/PREBAS folder.


source('scripts/settings.R')
source('./r/utils.R')


# Get climate data
climate_path <- paste0(config$VAR_RAW_climate_paths[config$VAR_climate_id])

# Get climate name from path without "RAW"
climate_name <- str_flatten(strsplit(sub(".*\\/([^\\/]+)\\..*", "\\1", climate_path), split="_")[[1]][-1], collapse = "_")

print(paste0("Climate name is ", climate_name))

# Load
climate_dt <- fread(climate_path)


# Print all column ranges
invisible(lapply(colnames(climate_dt), function(x){
  message(paste0(x))
  print(paste0(range(climate_dt[[x]])))
}))


# Count N of sites to find duplicates 
count_dt <- climate_dt[, .N, by = PlgID]
len_unique_site_counts <- length(unique(count_dt$N))

# Remove duplicate sites if they exist
if(len_unique_site_counts > 1) {
  print(paste0("Duplicate sites found. Removing..."))
  min_N <- count_dt[N!=min(N)]
  
  # Remove duplicate sites
  climate_dt <- climate_dt[!PlgID %in% min_N$PlgID]
  print(paste0("Removed sites: "))
  print(paste0(min_N$PlgID))
  
}



cat("\n")
print(paste0("Number of rows is ", nrow(climate_dt)))
print(paste0("Number of unique sites is ", length(unique(climate_dt$PlgID))))
print(paste0("Number of unique Lat Lons is ", uniqueN(climate_dt[, .(Latitude, Longitude)])))
cat("\n")
print(paste0("Column names are:"))
print(paste0(colnames(climate_dt)))
cat("\n")



# Find the index of the column with IDate class
logical_class_list <- lapply(names(climate_dt), function(x) class(climate_dt[[x]]) %in% c("IDate", "Date"))
date_col_idx <- which(lapply(logical_class_list, function(x) x[[1]])==T)
date_col <- names(climate_dt)[[date_col_idx]]

# Rename the column to 'time'
setnames(climate_dt, old = date_col, new = "time")

print(paste0("Column names are:"))
print(paste0(colnames(climate_dt)))
cat("\n")


# Get co2 data
co2_name <- "co2_annual_1850_2100_combined_historic_ssp370"
co2_path <- paste0(config$PATH_co2, co2_name, ".csv")

# Filter co2 years
range_years <- format(range(unique(climate_dt$time)),"%Y")
co2_data <- fread(co2_path)[year >= range_years[1] & year <= range_years[2]]


# # Drop unnecessary cols
keep_cols <- c("PlgID", "time", "pr", "rsds", "tas", "vpd", "climID")
rm_cols <- colnames(climate_dt)[!colnames(climate_dt) %in% keep_cols]
climate_dt[, (rm_cols) := NULL]

# Get climate IDs from siteInfo
soil_name <- "prebas_hwsd_data_fc_wp_depth"
site_info_path <- paste0(config$PATH_site_info, soil_name, "_", config$VAR_estimated_names[config$VAR_estimated_id], ".rdata")
siteInfo_ids <- load_data(site_info_path)[,c(1:2)]

# Change colnames
colnames(climate_dt) <- c("siteID", "time", "precip", "qq", "tair", "vpd")

# Assign climate IDs from siteInfo
climate_dt <- merge(climate_dt, siteInfo_ids, by = "siteID", all.x = TRUE)

# # Vpd from p to kpa
climate_dt[, vpd := vpd/1000]

# Get rss from qq
climate_dt[, rss := qq*0.0864]

# Get par from rss
climate_dt[, par := rss*0.44*4.56]

# Convert precipitation from [kg.m-2.s-1] to mm/d (86400 seconds in day)
climate_dt[, precip := precip*86400]

# Convert tair from kelvin to celcius
climate_dt[, tair := tair-273.15]

# Assign year helper column to data
# data[, year := as.numeric(format(time, "%Y"))]
climate_dt[, year := year(time)]

# Set key and left join co2
setkey(climate_dt,"year")
climate_dt <- climate_dt[co2_data]

# Remove year helper column
climate_dt[, year := NULL]

# Remove NAs (WHEN DATA YEARS DIFFER FROM CO2 TABLE YEARS)
climate_dt <- climate_dt[complete.cases(climate_dt)]

# Filter prebas columns
climate_dt_prebas <- climate_dt[,c("time", "siteID", "climID", "par", "tair", "vpd", "precip", "co2")]


# SPLIT IDs

max_part_size <- 200     # Define rough size of a split part

split_by <- c("siteID")     # Define constraint
split_id_name <- "splitID"
split_dt <- split_dt_equal_with_constraint(climate_dt_prebas, max_part_size, split_by, split_id_name = split_id_name) # Assign splitIDs

# Name of file
prebas_climate_name <- paste0("CLEAN_", climate_name, "_splitID")

# # Write as csv
# prebas_climate_path_csv <- paste0("data/climate/provided/", prebas_climate_name, ".csv")
# fwrite(split_dt, prebas_climate_path_csv)

# Write as rdata
prebas_climate_path_rdata <- paste0("data/climate/provided/rdata/", prebas_climate_name, ".rdata")

cat("\n")
print(paste0("Saving climate data into ", prebas_climate_path_rdata, "..."))
save(split_dt, file = prebas_climate_path_rdata)
print(paste0("Done."))
cat("\n")

# # Clean up if not using yaml.runner
# keep_vars <- c("config", "config_path")
# remove_selected_variables_from_env(keep_vars)


