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
data <- fread(climate_path)

# Count N of sites to find duplicates 
count_dt <- data[, .N, by = PlgID]
len_unique_site_counts <- length(unique(count_dt$N))

# Remove duplicate sites if they exist
if(len_unique_site_counts > 1) {
  print(paste0("Duplicate sites found. Removing..."))
  min_N <- count_dt[N!=min(N)]
  
  # Remove duplicate sites
  data <- data[!PlgID %in% min_N$PlgID]
  print(paste0("Removed sites: "))
  print(paste0(min_N$PlgID))
  
}



# Print all column ranges
invisible(lapply(colnames(data), function(x){
  message(paste0(x))
  print(paste0(range(data[[x]])))
}))



cat("\n")
print(paste0("Number of rows is ", nrow(data)))
print(paste0("Number of unique sites is ", length(unique(data$PlgID))))
print(paste0("Number of unique Lat Lons is ", uniqueN(data[, .(Latitude, Longitude)])))
cat("\n")
print(paste0("Column names are:"))
print(paste0(colnames(data)))
cat("\n")

# Get co2 data
co2_name <- "co2_ssp370_annual_2015_2100"
co2_path <- paste0("data/climate/provided/", co2_name, ".csv")
co2_data <- fread(co2_path)


# Find the index of the column with IDate class
logical_class_list <- lapply(names(data), function(x) class(data[[x]]) %in% c("IDate", "Date"))
date_col_idx <- which(lapply(logical_class_list, function(x) x[[1]])==T)
date_col <- names(data)[[date_col_idx]]

# Rename the column to 'time'
setnames(data, old = date_col, new = "time")

print(paste0("Column names are:"))
print(paste0(colnames(data)))
cat("\n")

# # Drop unnecessary cols
keep_cols <- c("PlgID", "time", "pr", "rsds", "tas", "vpd", "climID")
rm_cols <- colnames(data)[!colnames(data) %in% keep_cols]
data[, (rm_cols) := NULL]

# Assign climate IDs
data[, climID := .GRP, by = PlgID]

# Change colnames
colnames(data) <- c("siteID", "time", "precip", "qq", "tair", "vpd", "climID")


# # Vpd from p to kpa
data[, vpd := vpd/1000]

# Get rss from qq
data[, rss := qq*0.0864]

# Get par from rss
data[, par := rss*0.44*4.56]

# Convert precipitation from [kg.m-2.s-1] to mm/d (86400 seconds in day)
data[, precip := precip*86400]

# Convert tair from kelvin to celcius
data[, tair := tair-273.15]

# Assign year helper column to data
# data[, year := as.numeric(format(time, "%Y"))]
data[, year := year(time)]

# Set key and left join co2
setkey(data,"year")
data <- data[co2_data]

# Remove year helper column
data[, year := NULL]

# Remove NAs (WHEN DATA YEARS DIFFER FROM CO2 TABLE YEARS)
data <- data[complete.cases(data)]

# Filter prebas columns
data_prebas <- data[,c("time", "siteID", "climID", "par", "tair", "vpd", "precip", "co2")]


# SPLIT IDs

max_part_size <- 200     # Define rough size of a split part

split_by <- c("siteID")     # Define constraint
split_id_name <- "splitID"
split_dt <- split_dt_equal_with_constraint(data_prebas, max_part_size, split_by, split_id_name = split_id_name) # Assign splitIDs

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


