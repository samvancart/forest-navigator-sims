source('scripts/settings.R')

# Climate scenario
# climate_name <- "GFDL-ESM4_SSP370"
# climate_name <- "UKESM1-0-LL_ssp370"
climate_name <- "detrend_climate_data_comparison_prebas_folder"

# Get climate data
climate_path <- paste0("data/climate/provided/", climate_name, ".csv")
data <- fread(climate_path)


# Print all column ranges
invisible(lapply(colnames(data), function(x){
  message(paste0(x))
  print(paste0(range(data[[x]])))
}))


# Get climate reference data
ref_data <- fread(config$PATH_prebas_gitlab)


# unique(ref_data$siteID)
# unique(data[PlgID %in% ref_data$siteID]$PlgID)


# Get co2 data
co2_name <- "co2_ssp370_annual_2015_2100"
co2_path <- paste0("data/climate/provided/", co2_name, ".csv")
co2_data <- fread(co2_path)


# # Drop unnecessary cols
# data <- data[, c(1,4,5,6,7,10,11)]
keep_cols <- c("PlgID", "time", "pr", "rsds", "tas", "vpd", "climID")
rm_cols <- colnames(data)[!colnames(data) %in% keep_cols]
data[, (rm_cols) := NULL]

# Assign climate IDs
data[, climID := .GRP, by = PlgID]

# Change colnames
# colnames(data) <- c("siteID", "time", "precip", "qq", "tair", "rh", "vpd", "climID")
colnames(data) <- c("siteID", "time", "precip", "qq", "tair", "vpd", "climID")

# Filter using reference table
data <- data[siteID %in% ref_data$siteID]

# # Vpd from hpa to kpa
# data[, vpd := vpd/10]

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


# Write as csv
prebas_climate_name <- paste0(climate_name, "_prebas")
prebas_climate_path_csv <- paste0("data/climate/provided/", prebas_climate_name, ".csv")
fwrite(data_prebas, prebas_climate_path_csv)

# Write as rdata
prebas_climate_path_rdata <- paste0("data/climate/provided/rdata/", prebas_climate_name, ".rdata")
save(data_prebas, file = prebas_climate_path_rdata)










######### -------------- DETREND DATA -------------- #########

# # Get prebas sites from detrend data
# climate_name <- "detrend_climate_data_comparison_prebas_folder"
# # climate_name <- "ICHEC-EC-EARTH_rcp85_3models_new"
# climate_path <- paste0("data/climate/provided/", climate_name, ".csv")
# # detrend_data <- fread(climate_path)
# data <- fread(climate_path)
# data <- data[Latitude>=53]
# length(unique(data$PlgID))
# (max(data$time)-min(data$time))/365
# colnames(data)
# 
# # PREBAS HWSD DATA
soil_name <- "prebas_hwsd_data"
soil_path <- paste0("data/soil/", soil_name, ".csv")
data <- fread(soil_path)
# 
# data[PlgID %in% (data[duplicated(data$PlgID)]$PlgID)]
# 
# 
# nrow(data)

# # Print all column ranges
# invisible(lapply(colnames(data), function(x){
#   message(paste0(x))
#   print(paste0(range(data[[x]])))
# }))



# # TO SF
# # Unique coords
# coords_dt <- data[!duplicated(data$PlgID), c("Latitude", "Longitude", "PlgID")]
# data_sf <- st_as_sf(coords_dt, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
# st_write(data_sf, "data/sf/historical_detrend_coords.shp")
# coords_dt[!which(coords_dt$Latitude<53)]


# detrend_prebas_sites <- detrend_data[(PlgID %in% data[,PlgID])]
# unique(detrend_prebas_sites$PlgID)
# unique(data$PlgID)
# unique(detrend_data$PlgID)
# 
# length(unique(data$PlgID))
# length(unique(detrend_prebas_sites$PlgID))
# length(which(unique(data$PlgID) %in% unique(detrend_data$PlgID)))
# which(unique(data$PlgID) %in% unique(detrend_data$PlgID))

######### -------------- END DETREND DATA -------------- #########





######### -------------- TEST -------------- #########

# # Drop unnecessary cols
# keep_cols <- c("PlgID", "time", "pr", "rsds", "tas", "hurs", "vpd")
# keep_cols_idxs <- which(colnames(data) %in% keep_cols)
# data <- data[, ..keep_cols_idxs]
# data <- data[PlgID %in% ref_data$siteID]
# 
# 
# 
# unique(ref_data$climID)
# climID_dt <- ref_data[,c("siteID", "climID")]
# colnames(climID_dt)[[1]] <- "PlgID"
# climID_dt <- climID_dt[!duplicated(climID_dt)]
# 
# data_climID <- left_join(data, climID_dt, by = "PlgID")
# colnames(data_climID) <- c("time", "siteID", "precip", "qq", "tair", "vpd", "climID")
# 
# data <- data_climID
# 
# unique(data_climID$PlgID)
# ref_data[siteID==7480366]


######### -------------- END TEST -------------- #########