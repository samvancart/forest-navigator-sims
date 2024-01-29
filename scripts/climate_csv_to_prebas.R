source('scripts/settings.R')

# Climate scenario
# climate_name <- "GFDL-ESM4_SSP370"
climate_name <- "UKESM1-0-LL_ssp370"

# Get climate data
climate_path <- paste0("data/climate/provided/", climate_name, ".csv")
data <- fread(climate_path)

# Get climate reference data
ref_data <- fread(prebas_gitlab_path)

# Get co2 data
co2_name <- "co2_ssp370_annual_2015_2100"
co2_path <- paste0("data/climate/provided/", co2_name, ".csv")
co2_data <- fread(co2_path)


# Drop unnecessary cols
data <- data[, c(1,4,5,6,7,10,11)]

# Assign climate IDs
data[, climID := .GRP, by = PlgID]

# Change colnames
colnames(data) <- c("siteID", "time", "precip", "qq", "tair", "rh", "vpd", "climID")

# Filter using reference table
data <- data[siteID %in% ref_data$siteID]

# Get rss from qq
data[, rss := qq*0.0864]

# Get par from rss
data[, par := rss*0.44*4.56]

# Convert precipitation from [kg.m-2.s-1] to mm/d (86400 seconds in day)
data[, precip := precip*86400]

# Convert tair from kelvin to celcius
data[, tair := tair-273.15]

# Assign year helper column to data
data[, year := as.numeric(format(time, "%Y"))]

# Set key and left join co2
setkey(data,"year")
data <- data[co2_data]

# Remove year helper column
data[, year := NULL]

# Filter prebas columns
data_prebas <- data[,c("time", "siteID", "climID", "par", "tair", "vpd", "precip", "co2")]


# Write as csv
prebas_climate_name <- paste0(climate_name, "_prebas")
prebas_climate_path <- paste0("data/climate/provided/", prebas_climate_name, ".csv")
fwrite(data_prebas, prebas_climate_path)















