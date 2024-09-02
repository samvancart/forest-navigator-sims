# Script to assign IDs to 1-by-1 km grid cells based on the 10-by-10 km grid cells
# they fall into.

# Load libs and config
source('scripts/settings.R')

# 10-by-10 grid
se_10km_path <- "10km/se_10km_forest_class.shp"
sf_10 <- st_read(paste0(config$PATH_swe_sf,"/", se_10km_path))

# 1-by-1 grid
se_1km_path <- "1km/se_1km.shp"
sf_1 <- st_read(paste0(config$PATH_swe_sf,"/", se_1km_path))

# Make sure sf_1 rows are unique
setequal(length(unique(sf_1$CELLCODE)),length(sf_1$CELLCODE))

# Assign unique IDs
sf_1$cell_id <- 1:nrow(sf_1)

# 1km centroids
sf_1_centroids <- st_centroid(sf_1)

# Set 10km grid crs
sf_10 <- st_transform(sf_10, crs = st_crs(sf_1))

# Get 1-by-1 centroids within 10-by-10
sf_1_within <- st_join(sf_1_centroids, sf_10, join = st_within)

# 1-by-1 with 10-by-10 id
sf_1_centroids_id10 <- sf_1_within[, c(colnames(sf_1), "id")]

# Modify 10km id name
colnames(sf_1_centroids_id10)[which(colnames(sf_1_centroids_id10) == "id")] <- "id_10km"

# Convert back to polygons with added id_10km column 
sf_1_id10 <- st_join(sf_1, sf_1_centroids_id10[, c("id_10km")])

## Remove unused
# rm(sf_1, sf_1_centroids, sf_1_within, sf_10, sf_1_centroids_id10)
# gc()

## Write files
# sf_1_id10_path <- paste0(config$PATH_swe_sf, "/1km")
# st_write(sf_1_id10, dsn = sf_1_id10_path, layer = "se_1km_id10km", driver = "ESRI Shapefile")










