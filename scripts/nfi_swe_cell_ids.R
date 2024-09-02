# Script to assign IDs to nfi plots based on the 1-by-1 km grid cells
# they are in.

# Load libs and config
source('scripts/settings.R')

# Nfi plots
se_nfi_path <- "1km/se_1km_sites_n.shp"
sf_nfi <- st_read(paste0(config$PATH_swe_sf,"/", se_nfi_path))


# 1-by-1 grid
se_1km_id10km_path <- "1km/se_1km_id10km.shp"
sf_1 <- st_read(paste0(config$PATH_swe_sf,"/", se_1km_id10km_path))

# Set crs
sf_nfi <- st_transform(sf_nfi, crs = st_crs(sf_1))

# Nfi centroids
sf_nfi_centroids <- st_centroid(sf_nfi)

# Join IDs
sf_nfi_centroids_id <- st_join(sf_nfi_centroids, sf_1[, c("cell_id", "id_10km")], join = st_within)

# Back to polygons
sf_nfi_id <- sf_1[which(sf_1$cell_id %in% sf_nfi_centroids_id$cell_id),]

# # Write files
# sf_nfi_id_path <- paste0(config$PATH_swe_sf, "/1km")
# st_write(sf_nfi_id, dsn = sf_nfi_id_path, layer = "se_1km_sites_n_id", driver = "ESRI Shapefile")



