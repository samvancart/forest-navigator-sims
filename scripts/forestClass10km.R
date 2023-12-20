source('scripts/settings.R')
source('./r/utils.R')
source('./r/parallelProcess.R')



# LOAD FILES



# CORINE Sweden
raster_path <- paste0("data/corine/swedenTrees.tif")
se_raster <- raster:: raster(raster_path)

# Shape file Sweden 10km
path10 <- "data/nfi/sweden/shape_files/10km/"
filename10 <- "se_10km.shp"
filepath10 <- paste0(path10, filename10)
sf10 <- st_read(filepath10)



# PRE PROCESS



# Transform crs
sf_utm <- st_transform(sf10, st_crs(se_raster))

# Get available cores
# cores <- detectCores(logical = T)
cores <- parallelly::availableCores()

# Define sources
sources <- c("./r/utils.R")

# Define kwargs
fun_kwargs <- list(se_raster)



# PROCESS



# Get forest classes in 3 batches
forest_classes1 <- extract_forest_classes_10km(sf_utm, cores, 1, 2500, sources = sources, fun_kwargs = fun_kwargs) # 235.17 s
forest_classes2 <- extract_forest_classes_10km(sf_utm, cores, 2501, 5000, sources = sources, fun_kwargs = fun_kwargs) # 243.53 s
forest_classes3 <- extract_forest_classes_10km(sf_utm, cores, 5001, 7000, sources = sources, fun_kwargs = fun_kwargs) # 129.28 s



# POST PROCESS



# Bind rows
all_forest_classes <- bind_rows(forest_classes1,forest_classes2,forest_classes3)

# Add ID
all_forest_classes$id <- sf_utm$id

# Unlist column
all_forest_classes$forest_class_10km <- unlist(all_forest_classes$forest_class_10km)

# Join with original sf
sf_forest_class <- left_join(sf_utm, all_forest_classes, by="id")



# WRITE FILE



# # Path
# path <- paste0("data/nfi/sweden/shape_files/10km/se_10km_forest_class.shp")
# 
# # Write sf
# st_write(sf_forest_class, path)















