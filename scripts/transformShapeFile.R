source('scripts/settings.R')


# Functions

get_grid_centre <- function(df, x, y) {
  y_centre <- max(y) - ((max(y)-min(y))/2)
  x_centre <- max(x) - ((max(x)-min(x))/2)
  return(data.table(x=x_centre,y=y_centre))
}


# Get shape file
path <- "data/nfi/sweden/shape_files/"
shape_file_name <- "se_1km.shp"
shape_file_path <- paste0(path, shape_file_name)
shape_file <- st_read(shape_file_path)

# st_crs(shape_file)
# "EPSG:4326"
# "EPSG:4258"

# Get nfi data
nfi_df <- read.csv(nfi_sweden_paths[1])
merge_by <- "Inspire"


# Get cellcodes from nfi data
cellcode <- unique(paste0("1kmE",gsub("_","N",nfi_df$Inspire)))

# Filter shape file by nfi cellcodes
filtered_cc <- filter(shape_file,shape_file$CELLCODE %in% cellcode)

# Original bounds for testing
lat_lon <- filtered_cc %>%
  st_transform(4258) %>%
  st_coordinates() %>%
  as.data.table() %>%
  unique()

nrow(filtered_cc)
head(lat_lon)
class(lat_lon)
f<-lat_lon[L2==max(L2)]

for(i in 1:nrow(f)){
  print(paste(f[i]$Y,",",f[i]$X,",1"))
}

# Get lat lons
nfi_lat_lon <- filtered_cc %>%
  st_transform(4258) %>% # Transform to desired coordinate reference system
  st_coordinates() %>% # Get all coordinates (bboxes of grid cells)
  as.data.frame() %>%
  group_by(L2) %>% # Group by bboxes
  unique() %>% # Get rid of duplicates
  reframe(get_grid_centre(cur_data(),X,Y)) %>% # Get grid centre coords
  rename(c("lon"="x","lat"="y")) %>%
  ungroup() %>%
  cbind(.,Inspire=filtered_cc$CELLCODE) %>% # Bind cellcodes column
  mutate(Inspire=gsub("1kmE","", gsub("N","_", Inspire))) %>% # Format cellcode names
  select(.,-L2) %>% # Drop L2 column
  merge(nfi_df,.,by=merge_by) %>% # Merge with nfi data
  reframe(.[with(.,order(groupID,speciesID,clusterID)),]) %>% # Sort
  as.data.table()

head(data.table(nfi_lat_lon))
# nfi_lat_lon_insp <- data.table(Inspire=nfi_lat_lon$Inspire,groupID=nfi_lat_lon$groupID)
insp_rows <- nfi_lat_lon %>%
  group_by(Inspire,groupID) %>%
  count() %>%
  ungroup()

unique(nfi_lat_lon$speciesID)





ref<-nfi_lat_lon[Inspire=="4675_3690",.(lon,lat)][1]
nfi_lat_lon$dlonX <- ref$lon - nfi_lat_lon$lon
nfi_lat_lon$dlatX <- ref$lat - nfi_lat_lon$lat

nfi_lat_lon[,eucDist := dlonX^2 + dlatX^2]
distX <- sort(unique(nfi_lat_lon$eucDist))[1]

hist(nfi_lat_lon$Height,freq = 0,breaks = 20)
hist(nfi_lat_lon[eucDist %in% distX]$Height,add=T,col=2,freq = 0,breaks = 20)

length(unique(insp_rows$groupID))


# DOUBLE CHECK THIS
nfi_lat_lon[,BAtot := sum(basal_area),by=groupID]

nfi_lat_lon[,sum(basal_area)/BAtot,by=groupID]


# Plot
nfi_sites <- ggplot() + 
  geom_sf(data = shape_file, size = 3, color = "#006AA7") +
  geom_sf(data = filtered_cc, size = 3, color = "#FECC02")

plot_path <- paste0("data/plots/nfi/sweden/sites.pdf")
pdf(plot_path, width=14, height=7)
print(nfi_sites)
dev.off()
