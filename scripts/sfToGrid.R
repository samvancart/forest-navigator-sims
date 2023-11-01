source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')


# Create 10km by 10km grid and assign gridIDs for 1km by 1km pixels 


# Get shape file
path <- "data/nfi/sweden/shape_files/"
shape_file_name <- "se_1km.shp"
shape_file_path <- paste0(path, shape_file_name)
shape_file <- st_read(shape_file_path)

# Get nfi data
nfi_df <- fread(paste0(nfi_sweden_path,"forest_classes.csv"))


# Get cellcodes from nfi data
cellcode <- unique(paste0("1kmE",gsub("_","N",nfi_df$Inspire)))

# Filter shape file by nfi cellcodes
filtered_cc <- filter(shape_file,shape_file$CELLCODE %in% cellcode)

# Get 10by10km grid
grid = st_as_stars(st_bbox(shape_file), dx = 10000, dy = 10000)
grid = st_as_sf(grid)


# Get lat lons
lat_lons_10by10 <- get_sf_centre_coords(sf=grid)
dt_10 <- data.table(lat_lons_10by10)
m_10 <- as.matrix(dt_10)
# Assign IDs to 10by10 based on index
dt_10[, ID := .GRP, by=list(lon,lat)]


# Get lat lons
lat_lons_1by1 <- get_sf_centre_coords(sf=filtered_cc)
dt_1 <- data.table(lat_lons_1by1)
m_1 <- as.matrix(dt_1)

# Get 10by10 table indexes for nearest neighbour coordinates of 1by1 table
ind <- sapply(1:nrow(m_1), function(x) {
  which.min(geosphere::distHaversine(m_1[x, ], m_10))
})

# Get unique 10by10 coordinate points
filtered_10by10 <- dt_10[c(unique(ind)),]
nrow(filtered_10by10)

# Assign gridIDs to 1by1
dt_1$gridID <- ind


# Get rid of rounding errors
round_by <- 5
dt_1$lon <- round(dt_1$lon,round_by)
dt_1$lat <- round(dt_1$lat,round_by)

nrow(dt_1)
nrow(unique(nfi_df[,c("lon", "lat")]))

# Add gridID column to nfi
nfi_gridId_df <- merge(x=nfi_df, y=dt_1, by.x = c("lon","lat"), by.y = c("lon","lat"))

# Sort
nfi_gridId_df <- nfi_gridId_df[with(nfi_gridId_df, order(groupID,speciesID,clusterID)),]




# Filter gridIDs


# Get gridID counts
n_gridIDs <- dt_1 %>% count(gridID)
n_gridIDs <- data.table(n_gridIDs)
n_gridIDs[which(n==max(n))]

# Which gridID to filter by
by_gridID <- 9960

# Get 1by1 cells based on gridID
sites_cc <- paste0("1kmE",gsub("_","N",  unique(nfi_gridId_df[gridID == by_gridID]$Inspire)))
filtered_sites_cc <- filter(filtered_cc, filtered_cc$CELLCODE %in% sites_cc)

# Get 10by10 grid cell
filtered_grid <- grid[by_gridID,]

# Plot one
plot <- ggplot() + 
  geom_sf(data = filtered_grid, size = 3, color = "#FECC02", alpha=0.6) +
  geom_sf(data = filtered_sites_cc, size = 3, color = "white", fill = "#006AA7",)


setequal(unique(nfi_gridId_df$gridID), unique(dt_1$gridID))


# Plot all
nfi_sites_plot <- get_shape_file_plot(backgroundData = grid,
                                      backgroundColour = "#FECC02",
                                      topData = filtered_cc,
                                      topColour = "#006AA7",
                                      size = 3)







# plot_path <- paste0("data/plots/nfi/sweden/sites_on_10by10_grid.pdf")
# pdf(plot_path, width=14, height=7)
# print(nfi_sites_plot)
# dev.off()








