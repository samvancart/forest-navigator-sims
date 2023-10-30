source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')

# Get shape file
path <- "data/nfi/sweden/shape_files/"
shape_file_name <- "se_1km.shp"
shape_file_path <- paste0(path, shape_file_name)
shape_file <- st_read(shape_file_path)

# Get nfi data
nfi_df <- fread(nfi_sweden_paths[1])

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



# Plot
nfi_sites_plot <- get_shape_file_plot(backgroundData = grid,
                                      backgroundColour = "#FECC02",
                                      topData = filtered_cc,
                                      topColour = "#006AA7",
                                      size = 3)







# plot_path <- paste0("data/plots/nfi/sweden/sites_on_10by10_grid.pdf")
# pdf(plot_path, width=14, height=7)
# print(nfi_sites_plot)
# dev.off()








