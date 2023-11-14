source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')


# Create 10km by 10km grid and assign gridIDs for 1km by 1km pixels 


# Get shape file
path <- "data/nfi/sweden/shape_files/1km/"
shape_file_name <- "se_1km.shp"
shape_file_path <- paste0(path, shape_file_name)
shape_file <- st_read(shape_file_path)

# Get nfi data
nfi_df <- fread(paste0(nfi_sweden_path,"mix_types_originalSpeciesID.csv"))

nfi_df[Inspire=="4675_3690"]

# Get cellcodes from nfi data
cellcode <- unique(paste0("1kmE",gsub("_","N",nfi_df$Inspire)))

# Filter shape file by nfi cellcodes
filtered_cc <- filter(shape_file,shape_file$CELLCODE %in% cellcode)


# Get 10by10km grid
# grid = st_as_stars(st_bbox(shape_file), dx = 10000, dy = 10000)
# grid = st_as_sf(grid)
grid_path <- "data/nfi/sweden/shape_files/10km/se_10km.shp"
grid <- st_read(grid_path)


# Get lat lons
lat_lons_10by10 <- get_sf_centre_coords_parallel(sf=grid)
dt_10 <- data.table(lat_lons_10by10)
m_10 <- as.matrix(dt_10)
# Assign IDs to 10by10 based on index
# dt_10[, ID := .GRP, by=list(lon,lat)]
dt_10$id10 <- grid$id


# Get lat lons
# lat_lons_1by1 <- get_sf_centre_coords(sf=filtered_cc)
lat_lons_1by1 <- get_sf_centre_coords_parallel(sf=shape_file)
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
dt_1$gridID_10km <- ind


# Get rid of rounding errors
round_by <- 5
dt_1$lon <- round(dt_1$lon,round_by)
dt_1$lat <- round(dt_1$lat,round_by)

nrow(dt_1)
nrow(unique(nfi_df[,c("lon", "lat")]))

# Add gridID_10km column to nfi
nfi_gridId_df <- merge(x=nfi_df, y=dt_1, by.x = c("lon","lat"), by.y = c("lon","lat"))

# Sort
nfi_gridId_df <- nfi_gridId_df[with(nfi_gridId_df, order(groupID,speciesID,clusterID)),]

# Add gridID_1km column to nfi
nfi_gridId_df[, gridID_1km := .GRP, by="Inspire"]

unique(dt_1$gridID_10km)

# Filter gridIDs


# Get gridID counts
n_gridIDs <- dt_1 %>% count(gridID_10km)
n_gridIDs <- data.table(n_gridIDs)
n_gridIDs[which(n==max(n))]

# Which gridID_10km to filter by
by_gridID <- 6027 

# Get 1by1 cells based on gridID
sites_cc <- paste0("1kmE",gsub("_","N",  unique(nfi_gridId_df[gridID_10km == by_gridID]$Inspire)))
filtered_sites_cc <- filter(filtered_cc, filtered_cc$CELLCODE %in% sites_cc)

# Get number of forest sites in a 1km by 1km grid cell
site_counts <- nfi_gridId_df[gridID_10km==by_gridID] %>%
  group_by(Inspire) %>%
  reframe(.,count(unique(pick(groupID)))) %>%
  ungroup() %>%
  rename("CELLCODE"="Inspire")

# Add forest site counts to filtered sf table
site_counts$CELLCODE <- paste0("1kmE",gsub("_","N",  unique(site_counts$CELLCODE)))
filtered_sites_cc_n <- left_join(filtered_sites_cc, site_counts, by="CELLCODE")


# Get 10by10 grid cell
filtered_grid <- grid[by_gridID,]

# Get all 10by10 grid cells that contain 1by1 grid cells
filtered_grid_all <- grid[n_gridIDs$gridID_10km,]

# Test
setequal(unique(nfi_gridId_df$gridID_10km), unique(dt_1$gridID_10km))

unique(nfi_gridId_df[gridID_10km==6027]$gridID_1km)
nfi_gridId_df[gridID_1km==9344]



# Plot one
one_site_plot <- ggplot() + 
  geom_sf(data = filtered_grid, size = 3, colour = "#FECC02", alpha=0.6,) +
  xlab(expression(paste("Longitude"))) +
  ylab(expression(paste("Latitude"))) +
  geom_sf(data = filtered_sites_cc_n, size = 3, colour = "white", fill = "#006AA7",) +
  geom_sf_text(data = filtered_sites_cc_n, aes(label = n), colour = "#FECC02") +
  ggtitle(paste0("10 km by 10 km grid cell"), paste0("ID: ", by_gridID))



# Plot all on filtered grid
filtered_nfi_sites_plot <- get_shape_file_plot(backgroundData = filtered_grid_all,
                                               backgroundColour = "#FECC02",
                                               topData = filtered_cc,
                                               topColour = "#006AA7",
                                               size = 3)


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








