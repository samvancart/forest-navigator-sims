source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')

# Transform Sweden shape file by nfi easting northing (Inspire column) to lat lons data.table



# Get shape file
path <- "data/nfi/sweden/shape_files/"
shape_file_name <- "se_1km.shp"
shape_file_path <- paste0(path, shape_file_name)
shape_file <- st_read(shape_file_path)


# Get nfi data
nfi_df <- fread(config$VAR_nfi_sweden_paths[1])
merge_by <- "Inspire"


# Get cellcodes from nfi data
cellcode <- unique(paste0("1kmE",gsub("_","N",nfi_df$Inspire)))

# Filter shape file by nfi cellcodes
filtered_cc <- filter(shape_file,shape_file$CELLCODE %in% cellcode)

# Get lat lons
lat_lons <- get_sf_centre_coords(filtered_cc)

# Format and merge
nfi_lat_lon <- lat_lons  %>%
  cbind(.,Inspire=filtered_cc$CELLCODE) %>% # Attach cellcodes column
  mutate(Inspire=gsub("1kmE","", gsub("N","_", Inspire))) %>% # Format cellcode names
  merge(nfi_df,.,by=merge_by) %>% # Merge with nfi data
  reframe(.[with(.,order(groupID,speciesID,clusterID)),]) %>% # Sort
  as.data.table()

# Round coords
round_by <- 5

nfi_lat_lon$lon <- round(nfi_lat_lon$lon,round_by)
nfi_lat_lon$lat <- round(nfi_lat_lon$lat,round_by)


# Plot
nfi_sites_plot <- get_shape_file_plot(backgroundData = shape_file,
                                      backgroundColour = "#006AA7",
                                      topData = filtered_cc,
                                      topColour = "#FECC02",
                                      size = 3)


# plot_path <- paste0("data/plots/nfi/sweden/sites.pdf")
# pdf(plot_path, width=14, height=7)
# print(nfi_sites_plot)
# dev.off()
