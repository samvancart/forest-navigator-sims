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
nfi_df <- read.csv(nfi_sweden_paths[1])
merge_by <- "Inspire"


# Get cellcodes from nfi data
cellcode <- unique(paste0("1kmE",gsub("_","N",nfi_df$Inspire)))

# Filter shape file by nfi cellcodes
filtered_cc <- filter(shape_file,shape_file$CELLCODE %in% cellcode)


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
