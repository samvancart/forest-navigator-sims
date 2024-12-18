source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


soil_dt <- fread(soil_file_path)
split_soil_dts <- split(soil_dt, by = "PlgID")

filtered_soil_dts <- lapply(names(split_soil_dts), function(id) {
  dt <- split_soil_dts[[id]]
  print(id)
  filtered_dt <- filter_data_by_tree_data_cells(data = dt, tree_data = aaa_file)
  sites <- filtered_dt[["BOKU_ID"]]
  nSites <- length(sites)
  base_clim_path <- file.path(clean_data_base_path, paste0("plgid_", id), "climate")
  clim_dirs <- list.files(base_clim_path)
  clim_dir_paths <- file.path(base_clim_path, clim_dirs)
  print(clim_dirs)
  print(nYears_lookup[clim_dirs])
  # nYears_list <- lapply(clim_dir_paths, get_nYears_from_acc_tran)
  # print(nYears_list)
  soil_depth <- filtered_dt$soil_depth*10
  fc <- filtered_dt$FC/soil_depth
  wp <- filtered_dt$WP/soil_depth
  print(fc)
  print(wp)
  filtered_dt
})




# Define the quantile breaks and corresponding values
quantiles <- quantile(soil_dt$N, probs = c(0.1, 0.3, 0.7, 0.9))
breaks <- c(0, unique(quantiles), max(soil_dt$N + 10))
labels <- c(5, 4, 3, 2, 1)

# Create the vector of y based on quantiles of x
site_type <- cut(soil_dt$N, breaks = breaks, labels = F)

# Convert the factor to numeric
soil_dt[, site_type := site_type]









nYears_lookup[["detrended"]]



c1 <- loadRDataFile(list.files(file.path(boku_data_path, "clustered"), full.names = T)[1])

aaa_all


# Sites to run
sites <- parTran[,1]

# Number of sites in this case matches the number of climIDs
nSites <- nrow(parTran)

# Number of simulation years
nYears <- floor(ncol(parTran)/365)





























