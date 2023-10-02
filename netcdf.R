source('settings.R')

# Functions

get_netcdf_by_nearest_coords <- function(netCdf_path, req_coords, req_var, siteIDs,
                                         time_var = "time", lon_var = "longitude", lat_var = "latitude", round_dec = 3) {
  # Open file
  nc <- nc_open(netCdf_path)
  
  # Get lon,lat and time. Round lon and lat and format lon
  dim_lon <- round(ncvar_get(nc, varid = lon_var),round_dec)
  dim_lon <- ifelse(dim_lon > 180, dim_lon - 360, dim_lon)
  dim_lat <- round(ncvar_get(nc, varid = lat_var),round_dec)
  dim_time <- ncvar_get(nc, time_var)
  
  # Format time to "yyyy-mm-dd"
  t_units <- ncatt_get(nc, "time", "units")
  t_ustr <- strsplit(t_units$value, " ")
  t_dstr <- strsplit(unlist(t_ustr)[3], "-")
  date <- ymd(t_dstr)
  dim_time <- date + dim_time
  
  # Coordinates matrix
  nc_coords <- as.matrix(expand.grid(dim_lon, dim_lat))
  
  # Get nearest coordinates indexes from nc_coords
  ind <- sapply(1:nrow(req_coords), function(x) {
    which.min(geosphere::distHaversine(req_coords[x, ], nc_coords))
  })
  
  # Get coordinates by index from nc_coords
  req_nc_coords <- round(nc_coords[ind,],round_dec)
  
  # Get lon and lat indexes in netcdf
  lon_idxs <- sapply(req_nc_coords[,1], function(x) which(dim_lon == x))
  lat_idxs <- sapply(req_nc_coords[,2], function(x) which(dim_lat == x))
  
  # Matrix of indexes of coordinates in netcdf
  coord_idxs <- cbind(lon_idxs,lat_idxs)
  
  # Get requested variable df as tibble
  system.time(
    df <- coord_idxs %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(row = 1:n()) %>% 
      dplyr::rowwise() %>% 
      dplyr::do({
        tmp <- ncdf4::ncvar_get(
          nc,
          varid = req_var,
          start = c(.$lon_idxs, .$lat_idxs, 1),
          count = c(1, 1, -1)
        )
        data.frame(siteID = siteIDs[.$row], time = dim_time, lon = dim_lon[lon_idxs[.$row]], lat = dim_lat[lat_idxs[.$row]],
                   req_var = tmp)
        
      })
  )
  
  nc_close(nc)
  
  # Change req_var column name to actual variable name
  colnames(df)[which(names(df) == "req_var")] <- req_var
  
  return(df)
  
}


get_nearest_coords_table <- function(netCdf_path, req_coords, 
                                     lon_var = "longitude", lat_var = "latitude", round_dec = 3) {
  
  # Open file
  nc <- nc_open(netCdf_path)
  
  # Get lon and lat. Round lon and lat and format lon
  dim_lon <- round(ncvar_get(nc, varid = lon_var),round_dec)
  dim_lon <- ifelse(dim_lon > 180, dim_lon - 360, dim_lon)
  dim_lat <- round(ncvar_get(nc, varid = lat_var),round_dec)

  
  # Coordinates matrix
  nc_coords <- as.matrix(expand.grid(dim_lon, dim_lat))
  
  # Get nearest coordinates indexes from nc_coords
  ind <- sapply(1:nrow(req_coords), function(x) {
    which.min(geosphere::distHaversine(req_coords[x, ], nc_coords))
  })
  
  # Get coordinates by index from nc_coords
  req_nc_coords <- round(nc_coords[ind,],round_dec)
  
  colnames(req_nc_coords) <- c("lon", "lat")
  
  return(req_nc_coords)
}




netCdf_file <- "chelsa_EU_tas_300arcsec_daily197901"
# netCdf_file <- "tg_ens_mean_0.1deg_reg_1995-2010_v27.0e"
# netCdf_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/sweden-may23/data/netcdf/vars/tg/",netCdf_file,".nc")
netCdf_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/netcdf/CHELSA_EU/tas/",netCdf_file,".nc")

path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/prebas_sites_coords.csv")
prebas_sites <- read.csv(path,header=T)
siteIDs <- prebas_sites$siteID


lon_var <- "lon"
lat_var <- "lat"
time_var <- "time"
req_var <- "tas"

req_coords <- cbind(prebas_sites$lon,prebas_sites$lat)

# Get table of nearest coords
coords <- get_nearest_coords_table(netCdf_path = netCdf_path, req_coords = req_coords, 
                                   lon_var = lon_var, lat_var = lat_var, round_dec = 3)

# coords_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/site/coords_to_get.csv")
# write.csv(coords, coords_path, row.names = F)

df <- get_netcdf_by_nearest_coords(netCdf_path = netCdf_path, req_coords = req_coords, req_var = req_var, 
                             siteIDs = siteIDs, time_var = time_var,lon_var = lon_var, lat_var = lat_var, round_dec = 3)

print(df)
print(sapply(df,anyNA))
