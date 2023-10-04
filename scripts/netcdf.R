source('scripts/settings.R')

# Functions

get_netcdf_by_nearest_coords <- function(netCdf_path, req_coords, req_var, siteIDs,time_var = "time", 
                                         lon_var = "longitude", lat_var = "latitude", round_dec = 3, req_nc_coords = NULL) {
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
  
  
  if (is.null(req_nc_coords)) {
    # Coordinates matrix
    nc_coords <- as.matrix(expand.grid(dim_lon, dim_lat))
    
    # Get nearest coordinates indexes from nc_coords
    ind <- sapply(1:nrow(req_coords), function(x) {
      which.min(geosphere::distHaversine(req_coords[x, ], nc_coords))
    })
    
    # Get coordinates by index from nc_coords
    req_nc_coords <- round(nc_coords[ind,],round_dec)
  }
  
  # Get lon and lat indexes in netcdf
  lon_idxs <- sapply(req_nc_coords[,1], function(x) which(dim_lon == x))
  lat_idxs <- sapply(req_nc_coords[,2], function(x) which(dim_lat == x))
  
  # Unlist if necessary
  lon_idxs <- ifelse(class(lon_idxs) == "list", unlist(lon_idxs), lon_idxs)
  lat_idxs <- ifelse(class(lat_idxs) == "list", unlist(lat_idxs), lat_idxs)
  
  # Matrix of indexes of coordinates in netcdf
  coord_idxs <- cbind(lon_idxs,lat_idxs)
  
  df_all <- data.table()
  
  # Get requested variables tibble
  for(i in 1:length(req_var)) {
    system.time(
      df <- coord_idxs %>% 
        tibble::as_tibble() %>% 
        dplyr::mutate(row = 1:n()) %>% 
        dplyr::rowwise() %>% 
        dplyr::do({
          tmp <- ncdf4::ncvar_get(
            nc,
            varid = req_var[i],
            start = c(.$lon_idxs, .$lat_idxs, 1),
            count = c(1, 1, -1)
          )
          data.frame(siteID = siteIDs[.$row], time = dim_time, lon = dim_lon[lon_idxs[.$row]], lat = dim_lat[lat_idxs[.$row]],
                     var = tmp)
          

        })
    )
    # Change req_var column name to actual variable name
    colnames(df)[which(names(df) == "var")] <- req_var[i]

    ifelse(i==1,df_all<-df,df_all<-as_tibble(cbind(df_all,df[5])))
  }

  nc_close(nc)

  
  return(df_all)
  
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



get_netcdf <- function(netCdf_path,sites_path, req_var, 
                       req_coords, lon_var="longitude",lat_var="latitude",
                       time_var = "time", coords = NULL) {

  prebas_sites <- read.csv(sites_path,header=T)
  siteIDs <- prebas_sites$siteID
  
  req_coords <- cbind(prebas_sites$lon,prebas_sites$lat)
  
  df <- get_netcdf_by_nearest_coords(netCdf_path = netCdf_path, req_coords = req_coords, req_var = req_var, 
                                      siteIDs = siteIDs, time_var = time_var,
                                      lon_var = lon_var, lat_var = lat_var, round_dec = 3, req_nc_coords = coords)
  return(df)
}



chelsa_df <- get_netcdf(chelsa_path,
                        sites_path,
                        req_var = c("pr","rsds","tas","tasmax","tasmin"),
                        # req_var = c("pr","tasmin"),
                        lon_var = "lon",
                        lat_var = "lat",
                        time_var = "time",
                        coords = NULL)

eobs_df <- get_netcdf(eobs_path,
                        sites_path,
                        req_var = c("hu","qq","rr","tg","tn", "tx"),
                        lon_var = "lon",
                        lat_var = "lat",
                        time_var = "time",
                        coords = NULL)

print(chelsa_df)
print(sapply(chelsa_df,anyNA))
print(eobs_df)
print(sapply(eobs_df,anyNA))
nrows_na <- nrow(eobs_df[is.na(eobs_df$hu),])
print(eobs_df[is.na(eobs_df$hu),],n=nrows_na)






