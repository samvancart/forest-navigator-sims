
# Functions

#' Extract Data from NetCDF by Nearest Coordinates
#'
#' This function extracts data from a NetCDF file based on the nearest coordinates to the requested coordinates.
#'
#' @param netCdf_path Character. Path to the NetCDF file.
#' @param req_coords Matrix. A matrix of requested coordinates with columns for longitude and latitude.
#' @param req_var Character vector. Names of the variables to extract from the NetCDF file.
#' @param siteIDs Character vector. Optional. Site IDs for the requested coordinates. If NULL, site IDs will be generated.
#' @param time_var Character. Name of the time variable in the NetCDF file. Default is "time".
#' @param lon_var Character. Name of the longitude variable in the NetCDF file. Default is "longitude".
#' @param lat_var Character. Name of the latitude variable in the NetCDF file. Default is "latitude".
#' @param round_dec Integer. Number of decimal places to round the coordinates. Default is 3.
#' @param req_nc_coords Matrix. Optional. A matrix of coordinates in the NetCDF file. If NULL, coordinates will be determined based on the nearest neighbour.
#'
#' @return A tibble containing the extracted data with columns for site ID, time, longitude, latitude, and the requested variables.
#' @import ncdf4
#' @importFrom geosphere distHaversine
#' @importFrom lubridate ymd
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate rowwise do
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' \dontrun{
#' netCdf_path <- "path/to/netcdf/file.nc"
#' req_coords <- matrix(c(10, 20, 30, 40), ncol = 2)
#' req_var <- c("temperature", "humidity")
#' data <- get_netcdf_by_nearest_coords(netCdf_path, req_coords, req_var)
#' }
get_netcdf_by_nearest_coords <- function(netCdf_path, req_coords, req_var, siteIDs = NULL ,time_var = "time", 
                                         lon_var = "longitude", lat_var = "latitude", round_dec = 3, req_nc_coords = NULL) {
  # Open file
  nc <- nc_open(netCdf_path)
    
  # Produce siteIDs if none are provided
  ifelse(is.null(siteIDs), siteIDs <- 1:nrow(req_coords), siteIDs<-siteIDs)
  
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
  
  # Get coordinates by index in netcdf. Skip if provided.
  if (is.null(req_nc_coords)) {
    # Coordinates matrix
    nc_coords <- as.matrix(expand.grid(dim_lon, dim_lat))
    
    # Get indexes in netcdf for nearest neighbour coordinates
    ind <- sapply(1:nrow(req_coords), function(x) {
      which.min(geosphere::distHaversine(req_coords[x, ], nc_coords))
    })
    
    # Get coordinates by index from nc_coords
    req_nc_coords <- round(nc_coords[ind,],round_dec)

  }
  
  # Get lon and lat indexes in netcdf
  lon_idxs <- sapply(req_nc_coords[,1], function(x) which(unique(dim_lon) == x))
  lat_idxs <- sapply(req_nc_coords[,2], function(x) which(unique(dim_lat) == x))
  
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

          data.frame(siteID = siteIDs[.$row], time = dim_time, lon = req_nc_coords[.$row,1], 
                     lat = req_nc_coords[.$row,2], var = tmp, row.names = NULL)
          
        })
    )
    # Change req_var column name to actual variable name
    colnames(df)[which(names(df) == "var")] <- req_var[i]
    
    # Build tibble with all requsted variables
    ifelse(i==1, df_all<-df, df_all<-as_tibble(cbind(df_all,df[ncol(df)])))
  }
  
  nc_close(nc)
  
  
  return(df_all)
  
}


# Get a table of nearest neighbour coordinate indexes in a netcdf file.
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


# Call get_netcdf_by_nearest_coords function with params.
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



