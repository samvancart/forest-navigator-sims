source('settings.R')










netCdf_file <- "tg_ens_mean_0.1deg_reg_1995-2010_v27.0e"
netCdf_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/sweden-may23/data/netcdf/vars/tg/",netCdf_file,".nc")

path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/climate/prebas_sites_coords.csv")
prebas_sites <- read.csv(path,header=T)


req_coords <- cbind(prebas_sites$lon,prebas_sites$lat)

nc <- nc_open(netCdf_path)
attributes(nc$var)$names

dim_lon <- round(ncvar_get(nc, "longitude"),3)
dim_lat <- round(ncvar_get(nc, "latitude"),3)
dim_time <- ncvar_get(nc, "time")

t_units <- ncatt_get(nc, "time", "units")
t_ustr <- strsplit(t_units$value, " ")
t_dstr <- strsplit(unlist(t_ustr)[3], "-")
date <- ymd(t_dstr)
dim_time <- date + dim_time
length(dim_time)


nc_coords <- as.matrix(expand.grid(dim_lon, dim_lat))



ind <- sapply(1:nrow(req_coords), function(x) {
  which.min(geosphere::distHaversine(req_coords[x, ], nc_coords))
})

req_nc_coords <- round(nc_coords[ind,],3)
req_nc_coords[1,]

lon_idxs <- sapply(req_nc_coords[,1], function(x) which(dim_lon == x))
lat_idxs <- sapply(req_nc_coords[,2], function(x) which(dim_lat == x))

coord_idxs <- cbind(lon_idxs,lat_idxs)
coord_idxs <- coord_idxs[1:3,]

siteIDs <- prebas_sites$siteID[rep(seq_len(nrow(prebas_sites)), each = length(dim_time))]

# for(i in 1:nrow(coord_idxs)) {
#   var1 <- ncvar_get(nc, "tg", start=c( lon_idxs[i], lat_idxs[i], 1), count=c(1,1,-1))
#   print(head(var1),1)
# }

system.time(
  dat.out <- coord_idxs %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(row = 1:n()) %>% 
    dplyr::rowwise() %>% 
    dplyr::do({
      tmp <- ncdf4::ncvar_get(
        nc,
        varid = "tg",
        start = c(.$lon_idxs, .$lat_idxs, 1),
        count = c(1, 1, -1)
      )
      # data.frame(lon = dim_lon[.$lon_idxs], lat = dim_lat[.$lat_idxs], time = as.character(dim_time),
      #            req_var = tmp)
      data.frame(location = as.character(.$row), date = 1:length(tmp),
                 req.var = tmp)

    })
)


df <- dat.out
unique(df[,2])
unique(df[,3])


# df$time <- sapply(df$date, function(x) as.character(dim_time[x]))



var1 <- ncvar_get(nc, "tg", start=c( lon_idxs, .$lat_idxs, 1), count=c(1,1,-1))
head(var1,5)

nc_close(nc)
