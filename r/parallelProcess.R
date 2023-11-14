source('scripts/settings.R')
source('./r/utils.R')


# Parallel process
get_in_parallel <- function(data, fun, libs, sources, ...){
  
  print(paste0("Parallel processing..."))
  
  # Parallel process file
  cl <- makeCluster(length(data), type="SOCK")
  registerDoParallel(cl)
  t <- system.time(
    result <- foreach(df = data) %dopar% {
      # Load libraries
      lapply(libs, require, character.only=T)
      
      # Source files
      lapply(sources, source)
      
      # Call function
      fun(df, ...)
    }
  )
  
  print("Done.")
  print(t)
  
  # Remember to stop cluster!
  on.exit(parallel::stopCluster(cl))
  
  return(bind_rows(result))
  
}



split_df_equal <- function(...){
  
  kwargs <- (...)
  df <- kwargs$df
  n <- kwargs$n
  
  print(paste0("Splitting frame..."))
  list <- split(df, factor(sort(rank(row.names(df))%%n)))
  print("Done.")
  
  return(list)
}



test_fun <- function(data, fun, ...) {
  kwargs <- list(...)
  args <-kwargs$fun_args
  # print(args$n)
  fun(args)
}


# Get shape file
path <- "data/nfi/sweden/shape_files/1km/"
shape_file_name <- "se_1km.shp"
shape_file_path <- paste0(path, shape_file_name)
sf <- st_read(shape_file_path)

cores <- detectCores(logical = T)
data <- split_df_equal(list(df=sf,n=cores))



test_fun(sf, split_df_equal, fun_args=list(df=sf,n=cores))


libs <- c("data.table", "sf", "dplyr")
sources <- c("./r/utils.R")




lat_lons_1by1 <- get_in_parallel(data, fun = get_sf_centre_coords, libs = libs, sources = sources)
dt_1 <- data.table(lat_lons_1by1)
m_1 <- as.matrix(dt_1)

















