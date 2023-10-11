source('scripts/settings.R')
source('./r/sample.R')
source('./r/netcdf.R')
source('./r/multiSite.R')



# Get eobs df
path <- paste0("data/climate/extracted/eobs/leap_years_sampled_eobs.csv")
df <- as_tibble(read.csv(path, header=T))

# Prebas variables
prebas_df <- df[,c(1:4,7:8,12:14)]
prebas_df <- prebas_df %>%
  rename("precip"="rr",
         "tair"="tg",)

# # Write tran files for each variable
# vars <- colnames(prebas_df[5:ncol(prebas_df)])
# 
# for (v in vars) {
#   tran <- get_prebas_tran(prebas_df, v)
#   path <- paste0("data/climate/extracted/eobs/tran/",v,".csv")
#   print(paste0("Writing ",v,"..."))
#   write.csv(tran, path, row.names = F)
#   print("Done.")
# }





